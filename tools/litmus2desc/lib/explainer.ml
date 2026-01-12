open Dot_parser

module A = LitmusTest.MakeArch (struct
  let is_morello = false
end)

module Instruction = struct
  type t = A.instruction

  let render = A.show_instruction

  let opcode_name ins =
    let str = A.show_instruction ~latex:false ins in
    let splitted = String.split_on_char ' ' str in
    List.hd splitted

  let english_name ins =
    match opcode_name ins with
    | "LDR" -> Some "Load"
    | "STR" -> Some "Store"
    | "LDAR" -> Some "Load Acquire"
    | "STLR" -> Some "Store Release"
    | "EOR" -> Some "Exclusive Or"
    | "ADD" -> Some "Add"
    | "CBNZ" -> Some "Branch"
    | _ -> None
end

module Component = struct
  type register_eq = { proc : int; reg_name : string; value : string }
  type mem_loc_eq = { mem_loc : string; value : int }
  type condition_eq = Register_eq of register_eq | Mem_loc_eq of mem_loc_eq

  type dep_path_event = {
    event : event;
    proc : int;
    poi : int;
    ins : Instruction.t;
  }

  type dependency = {
    name : string;
    path : dep_path_event list;
    source : regular_mem_event;
    target : regular_mem_event;
    has_matching_ob_edge : bool;
  }

  type located_component =
    | Memory_event of { ev : regular_mem_event; ins : Instruction.t }
    | Register_event of { ev : register_event; ins : Instruction.t }
    | Dependency of dependency

  type reg_condition_data =
    | Reg_load_init
    | Reg_load_mem_ev of {
        load_ev : regular_mem_event;
        source_store : memory_event;
        ca_target : memory_event option;
      }
    | Reg_load_reg_ev of {
        load_ev : register_event;
        source_store : register_event;
      }

  type mem_loc_condition_data = {
    store_ev : memory_event;
    ca_sources : memory_event list;
  }

  type condition =
    | Reg_condition of { eq : register_eq; data : reg_condition_data option }
    | Mem_condition of { eq : mem_loc_eq; data : mem_loc_condition_data option }

  type located = { proc : int; poi : int; component : located_component }

  let located_compare (l1 : located) (l2 : located) =
    Misc.pair_compare Int.compare Int.compare (l1.proc, l1.poi) (l2.proc, l2.poi)
end

module Description (C : sig
  val num_of_procs : int
  val num_of_execs : int
  val latex : bool
  val regular_mem_events_of_proc : Execution.dir -> proc:int -> int list
  val instructions_of_proc : proc:int -> (int * Instruction.t) list
  val instr_by_proc_poi : proc:int -> poi:int -> Instruction.t
end) =
struct
  open C

  let conjunctive_phrase =
    if num_of_execs > 1 then "because" else "which means that"

  let render_instruction ins = Instruction.render ~latex ins

  let describe_ins (ev : event) : string =
    let proc, poi =
      match ev with
      | Memory (Regular ev) -> (ev.proc, ev.poi)
      | Branching ev -> (ev.proc, ev.poi)
      | Register ev -> (ev.proc, ev.poi)
      | Fence ev -> (ev.proc, ev.poi)
      | Memory (Init _) -> failwith "cannot describe instruction of init event"
    in
    let ins = instr_by_proc_poi ~proc ~poi in
    let ins_name = Instruction.opcode_name ins in
    let instrs_of_type =
      instructions_of_proc ~proc
      |> List.filter (fun (_, other_ins) ->
          Instruction.opcode_name other_ins = ins_name)
    in
    let poi_description, in_po =
      if List.length instrs_of_type = 1 then ("", "")
      else
        let ix =
          Misc.List.find_index
            (fun (other_poi, _) -> poi = other_poi)
            instrs_of_type
          |> Option.get
        in
        (Util.verbalize_index ix ^ " ", "in program order ")
    in
    let ins_render_name =
      match Instruction.english_name ins with
      | Some name -> name
      | None -> ins_name
    in
    Format.sprintf "the %s%s instruction %son P%d" poi_description
      ins_render_name in_po proc

  let describe_mem_event (ev : regular_mem_event) (ins : Instruction.t) =
    let eff_ty = match ev.dir with R -> "Read" | W -> "Write" in
    let instr_desc =
      describe_ins (Memory (Regular ev)) |> String.capitalize_ascii
    in
    let ins_full = render_instruction ins in
    let value_descr =
      if ev.dir = W then Format.sprintf " with value %d" ev.value else ""
    in
    Format.sprintf
      "%s, i.e. %s, generates an Explicit Memory %s Effect of Location %s%s"
      instr_desc ins_full eff_ty ev.mem_loc value_descr

  let describe_reg_event (ev : register_event) (ins : Instruction.t) =
    let instr_desc = describe_ins (Register ev) |> String.capitalize_ascii in
    let ins_full = render_instruction ins in
    let eff_ty = if ev.dir = R then "Read" else "Write" in
    Format.asprintf
      "%s, i.e. %s, generates a Register %s Effect of Register %s with value %a"
      instr_desc ins_full eff_ty ev.reg.name Dot_parser.pp_reg_value ev.value

  let describe_dep (dep : Component.dependency) : string =
    let open Component in
    let source = dep.source in
    let target = dep.target in
    let path = dep.path in
    let describe_path_ev ev =
      let descr = describe_ins ev.event in
      let ins_full = render_instruction ev.ins in
      Format.sprintf "%s, i.e. %s," descr ins_full
    in
    let path_descrs =
      path |> List.map describe_path_ev |> String.concat " and "
      |> String.capitalize_ascii
    in
    let source_desc = describe_ins (Memory (Regular source)) in
    let target_desc = describe_ins (Memory (Regular target)) in
    let match_phrase =
      if dep.has_matching_ob_edge then
        let plural = if List.length path > 1 then "" else "s" in
        Format.sprintf "create%s" plural
      else "does not create"
    in
    let name_phrase =
      if dep.has_matching_ob_edge then
        let article =
          if List.mem (String.get dep.name 0) [ 'a'; 'i'; 'u'; 'e'; 'o' ] then
            "an"
          else "a"
        in
        Format.sprintf "%s %s dependency" article
          (String.capitalize_ascii dep.name)
      else "any dependency"
    in
    let from_descr =
      if dep.has_matching_ob_edge then
        Format.sprintf "from %s, i.e. %s, " source_desc source.instruction
      else ""
    in
    Format.sprintf "%s %s %s %sto %s, i.e. %s" path_descrs match_phrase
      name_phrase from_descr target_desc target.instruction

  let describe_located (c : Component.located) : string =
    match c.component with
    | Memory_event mev -> describe_mem_event mev.ev mev.ins
    | Register_event rev -> describe_reg_event rev.ev rev.ins
    | Dependency dep -> describe_dep dep

  let describe_ev (ev : event) : string =
    let print_proc = num_of_procs <> 1 in
    match ev with
    | Memory (Init ev) ->
        Format.sprintf "the initialization event of location %s" ev.mem_loc
    | Memory (Regular ev) ->
        let eff_ty, should_dump_instr =
          match ev.dir with
          | R ->
              let proc_loads = regular_mem_events_of_proc R ~proc:ev.proc in
              ("Read", List.length proc_loads > 1)
          | W ->
              let proc_stores = regular_mem_events_of_proc W ~proc:ev.proc in
              ("Write", List.length proc_stores > 1)
        in
        let proc_details =
          if print_proc then Format.sprintf " on P%d" ev.proc else ""
        in
        let loc_descr, instr_descr =
          if should_dump_instr then
            let instr_descr =
              let ins = instr_by_proc_poi ~proc:ev.proc ~poi:ev.poi in
              Format.sprintf " generated by %s" (render_instruction ins)
            in
            let loc_descr = Format.sprintf " of Location %s" ev.mem_loc in
            (loc_descr, instr_descr)
          else ("", "")
        in
        Format.sprintf "the Explicit Memory %s Effect%s%s%s" eff_ty loc_descr
          proc_details instr_descr
    | Register { proc; poi; dir; _ } ->
        let ins = instr_by_proc_poi ~proc ~poi in
        let proc_details =
          if print_proc then Format.sprintf " on P%d" proc else ""
        in
        let eff_ty = if dir = R then "Read" else "Write" in
        Format.sprintf
          "the Register %s Effect originating from the %s instruction%s" eff_ty
          (Instruction.opcode_name ins)
          proc_details
    | _ -> failwith "NIY"

  let describe_mem_loc_condition (eq : Component.mem_loc_eq)
      (data : Component.mem_loc_condition_data option) : string =
    let extra_text =
      match data with
      | Some data ->
          let ev = data.store_ev in
          let ca_sources_count = List.length data.ca_sources in
          let extra_text =
            if ca_sources_count = 0 then
              Format.sprintf "%s has updated the memory"
                (describe_ins (Memory ev))
            else
              let explained_ca_sources =
                data.ca_sources
                |> List.map (fun source_ev -> describe_ev (Memory source_ev))
              in
              let ca_source = String.concat " and " explained_ca_sources in
              if List.length data.ca_sources = 1 then
                Format.sprintf "%s is Coherence-before %s" ca_source
                  (describe_ev (Memory ev))
              else
                Format.sprintf "%s are Coherence-before %s" ca_source
                  (describe_ev (Memory ev))
          in
          Format.sprintf ", %s %s" conjunctive_phrase extra_text
      | None -> ""
    in
    Format.sprintf "The value of %s is %d in the end%s" eq.mem_loc eq.value
      extra_text

  let describe_reg_condition (eq : Component.register_eq)
      (data : Component.reg_condition_data option) : string =
    let preamble =
      Format.sprintf "The Register %s on P%d holds the value %s in the end"
        eq.reg_name eq.proc eq.value
    in
    let extra_text =
      match data with
      | Some (Reg_load_mem_ev data) ->
          let load_ev = data.load_ev in
          let load_ev_descr = describe_ev (Memory (Regular load_ev)) in
          let source_store = data.source_store in
          let source_value_descr, ca_phrase =
            match source_store with
            | Init _ ->
                let ca_phrase =
                  match data.ca_target with
                  | Some ca_target ->
                      let ca_target_descr = describe_ev (Memory ca_target) in
                      Format.sprintf ", and therefore is Coherence-before %s"
                        ca_target_descr
                  | None -> ""
                in
                ("the initial state", ca_phrase)
            | Regular ev -> (describe_ev (Memory (Regular ev)), "")
          in
          let rf_phrase =
            Format.sprintf ", %s %s Reads-from %s" conjunctive_phrase
              load_ev_descr source_value_descr
          in
          let full_phrase = rf_phrase ^ ca_phrase in
          full_phrase
      | Some (Reg_load_reg_ev data) ->
          let load_ev = data.load_ev in
          let load_ev_descr = describe_ev (Register load_ev) in
          let source_store = data.source_store in
          let source_store_descr = describe_ev (Register source_store) in
          Format.sprintf ", %s %s has read the value %s from %s"
            conjunctive_phrase load_ev_descr eq.value source_store_descr
      | Some Reg_load_init ->
          Format.sprintf ", %s it holds its initialization value."
            conjunctive_phrase
      | None -> ""
    in
    Format.sprintf "%s%s" preamble extra_text

  let describe_condition : Component.condition -> string = function
    | Mem_condition { eq; data } -> describe_mem_loc_condition eq data
    | Reg_condition { eq; data } -> describe_reg_condition eq data
end

module ExecData = struct
  type instruction = { proc : int; poi : int; ins : Instruction.t }

  type t = {
    instructions : instruction list;
    test : LitmusTest.test;
    events : event list;
    src_exec : Execution.t;
    mevent_map : memory_event IntMap.t;
    event_map : event IntMap.t;
    dgraph : EdgeGraph.t;
    edges : EdgeGraph.edge list;
  }

  let make test src_exec : t =
    let events = Dot_parser.events_of_execution src_exec in
    let mevent_map =
      IntMap.from_bindings
        (List.filter_map
           (function Memory ev -> Some (mevent_id ev, ev) | _ -> None)
           events)
    in
    let event_map =
      IntMap.from_bindings (List.map (fun ev -> (event_id ev, ev)) events)
    in
    let edges =
      let mk_edge (label : string) (src : Execution.rel_edge) : EdgeGraph.edge =
        let source = src.src.Execution.eiid in
        let target = src.tgt.Execution.eiid in
        { label; source; target }
      in

      let edges_iico : EdgeGraph.edge list =
        let map_one label rels = rels |> List.map (mk_edge label) in
        map_one "iico_data" src_exec.Execution.iico_data
        @ map_one "iico_ctrl" src_exec.iico_ctrl
      in

      let edges_vb : EdgeGraph.edge list =
        src_exec.viewed_before
        |> Misc.List.concat_map (fun (lbl, rels) ->
            rels |> List.map (mk_edge lbl))
      in

      let edges_po =
        match src_exec.Execution.visible_po with
        | None -> []
        | Some rels -> rels |> List.map (fun re -> mk_edge "po" re)
      in
      edges_iico @ edges_vb @ edges_po
    in

    let dgraph =
      let vertices = [] in
      EdgeGraph.make vertices edges
    in
    let instructions =
      let blocks = test.MiscParser.prog in
      blocks
      |> Misc.List.concat_map (fun ((proc, _, _), instrs) ->
          instrs
          |> List.filter_map (function
            | AArch64Base.Instruction ins -> Some ins
            | _ -> None)
          |> List.mapi (fun poi ins -> { proc; poi; ins }))
    in
    {
      instructions;
      test;
      src_exec;
      events;
      mevent_map;
      event_map;
      dgraph;
      edges;
    }

  let edges (t : t) = t.edges

  let find_reg_event_scalar_value (t : t) eiid =
    t.events
    |> Misc.List.find_map (function
      | Register { id; value = Scalar v; _ } when id = eiid -> Some v
      | _ -> None)

  let find_init_event_opt (t : t) loc =
    t.events
    |> Misc.List.find_map (function
      | Memory (Init { mem_loc; _ } as ev) when mem_loc = loc -> Some ev
      | _ -> None)

  let regular_mem_events_of_proc (t : t) dir proc =
    let open Execution in
    t.src_exec.events
    |> List.filter_map (function
      | { eiid; iiid = Index ix; act = Memory act; _ }
        when act.dir = dir && ix.proc = proc ->
          Some eiid
      | _ -> None)

  let find_event_opt (t : t) f = List.find_opt f t.events
  let find_event_by_id (t : t) eiid = IntMap.find eiid t.event_map
  let find_event_by_id_opt (t : t) eiid = IntMap.find_opt eiid t.event_map
  let find_mevent_by_id (t : t) eiid = IntMap.find eiid t.mevent_map
  let find_mevent_by_id_opt (t : t) eiid = IntMap.find_opt eiid t.mevent_map

  let find_reg_event_by_id (t : t) eiid =
    let item_opt =
      t.events
      |> Misc.List.find_map (function
        | Register ev when ev.id = eiid -> Some ev
        | _ -> None)
    in
    match item_opt with Some item -> item | None -> raise Not_found

  let instr_by_proc_poi (t : t) ~proc:target_proc ~poi:target_poi :
      Instruction.t =
    let { ins; _ } =
      t.instructions
      |> List.find (fun { proc; poi; _ } ->
          proc = target_proc && poi = target_poi)
    in
    ins

  let instrs_of_proc (t : t) target_proc =
    t.instructions
    |> List.filter_map (fun { proc; poi; ins } ->
        if proc = target_proc then Some (poi, ins) else None)
end

let conditions_of_test (exec_data : ExecData.t) : Component.condition list =
  let exec = exec_data.src_exec in
  let open Misc.Option.Infix in
  let open Component in
  let condition_of_reg proc reg_pretty : condition option =
    let* tgt =
      let open Execution in
      exec.rfmap
      |> Misc.List.find_map (function
        | RFMap.{ src = Final l; tgt }
          when location_equal l (Reg { proc; reg_pretty }) ->
            Some tgt
        | _ -> None)
    in
    let eq, data =
      match tgt with
      | Init ->
          let eq = { proc; reg_name = reg_pretty; value = failwith "NIY" } in
          (eq, Some Reg_load_init)
      | Store { eiid } ->
          let value =
            let ev = ExecData.find_reg_event_by_id exec_data eiid in
            Format.asprintf "%a" Dot_parser.pp_reg_value ev.Dot_parser.value
          in
          let data =
            let* load_ev_eiid =
              exec.iico_data
              |> Misc.List.find_map (function
                | Execution.{ src; tgt = { eiid = tgt_eiid } }
                  when tgt_eiid = eiid ->
                    Some src.eiid
                | _ -> None)
            in
            let* load_ev =
              ExecData.find_event_opt exec_data (fun ev ->
                  Dot_parser.is_load ev && event_id ev = load_ev_eiid)
            in
            match load_ev with
            | Memory (Regular load_ev) ->
                let* source_store_id =
                  ExecData.edges exec_data
                  |> List.find_opt (fun ed ->
                      ed.EdgeGraph.target = load_ev.id && ed.label = "rf")
                  |> Option.map (fun ed -> ed.EdgeGraph.source)
                in
                let source_store =
                  ExecData.find_mevent_by_id exec_data source_store_id
                in
                let ca_target =
                  ExecData.edges exec_data
                  |> List.find_map (fun ed ->
                      let open EdgeGraph in
                      if ed.source = load_ev.id && ed.label = "ca" then
                        let target_store =
                          ExecData.find_mevent_by_id exec_data ed.target
                        in
                        Some target_store
                      else None)
                in
                Some (Reg_load_mem_ev { load_ev; source_store; ca_target })
            | Register load_ev ->
                let* source_store_id =
                  let open EdgeGraph in
                  ExecData.edges exec_data
                  |> List.find_opt (fun ed ->
                      ed.target = load_ev.id && ed.label = "rf-reg")
                  |> Option.map (fun ed -> ed.source)
                in
                let source_store =
                  ExecData.find_event_by_id exec_data source_store_id
                in
                let* source_store =
                  match source_store with Register ev -> Some ev | _ -> None
                in
                Some (Reg_load_reg_ev { load_ev; source_store })
            | _ -> None
          in
          let eq = { proc; reg_name = reg_pretty; value } in
          (eq, data)
    in
    Some (Reg_condition { eq; data })
  in
  let condition_of_global (global_loc : string) : condition option =
    let* tgt =
      let open Execution in
      exec.rfmap
      |> Misc.List.find_map (function
        | RFMap.{ src = Final l; tgt } when location_equal l (Global global_loc)
          ->
            Some tgt
        | _ -> None)
    in
    let* store_ev =
      match tgt with
      | Init -> ExecData.find_init_event_opt exec_data global_loc
      | Store { eiid } -> ExecData.find_mevent_by_id_opt exec_data eiid
    in
    let ca_sources =
      ExecData.edges exec_data
      |> List.filter_map (fun ed ->
          let open EdgeGraph in
          if ed.target = mevent_id store_ev && ed.label = "ca" then
            Some ed.source
          else None)
      |> List.sort_uniq Int.compare
      |> List.filter_map (fun source ->
          let source_ev = ExecData.find_mevent_by_id exec_data source in
          match source_ev with
          | Regular ev when ev.dir = W -> Some source_ev
          | _ -> None)
    in
    let eq = { mem_loc = global_loc; value = mevent_value store_ev } in
    let data = Some { store_ev; ca_sources } in
    Some (Mem_condition { eq; data })
  in
  let condition_locs =
    ConstrGen.fold_constr
      (fun a l ->
        match a with
        | LV (Loc (MiscParser.Location_reg (proc, reg)), _) ->
            Execution.Reg { proc; reg_pretty = reg } :: l
        | LV (Loc (MiscParser.Location_global loc), _) ->
            let global_pretty = ParsedConstant.pp false loc in
            Execution.Global global_pretty :: l
        | _ -> l)
      exec_data.test.condition []
  in
  condition_locs
  |> List.filter_map (function
    | Execution.Reg { proc; reg_pretty } -> condition_of_reg proc reg_pretty
    | Global global -> condition_of_global global)

let make_dep_component ~name
    ~(ev_match : event -> Component.dep_path_event option) ~exec_data
    ~(source : regular_mem_event) ~(target : regular_mem_event) :
    Component.located option =
  let open Component in
  let open Misc.Option.Infix in
  let compare_proc_poi (e1 : dep_path_event) (e2 : dep_path_event) =
    Misc.pair_compare Int.compare Int.compare (e1.proc, e1.poi) (e2.proc, e2.poi)
  in
  let* path =
    let pred =
      if name = "control" then fun ~src ~tgt:_ label ->
        let is_source_branch () =
          let source_ev = ExecData.find_event_by_id exec_data src in
          match source_ev with Branching br -> br.ty = Bcc | _ -> false
        in
        List.mem label [ "iico_data"; "rf-reg" ]
        || (label = "po" && is_source_branch ())
      else fun ~src:_ ~tgt:_ label -> List.mem label [ "iico_data"; "rf-reg" ]
    in
    EdgeGraph.simple_paths_iter ~src:source.id ~dst:target.id ~pred
      exec_data.ExecData.dgraph
    |> Util.Iter.find_map (fun path ->
        let evs =
          path
          |> List.filter_map (fun id ->
              Option.bind (ExecData.find_event_by_id_opt exec_data id) ev_match)
          |> List.sort_uniq compare_proc_poi
        in
        if not (Misc.List.is_empty evs) then Some evs else None)
  in
  let first_path_ev = List.hd path in
  let has_matching_ob_edge =
    ExecData.edges exec_data
    |> List.exists (fun ed ->
        EdgeGraph.(
          ed.label = "ob" && ed.source = source.id && ed.target = target.id))
  in
  let proc = first_path_ev.proc in
  let poi = first_path_ev.poi in
  let dep = { name; path; source; target; has_matching_ob_edge } in
  Some { proc; poi; component = Dependency dep }

let make_data_dep_component ~exec_data =
  let name = "data" in
  let ev_match = function
    | Register ev as event
      when Misc.String.starts_with ~prefix:"EOR" ev.instruction
           || Misc.String.starts_with ~prefix:"ADD" ev.instruction ->
        let proc = ev.proc in
        let poi = ev.poi in
        let ins = ExecData.instr_by_proc_poi ~proc ~poi exec_data in
        Some Component.{ event; proc; poi; ins }
    | _ -> None
  in
  make_dep_component ~name ~ev_match ~exec_data

let make_ctrl_dep_component ~exec_data =
  let name = "control" in

  let ev_match = function
    | Branching ev as event when ev.ty = Bcc ->
        let proc = ev.proc in
        let poi = ev.poi in
        let ins = ExecData.instr_by_proc_poi ~proc ~poi exec_data in
        Some Component.{ event; proc; poi; ins }
    | _ -> None
  in
  make_dep_component ~name ~ev_match ~exec_data

let make_addr_dep_component ~exec_data =
  let name = "address" in

  let ev_match = function
    | Register ev as event
      when Misc.String.starts_with ~prefix:"EOR" ev.instruction ->
        let proc = ev.proc in
        let poi = ev.poi in
        let ins = ExecData.instr_by_proc_poi ~proc ~poi exec_data in
        Some Component.{ event; proc; poi; ins }
    | _ -> None
  in
  make_dep_component ~name ~ev_match ~exec_data

(* --- Explaining full executions --- *)

let describe_execution ~describe_regs ~latex ~num_of_execs
    (test : LitmusTest.test) (src_exec : Execution.t) : string list =
  let exec_data = ExecData.make test src_exec in
  let event_components =
    exec_data.events
    |> List.filter_map (function
      | Memory (Regular ev) ->
          let proc = ev.proc in
          let poi = ev.poi in
          let ins = ExecData.instr_by_proc_poi ~proc ~poi exec_data in
          Some Component.{ proc; poi; component = Memory_event { ev; ins } }
      | Register ev when describe_regs ->
          let proc = ev.proc in
          let poi = ev.poi in
          let ins = ExecData.instr_by_proc_poi ~proc ~poi exec_data in
          Some Component.{ proc; poi; component = Register_event { ev; ins } }
      | _ -> None)
  in
  let dep_components =
    ExecData.edges exec_data
    |> List.filter_map (fun ed ->
        let open Misc.Option.Infix in
        let open EdgeGraph in
        let* make_dep =
          match ed.label with
          | "data" -> Some (make_data_dep_component ~exec_data)
          | "ctrl" -> Some (make_ctrl_dep_component ~exec_data)
          | "addr" -> Some (make_addr_dep_component ~exec_data)
          | _ -> None
        in
        let* source, target =
          match
            ( ExecData.find_event_by_id exec_data ed.source,
              ExecData.find_event_by_id exec_data ed.target )
          with
          | Memory (Regular source), Memory (Regular target) ->
              Some (source, target)
          | _ -> None
        in
        make_dep ~source ~target)
  in
  let module D = Description (struct
    let num_of_execs = num_of_execs
    let num_of_procs = List.length exec_data.test.prog
    let latex = latex

    let regular_mem_events_of_proc dir ~proc =
      ExecData.regular_mem_events_of_proc exec_data dir proc

    let instructions_of_proc ~proc = ExecData.instrs_of_proc exec_data proc
    let instr_by_proc_poi = ExecData.instr_by_proc_poi exec_data
  end) in
  let located_descriptions =
    event_components @ dep_components
    |> List.sort Component.located_compare
    |> List.map D.describe_located
  in
  let conds = conditions_of_test exec_data in
  let deps_descriptions = List.map D.describe_condition conds in
  located_descriptions @ deps_descriptions

type config = {
  libdir : string option;
  herd_path : string option;
  describe_regs : bool;
  latex : bool;
}

let format_line_endings items =
  let items_count = List.length items in
  items
  |> List.mapi (fun ix s ->
      let line_end = if ix = items_count - 1 then "." else ";" in
      Format.sprintf "%s%s" s line_end)

let format_bullets ~latex items =
  let bullet = if latex then "  \\item" else "*" in
  let formatted_items =
    items
    |> List.map (fun item -> Format.sprintf "%s %s" bullet item)
    |> String.concat "\n"
  in
  if latex then
    String.concat "\n" [ "\\begin{itemize}"; formatted_items; "\\end{itemize}" ]
  else formatted_items

let explain_test_path ~(config : config) file_path : string =
  let test = A.parse_from_file file_path in
  let executions =
    LitmusTest.run_herd ~libdir:config.libdir ~herd_path:config.herd_path
      file_path
  in
  let executions =
    (* FIXME: temporarily needed, to stabilize test output *)
    List.sort Stdlib.compare executions
  in
  let num_of_execs = List.length executions in
  if num_of_execs = 0 then
    failwith
      "Could not generate explanation for the litmus test: no executions \
       generated."
  else ();
  let exec_explanations =
    executions
    |> List.map (fun src_exec ->
        let items =
          describe_execution ~describe_regs:config.describe_regs
            ~latex:config.latex ~num_of_execs test src_exec
        in
        items |> format_line_endings |> format_bullets ~latex:config.latex)
    (* As we only use a subset of an execution's data to generate its explanation text, *)
    (* it might be the case that different executions yield the same explanation. *)
    (* Here we filter them. *)
    |> Util.uniq ~eq:( = )
  in
  let multiple_executions = List.length exec_explanations > 1 in
  let preamble =
    if multiple_executions then
      "This test asks whether one of the following candidate executions is \
       architecturally Allowed:"
    else
      "This test asks whether the following execution is architecturally \
       Allowed:"
  in
  let exec_sections =
    exec_explanations
    |> List.mapi (fun ix expl ->
        if multiple_executions then
          let str_ix = String.capitalize_ascii (Util.verbalize_index ix) in
          if config.latex then
            Format.sprintf "\\paragraph{%s execution}@.@.%s@." str_ix expl
          else Format.sprintf "=== %s execution ===@.@.%s@." str_ix expl
        else expl)
    |> String.concat "\n"
  in
  Format.sprintf "%s@.@.%s@." preamble exec_sections |> String.trim
