open Dot_parser

let with_cmd_stdout (cmd : string) (f : in_channel -> 'a) : 'a =
  let inp = Unix.open_process_in cmd in
  let r = f inp in
  match Unix.close_process_in inp with
  | Unix.WEXITED code when code = 0 -> r
  | Unix.WEXITED code -> failwith (Printf.sprintf "Exited with %d\n" code)
  | Unix.WSIGNALED s -> failwith (Format.sprintf "Killed by signal %d\n" s)
  | Unix.WSTOPPED s -> failwith (Format.sprintf "Stopped by signal %d\n" s)

let reg_eq_re : Re.re =
  let open Re in
  seq
    [
      group (rep1 digit);
      char ':';
      alt [ char 'W'; char 'X' ];
      group (rep1 alnum);
      char '=';
      group (rep1 alnum);
    ]
  |> compile

let mem_loc_eq_re : Re.re =
  let open Re in
  seq [ char '['; group (rep1 alnum); char ']'; char '='; group (rep1 alnum) ]
  |> compile

let init_eqs_re : Re.re =
  let open Re in
  seq [ char '{'; rep1 (compl [ char '}' ]); char '}' ] |> compile

type init_eq = { proc : int; reg : string; mem_loc : string } [@@deriving show]
type register_eq = { proc : int; reg_ix : int; value : int } [@@deriving show]
type mem_loc_eq = { mem_loc : string; value : int } [@@deriving show]

type condition_eq = Register_eq of register_eq | Mem_loc_eq of mem_loc_eq
[@@deriving show]

let registers_of_instr (instr : string) : int list =
  let sep = Re.(alt [ char '['; char ']'; space; char ',' ]) in
  let re =
    Re.(
      seq [ sep; alt [ char 'W'; char 'X' ]; group (rep1 digit); sep ]
      |> compile)
  in
  Re.all re instr |> List.map (fun g -> Re.Group.get g 1 |> int_of_string)

let index_to_position (ix : int) : string =
  match ix with
  | 0 -> "first"
  | 1 -> "second"
  | 2 -> "third"
  | 3 -> "fourth"
  | 4 -> "fifth"
  | ix -> Format.sprintf "%dth" (ix + 1)

let is_load (ev : regular_mem_event) = ev.dir = R
let is_store (ev : regular_mem_event) = ev.dir = W
let is_initialization = function Init _ -> true | _ -> false

let make_event_describer ~(exec : execution) :
    ?as_instruction:bool -> event -> string =
  let processes : IntSet.t =
    exec.events
    |> List.filter_map (function
      | Memory (Regular ev) -> Some ev.proc
      | Branching ev -> Some ev.proc
      | Register ev -> Some ev.proc
      | _ -> None)
    |> IntSet.of_list
  in
  let regular_loads_of_proc proc =
    exec.events
    |> List.filter_map (function
      | Memory (Regular ev) ->
          if is_load ev && ev.proc = proc then Some ev.id else None
      | _ -> None)
  in
  let regular_stores_of_proc proc =
    exec.events
    |> List.filter_map (function
      | Memory (Regular ev) ->
          if is_store ev && ev.proc = proc then Some ev.id else None
      | _ -> None)
  in
  let branches_bcc_of_proc proc =
    exec.events
    |> List.filter_map (function
      | Branching ev when ev.ty = Bcc ->
          if ev.proc = proc then Some ev.id else None
      | _ -> None)
  in
  let get_poi_details ~id ev_set =
    if List.length ev_set = 1 then ""
    else
      let ev_ix =
        Misc.List.find_index (fun this_id -> id = this_id) ev_set |> Option.get
      in
      index_to_position ev_ix ^ " "
  in
  fun ?(as_instruction = false) (ev : event) ->
    let print_proc = IntSet.cardinal processes <> 1 || as_instruction in
    match ev with
    | Memory (Init ev) ->
        Format.sprintf "the initialization event of location %s" ev.mem_loc
    | Memory (Regular ev) ->
        let instr_ty, eff_ty, poi_details =
          match ev.dir with
          | R ->
              let proc_loads = regular_loads_of_proc ev.proc in
              let poi_details = get_poi_details ~id:ev.id proc_loads in
              ("Load", "Read", poi_details)
          | W ->
              let proc_stores = regular_stores_of_proc ev.proc in
              let poi_details = get_poi_details ~id:ev.id proc_stores in
              ("Store", "Write", poi_details)
        in
        let proc_details =
          if print_proc then Format.sprintf " on P%d" ev.proc else ""
        in
        if as_instruction then
          Format.sprintf "the %s%s instruction%s" poi_details instr_ty
            proc_details
        else
          Format.sprintf "the %sExplicit Memory %s Effect%s" poi_details eff_ty
            proc_details
    | Branching ev when ev.ty = Bcc ->
        let proc_branches = branches_bcc_of_proc ev.proc in
        let poi_details = get_poi_details ~id:ev.id proc_branches in
        let instr_ty = "Branch" in
        let proc_details =
          if print_proc then Format.sprintf " on P%d" ev.proc else ""
        in
        Format.sprintf "the %s%s instruction%s" poi_details instr_ty
          proc_details
    | Register ev ->
        let instr =
          if CCString.prefix ~pre:"EOR" ev.instruction then "Exclusive Or"
          else if CCString.prefix ~pre:"ADD" ev.instruction then "Add"
          else failwith "NIY"
        in
        let proc_details =
          if print_proc then Format.sprintf " on P%d" ev.proc else ""
        in
        Format.sprintf "the %s instruction%s" instr proc_details
    | _ -> failwith "NIY"

type config = {
  exec : execution;
  mevent_map : memory_event StringMap.t;
  event_map : event StringMap.t;
  reg_event_map : register_event StringMap.t;
  stores : memory_event list;
  ob_edges : edge list;
  describe_ev : ?as_instruction:bool -> event -> string;
}

let explain_regular_mem_event ~(config : config) (ev : regular_mem_event) :
    string =
  let eff_ty = match ev.dir with R -> "Read" | W -> "Write" in
  let instr_desc =
    config.describe_ev ~as_instruction:true (Memory (Regular ev))
    |> String.capitalize_ascii
  in
  Format.sprintf
    "%s, i.e. %s, generates an Explicit Memory %s Effect of Location %s with \
     value %d"
    instr_desc ev.instruction eff_ty ev.mem_loc ev.value

let explain_mem_loc_condition ~(config : config) (eq : mem_loc_eq) : string =
  let preamble =
    Format.sprintf "The value of %s is %d in the end" eq.mem_loc eq.value
  in
  let store_ev =
    List.find_map
      (fun (ev : memory_event) ->
        if mevent_mem_loc ev = eq.mem_loc && mevent_value ev = eq.value then
          Some ev
        else None)
      config.stores
  in
  match store_ev with
  | Some ev ->
      let ca_sources =
        config.exec.edges
        |> List.filter_map (fun ed ->
            if ed.target = mevent_id ev && ed.label = "ca" then Some ed.source
            else None)
        |> CCList.sort_uniq ~cmp:String.compare
        |> List.filter_map (fun source ->
            let source_ev = StringMap.find source config.mevent_map in
            match source_ev with
            | Regular ev when is_store ev ->
                Some (config.describe_ev (Memory source_ev))
            | _ -> None)
      in
      let ca_source = String.concat " and " ca_sources in
      let ca_desc =
        if List.length ca_sources = 1 then
          Format.sprintf "%s is Coherence-before %s" ca_source
            (config.describe_ev (Memory ev))
        else
          Format.sprintf "%s are Coherence-before %s" ca_source
            (config.describe_ev (Memory ev))
      in
      preamble ^ ", because " ^ ca_desc
  | None -> preamble

let explain_reg_condition ~(config : config) (eq : register_eq) : string =
  let preamble =
    Format.sprintf "The Register X%d on P%d holds the value %d in the end"
      eq.reg_ix eq.proc eq.value
  in
  let extra_text =
    let open CCOption.Infix in
    let* load_ev =
      List.find_map
        (function
          | Memory (Regular ev) when is_load ev && ev.proc = eq.proc ->
              let used_regs = registers_of_instr ev.instruction in
              if List.mem eq.reg_ix used_regs then Some ev else None
          | _ -> None)
        config.exec.events
    in
    let load_ev_descr = config.describe_ev (Memory (Regular load_ev)) in
    let* source_store_id =
      config.exec.edges
      |> List.find_opt (fun ed -> ed.target = load_ev.id && ed.label = "rf")
      |> Option.map (fun ed -> ed.source)
    in
    let source_store = StringMap.find source_store_id config.mevent_map in
    let source_store_descr = config.describe_ev (Memory source_store) in
    let rf_phrase =
      Format.sprintf ", because %s has read the value %d from %s" load_ev_descr
        eq.value source_store_descr
    in
    let ca_phrase =
      let ca_target =
        config.exec.edges
        |> List.find_map (fun ed ->
            if ed.source = load_ev.id && ed.label = "ca" then
              let target_store = StringMap.find ed.target config.mevent_map in
              Some target_store
            else None)
      in
      match ca_target with
      | Some ca_target ->
          let ca_target_descr = config.describe_ev (Memory ca_target) in
          Format.sprintf ". In other words, %s is Coherence-before %s"
            load_ev_descr ca_target_descr
      | None -> ""
    in
    let full_phrase = rf_phrase ^ ca_phrase in
    Some full_phrase
  in
  match extra_text with
  | Some extra_text -> Format.sprintf "%s%s" preamble extra_text
  | None -> preamble

let explain_condition ~(config : config) (eq : condition_eq) : string =
  match eq with
  | Mem_loc_eq eq -> explain_mem_loc_condition ~config eq
  | Register_eq eq -> explain_reg_condition ~config eq

(* --- Explaining dependency edges --- *)

module type Dep = sig
  type event_subtype

  val name : string
  val ev_match : event -> event_subtype option
  val ev_eq : event_subtype -> event_subtype -> bool
  val describe_ev : event_subtype -> string
  val ev_proc : event_subtype -> int
  val ev_poi : event_subtype -> int
end

module Dep_explainer (D : Dep) = struct
  let explain_dep ~config g (source : regular_mem_event)
      (target : regular_mem_event) : (int * int * string) option =
    let path =
      let pred =
       fun ed_label -> List.mem ed_label [ "po"; "iico_data"; "rf-reg" ]
      in
      simple_paths_iter ~src:source.id ~dst:target.id ~pred g
      |> Util.Iter.find_map (fun path ->
          let evs =
            path
            |> List.filter_map (fun id ->
                StringMap.find_opt id config.event_map
                |> CCOption.flat_map (fun (ev : event) -> D.ev_match ev))
            |> CCList.uniq ~eq:D.ev_eq
          in
          if not (Misc.List.is_empty evs) then Some evs else None)
    in
    match path with
    | Some path ->
        let first_path_ev = List.hd path in
        let path_descrs =
          path |> List.map D.describe_ev |> String.concat " and "
          |> String.capitalize_ascii
        in
        let source_desc =
          config.describe_ev ~as_instruction:true (Memory (Regular source))
        in
        let target_desc =
          config.describe_ev ~as_instruction:true (Memory (Regular target))
        in
        let has_matching_ob_ed =
          config.exec.edges
          |> List.exists (fun ed ->
              ed.label = "ob" && ed.source = source.id && ed.target = target.id)
        in
        let match_phrase =
          if has_matching_ob_ed then
            let plural = if List.length path > 1 then "" else "s" in
            Format.sprintf "create%s" plural
          else "does not create"
        in
        let name_phrase =
          if has_matching_ob_ed then
            let article =
              if List.mem (CCString.get D.name 0) [ 'a'; 'i'; 'u'; 'e'; 'o' ]
              then "an"
              else "a"
            in
            Format.sprintf "%s %s dependency" article
              (String.capitalize_ascii D.name)
          else "any dependency"
        in
        let phrase =
          Format.sprintf "%s %s %s from %s, i.e. %s, to %s, i.e. %s" path_descrs
            match_phrase name_phrase source_desc source.instruction target_desc
            target.instruction
        in
        Some (D.ev_proc first_path_ev, D.ev_poi first_path_ev, phrase)
    | None -> None
end

let make_data_dep (config : config) :
    (module Dep with type event_subtype = register_event) =
  (module struct
    type event_subtype = register_event

    let name = "data"

    let ev_match = function
      | Register ev
        when CCString.prefix ~pre:"EOR" ev.instruction
             || CCString.prefix ~pre:"ADD" ev.instruction ->
          Some ev
      | _ -> None

    let ev_eq (ev1 : register_event) (ev2 : register_event) =
      ev1.instruction = ev2.instruction

    let describe_ev (ev : register_event) =
      let descr = config.describe_ev ~as_instruction:true (Register ev) in
      Format.sprintf "%s, i.e. %s," descr ev.instruction

    let ev_proc (ev : register_event) = ev.proc
    let ev_poi (ev : register_event) = ev.poi
  end)

let make_ctrl_dep (config : config) : (module Dep) =
  (module struct
    type event_subtype = branching_event

    let name = "control"

    let ev_match = function
      | Branching ev when ev.ty = Bcc -> Some ev
      | _ -> None

    let ev_eq (ev1 : branching_event) (ev2 : branching_event) =
      (ev1.proc, ev1.poi) = (ev2.proc, ev2.poi)

    let describe_ev (ev : branching_event) =
      config.describe_ev ~as_instruction:true (Branching ev)

    let ev_proc (ev : branching_event) = ev.proc
    let ev_poi (ev : branching_event) = ev.poi
  end : Dep)

let make_addr_dep (config : config) : (module Dep) =
  let data_dep = make_data_dep config in
  let module Data_dep = (val data_dep) in
  (module struct
    type event_subtype = register_event

    let name = "address"

    let ev_match = function
      | Register ev when CCString.prefix ~pre:"EOR" ev.instruction -> Some ev
      | _ -> None

    let ev_eq = Data_dep.ev_eq
    let describe_ev = Data_dep.describe_ev
    let ev_proc = Data_dep.ev_proc
    let ev_poi = Data_dep.ev_poi
  end : Dep)

(* --- Explaining full executions --- *)

let explain_execution (condition_eqs : condition_eq list) (exec : execution) :
    string list =
  let mevent_map =
    StringMap.from_bindings
      (List.filter_map
         (function Memory ev -> Some (mevent_id ev, ev) | _ -> None)
         exec.events)
  in
  let event_map =
    StringMap.from_bindings (List.map (fun ev -> (event_id ev, ev)) exec.events)
  in
  let reg_event_map =
    StringMap.from_bindings
      (List.filter_map
         (function Register ev -> Some (ev.id, ev) | _ -> None)
         exec.events)
  in
  let stores =
    List.filter_map
      (function Memory ev when mevent_dir ev = W -> Some ev | _ -> None)
      exec.events
  in
  let ob_edges = exec.edges |> List.filter (fun ed -> ed.label = "ob") in
  let describe_ev = make_event_describer ~exec in
  let config =
    {
      mevent_map;
      event_map;
      reg_event_map;
      exec;
      stores;
      ob_edges;
      describe_ev;
    }
  in
  let explained_events =
    exec.events
    |> List.filter_map (function
      | Memory (Regular ev) ->
          Some (ev.proc, ev.poi, explain_regular_mem_event ~config ev)
      | _ -> None)
  in
  let g = to_dgraph exec in
  let explained_deps =
    let data_dep = make_data_dep config in
    let ctrl_dep = make_ctrl_dep config in
    let module Data_expl = Dep_explainer ((val data_dep)) in
    let module Ctrl_expl = Dep_explainer ((val ctrl_dep)) in
    let module Addr_expl = Dep_explainer ((val make_addr_dep config)) in
    exec.edges
    |> List.filter_map (fun ed ->
        match
          ( ed.label,
            StringMap.find ed.source config.event_map,
            StringMap.find ed.target config.event_map )
        with
        | "data", Memory (Regular source), Memory (Regular target) ->
            Data_expl.explain_dep ~config g source target
        | "ctrl", Memory (Regular source), Memory (Regular target) ->
            Ctrl_expl.explain_dep ~config g source target
        | "addr", Memory (Regular source), Memory (Regular target) ->
            Addr_expl.explain_dep ~config g source target
        | _ -> None)
  in
  let sorted_event_explanations =
    explained_events @ explained_deps
    |> List.sort (fun (p1, i1, _) (p2, i2, _) ->
        CCPair.compare Int.compare Int.compare (p1, i1) (p2, i2))
    |> List.map (fun (_, _, x) -> x)
  in
  let explained_conds = List.map (explain_condition ~config) condition_eqs in
  sorted_event_explanations @ explained_conds

let explain_test_path file_path : string =
  let herd_lines, executions =
    let herd_cmd =
      String.concat " "
        [
          "herd7";
          "-show";
          "prop";
          "-showevents";
          "all";
          "-through";
          "invalid";
          "-doshow";
          "ob";
          "-showraw";
          "ob";
          "-o -";
          file_path;
        ]
    in
    let herd_lines, dot_lines =
      with_cmd_stdout herd_cmd (fun ch ->
          let current_section = ref `Header in
          let herd_lines = ref [] in
          let dot_lines = ref [] in
          let () =
            Util.Iter.of_in_channel_lines ch (fun line ->
                if line = "DOTCOM dot" then current_section := `Dot
                else if Misc.String.starts_with ~prefix:"DOTEND" line then
                  current_section := `Herd
                else
                  match !current_section with
                  | `Dot -> dot_lines := line :: !dot_lines
                  | `Herd -> herd_lines := line :: !herd_lines
                  | _ -> ())
          in
          (List.rev !herd_lines, List.rev !dot_lines))
    in
    let dot_contents = String.concat "\n" dot_lines in
    let executions = parse_dot dot_contents in
    (herd_lines, executions)
  in
  let condition =
    CCList.find_map (CCString.chop_prefix ~pre:"Condition exists ") herd_lines
    |> Option.get
  in
  let condition_eqs =
    let reg_eqs =
      Re.all reg_eq_re condition
      |> List.map (fun g ->
          let proc = Re.Group.get g 1 |> int_of_string in
          let reg_ix = Re.Group.get g 2 |> int_of_string in
          let value = Re.Group.get g 3 |> int_of_string in
          Register_eq { proc; reg_ix; value })
    in

    let mem_loc_eqs =
      Re.all mem_loc_eq_re condition
      |> List.map (fun g ->
          let mem_loc = Re.Group.get g 1 in
          let value = Re.Group.get g 2 |> int_of_string in
          Mem_loc_eq { mem_loc; value })
    in
    reg_eqs @ mem_loc_eqs
  in
  if List.length executions = 0 then
    failwith
      "Could not generate explanation for the litmus test: no executions \
       generated."
  else ();
  let exec_explanations =
    executions
    |> List.map (fun exec ->
        explain_execution condition_eqs exec
        |> List.map (fun s -> s ^ ";")
        |> String.concat "\n")
    (* As we only use a subset of an execution's data to generate its explanation text, *)
    (* it might be the case that different executions yield the same explanation. *)
    (* Here we filter them. *)
    |> CCList.uniq ~eq:( = )
  in
  let multiple_executions = List.length exec_explanations > 1 in
  let preamble =
    if multiple_executions then
      "This litmus tests asks whether one of the following candidate \
       executions is architecturally allowed:"
    else
      "This litmus test asks whether the following execution is \
       architecturally allowed:"
  in
  let exec_sections =
    exec_explanations
    |> List.mapi (fun ix expl ->
        if multiple_executions then
          Format.sprintf "=== %s execution ===@.@.%s@." (index_to_position ix)
            expl
        else expl)
    |> String.concat "\n"
  in
  Format.sprintf "%s@.@.%s@." preamble exec_sections |> String.trim
