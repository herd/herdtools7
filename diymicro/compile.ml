(** Transforms a cycle into a test, ie generates instructions, init and final
    conditions, and dumps them to a litmus test *)
module A = struct
  include AArch64_compile
end

module C = struct
  include Cycle
end

module E = struct
  include Edge
end

type prog = int A.kinstruction A.kpseudo list * A.state
(** Instructions and meta (eg initials & finals) relevant to a single proc *)

(** Returns a list of lists of nodes (cycles), splitted by proc *)
let split_by_proc (cycle : C.t) =
  let first_node =
    C.find_first cycle (fun c ->
        c.C.prev.C.source_event.C.proc <> Some (C.Proc 0)
        && c.C.source_event.C.proc = Some (C.Proc 0))
  in
  let rec split_by_proc node =
    let next =
      if node.C.next != first_node then split_by_proc node.C.next else []
    in
    match next with
    | e :: q ->
        if node.C.next.C.source_event.C.proc <> node.C.source_event.C.proc then
          [node] :: e :: q
        else (node :: e) :: q
    | [] -> [[node]]
  in
  split_by_proc first_node

(** Add a control dep from reg r. Optionally v_opt specifies the known value of
    r *)
let add_ctrl_dep st (r : A.reg) v_opt ins =
  let lbl = Label.next_label "DCTRL" in
  let ins_zero, reg_zero, st = A.calc_value st 0 r v_opt in
  let hins, tins = match ins with [] -> A.Nop, [] | e :: q -> e, q in
  let ins = ins_zero @ A.pseudo [A.cbnz reg_zero lbl] @ [A.Label (lbl, hins)] in
  ins @ tins, st

let compile_event st (src : E.node_dep) event =
  let event_reg = A.get_register st (Utils.unsome event.C.location) in
  let annot_ldr annot dst_reg loc =
    match annot with
    | E.AnnotNone -> A.Instruction (A.do_ldr A.vloc dst_reg loc)
    | E.A -> A.Instruction (A.do_ldar A.vloc dst_reg loc)
    | E.X -> A.Instruction (A.ldxr dst_reg loc)
    | _ -> Warn.user_error "Invalid annot %s for ldr" (E.pp_annot annot)
  in
  let annot_ldr_idx annot dst_reg src_loc src_reg =
    match annot with
    | E.AnnotNone ->
        A.Instruction (A.do_ldr_idx A.vloc A.vloc dst_reg src_loc src_reg)
    | _ -> Warn.user_error "Invalid annot %s for ldr_idx" (E.pp_annot annot)
  in
  let annot_str annot src_reg loc =
    match annot with
    | E.AnnotNone -> A.Instruction (A.do_str A.vloc src_reg loc)
    | E.L -> A.Instruction (A.do_stlr A.vloc src_reg loc)
    | _ -> Warn.user_error "Invalid annot %s for str" (E.pp_annot annot)
  in
  let annot_str_idx annot src_reg dst_loc dst_reg =
    match annot with
    | E.AnnotNone -> A.Instruction (A.str_idx src_reg dst_loc dst_reg)
    | _ -> Warn.user_error "Invalid annot %s for ldr_idx" (E.pp_annot annot)
  in
  match event.C.direction with
  | E.Rm true | E.Wm true -> [], src, st
  | _ ->
      let ins, dst_dep, st =
        match event.C.direction, event.C.annot, src with
        | E.Rm false, _, E.DepNone ->
            let dst, st = A.next_reg st in
            let ins = [annot_ldr event.C.annot dst event_reg] in
            ins, E.DepReg (dst, event.C.value), st
        | E.Rm false, _, E.DepAddr (r, _) ->
            let reg_zero, st = A.next_reg st in
            let dst, st = A.next_reg st in
            let ins =
              A.pseudo [A.do_eor reg_zero r r]
              (* if r=0, we still need do_eor to avoid a mixed-size error *)
              @ [annot_ldr_idx event.C.annot dst event_reg reg_zero]
            in
            ins, E.DepReg (dst, event.C.value), st
        | E.Rm false, _, E.DepCtrl (r, v_opt) ->
            let dst, st = A.next_reg st in
            let ins, st =
              add_ctrl_dep st r v_opt [annot_ldr event.C.annot dst event_reg]
            in
            ins, E.DepReg (dst, event.C.value), st
        | E.Wm false, _, E.DepNone ->
            let reg_value, st = A.next_reg st in
            let ins =
              [
                A.mov reg_value (Utils.unsome event.C.value);
                annot_str event.C.annot reg_value event_reg;
              ]
            in
            ins, E.DepNone, st
        | E.Wm false, _, (E.DepData (r, v_opt) | E.DepReg (r, v_opt)) ->
            let ins_val, reg_value, st =
              A.calc_value st (Utils.unsome event.C.value) r v_opt
            in
            let ins = ins_val @ [annot_str event.C.annot reg_value event_reg] in
            ins, E.DepNone, st
        | E.Wm false, _, E.DepAddr (r, _) ->
            let reg_zero, st = A.next_reg st in
            let reg_value, st = A.next_reg st in
            let ins =
              A.pseudo [A.do_eor reg_zero r r]
              (* if r=0, we still need do_eor to avoid a mixed-size error *)
              @ [A.mov reg_value (Utils.unsome event.C.value)]
              @ [annot_str_idx event.C.annot reg_value event_reg reg_zero]
            in
            ins, E.DepNone, st
        | E.Wm false, _, E.DepCtrl (r, v_opt) ->
            let reg_value, st = A.next_reg st in
            let ins, st =
              add_ctrl_dep st r v_opt
                [
                  A.mov reg_value (Utils.unsome event.C.value);
                  annot_str event.C.annot reg_value event_reg;
                ]
            in
            ins, E.DepNone, st
        | E.RegEvent, _, E.DepNone ->
            Warn.fatal "No dependency passed to a register event"
        | E.RegEvent, E.AnnotNone, dep -> [], dep, st (* just carry on *)
        | dir, E.AnnotNone, _ ->
            Warn.user_error "Direction %s incompatible with dependency %s"
              (E.pp_direction dir) (E.pp_node_dep src)
        | dir, annot, _ ->
            Warn.fatal
              "Direction %s incompatible with dependency %s or annotation %s"
              (E.pp_direction dir) (E.pp_node_dep src) (E.pp_annot annot)
      in
      let st =
        (* if the event is marked as "significant", we add a final condition. If the edge compiles the event, this is done after the edge's compilation *)
        if event.C.is_significant then
          let dst = E.dependency_reg dst_dep in
          A.add_condition st dst (Utils.unsome event.C.value)
        else st
      in
      ins, dst_dep, st

(** compile a node (:= event -edge-> ), src is the previous register to which
    dependency should be added, ZR if no dependency *)
let compile_edge (st : A.state) (src : E.node_dep) (node : C.t) =
  match node.C.edge, src with
  | (E.Rf _ | E.Fr _ | E.Ws _ | E.Po _), _ -> [], E.DepNone, st
  | E.Dp (E.Addr, _, _, _), E.DepReg (r, v_opt) -> [], E.DepAddr (r, v_opt), st
  | E.Dp (E.Data, _, _, _), E.DepReg (r, v_opt) -> [], E.DepData (r, v_opt), st
  | E.Dp (E.Ctrl, _, _, _), E.DepReg (r, v_opt) -> [], E.DepCtrl (r, v_opt), st
  | E.RfReg, dep -> [], dep, st
  | E.Iico i, _ ->
      let src_event = node.C.source_event in
      let dst_event = node.C.next.C.source_event in
      let src_event_data =
        match src_event.C.direction with
        | E.Rm true | E.Wm true -> Some (C.get_event_data src_event)
        | _ -> None
      in
      let dst_event_data =
        match dst_event.C.direction with
        | E.Rm true | E.Wm true -> Some (C.get_event_data dst_event)
        | _ -> None
      in
      let ins, dst_dep, st =
        i.E.compile_edge st src src_event_data dst_event_data
      in
      let st =
        if dst_event.C.is_significant && dst_event_data <> None then
          let dep_reg = E.dependency_reg dst_dep in
          if dep_reg <> A.ZR then
            A.add_condition st dep_reg (Utils.unsome dst_event.C.value)
          else st
        else st
      in

      ins, dst_dep, st
  | _ ->
      Warn.fatal "Edge -%s->: compilation not implemented"
        (E.pp_edge node.C.edge)

(** Compile a cycle to relevant progs *)
let prog_of_cycle (cycle : C.t) : prog list =
  let loc_count = A.loc_count () in

  let compile_proc nodes =
    let rec init_st = function
      | A.Loc -1 ->
          A.
            {
              free_registers = A.allowed_for_symb;
              env = [];
              initial_values = [];
              final_conditions = [];
            }
      | A.Loc loc_i ->
          let st = init_st (A.Loc (loc_i - 1)) in
          let r, st = A.next_reg st in
          let st = A.set_register st (A.Loc loc_i) r in
          st
    in
    let rec compile_proc_aux st (src : E.node_dep) = function
      | [] -> [], st
      | n :: nq ->
          let evt_code, dst, st = compile_event st src n.C.source_event in
          let edge_code, dst, st = compile_edge st dst n in
          let next_code, st = compile_proc_aux st dst nq in
          evt_code @ edge_code @ next_code, st
    in
    let st = init_st (A.Loc (loc_count - 1)) in
    compile_proc_aux st E.DepNone nodes
  in
  let rec compile_by_proc nodes_by_proc proc =
    match nodes_by_proc, proc with
    | [], _ -> []
    | nodes :: nq, C.Proc proc_i ->
        let ins = compile_proc nodes in
        ins :: compile_by_proc nq (C.Proc (proc_i + 1))
  in
  compile_by_proc (split_by_proc cycle) (C.Proc 0)

(* Dumping test *)

let dump_init (stl : A.state list) (channel : out_channel) =
  let pp_initial_values () =
    let initial_values =
      List.map (fun st -> st.A.initial_values) stl |> List.flatten
    in
    A.pp_initial_values initial_values
    ^ match initial_values with [] -> "" | _ -> "\n"
  in

  let pp_envs () =
    let rec pp_proc_env proc = function
      | [] -> ""
      | (loc, reg) :: q ->
          Printf.sprintf "%d: %s = %s;\t" proc (A.pp_reg reg)
            (A.pp_location loc)
          ^ pp_proc_env proc q
    in
    List.mapi (fun proc st -> "  " ^ pp_proc_env proc st.A.env ^ "\n") stl
    |> String.concat ""
  in
  "{\n" ^ pp_initial_values () ^ pp_envs () ^ "}\n\n" |> output_string channel

(* Code pretty-print *)

let rec dump_pseudo = function
  | [] -> []
  | A.Instruction ins :: rem -> A.dump_instruction ins :: dump_pseudo rem
  | A.Label (lbl, ins) :: rem ->
      Printf.sprintf "%s:" lbl :: dump_pseudo (ins :: rem)
  | A.Nop :: rem -> dump_pseudo rem
  | A.Symbolic _ :: _ ->
      Warn.fatal "Got a symbolic instruction, which does not exist in diymicro"
      (* no symbolic in diy *)
  | A.Macro (m, args) :: rem ->
      Printf.sprintf "%s(%s)" m (String.concat "," (List.map A.pp_reg args))
      :: dump_pseudo rem

let fmt_cols =
  let rec fmt_col p k = function
    | [] -> k
    | cs :: prog ->
        (C.pp_proc (C.Proc p) :: dump_pseudo cs) :: fmt_col (p + 1) k prog
  in
  fmt_col 0 []

let dump_code code channel =
  let pp = fmt_cols code in
  Misc.pp_prog channel pp

let dump_final (stl : A.state list) channel =
  let pp_clause proc reg value =
    Printf.sprintf "%d: %s=%d" proc (A.pp_reg reg) value
  in
  let rec pp_clauses = function
    | [] -> ""
    | [(proc, reg, v)] -> pp_clause proc reg v
    | (proc, reg, v) :: q -> pp_clause proc reg v ^ " /\\ " ^ pp_clauses q
  in
  let rec add_proc proc = function
    | [] -> []
    | (reg, v) :: q -> (proc, reg, v) :: add_proc proc q
  in
  "exists ("
  ^ (List.mapi (fun proc st -> add_proc proc st.A.final_conditions) stl
    |> List.flatten |> pp_clauses)
  ^ ")\n"
  |> output_string channel

let dump_test stl instructions baseprog annot_edges ?(name = "test")
    (channel : out_channel) =
  Printf.fprintf channel "%s %s\n" (Archs.pp A.arch) name;
  Printf.fprintf channel "Generator=%s\n" baseprog;
  Printf.fprintf channel "Orig=%s\n"
    (List.map Edge.pp_quoted_annotated_edge annot_edges |> String.concat " ");
  dump_init stl channel;
  dump_code instructions channel;
  dump_final stl channel

let to_channel annot_edges ?(name = "test") channel =
  C.reset_proc_count ();
  A.reset_loc_count ();
  let cycle = Cycle.make_cycle annot_edges in
  let prog = prog_of_cycle cycle in

  let instructions = List.map (fun (a, _) -> a) prog in
  let stl = List.map (fun (_, b) -> b) prog in
  let baseprog =
    Printf.sprintf "%s (version %s)" Config.prog_name Version.version
  in
  dump_test stl instructions baseprog annot_edges ?name:(Some name) channel
