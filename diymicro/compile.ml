(** Transforms a cycle into a test, ie generates instructions, init and final
    conditions, and dumps them to a litmus test *)
module A = struct
  (* TODO Don't exactly know how to do modularity *)
  include AArch64_compile
end

module C = struct
  include Cycle
end

module E = struct
  include Edge
end

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

let add_ctrl_dep st (r : A.reg) ins =
  let lbl = Label.next_label "DCTRL" in
  let reg_zero, st = A.next_reg st in
  let hins, tins = match ins with [] -> A.Nop, [] | e :: q -> e, q in
  let ins =
    A.pseudo [A.do_eor reg_zero r r; A.cbnz reg_zero lbl]
    @ [A.Label (lbl, hins)]
  in
  ins @ tins, st

let compile_event st (src : E.node_dep) event =
  let event_reg = A.get_register st (Utils.unsome event.C.location) in
  let annot_ldr annot dst_reg loc =
    match annot with
    | E.AnnotNone -> A.Instruction (A.do_ldr A.vloc dst_reg loc)
    | E.A -> A.Instruction (A.do_ldar A.vloc dst_reg loc)
    | _ -> Warn.fatal "Invalid annot %s for ldr" (E.pp_annot annot)
  in
  let annot_ldr_idx annot dst_reg src_loc src_reg =
    match annot with
    | E.AnnotNone ->
        A.Instruction (A.do_ldr_idx A.vloc A.vloc dst_reg src_loc src_reg)
    | _ -> Warn.fatal "Invalid annot %s for ldr_idx" (E.pp_annot annot)
  in
  let annot_str annot src_reg loc =
    match annot with
    | E.AnnotNone -> A.Instruction (A.do_str A.vloc src_reg loc)
    | E.L -> A.Instruction (A.do_stlr A.vloc src_reg loc)
    | _ -> Warn.fatal "Invalid annot %s for str" (E.pp_annot annot)
  in
  let annot_str_idx annot src_reg dst_loc dst_reg =
    match annot with
    | E.AnnotNone -> A.Instruction (A.str_idx src_reg dst_loc dst_reg)
    | _ -> Warn.fatal "Invalid annot %s for ldr_idx" (E.pp_annot annot)
  in
  let ins, dst_dep, st =
    match event.C.direction, event.C.annot, src with
    | E.Rm, _, E.DepNone ->
        let dst, st = A.next_reg st in
        [annot_ldr event.C.annot dst event_reg], E.DepReg dst, st
    | E.Rm, _, E.DepAddr r ->
        let reg_zero, st = A.next_reg st in
        let dst, st = A.next_reg st in
        let ins =
          (*TODO: use the correct variants*)
          A.pseudo [A.do_eor reg_zero r r]
          @ [annot_ldr_idx event.C.annot dst event_reg reg_zero]
        in
        ins, E.DepReg dst, st
    | E.Rm, _, E.DepCtrl r ->
        let dst, st = A.next_reg st in
        let ins, st =
          add_ctrl_dep st r [annot_ldr event.C.annot dst event_reg]
        in
        ins, E.DepReg dst, st
    | E.Wm, _, E.DepNone ->
        let reg_value, st = A.next_reg st in
        let ins =
          [
            A.mov reg_value (Utils.unsome event.C.value);
            annot_str event.C.annot reg_value event_reg;
          ]
        in
        ins, E.DepReg event_reg, st
    | E.Wm, _, E.DepData r ->
        let reg_value, st = A.next_reg st in
        let ins =
          A.pseudo
            [
              A.do_eor reg_value r r;
              A.addi reg_value reg_value (Utils.unsome event.C.value);
            ]
          @ [annot_str event.C.annot reg_value event_reg]
        in
        ins, E.DepReg event_reg, st
    | E.Wm, _, E.DepAddr r ->
        let reg_zero, st = A.next_reg st in
        let reg_value, st = A.next_reg st in
        let ins =
          A.pseudo [A.do_eor reg_zero r r]
          @ [A.mov reg_value (Utils.unsome event.C.value)]
          @ [annot_str_idx event.C.annot reg_value event_reg reg_zero]
        in
        ins, E.DepReg event_reg, st
    | E.Wm, _, E.DepCtrl r ->
        let reg_value, st = A.next_reg st in
        let ins, st =
          add_ctrl_dep st r
            [
              A.mov event_reg (Utils.unsome event.C.value);
              annot_str event.C.annot reg_value event_reg;
            ]
        in
        ins, E.DepReg event_reg, st
    | E.Rr, E.AnnotNone, E.DepNone ->
        let dst, st = A.next_reg st in
        [A.mov_reg dst event_reg], E.DepReg dst, st
    | E.Wr, E.AnnotNone, E.DepNone ->
        if event.C.annot <> E.AnnotNone then
          Warn.fatal "Invalid annot %s for Wr" (E.pp_annot event.C.annot);
        [A.mov event_reg (Utils.unsome event.C.value)], E.DepReg event_reg, st
    | (E.Wr | E.Rr), E.AnnotNone, dep -> [], dep, st (* just carry on *)
    | dir, E.AnnotNone, _ ->
        Warn.fatal "Direction %s incompatible with dependency %s"
          (E.pp_direction dir) (E.pp_node_dep src)
    | dir, annot, _ ->
        Warn.fatal
          "Direction %s incompatible with dependency %s or annotation %s"
          (E.pp_direction dir) (E.pp_node_dep src) (E.pp_annot annot)
  in
  let st =
    let dst = E.dependency_reg dst_dep in
    if event.C.is_significant then
      A.add_condition st dst (Utils.unsome event.C.value)
    else st
  in
  ins, dst_dep, st

(** compile a node (:= event -edge-> ), src is the previous register to which
    dependency should be added, ZR if no dependency *)
let compile_edge (st : A.state) (src : E.node_dep) (node : C.t) =
  match node.C.edge, src with
  | (E.Rf _ | E.Fr _ | E.Ws _ | E.Po _), _ -> [], E.DepNone, st
  | E.Dp (E.Addr, _, _, _), E.DepReg r -> [], E.DepAddr r, st
  | E.Dp (E.Data, _, _, _), E.DepReg r -> [], E.DepData r, st
  | E.Dp (E.Ctrl, _, _, _), E.DepReg r -> [], E.DepCtrl r, st
  | E.BasicDep _, dep -> [], dep, st
  | E.Iico i, _ -> i.E.compile_edge st src
  | _ ->
      Warn.fatal "Edge -%s->: compilation not implemented"
        (E.pp_edge node.C.edge)

(** Generate test from a cycle *)
let make_test (cycle : C.t) =
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
  | A.Symbolic _ :: _ -> assert false (* no symbolic in diy *)
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

let dump_test stl instructions (channel : out_channel) =
  Printf.fprintf channel "%s %s\n" (Archs.pp A.arch) "test.litmus";
  dump_init stl channel;
  dump_code instructions channel;
  dump_final stl channel
