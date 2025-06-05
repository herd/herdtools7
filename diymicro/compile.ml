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

(** metadata needed along instructions for a litmus test *)

type t = {
  cycle : C.t; (* edges, read and write values etc *)
  env : (C.loc * C.proc, A.reg) Hashtbl.t; (* loc, proc to register *)
  initial_values : (C.loc, int) Hashtbl.t;
  mutable final_conditions : (C.proc * A.reg * int) list;
}

let pp_env env =
  Hashtbl.fold
    (fun (loc, proc) r s ->
      s ^ "\n" ^ C.pp_location loc ^ ", " ^ C.pp_proc proc ^ " -> " ^ A.pp_reg r)
    env ""

let get_register env loc proc =
  try Hashtbl.find env (loc, proc)
  with Not_found ->
    Warn.fatal "Unable to find register for %s %s" (C.pp_location loc)
      (C.pp_proc proc)

let add_condition test proc reg value =
  test.final_conditions <- (proc, reg, value) :: test.final_conditions

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

let compile_event st test (src : E.node_dep) event =
  let event_reg =
    get_register test.env
      (Utils.unsome event.C.location)
      (Utils.unsome event.C.proc)
  in
  let ins, dst, st =
    match event.C.direction, src with
    | None, _ -> Warn.fatal "Direction not loaded"
    | Some Edge.Rm, E.DepNone ->
        let dst, st = A.next_reg st in
        A.pseudo [A.do_ldr A.vloc dst event_reg], dst, st
    | Some Edge.Rm, E.DepAddr r ->
        let reg_zero, st = A.next_reg st in
        let dst, st = A.next_reg st in
        let ins =
          (*TODO: use the correct variants*)
          A.pseudo [A.do_eor reg_zero r r]
          @ A.pseudo [A.do_ldr_idx A.vloc A.vloc dst event_reg reg_zero]
        in
        ins, dst, st
    | Some Edge.Rm, E.DepCtrl r ->
        let dst, st = A.next_reg st in
        let ins, st =
          add_ctrl_dep st r (A.pseudo [A.do_ldr A.vloc dst event_reg])
        in
        ins, dst, st
    | Some Edge.Wm, E.DepNone ->
        let reg_value, st = A.next_reg st in
        let ins =
          [A.mov reg_value (Utils.unsome event.C.value)]
          @ A.pseudo [A.str reg_value event_reg]
        in
        ins, event_reg, st
    | Some Edge.Wm, E.DepData r ->
        let reg_value, st = A.next_reg st in
        let ins =
          A.pseudo
            [
              A.do_eor reg_value r r;
              A.addi reg_value reg_value (Utils.unsome event.C.value);
              A.str reg_value event_reg;
            ]
        in
        ins, event_reg, st
    | Some Edge.Wm, E.DepAddr r ->
        let reg_zero, st = A.next_reg st in
        let reg_value, st = A.next_reg st in
        let ins =
          A.pseudo [A.do_eor reg_zero r r]
          @ [A.mov reg_value (Utils.unsome event.C.value)]
          @ A.pseudo [A.str_idx reg_value event_reg reg_zero]
        in
        ins, event_reg, st
    | Some Edge.Wm, E.DepCtrl r ->
        let reg_value, st = A.next_reg st in
        let ins, st =
          add_ctrl_dep st r
            ([A.mov event_reg (Utils.unsome event.C.value)]
            @ A.pseudo [A.str reg_value event_reg])
        in
        ins, event_reg, st
    | Some Edge.Rr, E.DepNone ->
        let dst, st = A.next_reg st in
        [A.mov_reg dst event_reg], dst, st
    | Some Edge.Wr, E.DepNone ->
        [A.mov event_reg (Utils.unsome event.C.value)], event_reg, st
    | Some d, _ ->
        Warn.fatal "Dependency %s incompatible with direction %s"
          (E.pp_node_dep src) (E.pp_direction d)
  in
  if event.C.is_significant then
    add_condition test
      (Utils.unsome event.C.proc)
      dst (* This a read's destination *)
      (Utils.unsome event.C.value);
  ins, E.DepReg dst, st

(** compile a node (:= event -edge-> ), src is the previous register to which
    dependency should be added, ZR if no dependency *)
let compile_edge (st : A.state) test (src : E.node_dep) (node : C.t) =
  let _ = test in
  match node.C.edge, src with
  | (E.Rf _ | E.Fr _ | E.Ws _ | E.Po _), _ -> [], E.DepNone, st
  | E.Dp (E.Addr, _, _), E.DepReg r -> [], E.DepAddr r, st
  | E.Dp (E.Data, _, _), E.DepReg r -> [], E.DepData r, st
  | E.Dp (E.Ctrl, _, _), E.DepReg r -> [], E.DepCtrl r, st
  | _ ->
      Warn.fatal "Edge -%s->: compilation not implemented"
        (E.pp_edge node.C.edge)

(** Generate test from a cycle *)
let make_test (cycle : C.t) =
  let loc_count = C.loc_count () in
  let proc_count = C.proc_count () in
  let test =
    {
      cycle;
      env = Hashtbl.create (proc_count * loc_count);
      initial_values = Hashtbl.create loc_count;
      final_conditions = [];
    }
  in
  let compile_proc proc nodes =
    let rec init_st = function
      | C.Loc -1 -> A.{free_registers = A.allowed_for_symb; next_addr = 0}
      | C.Loc loc_i ->
          let st = init_st (C.Loc (loc_i - 1)) in
          let r, st = A.next_reg st in
          Hashtbl.add test.env (C.Loc loc_i, proc) r;
          st
    in
    let rec compile_proc_aux st (src : E.node_dep) = function
      | [] -> [], st
      | n :: nq ->
          let evt_code, dst, st = compile_event st test src n.C.source_event in
          let edge_code, dst, st = compile_edge st test dst n in
          let next_code, st = compile_proc_aux st dst nq in
          evt_code @ edge_code @ next_code, st
    in
    let st = init_st (C.Loc (loc_count - 1)) in
    let ins, _ = compile_proc_aux st E.DepNone nodes in
    ins
  in
  let rec compile_by_proc nodes_by_proc proc =
    match nodes_by_proc, proc with
    | [], _ -> []
    | nodes :: nq, C.Proc proc_i ->
        let ins = compile_proc proc nodes in
        ins :: compile_by_proc nq (C.Proc (proc_i + 1))
  in
  test, compile_by_proc (split_by_proc cycle) (C.Proc 0)

(* Dumping test *)

let dump_init (test : t) (channel : out_channel) =
  let dump_initial_values () =
    for loc_i = 0 to C.loc_count () - 1 do
      let loc = C.Loc loc_i in
      match Hashtbl.find_opt test.initial_values loc with
      | None -> ()
      | Some v -> Printf.fprintf channel " %s = %d;\n" (C.pp_location loc) v
    done
  in

  let dump_env () =
    for loc_i = 0 to C.loc_count () - 1 do
      let loc = C.Loc loc_i in
      "  " |> output_string channel;
      for proc_i = 0 to C.proc_count () - 1 do
        let proc = C.Proc proc_i in
        match Hashtbl.find_opt test.env (loc, proc) with
        | Some reg ->
            Printf.fprintf channel "%d: %s = %s;\t" proc_i (A.pp_reg reg)
              (C.pp_location loc)
        | None -> ()
      done;
      "\n" |> output_string channel
    done
  in
  "{\n" |> output_string channel;
  dump_initial_values ();
  "\n" |> output_string channel;
  dump_env ();
  "}\n\n" |> output_string channel

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

let dump_final test channel =
  let pp_clause (C.Proc proc, reg, value) =
    Printf.sprintf "%d:%s=%d" proc (A.pp_reg reg) value
  in
  let rec pp_clauses = function
    | [] -> ""
    | [e] -> pp_clause e
    | e :: q -> pp_clause e ^ " /\\ " ^ pp_clauses q
  in
  "exists (" ^ pp_clauses test.final_conditions ^ ")\n" |> output_string channel

let dump_test (test, instructions) (channel : out_channel) =
  Printf.fprintf channel "%s %s\n" (Archs.pp A.arch) "test.litmus";
  dump_init test channel;
  dump_code instructions channel;
  dump_final test channel
