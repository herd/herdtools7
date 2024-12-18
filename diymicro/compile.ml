(** Transforms a cycle into a test, ie generates instructions, init and final
    conditions, and dumps them to a litmus test *)

module A = AArch64_compile

type prog = int A.kinstruction A.kpseudo list * State.t
(** Instructions and meta (eg initials & finals) relevant to a single proc *)

(** Returns a list of lists of nodes (cycles), splitted by proc *)
let split_by_proc (cycle : Cycle.t) =
  let open Cycle in
  let first_node =
    find_first cycle (fun c ->
        c.prev.source_event.proc <> Some (Proc 0)
        && c.source_event.proc = Some (Proc 0))
  in
  let rec split_by_proc node =
    let next =
      if node.next != first_node then split_by_proc node.next else []
    in
    match next with
    | e :: q ->
        if node.next.source_event.proc <> node.source_event.proc then
          [node] :: e :: q
        else (node :: e) :: q
    | [] -> [[node]]
  in
  split_by_proc first_node

(** Add a control dep from reg r. Optionally v_opt specifies the known value of
    r *)
let add_ctrl_dep st (r : A.reg) v_opt ins =
  let lbl = Label.next_label "DCTRL" in
  let ins_zero, reg_zero, st = State.calc_value st 0 r v_opt in
  let hins, tins = match ins with [] -> A.Nop, [] | e :: q -> e, q in
  let ins = ins_zero @ A.pseudo [A.cbnz reg_zero lbl] @ [A.Label (lbl, hins)] in
  ins @ tins, st

let compile_event st (src : Edge.node_dep) event =
  let open Edge in
  let event_reg = State.get_register st (Option.get event.Cycle.location) in
  let annot_ldr annot dst_reg loc =
    match annot with
    | AnnotNone -> A.Instruction (A.do_ldr A.vloc dst_reg loc)
    | A -> A.Instruction (A.do_ldar A.vloc dst_reg loc)
    | X -> A.Instruction (A.ldxr dst_reg loc)
    | _ -> Warn.user_error "Invalid annot %s for ldr" (pp_annot annot)
  in
  let annot_ldr_idx annot dst_reg src_loc src_reg =
    match annot with
    | AnnotNone ->
        A.Instruction (A.do_ldr_idx A.vloc A.vloc dst_reg src_loc src_reg)
    | _ -> Warn.user_error "Invalid annot %s for ldr_idx" (pp_annot annot)
  in
  let annot_str annot src_reg loc =
    match annot with
    | AnnotNone -> A.Instruction (A.do_str A.vloc src_reg loc)
    | L -> A.Instruction (A.do_stlr A.vloc src_reg loc)
    | _ -> Warn.user_error "Invalid annot %s for str" (pp_annot annot)
  in
  let annot_str_idx annot src_reg dst_loc dst_reg =
    match annot with
    | AnnotNone -> A.Instruction (A.str_idx src_reg dst_loc dst_reg)
    | _ -> Warn.user_error "Invalid annot %s for ldr_idx" (pp_annot annot)
  in
  let open Cycle in
  match event.direction with
  | Rm true | Wm true ->
      [], src, st (* Compilation is managed by the previous or next edge *)
  | _ ->
      let ins, dst_dep, st =
        match event.direction, event.annot, src with
        | Rm false, _, DepNone ->
            let dst, st = State.next_reg st in
            let ins = [annot_ldr event.annot dst event_reg] in
            ins, DepReg (dst, event.value), st
        | Rm false, _, DepAddr (r, _) ->
            let reg_zero, st = State.next_reg st in
            let dst, st = State.next_reg st in
            let ins =
              A.pseudo [A.do_eor reg_zero r r]
              @ [annot_ldr_idx event.annot dst event_reg reg_zero]
            in
            ins, DepReg (dst, event.value), st
        | Rm false, _, DepCtrl (r, v_opt) ->
            let dst, st = State.next_reg st in
            let ins, st =
              add_ctrl_dep st r v_opt [annot_ldr event.annot dst event_reg]
            in
            ins, DepReg (dst, event.value), st
        | Wm false, _, DepNone ->
            let reg_value, st = State.next_reg st in
            let ins =
              [
                A.mov reg_value (Option.get event.value);
                annot_str event.annot reg_value event_reg;
              ]
            in
            ins, DepNone, st
        | Wm false, _, (DepData (r, v_opt) | DepReg (r, v_opt)) ->
            let ins_val, reg_value, st =
              State.calc_value st (Option.get event.value) r v_opt
            in
            let ins = ins_val @ [annot_str event.annot reg_value event_reg] in
            ins, DepNone, st
        | Wm false, _, DepAddr (r, _) ->
            let reg_zero, st = State.next_reg st in
            let reg_value, st = State.next_reg st in
            let ins =
              A.pseudo [A.do_eor reg_zero r r]
              @ [A.mov reg_value (Option.get event.value)]
              @ [annot_str_idx event.annot reg_value event_reg reg_zero]
            in
            ins, DepNone, st
        | Wm false, _, DepCtrl (r, v_opt) ->
            let reg_value, st = State.next_reg st in
            let ins, st =
              add_ctrl_dep st r v_opt
                [
                  A.mov reg_value (Option.get event.value);
                  annot_str event.annot reg_value event_reg;
                ]
            in
            ins, DepNone, st
        | RegEvent, _, DepNone ->
            Warn.fatal "No dependency passed to a register event"
        | RegEvent, AnnotNone, dep -> [], dep, st (* just carry on *)
        | dir, AnnotNone, _ ->
            Warn.user_error "Direction %s incompatible with dependency %s"
              (pp_direction dir) (pp_node_dep src)
        | dir, annot, _ ->
            Warn.fatal
              "Direction %s incompatible with dependency %s or annotation %s"
              (pp_direction dir) (pp_node_dep src) (pp_annot annot)
      in
      let st =
        (* if the event is marked as "significant", we add a final condition. If the edge compiles the event, this is done after the edge's compilation *)
        if event.is_significant then
          let dst = dependency_reg dst_dep in
          State.add_condition st dst (Option.get event.value)
        else st
      in
      ins, dst_dep, st

(** compile an edge, src is a dependency from the preceding event *)
let compile_edge (st : State.t) (src : Edge.node_dep) (node : Cycle.t) =
  let open Edge in
  let open Cycle in
  match node.edge, src with
  | (Rf _ | Fr _ | Ws _ | Po _), _ -> [], DepNone, st
  | Dp (Addr, _, _, _), DepReg (r, v_opt) -> [], DepAddr (r, v_opt), st
  | Dp (Data, _, _, _), DepReg (r, v_opt) -> [], DepData (r, v_opt), st
  | Dp (Ctrl, _, _, _), DepReg (r, v_opt) -> [], DepCtrl (r, v_opt), st
  | Dmb (b_type, _, _, RegEvent), dep ->
      (* TODO Not sure about whether to forward dependency. It is at least needed for dst=RegEvent *)
      [A.Instruction (A.I_FENCE (A.DMB (A.SY, b_type)))], dep, st
  | Dmb (b_type, _, _, _), _ ->
      [A.Instruction (A.I_FENCE (A.DMB (A.SY, b_type)))], DepNone, st
  | RfReg, dep -> [], dep, st
  | Iico i, _ ->
      let src_event = node.source_event in
      let dst_event = node.next.source_event in
      let src_event_data =
        match src_event.direction with
        | Rm true | Wm true -> Some (get_event_data src_event)
        | _ -> None
      in
      let dst_event_data =
        match dst_event.direction with
        | Rm true | Wm true -> Some (get_event_data dst_event)
        | _ -> None
      in
      let ins, dst_dep, st =
        i.compile_edge st src src_event_data dst_event_data
      in
      let st =
        if dst_event.is_significant && dst_event_data <> None then
          let dep_reg = dependency_reg dst_dep in
          if dep_reg <> A.ZR then
            State.add_condition st dep_reg (Option.get dst_event.value)
          else st
        else st
      in

      ins, dst_dep, st
  | _ ->
      Warn.fatal "Edge -%s->: compilation not implemented" (pp_edge node.edge)

(** Compile a cycle to relevant progs *)
let prog_of_cycle (cycle : Cycle.t) : prog list =
  let loc_count = State.loc_count () in

  let compile_proc nodes =
    let rec init_st = function
      | State.Loc -1 ->
          State.
            {
              free_registers = A.allowed_for_symb;
              env = [];
              initial_values = [];
              final_conditions = [];
            }
      | State.Loc loc_i ->
          let st = init_st (State.Loc (loc_i - 1)) in
          let r, st = State.next_reg st in
          let st = State.set_register st (State.Loc loc_i) r in
          st
    in
    let st = init_st (State.Loc (loc_count - 1)) in

    let ins, _, st =
      List.fold_left
        (fun (ins, dep, st) node ->
          let evt_code, dep, st =
            compile_event st dep node.Cycle.source_event
          in
          let edge_code, dep, st = compile_edge st dep node in
          ins @ evt_code @ edge_code, dep, st)
        ([], Edge.DepNone, st) nodes
    in
    ins, st
  in
  List.map compile_proc (split_by_proc cycle)

(* Dumping test *)

let dump_init (stl : State.t list) (channel : out_channel) =
  let pp_initial_values () =
    let initial_values =
      List.map (fun st -> st.State.initial_values) stl |> List.flatten
    in
    State.pp_initial_values initial_values
    ^ match initial_values with [] -> "" | _ -> "\n"
  in

  let pp_envs () =
    let rec pp_proc_env proc = function
      | [] -> ""
      | (loc, reg) :: q ->
          Printf.sprintf "%d: %s = %s;\t" proc (A.pp_reg reg)
            (State.pp_location loc)
          ^ pp_proc_env proc q
    in
    List.mapi (fun proc st -> "  " ^ pp_proc_env proc st.State.env ^ "\n") stl
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
  | A.Align _ :: _ ->
      Warn.fatal "Page alignment not available in diymicro"

let fmt_cols =
  let rec fmt_col p k = function
    | [] -> k
    | cs :: prog ->
        (Cycle.pp_proc (Cycle.Proc p) :: dump_pseudo cs)
        :: fmt_col (p + 1) k prog
  in
  fmt_col 0 []

let dump_code code channel =
  let pp = fmt_cols code in
  Misc.pp_prog channel pp

let dump_final (stl : State.t list) channel =
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
  ^ (List.mapi (fun proc st -> add_proc proc st.State.final_conditions) stl
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
  Cycle.reset_proc_count ();
  State.reset_loc_count ();
  let cycle = Cycle.make_cycle annot_edges in
  let prog = prog_of_cycle cycle in

  let instructions, stl = List.split prog in
  let baseprog =
    Printf.sprintf "%s (version %s)" Config.prog_name Version.version
  in
  dump_test stl instructions baseprog annot_edges ?name:(Some name) channel
