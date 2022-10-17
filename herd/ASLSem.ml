module Make (Conf : Sem.Config) (V : Value.S) = struct
  module ASL =
    GenArch_herd.Make (ASLBase) (SemExtra.ConfigToArchConfig (Conf)) (V)

  module Act = ASLAction.Make (ASL)
  include SemExtra.Make (Conf) (ASL) (Act)

  (* No barriers in ASL yet. *)
  let barriers = []
  let isync = None

  (* I am not sure I need this as I do not have any atomic operation. *)
  let atomic_pair_allowed e1 e2 = e1.E.iiid == e2.E.iiid
  let nat_sz = V.Cst.Scalar.machsize

  module Mixed (SZ : ByteSize.S) = struct
    (* Helpers *)
    let ( and* ) = M.( >>| )
    let ( let* ) = M.( >>= )

    let nextm ii a =
      M.addT (A.next_po_index ii.A.program_order_index) (M.unitT a)

    let nextT ii = nextm ii (B.Next [], false)

    let next ii =
      {
        ii with
        A.program_order_index = A.next_po_index ii.A.program_order_index;
      }

    let next_with_inst ii s = { (next ii) with A.inst = s }
    let loc_of_identifier x ii = A.Location_reg (ii.A.proc, x)

    let compute_addr (a : V.v) (i : V.v) : V.v M.t =
      let* i' = M.op Op.ShiftLeft i (V.intToV 2) in
      M.add a i'

    let read_loc loc ii =
      M.read_loc true (fun loc' v -> Act.Access (Dir.R, loc', v, nat_sz)) loc ii

    let write_loc loc v ii =
      M.write_loc (fun loc' -> Act.Access (Dir.W, loc', v, nat_sz)) loc ii

    (* Real semantic functions *)
    let rec build_semantics_expr (e : ASLBase.expr) ii : V.v M.t =
      match e with
      | ASLBase.ELiteral v -> M.unitT (V.maybevToV (ParsedConstant.intToV v))
      | ASLBase.EVar x -> read_loc (loc_of_identifier x ii) ii
      | ASLBase.EBinop (e1, op, e2) ->
          let* v1 = build_semantics_expr e1 ii
          and* v2 = build_semantics_expr e2 ii in
          M.op op v1 v2
      | ASLBase.EGet (e1, e2) ->
          let* a = build_semantics_expr e1 ii
          and* i = build_semantics_expr e2 ii in
          let* addr = compute_addr a i in
          read_loc (A.Location_global addr) ii
      | _ ->
          Warn.fatal "Not yet implemented for ASL: expression semantics for %s"
            (ASLBase.pp_expr e)

    and build_semantics_lexpr (le : ASLBase.lexpr) ii : ASL.location M.t =
      match le with
      | ASLBase.LEVar x -> M.unitT (loc_of_identifier x ii)
      | ASLBase.LESet (le, e) ->
          let* a =
            let* la = build_semantics_lexpr le ii in
            read_loc la ii
          and* i = build_semantics_expr e ii in
          let* addr = compute_addr a i in
          M.unitT (A.Location_global addr)

    and build_semantics_stmt s ii : (A.program_order_index * (B.t * bool)) M.t =
      match s with
      | ASLBase.SPass -> nextT ii
      | ASLBase.SReturn -> nextm ii (B.Next [], true)
      | ASLBase.SExit -> nextm ii (B.Exit, true)
      | ASLBase.SAssign (le, e) ->
          let* v = build_semantics_expr e ii
          and* loc = build_semantics_lexpr le ii in
          let* () = write_loc loc v ii in
          nextT ii
      | ASLBase.SThen (s1, s2) ->
          M.cseq (build_semantics_stmt s1 ii) (fun (poi, (branch, ret)) ->
              match (branch, ret) with
              | _, true -> M.unitT (poi, (branch, ret))
              | B.PushAndJump (i, l), _ ->
                  M.unitT (poi, (B.PushAndJump (SThen (i, s2), l), false))
              | _ ->
                  build_semantics_stmt s2
                    { ii with A.program_order_index = poi })
      | ASLBase.SCond (e, s1, s2) ->
          M.( >>*= ) (build_semantics_expr e ii) (fun v ->
              let then_branch = build_semantics_stmt s1 (next ii) in
              let else_branch = build_semantics_stmt s2 (next ii) in
              M.choiceT v then_branch else_branch)
      | ASLBase.SCall (name, args) ->
          (*
             Here we iterate on the arguments and place them inside the right registers.
             No checks are done at runtime, so everything is supposed to work.
             The iteration is a fold_left on args, inside a paralelization monad.
             The [acc] argument of the folder, ie the accumulater of the fold operation,
             is actually a counter for which argument we are at.
          *)
          let one_arg i e =
            let* v = build_semantics_expr e ii in
            let reg_name = ASLBase.reg_arg name i in
            write_loc (loc_of_identifier reg_name ii) v ii
          in
          let args_ops = List.mapi one_arg args in
          let folder a b =
            let* () = a and* () = b in
            M.unitT ()
          in
          let* () = List.fold_left folder (M.unitT ()) args_ops in
          nextm ii (B.PushAndJump (ASLBase.SPass, name), false)

    and build_semantics ii =
      let* poi, (branch, _ret) = build_semantics_stmt ii.A.inst ii in
      M.unitT (poi, branch)

    let spurious_setaf _ = assert false
  end
end
