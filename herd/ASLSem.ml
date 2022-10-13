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
    let nextT ii = M.addT (A.next_po_index ii.A.program_order_index) B.nextT

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

    and build_semantics ii : (A.program_order_index * B.t) M.t =
      match ii.A.inst with
      | ASLBase.SPass -> nextT ii
      | ASLBase.SAssign (le, e) ->
          let* v = build_semantics_expr e ii
          and* loc = build_semantics_lexpr le ii in
          let* () = write_loc loc v ii in
          nextT ii
      | ASLBase.SThen (s1, s2) ->
          M.cseq
            (build_semantics (next_with_inst ii s1))
            (fun (poi, _branch) ->
              build_semantics
                { ii with A.inst = s2; A.program_order_index = poi })
      | ASLBase.SCond (e, s1, s2) ->
          M.( >>*= ) (build_semantics_expr e ii) (fun v ->
              let then_branch = build_semantics (next_with_inst ii s1) in
              let else_branch = build_semantics (next_with_inst ii s2) in
              M.choiceT v then_branch else_branch)

    let spurious_setaf _ = assert false
  end
end
