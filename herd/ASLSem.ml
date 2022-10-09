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
    let m_par = M.( >>| )
    let ( let* ) = M.( >>= )
    let m_add_instr = M.( >>>> )
    let next ii = M.addT (A.next_po_index ii.A.program_order_index) B.nextT

    let rec build_semantics_expr (e : ASLBase.expr) _ii : V.v M.t =
      match e with
      | ASLBase.ELiteral v -> M.unitT (V.maybevToV (ParsedConstant.intToV v))
      | _ ->
          Warn.fatal "Not yet implemented for ASL: expression semantics for %s"
            (ASLBase.pp_expr e)

    and build_semantics_lexpr (le : ASLBase.lexpr) ii :
        (ASL.location * V.v list) M.t =
      match le with
      | ASLBase.LEVar x -> M.unitT (A.Location_reg (ii.A.proc, x), [])
      | _ ->
          Warn.fatal
            "Not yet implemented for ASL: left-expression semantics for %s"
            (ASLBase.pp_lexpr le)

    and build_semantics ii : (A.program_order_index * B.t) M.t =
      match ii.A.inst with
      | ASLBase.SPass -> next ii
      | ASLBase.SAssign (le, e) ->
          let* v, (loc, _) =
            m_par (build_semantics_expr e ii) (build_semantics_lexpr le ii)
          in
          let* () = M.mk_singleton_es (Act.Access (Dir.W, loc, v, nat_sz)) ii in
          next ii
      | ASLBase.SThen (s1, s2) ->
          m_add_instr
            (build_semantics { ii with A.inst = s1 })
            (fun (poi, _branch) ->
              build_semantics
                { ii with A.inst = s2; A.program_order_index = poi })
      | s ->
          Warn.fatal "Not yet implemented for ASL: statements semantics for %s"
            (ASLBase.pp_stmt s)

    let spurious_setaf _ = assert false
  end
end
