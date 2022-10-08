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

  module Mixed (SZ : ByteSize.S) = struct
    let rec build_semantics ii =
      M.addT
        (A.next_po_index ii.A.program_order_index)
        (match ii.A.inst with
        | ASLBase.SPass -> B.nextT
        | _ -> Warn.fatal "Not yet implemented: build_semantics")

    let spurious_setaf _ = assert false
  end
end
