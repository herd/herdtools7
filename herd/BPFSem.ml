(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Copyright (c) 2024 Puranjay Mohan <puranjay@kernel.org>                  *)
(*                                                                          *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Semantics of BPF instructions *)

module
  Make
    (C : Sem.Config)
    (V : Value.S with type Cst.Instr.exec = BPFBase.instruction) =
struct
  module BPF = BPFArch_herd.Make (SemExtra.ConfigToArchConfig (C)) (V)
  module Act = MachAction.Make (C.PC) (BPF)
  include SemExtra.Make (C) (BPF) (Act)

  (* Barrier pretty print *)
  let sync = None
  let barriers = []
  let isync = None

  (*  TODO: let nat_sz = MachSize.Quad (* 64-bit Registers *) *)
  let nat_sz = V.Cst.Scalar.machsize
  let atomic_pair_allowed _ _ = true

  (********************)
  (* Semantics proper *)
  (********************)

  module Mixed (SZ : ByteSize.S) = struct
    let ( >>= ) = M.( >>= )
    let ( >>*= ) = M.( >>*= )
    let ( >>| ) = M.( >>| )
    let ( >>! ) = M.( >>! )
    let ( >>:: ) = M.( >>:: )
    let unimplemented op = Warn.user_error "BPF operation %s is not implemented (yet)" op

    let tr_opamo op =
      match op with
      | BPF.AMOXCHG -> assert false
      | BPF.ADD -> Op.Add
      | BPF.AND -> Op.And
      | BPF.OR -> Op.Or
      | BPF.XOR -> Op.Xor
      | BPF.AMOCMPXCHG -> assert false
      | _ -> unimplemented "atomic op"
    ;;

    let tr_op = function
      | BPF.ADD -> Op.Add
      | BPF.SUB -> Op.Sub
      | BPF.AND -> Op.And
      | BPF.OR -> Op.Or
      | BPF.XOR -> Op.Xor
      | BPF.MUL -> Op.Mul
      | BPF.DIV -> Op.Div
      | BPF.REM -> Op.Rem
      | BPF.LSL -> Op.ShiftLeft
      | BPF.LSR -> Op.Lsr
      | BPF.ASR -> unimplemented (BPF.pp_op BPF.ASR)
      | BPF.AMOCMPXCHG -> unimplemented "non-atomic CMPXCHG"
      | BPF.AMOXCHG -> unimplemented "non-atomic XCHG"
    ;;

    let tr_cond cond =
      match cond with
      | BPF.EQ -> Op.Eq
      | BPF.NE -> Op.Ne
      | BPF.LT -> Op.Lt
      | BPF.GE -> Op.Ge
    ;;

    let mk_read sz ato loc v =
      Act.Access (Dir.R, loc, v, ato, (), sz, Act.access_of_location_std loc)
    ;;

    let read_reg port r ii =
      M.read_loc port (mk_read nat_sz BPF.N) (A.Location_reg (ii.A.proc, r)) ii
    ;;

    let read_reg_ord = read_reg Port.No
    let read_reg_data = read_reg Port.Data
    let read_reg_addr= read_reg Port.Addr

    let do_read_mem sz ato a ii =
      M.read_loc Port.No (mk_read sz ato) (A.Location_global a) ii
    ;;

    let read_mem sz a ii = do_read_mem sz BPF.N a ii
    let read_mem_sc sz a ii = do_read_mem sz BPF.SC a ii
    let read_mem_acq sz a ii = do_read_mem sz BPF.A a ii

    let write_reg r v ii =
      M.mk_singleton_es
        (Act.Access
           (Dir.W, A.Location_reg (ii.A.proc, r), v, BPF.N, (), nat_sz, Access.REG))
        ii
    ;;

    let write_mem_rel sz a v ii =
      M.mk_singleton_es
        (Act.Access (Dir.W, A.Location_global a, v, BPF.R, (), sz, Access.VIR))
        ii
    ;;

    let write_mem_sc sz a v ii =
      M.mk_singleton_es
        (Act.Access (Dir.W, A.Location_global a, v, BPF.SC, (), sz, Access.VIR))
        ii
    ;;

    let write_mem sz a v ii =
      M.mk_singleton_es
        (Act.Access (Dir.W, A.Location_global a, v, BPF.N, (), sz, Access.VIR))
        ii
    ;;

    let commit ii = M.mk_singleton_es (Act.Commit (Act.Bcc, None)) ii

    (* Signed *)
    let imm16ToV k =
      V.Cst.Scalar.of_int (k land 0xffff)
      |> V.Cst.Scalar.sxt MachSize.Short
      |> fun sc -> V.Val (Constant.Concrete sc)
    ;;

    let amo sz op an rd rs k f ii =
      let open BPF in
      let ra = read_reg_addr rd ii
      and rv = read_reg_data rs ii
      and r0 = read_reg_data (IReg R0) ii
      and rmem_sc vloc = read_mem_sc sz vloc ii
      and rmem vloc = read_mem sz vloc ii
      and wmem_sc vloc v = write_mem_sc sz vloc v ii >>! ()
      and ca v = M.add v (imm16ToV k) in
      let ra_c = ra >>= fun a -> ca a in
      match op with
      | AMOXCHG ->
        ra
        >>| rv
        >>= (fun (ea, vstore) ->
              ca ea
              >>= fun loc ->
              M.read_loc
                Port.Data
                (fun loc v -> Act.Amo (loc, v, vstore, an, (), sz, Access.VIR))
                (A.Location_global loc)
                ii)
        >>= fun r -> write_reg rs r ii
      | AMOCMPXCHG ->
        M.altT
          (M.linux_cmpexch_ok ra_c r0 rv rmem_sc wmem_sc M.assign)
          (M.linux_cmpexch_no ra_c r0 rmem M.neqT)
        >>= fun r -> write_reg (IReg R0) r ii
      | _ ->
        ra
        >>| rv
        >>= (fun (ea, v) ->
              ca ea
              >>= fun loc ->
              M.fetch
                (tr_opamo op)
                v
                (fun v vstored ->
                  Act.Amo (A.Location_global loc, v, vstored, an, (), sz, Access.VIR))
                ii)
        >>= fun v ->
        (match f with
         | true -> write_reg rs v ii
         | false -> M.unitT ())
    ;;

    (* Entry point *)

    let tr_sz = BPF.tr_width

    let build_semantics _ ii =
      M.addT
        (A.next_po_index ii.A.program_order_index)
        (match ii.A.inst with
         | BPF.OP (op, r1, r2) ->
           read_reg_ord r1 ii
           >>| read_reg_ord r2 ii
           >>= (fun (v1, v2) -> M.op (tr_op op) v1 v2)
           >>= (fun v -> write_reg r1 v ii)
           >>= B.next1T
         | BPF.OPI (op, r1, k) ->
           read_reg_ord r1 ii
           >>= fun v ->
           M.op (tr_op op) v (V.intToV k) >>= fun v -> write_reg r1 v ii >>= B.next1T
         | BPF.LOAD (w, _s, r1, r2, k) ->
           let sz = tr_sz w in
           read_reg_addr r2 ii
           >>= (fun a -> M.add a (imm16ToV k))
           >>= (fun ea -> read_mem sz ea ii)
           >>= (fun v -> write_reg r1 v ii)
           >>= B.next1T
         | BPF.LDAQ (w, r1, r2, k) ->
           let sz = tr_sz w in
           read_reg_addr r2 ii
           >>= (fun a -> M.add a (imm16ToV k))
           >>= (fun ea -> read_mem_acq sz ea ii)
           >>= (fun v -> write_reg r1 v ii)
           >>= B.next1T
         | BPF.STORE (sz, r1, k, r2) ->
           read_reg_addr r1 ii
           >>| read_reg_data r2 ii
           >>= (fun (a, d) ->
                 M.add a (imm16ToV k) >>= fun ea -> write_mem (tr_sz sz) ea d ii)
           >>= B.next1T
         | BPF.STRL (sz, r1, k, r2) ->
           read_reg_addr r1 ii
           >>| read_reg_data r2 ii
           >>= (fun (a, d) ->
                 M.add a (imm16ToV k) >>= fun ea -> write_mem_rel (tr_sz sz) ea d ii)
           >>= B.next1T
         | BPF.STOREI (sz, r1, k1, k2) ->
           read_reg_addr r1 ii
           >>= (fun a ->
                 M.add a (imm16ToV k1)
                 >>= fun ea -> write_mem (tr_sz sz) ea (V.intToV k2) ii)
           >>= B.next1T
         | BPF.MOV (rd, rs) ->
           read_reg_ord rs ii >>= fun v -> write_reg rd v ii >>= B.next1T
         | BPF.MOVI (rd, k) -> write_reg rd (V.intToV k) ii >>= B.next1T
         | BPF.AMO (aop, w, rd, k, rs, annot, f) ->
           amo (tr_sz w) aop annot rd rs k f ii >>= B.next1T
         | BPF.GOTO lbl -> B.branchT lbl
         | BPF.JCOND (c, r1, r2, lbl) ->
           read_reg_ord r1 ii
           >>| read_reg_ord r2 ii
           >>= fun (v1, v2) ->
           M.op (tr_cond c) v1 v2 >>= fun v -> commit ii >>= fun () -> B.bccT v lbl
         | BPF.JCONDI (c, r1, k, lbl) ->
           read_reg_ord r1 ii
           >>= fun v ->
           M.op (tr_cond c) v (V.intToV k)
           >>= fun v -> commit ii >>= fun () -> B.bccT v lbl)
    ;;

    let spurious_setaf _ = assert false
  end
end
