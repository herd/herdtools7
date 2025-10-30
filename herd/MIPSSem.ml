(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Semantics of MIPS instructions *)

module
  Make
    (C:Sem.Config)
    (V:Value.S with type Cst.Instr.exec = MIPSBase.instruction)
=
  struct
    module MIPS = MIPSArch_herd.Make(SemExtra.ConfigToArchConfig(C))(V)
    module Act = MachAction.Make(C.PC)(MIPS)
    include SemExtra.Make(C)(MIPS)(Act)

(* Barrier pretty print *)
    let sync = {barrier=MIPS.Sync; pp="sync";}

    let barriers = [sync;]
    let isync = None
    let nat_sz =V.Cst.Scalar.machsize
    let atomic_pair_allowed _ _ = true

(********************)
(* Semantics proper *)
(********************)

    module Mixed(SZ:ByteSize.S) = struct

      let (>>=) = M.(>>=)
      let (>>*=) = M.(>>*=)
      let (>>|) = M.(>>|)
      let (>>!) = M.(>>!)
      let (>>::) = M.(>>::)

      let tr_op = function
        | MIPS.ADD|MIPS.ADDU|MIPS.DADDU -> Op.Add (* NB confusing ADD and ADDU... *)
        | MIPS.SUB|MIPS.SUBU -> Op.Sub (* NB confusing SUB and SUBU... *)
        | MIPS.SLT|MIPS.SLTU -> Op.Lt  (* NB confusing SLT and SLTU... *)
        | MIPS.AND -> Op.And
        | MIPS.OR -> Op.Or
        | MIPS.XOR -> Op.Xor
        | MIPS.NOR -> Op.Nor
        | MIPS.DSLL -> Op.ShiftLeft

      let mk_read sz ato loc v =
        Act.Access
          (Dir.R, loc, v, ato, (), sz, Act.access_of_location_std loc)

      let read_reg port r ii = match r with
      | MIPS.IReg MIPS.R0 -> M.unitT V.zero
      | _ ->
          M.read_loc port (mk_read nat_sz false) (A.Location_reg (ii.A.proc,r)) ii

      let read_reg_ord = read_reg Port.No
      and read_reg_data = read_reg Port.Data
      let read_reg_addr = read_reg Port.Addr

      let do_read_mem sz ato a ii = M.read_loc Port.No (mk_read sz ato) (A.Location_global a) ii
      let read_mem sz a ii = do_read_mem sz false a ii
      let read_mem_atomic sz a ii = do_read_mem sz true a ii

      let write_reg r v ii = match r with
      | MIPS.IReg MIPS.R0 -> M.unitT ()
      | _ ->
          M.mk_singleton_es
            (Act.Access
               (Dir.W, (A.Location_reg (ii.A.proc,r)), v, false, (), nat_sz, Access.REG))
            ii

      let write_mem sz a v ii  =
        M.mk_singleton_es
          (Act.Access (Dir.W, A.Location_global a, v, false, (), sz, Access.VIR)) ii

      let write_mem_atomic sz a v resa ii =
        let eq = [M.VC.Assign (a,M.VC.Atom resa)] in
        M.mk_singleton_es_eq
          (Act.Access (Dir.W, A.Location_global a, v, true, (), sz, Access.VIR))
          eq ii

      let create_barrier b ii =
        M.mk_singleton_es (Act.Barrier b) ii

      let commit ii =
        M.mk_singleton_es (Act.Commit (Act.Bcc,None)) ii

      let v2tgt =
        let open Constant in
        function
        | M.A.V.Val(Label (_, lbl)) -> Some (B.Lbl lbl)
        | M.A.V.Val (Concrete i) -> Some (B.Addr (M.A.V.Cst.Scalar.to_int i))
        | _ -> None

      let do_indirect_jump test bds i v =
        match  v2tgt v with
        | Some tgt -> M.unitT (B.Jump (tgt,bds))
        | None ->
           match v with
           | M.A.V.Var(_) as v ->
              let lbls = get_exported_labels test in
              if Label.Full.Set.is_empty lbls && C.variant Variant.Telechat then
                (* We assume a ret/branch is an exit *)
                M.unitT () >>! B.Exit
              else if Label.Full.Set.is_empty lbls then
                Warn.fatal "Could find no potential target for indirect branch %s \
                (potential targets are statically known labels)" (MIPS.dump_instruction i)
              else
                B.indirectBranchT v lbls bds
        | _ -> Warn.fatal
            "illegal argument for the indirect branch instruction %s \
            (must be a label)" (MIPS.dump_instruction i)

(* Promote 16bit immediates to values *)

(* Signed *)
      let imm16ToV k =
        V.Cst.Scalar.of_int (k land 0xffff)
        |> V.Cst.Scalar.sxt MachSize.Short
        |> fun sc -> V.Val (Constant.Concrete sc)

(* Unsigned *)
      let immu16ToV k =
        V.Cst.Scalar.of_int (k land 0xffff)
        |> fun sc -> V.Val (Constant.Concrete sc)

      let is_logical =
        let open MIPS in
        function
        | ADD|ADDU|DADDU
        | SUB|SUBU
        | SLT|SLTU
          -> false
        | OR|AND|XOR|NOR|DSLL
          -> true

      let imm16 op =
        if is_logical op then immu16ToV
        else imm16ToV

(* Entry point *)

      let build_semantics test ii =
        M.addT (A.next_po_index ii.A.program_order_index)
          begin match ii.A.inst with
          | MIPS.NOP -> B.nextT
          | MIPS.LUI (r1,k) ->
             M.op Op.ShiftLeft (V.intToV k) (V.intToV 16)
             >>=  M.op1 (Op.Sxt MachSize.Word)
             >>= fun v -> write_reg r1 v ii
             >>= B.next1T
          | MIPS.LI (r,k) ->
             write_reg r (immu16ToV k) ii >>= B.next1T
          | MIPS.MOVE (r1,r2) ->
              read_reg_ord r2 ii >>= fun v -> write_reg r1 v ii
              >>= B.next1T
          | MIPS.OP (op,r1,r2,r3) ->
              (read_reg_ord r2 ii >>|  read_reg_ord r3 ii) >>=
              (fun (v1,v2) -> M.op (tr_op op) v1 v2) >>=
              (fun v -> write_reg r1 v ii) >>= B.next1T
          | MIPS.OPI (MIPS.DSLL,r1,r2,k) ->
              read_reg_ord r2 ii >>=
              M.op1 (Op.LeftShift k) >>=
              fun v -> write_reg r1 v ii >>= B.next1T
          | MIPS.OPI (op,r1,r2,k) ->
              read_reg_ord r2 ii >>=
              fun v -> M.op (tr_op op) v (imm16 op k) >>=
              fun v -> write_reg r1 v ii >>= B.next1T
          | MIPS.JR (MIPS.IReg MIPS.R31)
               when C.variant Variant.Telechat -> (* Telechat return *)
              M.unitT B.Exit
          | MIPS.JR r as t ->
            read_reg_ord r ii >>= do_indirect_jump test [] t
          | MIPS.B lbl -> B.branchT lbl
          | MIPS.BC (cond,r1,r2,lbl) ->
              (read_reg_ord r1 ii >>| read_reg_ord r2 ii) >>=
              (fun (v1,v2) ->
                M.op
                  (match cond with MIPS.EQ -> Op.Eq | MIPS.NE -> Op.Ne)
                  v1 v2) >>=
              fun v -> commit ii >>= fun () -> B.bccT v lbl
          | MIPS.BCZ (cond,r,lbl) ->
              read_reg_ord r ii >>=
              fun v ->
                M.op
                  (match cond with
                  | MIPS.LEZ -> Op.Le
                  | MIPS.GTZ -> Op.Gt
                  | MIPS.LTZ -> Op.Lt
                  | MIPS.GEZ -> Op.Ge)
                  v V.zero >>=
                fun v -> commit ii >>= fun () -> B.bccT v lbl
          | MIPS.LW (r1,k,r2) ->
              let sz = MachSize.Word in
              read_reg_addr r2 ii >>=
              (fun a -> M.add a (imm16ToV k)) >>=
              (fun ea -> read_mem sz ea ii) >>=
              M.op1 (Op.Sxt sz) >>=
              (fun v -> write_reg r1 v ii)  >>= B.next1T
          | MIPS.LD (r1,k,r2) ->
              read_reg_addr r2 ii >>=
              (fun a -> M.add a (imm16ToV k)) >>=
              (fun ea -> read_mem MachSize.Quad ea ii) >>=
              (fun v -> write_reg r1 v ii)  >>= B.next1T
          | MIPS.SW (r1,k,r2) ->
              (read_reg_data r1 ii >>| read_reg_addr r2 ii) >>=
              (fun (d,a) ->
                (M.add a (imm16ToV k)) >>=
                (fun ea -> write_mem nat_sz ea d ii)) >>= B.next1T
          | MIPS.LL (r1,k,r2) ->
              read_reg_addr r2 ii >>=
              (fun a ->
                (M.add a (imm16ToV k) >>=
                 (fun ea ->
                   write_reg MIPS.RESADDR ea ii >>|
                   (read_mem_atomic nat_sz ea ii >>= fun v -> write_reg r1 v ii))))
                >>= B.next2T
          | MIPS.SC (r1,k,r2) ->
              (read_reg_ord MIPS.RESADDR ii >>|
              read_reg_data r1 ii >>|
              read_reg_addr r2 ii) >>=
              (fun ((resa,v),a) ->
                M.add a (imm16ToV k) >>=
                (fun ea ->
                  write_reg MIPS.RESADDR V.zero ii >>| (* Cancel reservation... *)
                  M.altT
                    (write_reg r1 V.zero ii) (* Failure *)
                    ((write_reg r1 V.one ii
                        >>| write_mem_atomic nat_sz ea v resa ii) >>! ())))
                >>=  B.next2T
          | MIPS.EBF (r1,r2,k1,k2) ->
            let lsb = k1 in
            let msbd = k2 - 1 in
            let hex_mask = begin
              (* similar to UBFM/SBFM in AArch64*)
              let f x = if x < lsb then "0"
                else if x > lsb+msbd then "0"
                else "1" in
              let bitmask = List.rev (List.init 64 f) in
              let dec_mask = Int64.of_string
                (Printf.sprintf "0b%s" (String.concat "" bitmask)) in
              Printf.sprintf "0x%Lx" dec_mask
             end in
            read_reg_ord r2 ii
            >>= M.op1 (Op.AndK hex_mask)
            >>= M.op1 (Op.LogicalRightShift lsb)
            >>= fun v -> write_reg r1 v ii
            >>= B.next1T
          | MIPS.SYNC ->
              create_barrier MIPS.Sync ii >>= B.next1T
          end

      let spurious_setaf _ = assert false

    end
  end
