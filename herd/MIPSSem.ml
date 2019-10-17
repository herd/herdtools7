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

module Make (C:Sem.Config)(V:Value.S)
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
        | MIPS.ADD|MIPS.ADDU -> Op.Add (* NB confusing ADD and ADDU... *)
        | MIPS.SUB|MIPS.SUBU -> Op.Sub (* NB confusing SUB and SUBU... *)
        | MIPS.SLT|MIPS.SLTU -> Op.Lt  (* NB confusing SLT and SLTU... *)
        | MIPS.AND -> Op.And
        | MIPS.OR -> Op.Or
        | MIPS.XOR -> Op.Xor
        | MIPS.NOR -> Op.Nor

      let mk_read sz ato loc v = Act.Access (Dir.R, loc, v, ato, sz)

      let read_reg is_data r ii = match r with
      | MIPS.IReg MIPS.R0 -> M.unitT V.zero
      | _ ->
          M.read_loc is_data (mk_read nat_sz false) (A.Location_reg (ii.A.proc,r)) ii

      let read_reg_ord = read_reg false
      let read_reg_data = read_reg true

      let do_read_mem sz ato a ii = M.read_loc false (mk_read sz ato) (A.Location_global a) ii
      let read_mem sz a ii = do_read_mem sz false a ii
      let read_mem_atomic sz a ii = do_read_mem sz true a ii

      let write_reg r v ii = match r with
      | MIPS.IReg MIPS.R0 -> M.unitT ()
      | _ ->
          M.mk_singleton_es
            (Act.Access (Dir.W, (A.Location_reg (ii.A.proc,r)), v, false, nat_sz)) ii

      let write_mem sz a v ii  =
        M.mk_singleton_es (Act.Access (Dir.W, A.Location_global a, v, false, sz)) ii

      let write_mem_atomic sz a v resa ii =
        let eq = [M.VC.Assign (a,M.VC.Atom resa)] in
        M.mk_singleton_es_eq (Act.Access (Dir.W, A.Location_global a, v, true, sz)) eq ii

      let create_barrier b ii =
        M.mk_singleton_es (Act.Barrier b) ii

      let commit ii =
        M.mk_singleton_es (Act.Commit true) ii

(* Entry point *)

      let build_semantics ii =
        M.addT (A.next_po_index ii.A.program_order_index)
          begin match ii.A.inst with
          | MIPS.NOP -> M.unitT B.Next
          | MIPS.LI (r,k) ->
              write_reg r (V.intToV k) ii >>! B.Next
          | MIPS.OP (op,r1,r2,r3) ->
              (read_reg_ord r2 ii >>|  read_reg_ord r3 ii) >>=
              (fun (v1,v2) -> M.op (tr_op op) v1 v2) >>=
              (fun v -> write_reg r1 v ii) >>! B.Next
          | MIPS.OPI (op,r1,r2,k) ->
              read_reg_ord r2 ii >>=
              fun v -> M.op (tr_op op) v (V.intToV k) >>=
                fun v -> write_reg r1 v ii >>! B.Next
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
              read_reg_ord r2 ii >>=
              (fun a -> M.add a (V.intToV k)) >>=
              (fun ea -> read_mem nat_sz ea ii) >>=
              (fun v -> write_reg r1 v ii) >>! B.Next
          | MIPS.SW (r1,k,r2) ->
              (read_reg_data r1 ii >>| read_reg_ord r2 ii) >>=
              (fun (d,a) ->
                (M.add a (V.intToV k)) >>=
                (fun ea -> write_mem nat_sz ea d ii)) >>! B.Next
          | MIPS.LL (r1,k,r2) ->
              read_reg_ord r2 ii >>=
              (fun a ->
                (M.add a (V.intToV k) >>=
                 (fun ea ->
                   write_reg MIPS.RESADDR ea ii >>|
                   (read_mem_atomic nat_sz ea ii >>= fun v -> write_reg r1 v ii))))
                >>! B.Next
          | MIPS.SC (r1,k,r2) ->
              (read_reg_ord MIPS.RESADDR ii >>|
              read_reg_data r1 ii >>|
              read_reg_ord r2 ii) >>=
              (fun ((resa,v),a) ->
                M.add a (V.intToV k) >>=
                (fun ea ->
                  write_reg MIPS.RESADDR V.zero ii >>| (* Cancel reservation... *)
                  M.altT
                    (write_reg r1 V.zero ii) (* Failure *)
                    ((write_reg r1 V.one ii
                        >>| write_mem_atomic nat_sz ea v resa ii) >>! ())))
                >>! B.Next
          | MIPS.SYNC ->
              create_barrier MIPS.Sync ii >>! B.Next
          end
    end
  end
