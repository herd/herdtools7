(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Semantics of ARM instructions *)

module Make (C:Sem.Config)(V:Value.S)
    =
  struct
    module ARM = ARMArch_herd.Make(SemExtra.ConfigToArchConfig(C))(V)
    module Act = MachAction.Make(C.PC)(ARM)
    include SemExtra.Make(C)(ARM)(Act)

(* Barrier pretty print *)
    let  dmb =
      ARMBase.fold_barrier_option
        (fun o k ->
          { barrier = ARMBase.DMB o;
            pp = Misc.lowercase
              (ARMBase.pp_barrier_option "dmb" o);}::k)
        []

    let  dsb =
      ARMBase.fold_barrier_option
        (fun o k ->
          { barrier = ARMBase.DSB o;
            pp = Misc.lowercase
              (ARMBase.pp_barrier_option "dsb" o);}::k)
        dmb

    let barriers = dsb
    let isync = Some { barrier = ARMBase.ISB;pp = "isb";}

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

      let reg_sz = V.Cst.Scalar.machsize
      and nat_sz = V.Cst.Scalar.machsize

      let mk_read ato sz loc v = Act.Access (Dir.R, loc, v, ato, sz)

      let read_reg is_data r ii =
        M.read_loc is_data
          (mk_read false reg_sz)
          (A.Location_reg (ii.A.proc,r)) ii

      let read_reg_ord = read_reg false
      let read_reg_data = read_reg true

      let read_mem sz a ii  =
        M.read_loc false (mk_read false sz) (A.Location_global a) ii
      let read_mem_atomic sz a ii =
        M.read_loc false (mk_read true sz) (A.Location_global a) ii

      let write_loc sz loc v ii =
        M.mk_singleton_es (Act.Access (Dir.W, loc, v, false,sz)) ii

      let write_reg r v ii =
        M.mk_singleton_es (Act.Access (Dir.W, (A.Location_reg (ii.A.proc,r)), v, false,reg_sz)) ii

      let write_mem sz a v ii  =
        M.mk_singleton_es
          (Act.Access (Dir.W, A.Location_global a, v, false, sz)) ii

      let write_mem_atomic sz a v resa ii =
        let eq = [M.VC.Assign (a,M.VC.Atom resa)] in
        M.mk_singleton_es_eq
          (Act.Access (Dir.W, A.Location_global a, v, true, sz)) eq ii

      let write_flag r o v1 v2 ii =
        M.addT
          (A.Location_reg (ii.A.proc,r))
          (M.op o v1 v2) >>= (fun (loc,v) -> write_loc reg_sz loc v ii)

      let create_barrier b ii =
        M.mk_singleton_es (Act.Barrier b) ii

      let commit bcc ii =
        M.mk_singleton_es (Act.Commit bcc) ii

      let flip_flag v = M.op Op.Xor v V.one
      let is_zero v = M.op Op.Eq v V.zero
      let is_not_zero v = M.op Op.Ne v V.zero
      let check_flag = function
        |ARM.AL -> assert false
        |ARM.NE -> flip_flag
        |ARM.EQ -> M.unitT

      let checkZ op c ii = match c with
      | ARM.AL -> op ii >>! B.Next
      | ARM.NE ->
          ((read_reg_ord  ARM.Z ii)
             >>=
           (fun veq ->
             flip_flag veq >>=
             fun veqneg ->
               M.choiceT veqneg
                 (op ii)
                 (M.unitT ())))
            >>! B.Next
      | ARM.EQ ->
          ((read_reg_ord  ARM.Z ii)
             >>=
           (fun veq ->
             M.choiceT veq
               (op ii)
               (M.unitT ())))
            >>! B.Next

      let checkCZ op c ii = match c with
      | ARM.AL -> op ii >>! B.Next
      | ARM.NE ->
          ((read_reg_ord  ARM.Z ii)
             >>=
           (fun veq ->
             flip_flag veq >>=
             fun veqneg ->
               commit false ii >>*=
               fun () ->
                 M.choiceT veqneg
                   (op ii)
                   (M.unitT ())))
            >>! B.Next
      | ARM.EQ ->
          ((read_reg_ord  ARM.Z ii)
             >>=
           (fun veq ->
             commit false ii >>*=
             fun () -> M.choiceT veq
                 (op ii)
                 (M.unitT ())))
            >>! B.Next

      let write_flags set v1 v2 ii = match set with
      | ARM.SetFlags -> write_flag ARM.Z Op.Eq v1 v2 ii
      | ARM.DontSetFlags -> M.unitT ()

      let build_semantics ii =
        M.addT (A.next_po_index ii.A.program_order_index)
          begin match ii.A.inst with
          | ARM.I_NOP -> M.unitT B.Next
          | ARM.I_ADD (set,rd,rs,v) ->
              ((read_reg_ord rs ii)
                 >>=
               (fun vs ->
                 M.add vs (V.intToV v))
                 >>=
               (fun vres ->
                 (write_reg rd vres ii)
                   >>|
                   write_flags set vres (V.intToV 0) ii))
                >>! B.Next
          | ARM.I_SUB (set,rd,rs,v) ->
              ((read_reg_ord rs ii)
                 >>=
               (fun vs ->
                 M.op Op.Sub vs (V.intToV v))
                 >>=
               (fun vres ->
                 (write_reg rd vres ii)
                   >>|
                   write_flags set vres (V.intToV 0) ii))
                >>! B.Next
          | ARM.I_ADD3 (set,rd,rn,rm) ->
              (((read_reg_ord  rn ii) >>| (read_reg_ord rm ii))
                 >>=
               (fun (vn,vm) ->
                 M.op Op.Add vn vm
                   >>=
                 (fun vd ->
                   write_reg rd vd ii
                     >>|
                     write_flags set vd (V.intToV 0) ii)))
                >>! B.Next
          | ARM.I_SUB3 (set,rd,rn,rm) ->
              (((read_reg_ord  rn ii) >>| (read_reg_ord rm ii))
                 >>=
               (fun (vn,vm) ->
                 M.op Op.Sub vn vm
                   >>=
                 (fun vd ->
                   write_reg rd vd ii
                     >>|
                     write_flags set vd (V.intToV 0) ii)))
                >>! B.Next
          | ARM.I_AND (set,rd,rs,v) ->
              ((read_reg_ord  rs ii)
                 >>=
               (fun vs ->
                 M.op Op.And vs (V.intToV v))
                 >>=
               (fun vres ->
                 write_reg  rd vres ii
                   >>|
                   write_flags set vres (V.intToV 0) ii))
                >>! B.Next
          | ARM.I_B lbl -> B.branchT lbl
          | ARM.I_BEQ (lbl) ->
              read_reg_ord ARM.Z ii >>=
              fun v -> commit true ii >>= fun () -> B.bccT v lbl
          | ARM.I_BNE (lbl) ->
              read_reg_ord ARM.Z ii >>=
              fun v -> flip_flag v >>= fun vneg -> commit true ii >>=
                fun () -> B.bccT vneg lbl
          | ARM.I_CB (n,r,lbl) ->
              let cond = if n then is_not_zero else is_zero in
              read_reg_ord r ii >>= cond >>=
              fun v -> commit true ii >>= fun () -> B.bccT v lbl
          | ARM.I_CMPI (r,v) ->
              ((read_reg_ord  r ii)
                 >>=
               (fun vr ->
                 write_flags ARM.SetFlags vr (V.intToV v) ii))
                >>! B.Next
          | ARM.I_CMP (r1,r2) ->
              (((read_reg_ord  r1 ii)  >>| (read_reg_ord  r2 ii))
                 >>=
               (fun (v1,v2) ->
                 write_flags ARM.SetFlags v1 v2 ii))
                >>! B.Next
          |  ARM.I_LDR (rt,rn,c) ->
              let ldr ii =
                (read_reg_ord  rn ii)
                  >>=
                (fun vn ->
                  (read_mem nat_sz vn ii) >>=
                  (fun v -> write_reg  rt v ii)) in
              checkCZ ldr c ii
          |  ARM.I_LDREX (rt,rn) ->
              let ldr ii =
                (read_reg_ord  rn ii)
                  >>=
                (fun vn ->
                  write_reg ARM.RESADDR vn ii >>|
                  (read_mem_atomic nat_sz vn ii >>=
                   fun v -> write_reg  rt v ii)) in
              ldr ii >>! B.Next
          |  ARM.I_LDR3 (rt,rn,rm,c) ->
              let ldr3 ii =
                ((read_reg_ord  rn ii) >>| (read_reg_ord  rm ii))
                  >>=
                (fun (vn,vm) ->
                  (M.add vn vm) >>=
                  (fun vaddr ->
                    (read_mem nat_sz vaddr ii) >>=
                    (fun v -> write_reg  rt v ii))) in
              checkZ ldr3 c ii
          |  ARM.I_STR (rt,rn,c) ->
              let str ii =
                ((read_reg_ord  rn ii) >>| (read_reg_data  rt ii))
                  >>=
                (fun (vn,vt) ->
                  let a = vn in
                  (write_mem nat_sz a vt ii)) in
              checkCZ str c ii
          |  ARM.I_STR3 (rt,rn,rm,c) ->
              let str3 ii =
                (((read_reg_ord  rm ii) >>|
                ((read_reg_ord  rn ii) >>|
                (read_reg_data  rt ii)))
                   >>=
                 (fun (vm,(vn,vt)) ->
                   (M.add vn vm) >>=
                   (fun a ->
                     (write_mem nat_sz a vt ii)))) in
              checkZ str3 c ii
          | ARM.I_STREX (r1,r2,r3,c) ->
              let strex ii =
                (read_reg_ord ARM.RESADDR ii >>| read_reg_data r2 ii >>| read_reg_ord r3 ii) >>=
                fun ((resa,v),a) ->
                  (write_reg ARM.RESADDR V.zero ii >>|
                  M.altT
                    (write_reg r1 V.one ii)
                    ((write_reg r1 V.zero ii >>|
                    write_mem_atomic nat_sz a v resa ii) >>! ())) >>! () in
              checkZ strex c ii
          | ARM.I_MOV (rd, rs, c) ->
              let mov ii =
                read_reg_ord  rs ii >>=
                fun v -> write_reg  rd v ii in
              checkCZ mov c ii
          | ARM.I_MOVI (rt, i, c) ->
              let movi ii =  write_reg  rt (V.intToV i) ii in
              checkCZ movi c ii
          | ARM.I_XOR (set,r3,r1,r2) ->
              (((read_reg_ord  r1 ii) >>| (read_reg_ord r2 ii))
                 >>=
               (fun (v1,v2) ->
                 M.op Op.Xor v1 v2
                   >>=
                 (fun v3 ->
                   write_reg  r3 v3 ii
                     >>|
                     write_flags set v3 (V.intToV 0) ii)))
                >>! B.Next
          | ARM.I_DMB o ->
              (create_barrier (ARM.DMB o) ii)
                >>! B.Next
          | ARM.I_DSB o ->
              (create_barrier (ARM.DSB o) ii)
                >>! B.Next
          | ARM.I_ISB ->
              (create_barrier ARM.ISB ii)
                >>! B.Next
          | ARM.I_SADD16 _ ->
              Warn.user_error "SADD16 not implemented"
          | ARM.I_SEL _ ->
              Warn.user_error "SEL not implemented"
          end
    end
  end
