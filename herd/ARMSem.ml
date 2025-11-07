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

module
  Make
    (C:Sem.Config)
    (V:Value.S with type Cst.Instr.exec = ARMBase.instruction)
    =
  struct
    module ARM = ARMArch_herd.Make(SemExtra.ConfigToArchConfig(C))(V)
    module Act = MachAction.Make(C.PC)(ARM)
    include SemExtra.Make(C)(ARM)(Act)

    let aexp = ARM.Exp    (* Explicit accesses *)
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

    let v2tgt =
      let open Constant in
      function
      | M.A.V.Val(Label (_, lbl)) -> Some (B.Lbl lbl)
      | M.A.V.Val (Concrete i) -> Some (B.Addr (M.A.V.Cst.Scalar.to_int i))
      | _ -> None


(********************)
(* Semantics proper *)
(********************)

    module Mixed(SZ:ByteSize.S) = struct

      let (>>=) = M.(>>=)
      let (>>*=) = M.(>>*=)
      let (>>|) = M.(>>|)
      let (>>!) = M.(>>!)
      let (>>::) = M.(>>::)

      let do_indirect_jump test bds i v =
      match  v2tgt v with
      | Some tgt -> M.unitT (B.Jump (tgt,bds))
      | None ->
         match v with
         | M.A.V.Var(_) as v ->
            let lbls = get_exported_labels test in
              if Label.Full.Set.is_empty lbls then begin
                if C.variant Variant.Telechat then M.unitT () >>! B.Exit
                else
                  Warn.fatal "Could find no potential target for indirect branch %s \
                    (potential targets are statically known labels)" (ARM.dump_instruction i)
                end
              else
                B.indirectBranchT v lbls bds
      | _ -> Warn.fatal
          "illegal argument for the indirect branch instruction %s \
           (must be a label)" (ARM.dump_instruction i)

      let reg_sz = V.Cst.Scalar.machsize
      and nat_sz = V.Cst.Scalar.machsize

      let mk_read ato sz loc v =
        Act.Access (Dir.R, loc, v, ato,ARM.Exp, sz, Act.access_of_location_std loc)

      let read_reg port r ii =
        M.read_loc port
          (mk_read ARM.N reg_sz)
          (A.Location_reg (ii.A.proc,r)) ii

      let read_reg_ord = read_reg Port.No
      let read_reg_data = read_reg Port.Data
      let read_reg_addr = read_reg Port.Addr

      let read_mem sz a ii  =
        M.read_loc Port.No (mk_read ARM.N sz) (A.Location_global a) ii
      let read_mem_atomic sz a ii =
        M.read_loc Port.No (mk_read ARM.X sz) (A.Location_global a) ii

      let do_read_mem_ret sz an anexp ac a ii =
        let mk_act loc v =  Act.Access (Dir.R,loc,v,an,anexp,sz,ac) in
        let loc = A.Location_global a in
        M.read_loc Port.No mk_act loc ii

      let write_loc sz loc v ii =
        let ac = Act.access_of_location_std loc in
        M.mk_singleton_es (Act.Access (Dir.W, loc, v, ARM.N, ARM.Exp, sz, ac)) ii

      let write_reg r v ii =
        M.mk_singleton_es
          (Act.Access (Dir.W, (A.Location_reg (ii.A.proc,r)), v, ARM.N, ARM.exp_annot, reg_sz, Access.REG))
          ii

      let write_mem sz a v ii  =
        M.mk_singleton_es
          (Act.Access (Dir.W, A.Location_global a, v, ARM.N, ARM.Exp, sz, Access.VIR))
          ii

      (* Acquire / release semantics like AArch64 *)
      let do_read_mem sz an anexp ac rd a ii =
        do_read_mem_ret sz an anexp ac a ii
        >>= fun v -> write_reg rd v ii
        >>= fun () -> B.nextT

      let read_mem_acquire sz = do_read_mem sz ARM.A aexp Access.VIR
      let read_mem_acquire_ex sz = do_read_mem sz ARM.XA aexp Access.VIR

      let write_mem_atomic sz a v resa ii =
        let eq = [M.VC.Assign (a,M.VC.Atom resa)] in
        M.mk_singleton_es_eq
          (Act.Access (Dir.W, A.Location_global a, v, ARM.X, ARM.Exp, sz, Access.VIR))
          eq ii

      let write_mem_atomic_release sz a v resa ii =
        let eq = [M.VC.Assign (a,M.VC.Atom resa)] in
        M.mk_singleton_es_eq
          (Act.Access (Dir.W, A.Location_global a, v, ARM.XL, ARM.Exp, sz, Access.VIR))
          eq ii

      let write_mem_release sz a v ii  =
        M.mk_singleton_es
          (Act.Access (Dir.W, A.Location_global a, v, ARM.L, ARM.Exp, sz, Access.VIR))
          ii

      let write_flag r o v1 v2 ii =
        M.addT
          (A.Location_reg (ii.A.proc,r))
          (M.op o v1 v2) >>= (fun (loc,v) -> write_loc reg_sz loc v ii)

      let create_barrier b ii =
        M.mk_singleton_es (Act.Barrier b) ii

      let commit bcc ii =
        M.mk_singleton_es (Act.Commit (bcc,None)) ii

      let flip_flag v = M.op Op.Xor v V.one
      let is_zero v = M.op Op.Eq v V.zero
      let is_not_zero v = M.op Op.Ne v V.zero

      let check_flag = function
        |ARM.AL -> assert false
        |ARM.NE -> flip_flag
        |ARM.EQ -> M.unitT

      let check_flag_op mf op ii =
        mf ii >>*= fun b ->  M.choiceT b (op ii) (M.unitT ())
        >>= B.next1T

      let checkZ op c ii = match c with
      | ARM.AL -> op ii >>= B.next1T
      | ARM.NE ->
          check_flag_op
            (fun ii -> read_reg_ord ARM.Z ii >>= flip_flag) op ii
      | ARM.EQ ->
          check_flag_op (read_reg_ord ARM.Z) op ii


      let write_flags set v1 v2 ii = match set with
      | ARM.SetFlags -> write_flag ARM.Z Op.Eq v1 v2 ii
      | ARM.DontSetFlags -> M.unitT ()

      let build_semantics test ii =
        M.addT (A.next_po_index ii.A.program_order_index)
          begin match ii.A.inst with
          | ARM.I_NOP -> B.nextT
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
                >>= B.next2T
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
                >>= B.next2T
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
                >>= B.next2T
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
                >>= B.next2T
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
                >>= B.next2T
          | ARM.I_ANDC (c,rd,rs,rs2) ->
              let andc ii = ((read_reg_ord rs ii) >>| (read_reg_ord rs2 ii)
                 >>=
               (fun (v1,v2) -> M.op Op.And v1 v2)
                 >>=
               (fun vres -> write_reg rd vres ii)) in
              checkZ andc c ii
          | ARM.I_ORR (set,rd,rs,v) ->
              ((read_reg_ord  rs ii)
                 >>=
               (fun vs ->
                 M.op Op.Or vs (V.intToV v))
                 >>=
               (fun vres ->
                 write_reg  rd vres ii
                   >>|
                   write_flags set vres (V.intToV 0) ii))
                >>= B.next2T
          | ARM.I_B lbl -> B.branchT lbl
          | ARM.I_BEQ (lbl) ->
              read_reg_ord ARM.Z ii >>=
              fun v -> commit Act.Bcc ii >>= fun () -> B.bccT v lbl
          | ARM.I_BNE (lbl) ->
              read_reg_ord ARM.Z ii >>=
              fun v -> flip_flag v >>= fun vneg -> commit Act.Bcc ii >>=
                fun () -> B.bccT vneg lbl
          | ARM.I_BX r as i->
            read_reg_ord r ii >>= do_indirect_jump test [] i

          | ARM.I_CB (n,r,lbl) ->
              let cond = if n then is_not_zero else is_zero in
              read_reg_ord r ii >>= cond >>=
              fun v -> commit Act.Bcc ii >>= fun () -> B.bccT v lbl
          | ARM.I_CMPI (r,v) ->
              ((read_reg_ord  r ii)
                 >>=
               (fun vr ->
                 write_flags ARM.SetFlags vr (V.intToV v) ii))
                >>= B.next1T
          | ARM.I_CMP (r1,r2) ->
              (((read_reg_ord  r1 ii)  >>| (read_reg_ord  r2 ii))
                 >>=
               (fun (v1,v2) ->
                 write_flags ARM.SetFlags v1 v2 ii))
                >>= B.next1T
          |  ARM.I_LDR (rt,rn,c) ->
              let ldr ii =
                (read_reg_addr  rn ii)
                  >>=
                (fun vn ->
                  (read_mem nat_sz vn ii) >>=
                  (fun v -> write_reg  rt v ii)) in
              checkZ ldr c ii
          |  ARM.I_LDRD (rd1,rd2,ra, None) ->
            read_reg_addr ra ii
            >>= fun a ->
              (read_mem nat_sz a ii) >>|
              (M.add a (V.intToV 4) >>= fun a2->read_mem nat_sz a2 ii)
            >>= fun (v1,v2) ->
              write_reg rd1 v1 ii >>| write_reg rd2 v2 ii
            >>= B.next2T
          |  ARM.I_LDRD (rd1,rd2,ra, Some k) ->
            read_reg_addr ra ii
            >>= fun a ->
              (M.add a (V.intToV k) >>= fun a->
              ((read_mem nat_sz a ii) >>|
              (M.add a (V.intToV 4) >>= fun a2->read_mem nat_sz a2 ii)))
            >>= fun (v1,v2) ->
              write_reg rd1 v1 ii >>| write_reg rd2 v2 ii
            >>= B.next2T

          |  ARM.I_LDM2 (ra,r1,r2,i) ->
              (read_reg_addr ra ii)
                >>=
              (fun va ->
                (match i with
                 | ARM.NO -> M.unitT va
                 | ARM.IB -> (M.add va (V.intToV 4)))
                >>= fun va ->
                (read_mem nat_sz va ii) >>|
                (M.add va (V.intToV 4) >>=
                  fun vb -> read_mem nat_sz vb ii))
              >>= fun (v1,v2) ->
                  (write_reg r1 v1 ii >>| write_reg r2 v2 ii)
              >>= B.next2T
          |  ARM.I_LDM3 (ra,r1,r2,r3,i) ->
              (read_reg_addr ra ii)
                >>=
              (fun va ->
                (match i with
                 | ARM.NO -> M.unitT va
                 | ARM.IB -> (M.add va (V.intToV 4)))
                >>= fun va ->
                  (M.unitT va >>|
                  M.add va (V.intToV 4) >>|
                  M.add va (V.intToV 8))
                >>= fun ((v1,v2),v3) ->
                  (read_mem nat_sz v1 ii >>|
                  (read_mem nat_sz v2 ii >>|
                  read_mem nat_sz v3 ii)))
                >>= fun (v1,(v2,v3)) ->
                  (write_reg r1 v1 ii >>|
                   write_reg r2 v2 ii >>|
                   write_reg r3 v3 ii)
              >>= B.next3T
          | ARM.I_LDRO (rd,rs,v,c) ->
              let ldr ii =
                read_reg_addr rs ii
                  >>= (fun vn ->
                    M.add vn (V.intToV v)
                    >>= fun vn -> read_mem nat_sz vn ii
                    >>= fun v -> write_reg rd v ii) in
              checkZ ldr c ii
          |  ARM.I_LDREX (rt,rn) ->
              let ldr ii =
                (read_reg_addr  rn ii)
                  >>=
                (fun vn ->
                  write_reg ARM.RESADDR vn ii >>|
                  (read_mem_atomic nat_sz vn ii >>=
                   fun v -> write_reg  rt v ii)) in
              ldr ii >>= B.next2T
          |  ARM.I_LDAEX (rt,rn) ->
              (read_reg_addr rn ii)
                >>=
              (fun vn ->
                write_reg ARM.RESADDR vn ii >>|
                (read_mem_acquire_ex nat_sz rt vn ii))
                >>= fun (_,_) -> B.nextT
          |  ARM.I_LDA (rt,rn) ->
              (read_reg_addr rn ii)
                  >>=
              fun vn ->
                read_mem_acquire nat_sz rt vn ii
                  >>=
              fun _ -> B.nextT
          |  ARM.I_LDR3 (rt,rn,rm,c) ->
              let ldr3 ii =
                ((read_reg_addr rn ii) >>| (read_reg_addr rm ii))
                  >>=
                (fun (vn,vm) ->
                  (M.add vn vm) >>=
                  (fun vaddr ->
                    (read_mem nat_sz vaddr ii) >>=
                    (fun v -> write_reg  rt v ii))) in
              checkZ ldr3 c ii
          |  ARM.I_LDR3_S (rt,rn,rm,ARM.S_LSL k, c) ->
              let ldr3 ii =
                ((read_reg_addr rn ii) >>| (read_reg_addr rm ii))
                  >>=
                (fun (vn,vm) ->
                  (M.op1 (Op.LeftShift k) vm)
                  >>= fun vm -> (M.add vn vm) >>=
                  (fun vaddr ->
                    (read_mem nat_sz vaddr ii) >>=
                    (fun v -> write_reg  rt v ii))) in
              checkZ ldr3 c ii
          |  ARM.I_STR (rt,rn,c) ->
              let str ii =
                ((read_reg_addr rn ii) >>| (read_reg_data  rt ii))
                  >>=
                (fun (vn,vt) ->
                  let a = vn in
                  (write_mem nat_sz a vt ii)) in
              checkZ str c ii
          |  ARM.I_STL (rt,rn,c) ->
              let str ii =
                ((read_reg_addr rn ii) >>| (read_reg_data  rt ii))
                  >>=
                (fun (vn,vt) ->
                  let a = vn in
                  (write_mem_release nat_sz a vt ii)) in
              checkZ str c ii
          |  ARM.I_STR3 (rt,rn,rm,c) ->
              let str3 ii =
                (((read_reg_addr rm ii) >>|
                ((read_reg_addr rn ii) >>|
                (read_reg_data  rt ii)))
                   >>=
                 (fun (vm,(vn,vt)) ->
                   (M.add vn vm) >>=
                   (fun a ->
                     (write_mem nat_sz a vt ii)))) in
              checkZ str3 c ii
          |  ARM.I_STR3_S (rt,rn,rm,ARM.S_LSL k, c) ->
              let str3 ii =
                (((read_reg_addr rm ii) >>|
                ((read_reg_addr rn ii) >>|
                (read_reg_data  rt ii)))
                   >>=
                 (fun (vm,(vn,vt)) -> (M.op1 (Op.LeftShift k) vm)
                   >>= fun vm -> (M.add vn vm) >>=
                   (fun a ->
                     (write_mem nat_sz a vt ii)))) in
              checkZ str3 c ii
          | ARM.I_STREX (r1,r2,r3,c) ->
              let strex ii =
                (read_reg_ord ARM.RESADDR ii >>| read_reg_data r2 ii
                 >>| read_reg_addr r3 ii) >>=
                fun ((resa,v),a) ->
                  (write_reg ARM.RESADDR V.zero ii >>|
                  M.altT
                    (write_reg r1 V.one ii)
                    ((write_reg r1 V.zero ii >>|
                    write_mem_atomic nat_sz a v resa ii) >>! ())) >>! () in
              checkZ strex c ii
          | ARM.I_STLEX (r1,r2,r3) ->
              let stlex ii =
                (read_reg_ord ARM.RESADDR ii >>| read_reg_data r2 ii
                 >>| read_reg_addr r3 ii) >>=
                fun ((resa,v),a) ->
                  (write_reg ARM.RESADDR V.zero ii >>|
                  M.altT
                    (write_reg r1 V.one ii)
                    ((write_reg r1 V.zero ii >>|
                    write_mem_atomic_release nat_sz a v resa ii) >>! ())) >>! () in
              checkZ stlex ARM.AL ii
          | ARM.I_MOV (rd, rs, c) ->
              let mov ii =
                read_reg_ord  rs ii >>=
                fun v -> write_reg  rd v ii in
              checkZ mov c ii
          | ARM.I_MOVI (rt, i, c) ->
              let movi ii =  write_reg  rt (V.intToV i) ii in
              checkZ movi c ii
          | ARM.I_MOVW (rt, k, c) ->
              assert (MachSize.is_imm16 k);
              let movi ii =  write_reg  rt (V.intToV k) ii in
              checkZ movi c ii
          | ARM.I_MOVT (rt, k, c) ->
              assert (MachSize.is_imm16 k);
              let movi ii =
                M.op1 (Op.LeftShift 16) (V.intToV k)
                >>= fun k -> write_reg  rt k ii in
              checkZ movi c ii
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
                >>= B.next2T
          | ARM.I_DMB o ->
              (create_barrier (ARM.DMB o) ii)
                >>= B.next1T
          | ARM.I_DSB o ->
              (create_barrier (ARM.DSB o) ii)
                >>= B.next1T
          | ARM.I_ISB ->
              (create_barrier ARM.ISB ii)
                >>= B.next1T
          | ARM.I_SADD16 _ ->
              Warn.user_error "SADD16 not implemented"
          | ARM.I_SEL _ ->
              Warn.user_error "SEL not implemented"

          end

      let spurious_setaf _ = assert false

    end

  end
