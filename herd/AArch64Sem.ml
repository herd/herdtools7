(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make
    (TopConf:sig
      module C : Sem.Config
      val precision : bool
      val dirty : DirtyBit.t
    end)
    (V:Value.S)
    =
  struct
    module C = TopConf.C
    module ConfLoc = SemExtra.ConfigToArchConfig(C)
    module AArch64 = AArch64Arch_herd.Make(ConfLoc)(V)

    module Act = MachAction.Make(ConfLoc)(AArch64)
    include SemExtra.Make(C)(AArch64)(Act)

    let mixed = AArch64.is_mixed
    let memtag = C.variant Variant.MemTag
    let is_deps = C.variant Variant.Deps
    let kvm = C.variant Variant.Kvm

(* Barrier pretty print *)
    let barriers =
      let bs = AArch64Base.do_fold_dmb_dsb true (fun h t -> h::t) []
      in List.map
        (fun b ->
          { barrier = b;
            pp = Misc.lowercase (AArch64Base.pp_barrier b)})
        bs
    let isync = Some { barrier = AArch64Base.ISB;pp = "isb";}

    let atomic_pair_allowed _ _ = true

    let quad = MachSize.Quad (* This machine natural size *)

(* Semantics proper *)
    module Mixed(SZ:ByteSize.S) = struct

      module Mixed = M.Mixed(SZ)

      let (>>=) = M.(>>=)
      let (>>==) = M.(>>==)
      let (>>*=) = M.(>>*=)
      let (>>|) = M.(>>|)
      let (>>!) = M.(>>!)
      let (>>::) = M.(>>::)

      let mask32 ty m =
        let open AArch64Base in
        match ty with
        | V32 -> fun v -> M.op1 (Op.Mask MachSize.Word) v >>= m
        | V64 -> m

      let is_zero v = M.op Op.Eq v V.zero
      
(* Ordinary access action *)
     let access_anexp anexp d loc v ac = Act.Access (d,loc,v,AArch64.N,anexp,quad,ac)
     let access_ord d loc v ac = access_anexp AArch64.Exp d loc v ac 

(* Basic read, from register *)
      let mk_read sz an anexp loc v =
        let ac = Act.access_of_location_std loc in
        Act.Access (Dir.R, loc, v, an, anexp, sz, ac)

      let mk_read_std = mk_read quad AArch64.N

      let mk_fault a ii msg =
        M.mk_singleton_es (Act.Fault (ii,A.Location_global a,msg)) ii

      let read_loc v is_data = M.read_loc is_data (mk_read v AArch64.N AArch64.Exp)

      let read_reg is_data r ii = match r with
      | AArch64.ZR -> M.unitT V.zero
      | _ ->
          M.read_loc is_data (mk_read quad AArch64.N AArch64.Exp) (A.Location_reg (ii.A.proc,r)) ii

      let read_reg_sz sz is_data r ii = match sz with
      | MachSize.Quad -> read_reg is_data r ii
      | MachSize.Word|MachSize.Short|MachSize.Byte ->
          read_reg is_data r ii >>= fun v -> M.op1 (Op.Mask sz) v

      let read_reg_ord = read_reg_sz quad false
      let read_reg_ord_sz sz = read_reg_sz sz false
      let read_reg_data sz = read_reg_sz sz true
      let read_reg_tag is_data =  read_reg is_data

(* Basic write, to register  *)
      let mk_write sz an anexp loc v ac = Act.Access (Dir.W, loc, v, an, anexp, sz, ac)

      let write_loc sz an anexp loc v ac ii =
        M.mk_singleton_es (mk_write sz an anexp loc v ac) ii

      let write_reg r v ii = match r with
      | AArch64.ZR -> M.unitT ()
      | _ ->
          write_loc 
            MachSize.Quad AArch64.N AArch64.Exp 
            (A.Location_reg (ii.A.proc,r)) 
            v Act.A_REG ii  

      let write_reg_sz sz r v ii = match r with
      | AArch64.ZR -> M.unitT ()
      | _ -> match sz with
        | MachSize.Quad -> write_reg r v ii
        | MachSize.Word|MachSize.Short|MachSize.Byte ->
            M.op1 (Op.Mask sz) v >>= fun v -> write_reg r v ii

      let write_reg_sz_non_mixed =
        if mixed then fun _sz -> write_reg
        else write_reg_sz

(* Emit commit event *)
      let commit_bcc ii = M.mk_singleton_es (Act.Commit true) ii
      let commit_pred ii = M.mk_singleton_es (Act.Commit false) ii

(* Fence *)
      let create_barrier b ii = M.mk_singleton_es (Act.Barrier b) ii

(* Page tables and TLBs *)
      let inv_loc op loc ii =
        let oloc = if A.TLBI.inv_all op then None else Some loc in
        M.mk_singleton_es (Act.Inv (op,oloc)) ii

(* Data cache operations *)
      let dc_loc op loc ii =
        let oloc = if AArch64Base.DC.sw op then None else Some loc in
        M.mk_singleton_es (Act.DC (op,oloc)) ii


(******************)
(* Memory Tagging *)
(******************)


(* Decompose tagged location *)
      let tag_extract a = M.op1 Op.TagExtract a
      let loc_extract a = M.op1 Op.LocExtract a

(*  Low level tag access *)
      let do_read_tag a ii =
        M.read_loc false
          (fun loc v -> access_ord Dir.R loc v Act.A_TAG)
          (A.Location_global a) ii
      and do_read_tag_nexp a ii =
        M.read_loc false
          (fun loc v -> access_anexp AArch64.NExp Dir.R loc v Act.A_TAG)
          (A.Location_global a) ii
      and do_write_tag a v ii =
        let loc = A.Location_global a in
        M.mk_singleton_es
          (access_ord Dir.W loc v Act.A_TAG)
          ii

(* Read tag from memory *)
      let read_tag_mem a ii =
        M.op1 Op.TagLoc a >>= fun atag -> do_read_tag_nexp atag ii

(*******************)
(* Memory accesses *)
(*******************)


(*
  Implementation of accesses depends upon the appropriate variants,
 *)

      let delayed_check_tags a_virt ma ii m1 m2 =
        let (++) = M.bind_ctrl_avoid ma in
        M.check_tags
          ma (fun a -> read_tag_mem a ii)
          (fun tag1 -> tag_extract a_virt  >>= fun tag2 -> M.op Op.Eq tag1 tag2)
          (commit_pred ii)  ++ fun cond ->  M.choiceT cond m1 m2

      let do_checked_read sz an aexp rd a ii =
        check_tags a ii
          (loc_extract a >>= fun a ->
           M.read_loc false (mk_read sz an aexp) (A.Location_global a) ii >>= fun v ->
           write_reg_sz sz rd v ii >>! B.Next)
          (mk_fault a ii (Some "MTE read") >>! B.Exit)

(* PTW Basics *)

	open Constant 

	let extract_af v = M.op1 Op.AF v 
	let extract_db v = M.op1 Op.DB v 
	let extract_dbm v = M.op1 Op.DBM v 
	let extract_valid v = M.op1 Op.Valid v 
	let extract_oa v = M.op1 Op.OA v 

        let mextract_whole_pte_val a_pte an ii =
	  (M.read_loc false
	    (fun loc v ->
	      Act.Access (Dir.R,loc,v,an,AArch64.NExp,quad,Act.A_PTE))
	     (A.Location_global a_pte) ii)

	let mextract_pte_vals pte_v =
	  (extract_oa pte_v >>|  
	   extract_valid pte_v >>| 
	   extract_af pte_v >>| 
	   extract_db pte_v >>| 
	   extract_dbm pte_v) 

       let check_ptw proc dir a_virt ma an ii mdirect mok mfault =

          let do_m a_phy = (mok ma a_phy) in 
          let set_bit a_pte new_val =
            (M.read_loc false 
               (fun loc _ -> 
                  Act.Access (Dir.W,loc,new_val,an,AArch64.NExp,quad,Act.A_PTE)) 
             (A.Location_global a_pte) ii) in

   (*
     The dirty bit correspond to HW level write permission in PTE's.
     Hence, in simple (stage 1) case, we have AP[2] == 0b1 for clean,
     and AP[2] for dirty, with AP[2] == 0b0 being more directly "writable".

     Without HW-management (on old CPUs, or where TCR_ELx.{HA,HD} == {0,0}): 

     A load/store to x where pte_x has the access flag clear will raise a
     permission fault 

     A store to x where pte_x has the dirty bit clear will raise a permission
     fault 

     and SW is expected to deal with this by updating the translation tables with
     explicit stores or atomics
   *)

        let check_bit_clear bit_v kont1 kont2 = 
          is_zero bit_v >>= fun bit_clear ->
             commit_bcc ii >>= fun () ->
             M.choiceT bit_clear 
              kont1 
              kont2 in

         let notTTHM a_virt ma a_phy af_v db_v = 
           let check_db = check_bit_clear db_v (mfault ma a_virt) (do_m a_phy) in
           let kont_af = begin match dir with  
                         | Dir.R -> (do_m a_phy) 
                         | Dir.W -> check_db
                         end in 
           check_bit_clear af_v (mfault ma a_virt) kont_af in

    (*
      With HW management (i.e. when ARMv8.1-TTHM is implemented) where TCR_ELx.HA = 1: 
      A load/store to x where pte_x has the access flag clear results in the MMU
      updating the translation table entry to set the access flag, and continuing
      without a fault.

      A store where pte_x has the dirty bit clear will raise a permission fault.
     *)
 
         let isTTHM_and_HA a_pte pte_v af_v kont_R kont_W =
           let kont_af_clear = begin match dir with
                               | Dir.R -> (M.op1 Op.SetAF pte_v >>= fun new_af -> 
                                           set_bit a_pte new_af >>= fun _ -> kont_R)
                               | Dir.W -> kont_W
                               end in
           let kont_af_set = begin match dir with
                               | Dir.R -> kont_R
                               | Dir.W -> kont_W
                               end in
           check_bit_clear af_v 
             kont_af_clear 
             kont_af_set
           in 

     (*
       With HW management (i.e. when ARMv8.1-TTHM is implemented) where TCR_ELx.{HA,HD} == {1,1}:

       A load/store to x where pte_x has the access flag clear results in the
       MMU updating the translation table entry to set the access flag, and continuing
       without a fault.

       A store to x where pte_x has the dirty bit clear and also has DBM clear
       will raise a permission fault

       A store to x where pte_x has the dirty bit clear and has DBM set results in the
       MMU updating the translation table entry to set the dirty bit, and continuing
       without a fault.  
      *)

         let isTTHM_and_HA_and_HD a_pte pte_v a_phy af_v db_v dbm_v =
           let kont_R = do_m a_phy in 
           let kont_W =
             let kont_db = begin check_bit_clear dbm_v
                           (mfault ma a_virt)
                           (M.op1 Op.SetDB pte_v >>= fun new_db ->
                            set_bit a_pte new_db >>= fun _ -> kont_R) end in
             check_bit_clear db_v kont_db kont_R in 
           (isTTHM_and_HA a_pte pte_v af_v kont_R kont_W)  
      
         in


         let mvirt = begin
           (M.op1 Op.PTELoc a_virt) >>= fun a_pte -> 
             (mextract_whole_pte_val a_pte an ii) >>= fun pte_v ->
             (mextract_pte_vals pte_v) >>=
             fun ((((a_phy,valid_v),af_v),db_v),dbm_v) -> 

           let kont_valid =
             let open DirtyBit in
             let tthm = TopConf.dirty.tthm proc
             and ha = TopConf.dirty.ha proc
             and hd = TopConf.dirty.hd proc in
             if (not tthm || (tthm && (not ha && not hd))) then 
               (notTTHM a_virt ma a_phy af_v db_v) 
             else if (tthm && ha && not hd) then
               let kont_R = (do_m a_phy) in  
               let kont_W = check_bit_clear db_v (mfault ma a_virt) kont_R in
               (isTTHM_and_HA a_pte pte_v af_v kont_R kont_W) 
             else (*if (tthm && ha && hd)*) 
               (isTTHM_and_HA_and_HD a_pte pte_v a_phy af_v db_v dbm_v) in 

           check_bit_clear valid_v (mfault ma a_virt) kont_valid end in
            
        (M.op1 Op.IsVirtual a_virt >>= fun cond ->
           M.choiceT cond mvirt mdirect)

(* Old read_mem that returns value read *)
      let do_read_mem_ret sz an anexp ac a ii =
        if mixed then begin
          Mixed.read_mixed false sz (fun sz -> mk_read sz an anexp) a ii
        end else
          let mk_act loc v =  Act.Access (Dir.R,loc,v,an,anexp,sz,ac) in
          let loc = A.Location_global a in
          M.read_loc false mk_act loc ii

(* Save value read in register rd *)

      let do_read_mem sz an anexp ac rd a ii =
        do_read_mem_ret sz an anexp ac a ii >>=
        fun v -> write_reg_sz_non_mixed sz rd v ii >>! B.Next

      let read_mem sz = do_read_mem sz AArch64.N
      let read_mem_acquire sz = do_read_mem sz AArch64.A
      let read_mem_acquire_pc sz = do_read_mem sz AArch64.Q
      let read_mem_noreturn sz = do_read_mem sz AArch64.NoRet

      let read_mem_reserve sz an anexp ac rd a ii =
        (write_reg AArch64.ResAddr a ii >>|
        do_read_mem sz an anexp ac rd a ii) >>=
        (fun ((),b) -> M.unitT b)


(* Write *)

      let do_write_mem sz an anexp ac a v ii =
        if mixed then begin
          Mixed.write_mixed sz
            (fun sz loc v -> mk_write sz an anexp loc v Act.A_VIR)
            a v ii
        end else
          write_loc sz an anexp (A.Location_global a) v ac ii

      let write_mem sz = do_write_mem sz AArch64.N
      let write_mem_release sz = do_write_mem sz AArch64.L
      let write_mem_amo sz = do_write_mem sz AArch64.X
      let write_mem_amo_release sz = do_write_mem sz AArch64.XL

(* Write atomic *)
      let write_mem_atomic sz an anexp ac a v resa ii =
        (M.assign a resa >>| do_write_mem sz an anexp ac a v ii) >>! ()

      let flip_flag v = M.op Op.Xor v V.one
      let is_zero v = M.op Op.Eq v V.zero
      let is_not_zero v = M.op Op.Ne v V.zero
      let is_ge v = M.op Op.Ge v V.zero
      let is_gt v = M.op Op.Gt v V.zero
      let is_le v = M.op Op.Le v V.zero
      let is_lt v = M.op Op.Lt v V.zero

      let tr_cond = function
        | AArch64.NE -> is_zero
        | AArch64.EQ -> is_not_zero
        | AArch64.GE -> is_ge
        | AArch64.GT -> is_gt
        | AArch64.LE -> is_le
        | AArch64.LT -> is_lt

(* Page tables and TLBs *)
    let do_inv op a ii = inv_loc op (A.Location_global a) ii

(* Data cache operations *)
    let do_dc op a ii = dc_loc op (A.Location_global a) ii

(***********************)
(* Memory instructions *)
(***********************)

      let get_ea rs kr ii = match kr with
      | AArch64.K k ->
          read_reg_ord rs ii >>= fun v -> M.add v (V.intToV k)
      | AArch64.RV(_,r) ->
          (read_reg_ord rs ii >>| read_reg_ord r ii) >>= fun (v1,v2) ->
            M.add v1 v2

        let lift_memtag_phy mop a_virt ma ii =
          M.delay ma >>= fun (_,ma) ->
          let mm = mop Act.A_PHY ma in
          delayed_check_tags a_virt ma ii
            (mm  >>! B.Next)
            (let mfault = mk_fault a_virt ii in
            if TopConf.precision then  mfault >>! B.Exit
           else (mfault >>| mm) >>! B.Next)

        let lift_memtag_virt mop ma ii =
          M.delay ma >>= fun (a_virt,ma) ->
          let mm = mop Act.A_VIR (ma >>= fun a -> loc_extract a) in
          delayed_check_tags a_virt ma ii
            (mm  >>! B.Next)
            (let mfault = ma >>= fun a -> mk_fault a ii in
            if TopConf.precision then  mfault >>! B.Exit
           else (mfault >>| mm) >>! B.Next)

        let lift_kvm dir mop ma an ii mphy =
           let mfault _ma a =
             mk_fault a ii
               >>! if TopConf.precision then B.Exit else B.ReExec
            in
            M.delay ma >>= fun (_,ma) ->
            ma >>= fun a ->
            match Act.access_of_location_std (A.Location_global a) with
            | Act.A_VIR ->
               check_ptw ii.AArch64.proc dir a ma an ii
                  (mop Act.A_PTE ma >>! B.Next)
                  mphy 
                  mfault
            | ac -> mop ac ma >>! B.Next

        let lift_memop dir mop ma an ii =
        if memtag then 
          if kvm then 
            let mphy = (fun ma a -> lift_memtag_phy mop a ma ii) in
            lift_kvm dir mop ma an ii mphy
          else lift_memtag_virt mop ma ii
        else if kvm then
          let mphy = (fun _ma a -> mop Act.A_PHY (M.unitT a) >>! B.Next) in
          lift_kvm dir mop ma an ii mphy
        else
          mop Act.A_VIR ma >>! B.Next

      let do_str sz an anexp ma mv ii =
        lift_memop Dir.W
          (fun ac ma ->
            (ma >>| mv) >>= fun (a,v) ->
            do_write_mem sz an anexp ac a v ii)
          ma an ii

      let do_ldr sz an anexp rd ma ii =
          lift_memop Dir.R
            (fun ac ma -> ma >>= fun a -> do_read_mem sz an anexp ac rd a ii)
          ma an ii
(* 
     let do_ldr sz an anexp rd ma ii =
          ma >>= fun a_virt ->
            (M.op1 Op.PTELoc a_virt) >>= fun a_pte ->
              (M.read_loc false
                (fun loc v ->
                  Act.Access (Dir.R,loc,v,an,AArch64.NExp,quad,Act.A_PTE))
                (A.Location_global a_pte) ii) >>= fun pte_v ->
             (M.op1 Op.OA pte_v) >>= fun a_phy ->
             do_read_mem sz an anexp Act.A_PHY rd a_phy ii
*)


      let ldr sz rd rs kr ii =
         do_ldr sz AArch64.N AArch64.Exp rd (get_ea rs kr ii) ii

      and str sz rs rd kr ii =
         do_str sz AArch64.N AArch64.Exp (get_ea rd kr ii) (read_reg_data sz rs ii) ii

      and stlr sz rs rd ii = do_str sz AArch64.L AArch64.Exp (read_reg_ord rd ii) (read_reg_data sz rs ii) ii

      and ldar sz t rd rs ii =
        let open AArch64 in
        let an = match t with
          | XX -> AArch64.X
          | AA -> AArch64.A
          | AX -> AArch64.XA
          | AQ -> AArch64.Q
        in
        lift_memop Dir.R
          (fun ac ma ->
            ma >>= fun a ->
              match t with
              | XX ->
                  read_mem_reserve sz AArch64.X AArch64.Exp ac rd a ii
              | AA ->
                  read_mem_acquire sz AArch64.Exp ac rd a ii
              | AX ->
                  read_mem_reserve sz AArch64.XA AArch64.Exp ac rd a ii
              | AQ ->
                  read_mem_acquire_pc sz AArch64.Exp ac rd a ii)
          (read_reg_ord rs ii) an ii

      and stxr sz t rr rs rd ii =
        let open AArch64Base in
        let an = match t with
          | YY -> AArch64.X
          | LY -> AArch64.XL
        in
         lift_memop Dir.W
         (fun ac ma ->
           M.riscv_store_conditional
             (read_reg_ord ResAddr ii)
             (read_reg_data sz rs ii)
             ma
             (write_reg ResAddr V.zero ii)
             (fun v -> write_reg rr v ii)
             (fun ea resa v -> match t with
             | YY -> write_mem_atomic sz AArch64.X AArch64.Exp ac ea v resa ii
             | LY -> write_mem_atomic sz AArch64.XL AArch64.Exp ac ea v resa ii))
          (read_reg_ord rd ii) an ii

      let csel_op op v =
        let open AArch64Base in  match op with
        | Cpy -> M.unitT v
        | Inc -> M.op Op.Add v V.one
        | Neg -> M.op Op.Sub V.zero v
        | Inv -> Warn.fatal "size dependent inverse not implemented"

      (* Apply a shift as monadic op *)
      let shift s =
        let open AArch64Base in
        match s with
          | S_NOEXT   -> M.unitT
          | S_LSL(n)  -> fun x -> M.op (Op.ShiftLeft) x (V.intToV n)
          | S_LSR(n)  -> fun x -> M.op (Op.ShiftRight) x (V.intToV n)
          | S_ASR(n)  -> fun x -> M.op (Op.ASR) x (V.intToV n)
          | S_SXTW -> fun x ->
            let m = V.op1 (Op.LeftShift 31) V.one in
            M.op Op.Xor x m
            >>= fun v -> M.op Op.Sub v m
          | S_UXTW->
            Warn.fatal "UXTW barrel shift not supported yet"

      let rmw_amo_read rmw sz = let open AArch64 in match rmw with
      | RMW_A|RMW_AL -> do_read_mem_ret sz XA
      | RMW_L|RMW_P  -> do_read_mem_ret sz X

      and rmw_amo_write rmw sz = let open AArch64 in match rmw with
      | RMW_L|RMW_AL -> do_write_mem sz XL
      | RMW_P|RMW_A  -> do_write_mem sz X

      let swp sz rmw r1 r2 r3 ii =
        let open AArch64Base in
        let an = match rmw with
        (*this an is passed to the check_ptw access, which is a read
          so keeping the annotations applicable to reads only*)
        | RMW_P | RMW_L -> AArch64.N
        | RMW_A | RMW_AL -> AArch64.A
        in
        match r2 with
        | ZR ->
            let write_mem = match rmw with
            | RMW_L|RMW_AL -> write_mem_release
            | RMW_P|RMW_A  -> write_mem in
            (*SWP is a write for the purpose of the DB*)
            lift_memop Dir.W
              (fun ac ma ->
                (ma >>| read_reg_data sz r1 ii) >>= fun (a,v) ->
                 write_mem sz AArch64.Exp ac a v ii)
              (read_reg_ord r3 ii) an ii
        |  _ ->
            let read_mem = rmw_amo_read rmw
            and write_mem = rmw_amo_write rmw in
            lift_memop Dir.W
              (fun ac ma ->
                let r2 = read_reg_data sz r1 ii
                and w2 v = write_reg r2 v ii (* no sz since alread masked *) 
                and r1 a = read_mem sz AArch64.Exp ac a ii
                and w1 a v = write_mem sz AArch64.Exp ac a v ii in
                M.swp ma r1 r2 w1 w2)
              (read_reg_ord r3 ii) an ii

      let cas sz rmw rs rt rn ii =
        let open AArch64 in
        let an = match rmw with
        (*this an is passed to the check_ptw access, which is a read
          so keeping the annotations applicable to reads only*)
        | RMW_P | RMW_L -> AArch64.N
        | RMW_A | RMW_AL -> AArch64.A
        in
        (*CAS is a write for the purpose of the DB*)
        lift_memop Dir.W
          (fun ac ma ->
            let read_rs = read_reg_ord_sz sz rs ii in
            M.altT
              (ma >>= fun a ->
               (read_rs >>|
              begin let read_mem sz = match rmw with
               | RMW_A|RMW_AL -> do_read_mem_ret sz A AArch64.Exp ac
               | RMW_L|RMW_P  -> do_read_mem_ret sz N AArch64.Exp ac in
               read_mem sz a ii >>=
               fun v -> write_reg_sz_non_mixed sz rs v ii >>! v end) >>=
               fun (cv,v) -> M.neqT cv v >>! ())
              (let read_rt =  read_reg_data sz rt ii
              and read_mem a = rmw_amo_read rmw sz AArch64.Exp ac a ii
              and write_mem a v = rmw_amo_write rmw sz AArch64.Exp ac a v ii
              and write_rs v =  write_reg rs v ii in
              M.aarch64_cas_ok
                ma read_rs read_rt write_rs read_mem write_mem M.eqT))
          (read_reg_ord rn ii) an ii

      let ldop op sz rmw rs rt rn ii =
        let open AArch64 in
        let an = match rmw with
        (*this an is passed to the check_ptw access, which is a read
          so keeping the annotations applicable to reads only*)
        | RMW_P | RMW_L -> AArch64.N
        | RMW_A | RMW_AL -> AArch64.A
        in
         lift_memop Dir.R
          (fun ac ma ->
            let noret = match rt with | ZR -> true | _ -> false in
            let op = match op with
            | A_ADD -> Op.Add
            | A_EOR -> Op.Xor
            | A_SET -> Op.Or
            | A_CLR -> Op.AndNot2
            | A_SMAX -> Op.Max
            | A_SMIN -> Op.Min in
            let read_mem = if noret then fun sz -> do_read_mem_ret sz NoRet AArch64.Exp 
                                    else fun sz -> rmw_amo_read rmw sz AArch64.Exp
            and write_mem = fun sz -> rmw_amo_write rmw sz AArch64.Exp in
            M.amo_strict op
              ma (fun a -> read_mem sz ac a ii) (read_reg_data sz rs ii)
              (fun a v -> write_mem sz ac a v ii)
              (fun w -> if noret then M.unitT () else write_reg_sz_non_mixed sz rt w ii))
         (read_reg_ord rn ii) an ii

      let build_semantics ii =
        M.addT (A.next_po_index ii.A.program_order_index)
          AArch64Base.(
        match ii.A.inst with
        | I_NOP ->
            M.unitT B.Next
              (* Branches *)
        | I_B l ->
            B.branchT l

        | I_BC(c,l)->
            read_reg_ord NZP ii  >>= tr_cond c >>= fun v ->
              commit_bcc ii >>= fun () -> B.bccT v l

        | I_CBZ(_,r,l) ->
            (read_reg_ord r ii)
              >>= is_zero
              >>= fun v -> commit_bcc ii
                  >>= fun () -> B.bccT v l

        | I_CBNZ(_,r,l) ->
            (read_reg_ord r ii)
              >>= is_not_zero
              >>= fun v -> commit_bcc ii
                  >>= fun () -> B.bccT v l
        | I_TBZ(_,r,k,l) ->
            (read_reg_ord r ii)
              >>= M.op1 (Op.ReadBit k)
              >>= is_zero
              >>= fun v -> commit_bcc ii
                  >>= fun () -> B.bccT v l
        | I_TBNZ(_,r,k,l) ->
            (read_reg_ord r ii)
              >>= M.op1 (Op.ReadBit k)
              >>= is_not_zero
              >>= fun v -> commit_bcc ii
                  >>= fun () -> B.bccT v l

                      (* Load and Store *)
        | I_LDR(var,rd,rs,kr) ->
            let sz = tr_variant var in
            ldr sz rd rs kr ii
        | I_LDRBH (bh, rd, rs, kr) ->
            let sz = bh_to_sz bh in
            ldr sz rd rs kr ii

        | I_LDAR(var,t,rd,rs) ->
            let sz = tr_variant var in
            ldar sz t rd rs ii
        | I_LDARBH(bh,t,rd,rs) ->
            let sz = bh_to_sz bh in
            ldar sz t rd rs ii

        | I_STR(var,rs,rd,kr) ->
            str (tr_variant var) rs rd kr ii

        | I_STRBH(bh,rs,rd,kr) ->
            str (bh_to_sz bh) rs rd kr ii

        | I_STLR(var,rs,rd) ->
            stlr (tr_variant var) rs rd ii

        | I_STLRBH(bh,rs,rd) ->
            stlr (bh_to_sz bh) rs rd ii

        | I_STZG(rt,rn,kr) ->
            if not memtag then Warn.user_error "STZG without -variant memtag" ;
            begin
              (read_reg_data MachSize.Quad rt ii >>= tag_extract) >>|
              get_ea rn kr ii
            end >>= fun (v,a) ->
            (M.op1 Op.TagLoc a >>| loc_extract a) >>= fun (atag,loc) ->
            (do_write_tag atag v ii >>| do_write_mem MachSize.Quad AArch64.N AArch64.Exp Act.A_VIR loc V.zero ii) >>! B.Next

        | I_STG(rt,rn,kr) ->
            if not memtag then Warn.user_error "STG without -variant memtag" ;
            begin
              (read_reg_data quad rt ii >>= tag_extract) >>|
              get_ea rn kr ii
            end >>= fun (v,a) ->
            M.op1 Op.TagLoc a  >>= fun a ->
            do_write_tag a v ii >>! B.Next
 
        | I_LDG (rt,rn,kr) ->
            if not memtag then Warn.user_error "LDG without -variant memtag" ;
            get_ea rn kr ii  >>=
            fun a -> M.op1 Op.TagLoc a >>=
            fun atag -> do_read_tag atag ii
            >>= fun tag ->
            M.op Op.SetTag a tag >>= fun v ->
            write_reg rt v ii >>! B.Next

        | I_STXR(var,t,rr,rs,rd) ->
            stxr (tr_variant var) t rr rs rd ii
        | I_STXRBH(bh,t,rr,rs,rd) ->
            stxr (bh_to_sz bh) t rr rs rd ii

        (* Operations *)
        | I_MOV(var,r,K k) ->
            (* Masking assumed to be useless, given k size. Hence _sz ignored *)
            (mask32 var
              (fun k -> write_reg r k ii)
              (V.intToV k))
              >>! B.Next

        | I_MOV(var,r1,RV (_,r2)) ->
            let sz = tr_variant var in
            read_reg_ord_sz sz r2 ii >>= fun v -> write_reg r1 v ii >>! B.Next

        | I_ADDR (r,lbl) ->
            write_reg r (V.nameToV lbl) ii >>! B.Next
        | I_SXTW(rd,rs) ->
            let m = V.op1 (Op.LeftShift 31) V.one in
            (read_reg_ord_sz MachSize.Word rs ii) >>=
            fun v -> (* Encode sign extension 32 -> 64 *)
              M.op Op.Xor v m >>=
              fun x -> M.op Op.Sub x m >>=
                fun v -> write_reg rd v ii >>! B.Next

        | I_OP3(ty,op,rd,rn,kr,os) ->
            let sz = tr_variant ty in
            (* Check correctness of shift, and shift if correct *)
            (* These checks aren't needed, but correctness checks are good! *)
            (* Besides this seems to be the only place they are checked... *)
            (* Details can be found in the Arm Arch reference manual *)
            let check_and_shift op ty s = begin match op, ty, s with
              (*These patterns could be further merged, but are not for legibility *)
            | _,V64,S_SXTW ->
                shift s (* sign extension should always be possible *)
            | (ADD|ADDS), V32, (S_LSL(n)|S_LSR(n)|S_ASR(n)) when (n >=0 && n < 32) ->
                shift s
            | (ADD|ADDS), V64, (S_LSL(n)|S_LSR(n)|S_ASR(n)) when (n >=0 && n < 64) ->
                shift s
            | (AND|ANDS), V32, (S_LSL(n)|S_LSR(n)|S_ASR(n)) when (n >=0 && n < 32) ->
                shift s (* todo add ROR shift if it occurs*)
            | (AND|ANDS), V64, (S_LSL(n)|S_LSR(n)|S_ASR(n)) when (n >=0 && n < 64) ->
                shift s (* todo add ROR shift if it occurs*)
            | (SUB|SUBS), V32, (S_LSL(n)|S_LSR(n)|S_ASR(n)) when (n >=0 && n < 32) ->
                shift s
            | (SUB|SUBS), V64, (S_LSL(n)|S_LSR(n)|S_ASR(n)) when (n >=0 && n < 64) ->
                shift s
            | (ORR|EOR), V32, (S_LSL(n)|S_LSR(n)|S_ASR(n)) when (n >=0 && n < 32) ->
                shift s (* todo add ROR shift if it occurs*)
            | (ORR|EOR), V64, (S_LSL(n)|S_LSR(n)|S_ASR(n)) when (n >=0 && n < 64) ->
                shift s (* todo add ROR shift if it occues*)
            | _ ->
                Warn.fatal "Unsupported shift arg %s in %s instruction %s"
                  (pp_barrel_shift "" s pp_imm)
                  (pp_variant ty)
                  (pp_op op)
            end in
            begin match kr with
            | RV (_,r) when reg_compare r rn = 0 -> (* register variant*)
                (* Keep sharing here, otherwise performance penalty on address
                   dependency by r^r in mixed size mode *)
                read_reg_ord_sz sz rn ii >>= fun v ->
                (* if present, apply an optional inline barrel shift *)
                begin match os with
                | S_NOEXT    -> M.unitT (v,v)
                | s -> check_and_shift op ty s v
                       >>= fun v -> M.unitT (v,v)
                end
            | RV (_,r) -> (* register variant *)
                (* no sharing, we optionally shift v2 and return the pair *)
                read_reg_ord_sz sz rn ii  >>| read_reg_ord_sz sz r ii
                (* if present, apply an optional inline barrel shift *)
                >>= fun (v1,v2) ->
                begin match os with
                | S_NOEXT -> M.unitT (v1,v2)
                | s -> check_and_shift op ty s v2
                       >>= fun v2 -> M.unitT(v1,v2)
                end
            | K k -> (* immediate  *)
                read_reg_ord_sz sz rn ii >>|
                begin match os with
                | S_NOEXT -> M.unitT (V.intToV k)
                | s -> check_and_shift op ty s (V.intToV k)
                end
            end
              >>=
            begin match op with
            | ADD|ADDS -> fun (v1,v2) -> M.add v1 v2
            | EOR -> fun (v1,v2) -> M.op Op.Xor v1 v2
            | ORR -> fun (v1,v2) -> M.op Op.Or v1 v2
            | SUB|SUBS -> fun (v1,v2) -> M.op Op.Sub v1 v2
            | AND|ANDS -> fun (v1,v2) -> M.op Op.And v1 v2
            | ASR -> fun (v1, v2) -> M.op Op.ASR v1 v2
            | LSR -> fun (v1,v2) -> M.op Op.Lsr v1 v2
            end >>=
            (let m =  (fun v ->
              (write_reg rd v ii) >>|
              (match op with
              | ADDS|SUBS|ANDS -> is_zero v >>= fun v -> write_reg NZP v ii
              | ADD|EOR|ORR|AND|SUB|ASR|LSR -> M.unitT ())) in
            mask32 ty m) >>!
            B.Next
              (* Barrier *)
        | I_FENCE b ->
            (create_barrier b ii) >>! B.Next
              (* Conditional selection *)
        | I_CSEL (var,r1,r2,r3,c,op) ->
            let sz = tr_variant var in
            let mask = match op with
            | Cpy|Neg -> fun m -> m
            | Inc|Inv -> mask32 var in
            if not (C.variant Variant.NotWeakPredicated) then
              read_reg_ord NZP ii >>= tr_cond c >>= fun v ->
                commit_bcc ii >>= fun () ->
                M.choiceT v
                  (read_reg_data sz r2 ii >>= fun v -> write_reg r1 v ii)
                  (read_reg_data sz r3 ii >>=
                   csel_op op >>= mask (fun v ->  write_reg r1 v ii))
                  >>! B.Next
            else
              begin
                (read_reg_ord NZP ii >>= tr_cond c) >>|  read_reg_data sz r2 ii >>| read_reg_data sz r3 ii
              end >>= fun ((v,v2),v3) ->
                M.condPredT v
                  (M.unitT ())
                  (write_reg r1 v2 ii)
                  (csel_op op v3 >>= mask (fun v ->  write_reg r1 v ii))
                >>! B.Next
(* Swap *)
        | I_SWP (v,rmw,r1,r2,r3) -> swp (tr_variant v) rmw r1 r2 r3 ii >>! B.Next
        | I_SWPBH (v,rmw,r1,r2,r3) -> swp (bh_to_sz v) rmw r1 r2 r3 ii >>! B.Next
(* Compare & Swap *)
        | I_CAS (v,rmw,rs,rt,rn) ->
            cas (tr_variant v) rmw rs rt rn ii >>! B.Next
        | I_CASBH (v,rmw,rs,rt,rn) ->
            cas (bh_to_sz v) rmw rs rt rn ii >>! B.Next
(* Fetch and Op *)
        | I_STOP (op,v,w,rs,rn) ->
            ldop op (tr_variant v) (w_to_rmw w) rs ZR rn ii >>! B.Next
        | I_LDOP (op,v,rmw,rs,rt,rn) ->
            ldop op (tr_variant v) rmw rs rt rn ii >>! B.Next
        | I_STOPBH (op,v,w,rs,rn) ->
            ldop op (bh_to_sz v) (w_to_rmw w) rs ZR rn ii >>! B.Next
        | I_LDOPBH (op,v,rmw,rs,rt,rn) ->
            ldop op (bh_to_sz v) rmw rs rt rn ii >>! B.Next
(* Page tables and TLBs *)
        | I_TLBI (op, rd) ->
          read_reg_ord rd ii >>= fun a ->
          do_inv op a ii >>! B.Next
(* Data cache instructions *)
        | I_DC (op,rd) -> read_reg_ord rd ii >>= fun a ->
          do_dc op a ii >>! B.Next
(*  Cannot handle *)
     | (I_RBIT _|I_MRS _|I_LDP _|I_STP _|I_IC _|I_BL _|I_BLR _|I_BR _|I_RET _) as i ->
          Warn.fatal "illegal instruction: %s"
            (AArch64.dump_instruction i)
     )
  end
end

