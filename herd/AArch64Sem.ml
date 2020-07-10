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
    (C:sig include Sem.Config val precision : bool end)
    (V:Value.S)
    =
  struct
    module ConfLoc = SemExtra.ConfigToArchConfig(C)
    module AArch64 = AArch64Arch_herd.Make(ConfLoc)(V)

    module Act = MachAction.Make(ConfLoc)(AArch64)
    include SemExtra.Make(C)(AArch64)(Act)

    let mixed = AArch64.is_mixed
    let memtag = C.variant Variant.MemTag
    let is_deps = C.variant Variant.Deps
    let kvm = C.variant Variant.Kvm
    let tthm = C.variant Variant.TTHM
    let ha = C.variant Variant.HA
    let hd = C.variant Variant.HD

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

      let mk_fault a ii =
        M.mk_singleton_es (Act.Fault (ii,A.Location_global a)) ii

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

      let delayed_check_tags ma ii m1 m2 =
        let (++) = M.bind_ctrl_avoid ma in
        M.check_tags
          ma (fun a -> read_tag_mem a ii)
          (fun a tag1 -> tag_extract a  >>= fun tag2 -> M.op Op.Eq tag1 tag2)
          (commit_pred ii)  ++ fun cond ->  M.choiceT cond m1 m2

(* PTW Basics *)

      let check_ptw dir a_virt ma an ii mdirect mok mfault =
(*
         let mvirt =
             (M.op1 Op.PTELoc a_virt >>= fun a_pte ->
                let ma =
                  ma >>=
                  fun _ -> (M.read_loc false
                    (fun loc v ->
                      Act.Access (Dir.R,loc,v,an,AArch64.NExp,quad,Act.A_PTE))
                    (A.Location_global a_pte) ii) in
                 (M.delay ma >>=
                     fun (a_phy,ma) -> is_zero a_phy >>= fun cond ->
                        M.choiceT cond (mfault ma a_virt) (mok ma a_phy))) in 
*)

           let do_m = (M.op1 Op.PTELoc a_virt >>= fun a_pte ->
                let ma =
                  ma >>=
                  fun _ -> (M.read_loc false
                    (fun loc v ->
                      Act.Access (Dir.R,loc,v,an,AArch64.NExp,quad,Act.A_PTE))
                    (A.Location_global a_pte) ii) in
                 (M.delay ma >>=
                     fun (a_phy,ma) -> is_zero a_phy >>= fun cond ->
                        M.choiceT cond (mfault ma a_virt) (mok ma a_phy))) in 

         let check_bit_clear bit act =
            (M.read_loc false
              (fun loc v ->
                Act.Access (Dir.R,loc,v,an,AArch64.NExp,quad,act))
              (A.Location_global bit) ii) >>= 
                 fun v -> is_zero v in

          let set_bit bit act =
            (M.read_loc false 
               (fun loc _ -> 
                  Act.Access (Dir.W,loc,V.one,an,AArch64.NExp,quad,act)) 
             (A.Location_global bit) ii) in

   (*
     Without HW-management (on old CPUs, or where TCR_ELx.{HA,HD} == {0,0}): 

     A load/store to x where pte_x has the access flag clear will raise a
     permission fault 

     A store to x where pte_x has the dirty bit clear will raise a permission
     fault 

     and SW is expected to deal with this by updating the translation tables with
     explicit stores or atomics
   *)
         let notTTHM a_virt ma a_af a_db = 
              (check_bit_clear a_af Act.A_AF >>= fun af_clear ->
              M.choiceT af_clear (mfault ma a_virt) 
              (begin match dir with 
                | Dir.R -> do_m 
                | Dir.W -> check_bit_clear a_db Act.A_DB >>= fun db_clear ->
                           M.choiceT db_clear (mfault ma a_virt) do_m
              end)) in

    (*
      With HW management (i.e. when ARMv8.1-TTHM is implemented) where TCR_ELx.HA = 1: 
      A load/store to x where pte_x has the access flag clear results in the MMU
      updating the translation table entry to set the access flag, and continuing
      without a fault.
     *)
         let isTTHM_and_HA a_af kont = 
              (check_bit_clear a_af Act.A_AF >>= fun af_clear ->
               M.choiceT af_clear 
                 (set_bit a_af Act.A_AF >>= fun _ -> kont)
                 kont) in

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

         let isTTHM_and_HA_and_HD a_af a_db a_dbm = 
           let kont = 
              (begin match dir with 
                | Dir.R -> do_m 
                | Dir.W -> check_bit_clear a_db Act.A_DB >>= fun db_clear ->
                           M.choiceT db_clear 
                             (check_bit_clear a_dbm Act.A_DBM >>= fun dbm_clear ->
                              M.choiceT dbm_clear 
                                (mfault ma a_virt) 
                                (set_bit a_db Act.A_DB >>= fun _ -> do_m)) 
                             do_m
              end) in
           (isTTHM_and_HA a_af kont)  
      
         in

         let mvirt = 
            (M.op1 Op.AF a_virt >>| M.op1 Op.DB a_virt >>| M.op1 Op.DBM a_virt)  >>= 
              fun ((a_af,a_db),a_dbm) ->
            if (not tthm || (tthm && (not ha && not hd))) then 
              (notTTHM a_virt ma a_af a_db) 
            else if (tthm && ha && not hd) then
              (isTTHM_and_HA a_af do_m) 
            else (*if (tthm && ha && hd)*) 
              (isTTHM_and_HA_and_HD a_af a_db a_dbm) in 
             
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
        | AArch64.GE -> is_lt
        | AArch64.GT -> is_le
        | AArch64.LE -> is_gt
        | AArch64.LT -> is_ge

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

      let lift_memop dir mop ma an ii =
        if memtag then
          M.delay ma >>= fun (_,ma) ->
          let mm = mop Act.A_VIR (ma >>= fun a -> loc_extract a) in
          delayed_check_tags ma ii
            (mm  >>! B.Next)
            (let mfault = ma >>= fun a -> mk_fault a ii in
            if C.precision then  mfault >>! B.Exit
           else (mfault >>| mm) >>! B.Next)
        else if kvm then
          M.delay ma >>= fun (a,ma) ->
            match Act.access_of_location_std (A.Location_global a) with
            | Act.A_VIR ->
                check_ptw dir a ma an ii
                  (mop Act.A_PTE ma >>! B.Next)
                  (fun ma _a -> mop Act.A_PHY ma >>! B.Next)
                  (fun ma a -> ma >>= fun _ -> mk_fault a ii
                      >>! if C.precision then B.Exit else B.ReExec)
            | ac -> mop ac ma >>! B.Next
        else
          mop Act.A_VIR ma >>! B.Next

      let do_str sz an anexp rs ma ii =
        lift_memop Dir.W
          (fun ac ma ->
            (ma >>| read_reg_data sz rs ii) >>= fun (a,v) ->
            do_write_mem sz an anexp ac a v ii)
          ma an ii

      let do_ldr sz an anexp rd ma ii =
          lift_memop Dir.R
            (fun ac ma -> ma >>= fun a -> do_read_mem sz an anexp ac rd a ii)
          ma an ii

      let ldr sz rd rs kr ii =
         do_ldr sz AArch64.N AArch64.Exp rd (get_ea rs kr ii) ii

      and str sz rs rd kr ii =
         do_str sz AArch64.N AArch64.Exp rs (get_ea rd kr ii) ii

      and stlr sz rs rd ii = do_str sz AArch64.L AArch64.Exp rs (read_reg_ord rd ii) ii

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
          (read_reg_ord rd ii)
          an ii

      let csel_op op v =
        let open AArch64Base in  match op with
        | Cpy -> M.unitT v
        | Inc -> M.op Op.Add v V.one
        | Neg -> M.op Op.Sub V.zero v
        | Inv -> Warn.fatal "size dependent inverse not implemented"

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
                (read_reg_data sz r1 ii >>| ma) >>= fun (v,a) ->
                 write_mem sz AArch64.Exp ac a v ii)
              (read_reg_ord r3 ii)
              an ii
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
              (read_reg_ord r3 ii)
              an ii

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
          (read_reg_ord rn ii)
          an ii

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
         (read_reg_ord rn ii)
          an ii

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
            begin
              read_reg_ord rt ii >>|
              (get_ea rn kr ii  >>= fun a ->
               M.op1 Op.TagLoc a  >>= fun a ->
               do_read_tag a ii)
            end >>= fun (old,tag) ->
            M.op Op.SetTag old tag >>= fun v ->
            write_reg rt v ii >>! B.Next

        | I_STXR(var,t,rr,rs,rd) ->
            stxr (tr_variant var) t rr rs rd ii
        | I_STXRBH(bh,t,rr,rs,rd) ->
            stxr (bh_to_sz bh) t rr rs rd ii

        (* Operations *)
        | I_MOV(_sz,r,K k) ->
            (* Masking assumed to be useless, given k size. Hence _sz ignored *)
            write_reg r (V.intToV k) ii >>! B.Next

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

        | I_OP3(ty,op,rd,rn,kr) ->
            let sz = tr_variant ty in
            begin match kr with
            | RV (_,r) when reg_compare r rn = 0 ->
                (* Keep sharing here, otherwise performance penalty on address
                   dependency by r^r in mixed size mode *)
                read_reg_ord_sz sz rn ii >>=
                fun v -> M.unitT (v,v)
            |_ ->
                read_reg_ord_sz sz rn ii >>|
                begin match kr with
                | K k -> M.unitT (V.intToV k)
                | RV(_,r) -> read_reg_ord_sz sz r ii
                end
            end
              >>=
            begin match op with
            | ADD|ADDS -> fun (v1,v2) -> M.add v1 v2
            | EOR -> fun (v1,v2) -> M.op Op.Xor v1 v2
            | ORR -> fun (v1,v2) -> M.op Op.Or v1 v2
            | SUB|SUBS -> fun (v1,v2) -> M.op Op.Sub v1 v2
            | AND|ANDS -> fun (v1,v2) -> M.op Op.And v1 v2
            | LSR -> fun (v1,v2) -> M.op Op.Lsr v1 v2
            end >>=
            (let m =  (fun v ->
              (write_reg rd v ii) >>|
              (match op with
              | ADDS|SUBS|ANDS -> is_zero v >>= fun v -> write_reg NZP v ii
              | ADD|EOR|ORR|AND|SUB|LSR -> M.unitT ())) in
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
            let if_deps_commit_bcc ii  =
              if is_deps then commit_bcc ii else M.unitT () in
            if C.variant Variant.WeakPredicated then
              read_reg_ord NZP ii >>= tr_cond c >>= fun v ->
                if_deps_commit_bcc ii >>= fun () ->
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
                  (if_deps_commit_bcc ii)
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

