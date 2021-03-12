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
      val dirty : DirtyBit.t
      val procs_user : Proc.t list
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
    let morello = C.variant Variant.Morello
    let is_deps = C.variant Variant.Deps
    let kvm = C.variant Variant.Kvm
    let is_branching = kvm && not (C.variant Variant.NoPteBranch)
    let pte2 = kvm && C.variant Variant.PTE2

    let check_memtag ins =
      if not memtag then
        Warn.user_error "%s without -variant memtag" ins

    let check_morello ii =
      if not morello then
        Warn.user_error
          "morello instruction %s require -variant morello"
          (AArch64.dump_instruction ii.A.inst)

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
    and aexp = AArch64.Exp    (* Explicit accesses *)

    (* Semantics proper *)
    module Mixed(SZ:ByteSize.S) = struct

      module Mixed = M.Mixed(SZ)

      let (>>=) = M.(>>=)
      let (>>==) = M.(>>==)
      let (>>*=) = M.(>>*=)
      let (>>*==) = M.(>>*==)
      let (>>|) = M.(>>|)
      let (>>!) = M.(>>!)
      let (>>::) = M.(>>::)

      let mask32 ty m =
        let open AArch64Base in
        match ty with
        | V32 -> fun v -> M.op1 (Op.Mask MachSize.Word) v >>= m
        | V64 when not morello -> m
        | V64 -> fun v -> M.op1 (Op.Mask MachSize.Quad) v >>= m
        | V128 -> m

      let is_zero v = M.op Op.Eq v V.zero
      and is_not_zero v = M.op Op.Ne v V.zero

(* Ordinary access action *)
      let access_anexp anexp d loc v ac =
        Act.Access (d,loc,v,AArch64.N,anexp,quad,ac)
      let access_ord d loc v ac = access_anexp aexp d loc v ac

(* Basic read, from register *)
      let mk_read sz an anexp loc v =
        let ac = Act.access_of_location_std loc in
        Act.Access (Dir.R, loc, v, an, anexp, sz, ac)

      let mk_read_std = mk_read quad AArch64.N

      let mk_fault a ii msg =
        M.mk_singleton_es (Act.Fault (ii,A.Location_global a,msg)) ii

      let mk_pte_fault ma ii =
        ma >>= fun a ->
        mk_fault a ii (Some "EL0") >>! B.Exit

      let read_loc v is_data = M.read_loc is_data (mk_read v AArch64.N aexp)

      let read_reg is_data r ii = match r with
      | AArch64.ZR -> M.unitT V.zero
      | _ ->
          M.read_loc is_data (mk_read quad AArch64.N aexp) (A.Location_reg (ii.A.proc,r)) ii

      let read_reg_morello is_data r ii =
        if not morello then Warn.user_error "capabilities require -variant morello" ;
        match r with
        | AArch64.ZR -> M.unitT V.zero
        | _ ->
            M.read_loc is_data
              (mk_read MachSize.S128 AArch64.N aexp)
              (A.Location_reg (ii.A.proc,r)) ii

      let read_reg_sz sz is_data r ii = match sz with
      | MachSize.S128 -> read_reg_morello is_data r ii
      | MachSize.Quad when not morello || not is_data -> read_reg is_data r ii
      | MachSize.Quad|MachSize.Word|MachSize.Short|MachSize.Byte ->
          read_reg is_data r ii >>= fun v -> M.op1 (Op.Mask sz) v

      let read_reg_ord = read_reg_sz quad false
      let read_reg_ord_sz sz = read_reg_sz sz false
      let read_reg_data sz = read_reg_sz sz true
      let read_reg_tag is_data =  read_reg is_data

(* Basic write, to register  *)
      let mk_write sz an anexp ac v loc =
        Act.Access (Dir.W, loc, v, an, anexp, sz, ac)

      let write_reg r v ii = match r with
      | AArch64.ZR -> M.unitT ()
      | _ ->
          M.write_loc
            (mk_write quad AArch64.N aexp Act.A_REG v)
            (A.Location_reg (ii.A.proc,r)) ii

      let write_reg_morello r v ii =
        if not morello then
          Warn.user_error "capabilities require -variant morello" ;
        M.write_loc
          (mk_write MachSize.S128  AArch64.N aexp Act.A_REG v)
          (A.Location_reg (ii.A.proc,r)) ii

      let write_reg_sz sz r v ii = match r with
      | AArch64.ZR -> M.unitT ()
      | _ -> match sz with
        | MachSize.S128 -> write_reg_morello r v ii
        | MachSize.Quad when not morello -> write_reg r v ii
        | MachSize.Quad|MachSize.Word|MachSize.Short|MachSize.Byte ->
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
          (fun loc v -> access_anexp AArch64.nexp_annot Dir.R loc v Act.A_TAG)
          (A.Location_global a) ii
      and do_write_tag a v ii =
        let loc = A.Location_global a in
        M.mk_singleton_es
          (access_ord Dir.W loc v Act.A_TAG)
          ii

      let do_read_morello_tag a ii =
        M.add_atomic_tag_read (M.unitT M.A.V.one) a
          (fun loc v -> Act.tag_access quad Dir.R loc v) ii >>= fun tagged ->
          M.op1 Op.CapaGetTag tagged
      and do_write_morello_tag a v ii =
        M.add_atomic_tag_write (M.unitT ()) a v
          (fun loc v -> Act.tag_access quad Dir.W loc v) ii

(* Read tag from memory *)
      let read_tag_mem a ii =
        M.op1 Op.TagLoc a >>= fun atag -> do_read_tag_nexp atag ii

(*******************)
(* Memory accesses *)
(*******************)

(* Tag checking, MTE *)

      let delayed_check_tags a_virt ma ii m1 m2 =
        let (++) = M.bind_ctrl_avoid ma in
        M.check_tags
          ma (fun a -> read_tag_mem a ii)
          (fun tag1 -> tag_extract a_virt  >>= fun tag2 -> M.op Op.Eq tag1 tag2)
          (commit_pred ii)  ++ fun cond ->  M.choiceT cond m1 m2

(* Tag checking Morello *)
      let do_append_commit ma ii =
        ma >>== fun a -> commit_pred ii >>= fun () -> M.unitT a

      let mzero = M.unitT M.A.V.zero

      let check_morello_tag a ma mv mok mfault =
        M.op1 Op.CapaGetTag a >>= fun x ->
        M.op Op.Ne x V.zero >>= fun cond ->
        M.choiceT cond (mok ma mv) (mfault ma mzero)

      let check_morello_sealed a ma mv  mok mfault =
        M.op1 Op.CheckSealed a >>= fun x ->
        M.op Op.Ne x V.zero >>= fun cond ->
        M.choiceT cond (mfault ma mzero) (mok ma mv)

 (* Semantics has changed, no ctrl-dep on mv *)
      let check_morello_perms a ma mv perms mok mfault =
        M.delay_kont "morello_perms"
          mv
          (fun v mv ->
            let v =
              if String.contains perms 'w' && String.contains perms 'c'
              then v else M.A.V.zero in
            M.op (Op.CheckPerms perms) a v >>= fun cond ->
            M.choiceT cond (mok ma mv) (mfault ma mv))

      let process_read_capability sz a m ii =
        match sz with
        | MachSize.S128 ->
            (M.op1 Op.CapaStrip a >>= fun a ->
             M.add_atomic_tag_read (m a) a
               (fun loc v -> Act.tag_access quad Dir.R loc v) ii)
            >>= fun v -> M.op Op.SquashMutable a v
        | _ -> M.op1 Op.CapaStrip a >>= fun a -> m a >>= fun v -> M.op Op.CapaSetTag v V.zero

(****************)
(* PTW checking *)
(****************)

(* Group pteval components together *)

      type ipte =
          { pte_v:V.v; oa_v:V.v; af_v:V.v;
            db_v:V.v; dbm_v:V.v; valid_v:V.v;
            el0_v:V.v; }

      let extract_af v = M.op1 Op.AF v
      let extract_db v = M.op1 Op.DB v
      let extract_dbm v = M.op1 Op.DBM v
      let extract_valid v = M.op1 Op.Valid v
      let extract_el0 v = M.op1 Op.EL0 v
      let extract_oa v = M.op1 Op.OA v

      let mextract_whole_pte_val an nexp a_pte ii =
        (M.read_loc false
           (fun loc v ->
             Act.Access (Dir.R,loc,v,an,nexp,quad,Act.A_PTE))
           (A.Location_global a_pte) ii)

      and write_whole_pte_val an explicit a_pte v ii =
        M.write_loc
          (mk_write quad an explicit Act.A_PTE v)
          (A.Location_global a_pte) ii


      let op_of_set = function
        | AArch64.AF -> Op.SetAF
        | AArch64.DB -> Op.SetDB
        | AArch64.Other -> assert false

      let test_and_set_bit cond set a_pte ii =
        let nexp = AArch64.NExp set in
        mextract_whole_pte_val AArch64.X nexp a_pte ii >>= fun pte_v ->
        cond pte_v >>*= fun c ->
        M.choiceT c
            (M.op1 (op_of_set set) pte_v >>= fun v ->
             write_whole_pte_val AArch64.X nexp a_pte v ii)
            (M.unitT ())

      let bit_is_zero op v = M.op1 op v >>= is_zero
      let bit_is_not_zero op v = M.op1 op v >>= is_not_zero
      let m_op op m1 m2 = (m1 >>| m2) >>= fun (v1,v2) -> M.op op v1 v2

      let test_and_set_af =
        test_and_set_bit
          (fun v ->
            m_op Op.And
              (bit_is_zero Op.AF v) (bit_is_not_zero Op.Valid v))
          AArch64.AF

      and test_and_set_db =
        test_and_set_bit
          (fun v ->
            m_op Op.And
              (bit_is_zero Op.DB v) (bit_is_not_zero Op.Valid v))
          AArch64.DB

      let mextract_pte_vals pte_v =
        (extract_oa pte_v >>|
        extract_el0 pte_v >>|
        extract_valid pte_v >>|
        extract_af pte_v >>|
        extract_db pte_v >>|
        extract_dbm pte_v) >>=
        (fun (((((oa_v,el0_v),valid_v),af_v),db_v),dbm_v) ->
          M.unitT {pte_v; oa_v; af_v; db_v; dbm_v; valid_v; el0_v;})

      let get_oa a_virt mpte =
        (M.op1 Op.Offset a_virt >>| mpte)
        >>= fun (o,p) -> M.add p.oa_v o

(******************)
(* Checking flags *)
(******************)

(* With choice operator *)
      let do_check_cond m m_cond k1 k2 =
        M.delay_kont "1"
          (m >>= fun pte_v -> m_cond pte_v >>= fun c -> M.unitT (c,pte_v))
          (fun (c,pte_v) m ->
            let m = m >>= fun _ -> M.unitT pte_v in
            M.choiceT c (k1 m) (k2 m))

        (* Summary of access flag and dirty bit management.

          * Without HW-management (on old CPUs, or where TCR_ELx.{HA,HD} == {0,0}):

          A load/store to x where pte_x has the access flag clear will raise a
          permission fault

          A store to x where pte_x has the dirty bit clear will raise
          a permission fault

          and SW is expected to deal with this by updating the translation tables with
          explicit stores or atomics

          * With HW management (i.e. when ARMv8.1-TTHM is implemented) where TCR_ELx.HA = 1:
          A load/store to x where pte_x has the access flag clear results in the MMU
          updating the translation table entry to set the access flag, and continuing
          without a fault.

          A store where pte_x has the dirty bit clear will raise a permission fault.

          * With HW management (i.e. when ARMv8.1-TTHM is implemented) where TCR_ELx.{HA,HD} == {1,1}:
          A load/store to x where pte_x has the access flag clear results in the
          MMU updating the translation table entry to set the access flag, and continuing
          without a fault.

          A store to x where pte_x has the dirty bit clear and also has DBM clear
          will raise a permission fault

          A store to x where pte_x has the dirty bit clear and has DBM set results in the
          MMU updating the translation table entry to set the dirty bit, and continuing
          without a fault.

          Notice: The dirty bit correspond to HW level write permission in PTE's.
          Hence, in simple (stage 1) case, we have AP[2] == 0b1 for clean,
          and AP[2] == 0b0 for dirty, with AP[2] == 0b0 being more directly "writable".

         *)

      let check_ptw proc dir a_virt ma an ii mdirect mok mfault =

        let is_el0  = List.exists (Proc.equal proc) TopConf.procs_user in
        let check_el0 m =
          if is_el0 then
               fun pte_v -> m_op Op.Or (is_zero pte_v.el0_v) (m pte_v)
             else m in

        let open DirtyBit in
        let tthm = TopConf.dirty.tthm proc
        and ha = TopConf.dirty.ha proc
        and hd = TopConf.dirty.hd proc in
        let ha = ha || hd in (* As far as we know hd => ha *)
(* Perform PTE update, when told to do so *)
        let setbits_get_oa a_pte m =
          m >>*== fun pte_v ->
           (* >>== is important, as the test and set below
              is performed 'on the side ' *)
            begin if hd && dir = Dir.W then
              is_zero pte_v.db_v >>= fun c ->
              M.choiceT c
                  (test_and_set_db a_pte ii)
                  (M.unitT ())
            else M.unitT ()
            end
            >>| (M.op1 Op.Offset a_virt >>= M.add pte_v.oa_v)
            >>= fun (_,oa) -> M.unitT oa in
        let mfault m _a = mfault (get_oa a_virt m) a_virt
        and mok a_pte m a = mok (setbits_get_oa a_pte m) a in


(* Action on case of page table access.
   Delay is used so as to have correct dependencies,
   getting content of PTE by anticipation. *)
        let mvirt = begin
          M.delay_kont "3"
            begin
              let get_a_pte = ma >>= fun _ -> M.op1 Op.PTELoc a_virt
              and test_and_set_af a_pte =
                if tthm && ha then
                  test_and_set_af a_pte ii >>! a_pte
                else M.unitT a_pte in
              (get_a_pte >>== test_and_set_af) >>= fun a_pte ->
              mextract_whole_pte_val
                an AArch64.nexp_annot a_pte ii >>= fun pte_v ->
              (mextract_pte_vals pte_v) >>= fun pte_v -> M.unitT (pte_v,a_pte)
            end
          (fun (_,a_pte) ma -> (* now we have PTE content *)
            (* Monad will carry changing internal pte value *)
            let ma = ma >>= fun (pte_v,_) -> M.unitT pte_v in
            (* wrapping of success/failure continuations,
               only pte value may have changed *)
            let mok ma = mok a_pte ma a_virt
(* a_virt was (if pte2 then a_virt else pte_v.oa_v), why? *)
            and mno ma =  mfault ma a_virt in
            let check_cond cond =
              do_check_cond ma (check_el0 cond) mno mok in

            if (not tthm || (tthm && (not ha && not hd))) then
            (* No HW management *)
              let cond_R pte_v =
                m_op Op.Or (is_zero pte_v.valid_v) (is_zero pte_v.af_v) in
              let cond = match dir with (* No mercy, check all flags *)
              | Dir.R -> cond_R
              | Dir.W ->
                  fun pte_v ->
                    m_op Op.Or (cond_R pte_v) (is_zero pte_v.db_v) in
              check_cond cond
            else if (tthm && ha && not hd) then (* HW managment of AF *)
              let cond = match dir with (* Do not check AF *)
              | Dir.R -> fun pte_v -> is_zero pte_v.valid_v
              | Dir.W ->
                  fun pte_v ->
                    m_op Op.Or (is_zero pte_v.valid_v) (is_zero pte_v.db_v) in
              check_cond cond
            else (* HW management of AF and DB *)
              let cond = match dir with (* Do not check AF *)
              | Dir.R -> fun pte_v -> is_zero pte_v.valid_v
              | Dir.W ->
(* Check DB when dirty bit management disabled for this page *)
                  fun pte_v ->
                    m_op Op.Or
                      (is_zero pte_v.valid_v)
                      (m_op Op.And
                         (is_zero pte_v.db_v) (is_zero pte_v.dbm_v)) in
              check_cond cond)
          end in
        if pte2 then  mvirt
        else
          M.op1 Op.IsVirtual a_virt >>= fun cond ->
          M.choiceT cond mvirt
            (* Non-virtual accesses are disallowed from EL0.
               For instance, user code cannot access the page table. *)
            (if is_el0 then mk_pte_fault ma ii
             else mdirect)

(* Read memory, return value read *)
      let do_read_mem_ret sz an anexp ac a ii =
        let m a =
          if mixed then begin
              Mixed.read_mixed false sz (fun sz -> mk_read sz an anexp) a ii
            end else begin
              let mk_act loc v =  Act.Access (Dir.R,loc,v,an,anexp,sz,ac) in
              let loc = A.Location_global a in
              M.read_loc false mk_act loc ii
            end in
        if morello then process_read_capability sz a m ii
        else m a

(* Save value read in register rd *)
      let do_read_mem sz an anexp ac rd a ii =
        do_read_mem_ret sz an anexp ac a ii >>=
        fun v -> write_reg_sz_non_mixed sz rd v ii >>! B.Next

      let read_mem sz = do_read_mem sz AArch64.N
      let read_mem_acquire sz = do_read_mem sz AArch64.A
      let read_mem_acquire_pc sz = do_read_mem sz AArch64.Q
      let read_mem_noreturn sz = do_read_mem sz AArch64.NoRet

      let read_mem_reserve sz an anexp ac rd a ii =
        let m a = 
          (write_reg AArch64.ResAddr a ii
           >>| do_read_mem sz an anexp ac rd a ii)
          >>= fun ((),b) -> M.unitT b in
        if morello then
          M.op1 Op.CapaStrip a >>= m
        else
          m a

      (* Post-Indexed load immediate.
         Note: a (effective address) can be physical address,
         while postindex must apply to virtual address. *)
      let read_mem_postindexed a_virt sz an anexp ac rd rs k a ii =
        let m a =
          begin
            (M.add a_virt (V.intToV k) >>= fun b -> write_reg rs b ii)
            >>| do_read_mem sz an anexp ac rd a ii
          end >>= fun ((),r) -> M.unitT r in
        if morello then
          M.op1 Op.CapaStrip a >>= m
        else
          m a


(* Write *)
      let check_mixed_write_mem sz an anexp ac a v ii =
        if mixed then begin
            Mixed.write_mixed sz
              (fun sz loc v -> mk_write sz an anexp ac v loc)
              a v ii
          end else
          M.write_loc
            (mk_write sz an anexp ac v) (A.Location_global a) ii

      let check_morello_for_write m a v ii =
        if morello then
          M.op1 Op.CapaStrip a >>| M.op1 Op.CapaGetTag v >>= fun (a,tag) ->
          M.add_atomic_tag_write (m a) a tag
            (fun loc v -> Act.tag_access quad Dir.W loc v) ii
        else m a

      let do_write_mem sz an anexp ac a v ii =
        check_morello_for_write
          (fun a -> check_mixed_write_mem sz an anexp ac a v ii)
          a v ii


      let write_mem sz = do_write_mem sz AArch64.N
      let write_mem_release sz = do_write_mem sz AArch64.L
      let write_mem_amo sz = do_write_mem sz AArch64.X
      let write_mem_amo_release sz = do_write_mem sz AArch64.XL

(* Write atomic *)
      let write_mem_atomic sz an anexp ac a v resa ii =
        check_morello_for_write
          (fun a ->
            (M.assign a resa
             >>| check_mixed_write_mem sz an anexp ac a v ii)
            >>! ())
        a v ii

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

(***************************)
(* Various lift functions. *)
(***************************)

(*
  Those fonction take genric 'mop' memory operations,
  and change their behaviour according to variants.
  Most lift function introduce validity checks on
  addresses. Thus the resulting monads will possess
  extra dependencies w.r.t the simple case.
 *)
      let lift_memtag_phy mop a_virt ma ii =
        M.delay_kont "4" ma
          (fun _ ma ->
            let mm = mop Act.A_PHY ma in
            delayed_check_tags a_virt ma ii
              (mm  >>! B.Next)
              (let mfault = mk_fault a_virt ii None in
              if C.precision then  mfault >>! B.Exit
              else (mfault >>| mm) >>! B.Next))

      let lift_memtag_virt mop ma ii =
        M.delay_kont "5" ma
          (fun a_virt ma  ->
            let mm = mop Act.A_VIR (ma >>= fun a -> loc_extract a) in
            delayed_check_tags a_virt ma ii
              (mm  >>! B.Next)
              (let mfault = ma >>= fun a -> mk_fault a ii None in
              if C.precision then  mfault >>! B.Exit
              else (mfault >>| mm) >>! B.Next))

      let append_commit ma ii =
        if is_branching then do_append_commit ma ii else ma

      let do_insert_commit m1 m2 ii =
        m1 >>= fun a -> commit_pred ii >>*== fun _ -> m2 a

      let insert_commit m1 m2 ii =
        if is_branching || morello then do_insert_commit m1 m2 ii
        else m1 >>= m2

      let lift_kvm dir mop ma an ii mphy =
        let mfault ma a =
          insert_commit ma (fun _ -> mk_fault a ii None) ii
          >>! if C.precision then B.Exit else B.ReExec in
        let maccess a ma =
          check_ptw ii.AArch64.proc dir a ma an ii
            (mop Act.A_PTE ma >>! B.Next)
            mphy
            mfault in
        M.delay_kont "6" ma
          (if pte2 then maccess
           else
             fun a ma ->
             match Act.access_of_location_std (A.Location_global a) with
             | Act.A_VIR|Act.A_PTE -> maccess a ma
             | ac -> mop ac ma >>! B.Next)

      let lift_morello mop perms ma mv ii =
        let mfault msg ma mv =
          do_insert_commit
            (ma >>| mv)
            (fun (a,_v) -> mk_fault a ii (Some msg)) ii  >>! B.Exit in
        M.delay_kont "morello" ma
          (fun a ma ->
            (* Notice: virtual access only, beaause morello # kvm *)
            let mok ma mv = mop Act.A_VIR ma mv in
            check_morello_tag a ma mv
              (fun ma mv ->
                check_morello_sealed a ma mv
                  (fun ma mv ->
                    check_morello_perms a ma mv perms
                      (fun ma mv -> mok ma mv >>! B.Next)
                      (mfault "CapPerms"))
                  (mfault "CapSeal"))
              (mfault "CapTag"))

(* Main choice between supported variants, notice:
   + memtag and kvm can be combined, other variants cannot.
   + mv abstracted for morello sake only
   + ma abstracted for all variants
 *)

      let to_perms str sz = str ^ if sz = MachSize.S128 then "_c" else ""

      let apply_mv mop mv = fun ac ma -> mop ac ma mv

      let lift_memop dir mop perms ma mv an ii =
        if morello then
          lift_morello mop perms ma mv ii
        else
          let mop = apply_mv mop mv in
          if memtag then
            begin
              if kvm then
                let mphy = (fun ma a -> lift_memtag_phy mop a ma ii) in
                lift_kvm dir mop ma an ii mphy
              else lift_memtag_virt mop ma ii
            end
          else if kvm then
            let mphy =
              if pte2 then
                fun ma a_virt ->
                M.op1 Op.IsVirtual a_virt >>= fun c ->
                M.choiceT c
                  (mop Act.A_PHY ma) (mop Act.A_PHY_PTE ma) >>! B.Next
              else
                fun ma _a -> mop Act.A_PHY ma >>! B.Next in
            lift_kvm dir mop ma an ii mphy
        else
          mop Act.A_VIR ma >>! B.Next

(* Generic load *)
      let do_ldr sz an mop ma ii =
        lift_memop Dir.R
          (fun ac ma _mv -> (* value fake here *)
            if Act.is_physical ac then
              insert_commit ma (mop ac) ii
            else
              ma >>= mop ac)
          (to_perms "r" sz)
          ma mzero an ii

(* Generic store *)
      let do_str sz an ma mv ii =
        lift_memop Dir.W
          (fun ac ma mv ->
            if is_branching && Act.is_physical ac then
              (* additional ctrl dep on address *)
              M.bind_ctrl_data (append_commit ma ii) mv
                (fun a v ->
                  do_write_mem sz an aexp ac a v ii)
            else if morello then
              (* additional ctrl dep on address and data *)
              do_insert_commit (ma >>| mv)
                (fun (a,v) -> do_write_mem sz an aexp ac a v ii)
                ii
            else
              (ma >>| mv) >>= fun (a,v) ->
              do_write_mem sz an aexp ac a v ii)
          (to_perms "w" sz) ma mv an ii

(***********************)
(* Memory instructions *)
(***********************)

(* compute signed and unsized extension (32 -> 64 bits) *)
      let sxtw_op = M.op1 (Op.Sxt MachSize.Word)

      and uxtw_op = M.op1 (Op.Mask MachSize.Word)

(* Apply a shift as monadic op *)
      let shift s =
        let open AArch64Base in
        match s with
          | S_NOEXT   -> M.unitT
          | S_LSL(n)  -> fun x -> M.op (Op.ShiftLeft) x (V.intToV n)
          | S_LSR(n)  -> fun x -> M.op (Op.ShiftRight) x (V.intToV n)
          | S_ASR(n)  -> fun x -> M.op (Op.ASR) x (V.intToV n)
          | S_SXTW -> sxtw_op
          | S_UXTW -> uxtw_op
          | S_MSL(_)  ->
              Warn.fatal "Neon instructions are not currently supported"

(* Complete effective adress computation *)
      let get_ea rs kr s ii =
        let open AArch64Base in
        match kr, s with
        | K 0, S_NOEXT -> (* Immediate with no shift*)
            read_reg_ord rs ii
        | K k, s -> (* Immediate with offset, with shift *)
            read_reg_ord rs ii
            >>= fun v -> shift s (V.intToV k)
            >>= M.add v
        | RV(_,r), S_NOEXT -> (* register, no shift *)
            (read_reg_ord rs ii >>| read_reg_ord r ii)
            >>= fun (v1,v2) -> M.add v2 v1
        | RV(_,r), s -> (* register, with shift *)
            (read_reg_ord rs ii >>| read_reg_ord r ii)
            >>= fun (v1,v2) -> shift s v2
            >>= fun v2 -> M.add v1 v2

      let get_ea_noext rs kr ii = get_ea rs kr AArch64.S_NOEXT ii

      let ldr sz rd rs kr s ii = (* load *)
        do_ldr sz AArch64.N
          (fun ac a -> do_read_mem sz AArch64.N aexp ac rd a ii)
          (get_ea rs kr s ii) ii

      and ldar sz t rd rs ii =
        let open AArch64 in
        let an = match t with
        | XX -> AArch64.X
        | AA -> AArch64.A
        | AX -> AArch64.XA
        | AQ -> AArch64.Q in
        do_ldr sz an
          (fun ac a ->
            let read =
              match t with
              | XX -> read_mem_reserve sz AArch64.X
              | AA -> read_mem_acquire sz
              | AX -> read_mem_reserve sz AArch64.XA
              | AQ -> read_mem_acquire_pc sz in
            read aexp ac rd a ii)
          (read_reg_ord rs ii)  ii

      and ldr_p sz rd rs k ii = (* load post-index *)
        M.delay_kont "ldr_p"
          (read_reg_ord rs ii)
          (fun a_virt ma ->
            do_ldr sz AArch64.N
              (fun ac a ->
                read_mem_postindexed a_virt sz AArch64.N aexp ac rd rs k a ii)
              ma ii)

      and str sz rs rd kr ii =
        do_str sz AArch64.N
          (get_ea_noext rd kr ii) (read_reg_data sz rs ii) ii

      and stlr sz rs rd ii =
        do_str sz AArch64.L
          (read_reg_ord rd ii) (read_reg_data sz rs ii) ii

      and stxr sz t rr rs rd ii =
        let open AArch64Base in
        let an = match t with
        | YY -> AArch64.X
        | LY -> AArch64.XL in
        lift_memop Dir.W
          (fun ac ma mv ->
            M.riscv_store_conditional
              (read_reg_ord ResAddr ii)
              mv
              (append_commit ma ii)
              (write_reg ResAddr V.zero ii)
              (fun v -> write_reg rr v ii)
              (fun ea resa v ->
                write_mem_atomic sz an aexp ac ea v resa ii))
          (to_perms "w" sz)
          (read_reg_ord rd ii)
          (read_reg_data sz rs ii) an ii

(* AMO instructions *)          
      let rmw_amo_read sz rmw =
        let open AArch64 in
        match rmw with
        | RMW_A|RMW_AL -> do_read_mem_ret sz XA Exp
        | RMW_L|RMW_P  -> do_read_mem_ret sz X Exp

      and rmw_amo_write sz rmw =
        let open AArch64 in
        match rmw with
        | RMW_L|RMW_AL -> do_write_mem sz XL Exp
        | RMW_P|RMW_A  -> do_write_mem sz X Exp

      let rmw_to_read rmw =
        let open AArch64 in
        match rmw with
        | RMW_P | RMW_L -> N
        | RMW_A | RMW_AL -> A

      let swp sz rmw r1 r2 r3 ii =
        lift_memop Dir.W (* swp is a write for the purpose of DB *)
          (fun ac ma mv ->
            let r2 = mv
            and w2 v = write_reg_sz_non_mixed sz r2 v ii
            and r1 a = rmw_amo_read sz rmw ac a ii
            and w1 a v = rmw_amo_write sz rmw ac a v ii in
            M.swp (Act.is_physical ac) (append_commit ma ii) r1 r2 w1 w2)
          (to_perms "rw" sz)
          (read_reg_ord r3 ii)
          (read_reg_data sz r1 ii)
          (rmw_to_read rmw)
          ii

      let cas sz rmw rs rt rn ii =
        let an = rmw_to_read rmw in
        let read_rs = read_reg_data sz rs ii
        and write_rs v = write_reg_sz_non_mixed sz rs v ii in
        lift_memop Dir.W
           (* mv is read new value from reg, not important
              as this code is not executed in morello mode *)
          (fun ac ma mv ->
            let is_phy = Act.is_physical ac in
            let ma = if is_phy then append_commit ma ii else ma in
             M.altT
              (let read_mem a = do_read_mem_ret sz an aexp ac a ii in
               M.aarch64_cas_no is_phy ma read_rs write_rs read_mem M.neqT)
              (let read_rt = mv
               and read_mem a = rmw_amo_read sz rmw ac a ii
               and write_mem a v = rmw_amo_write sz rmw ac a v ii in
               M.aarch64_cas_ok is_phy ma read_rs read_rt write_rs
                 read_mem write_mem M.eqT))
          (to_perms "rw" sz) (read_reg_ord rn ii) (read_reg_data sz rt ii)
        an ii

      (* Temporary morello variation of CAS *)
      let cas_morello sz rmw rs rt rn ii =
        (* As morello and kvm are incompatible, all accesses are virtual *)
        lift_morello
          (fun ac ma mv ->
            let read_mem sz = rmw_amo_read sz rmw in
            let mrs = read_reg_data sz rs ii in
            let mrt = mv in
            M.delay ma >>| M.delay mrs >>| M.delay mrt
            >>= fun (((_,ma),(_,mrs)),(_,mrt)) ->
            let muncond = ma >>| mrs >>| mrt in
            let mmem = ma >>= fun a -> read_mem sz ac a ii in
            let write_rs mv =
              mv >>= fun v -> write_reg_sz_non_mixed sz rs v ii in
            let branch = fun mrs mmem mavoid m1 m2 ->
              let (++) = M.bind_ctrl_avoid mavoid in
              (mrs >>| mmem >>= fun (rs,mem) -> (M.op Op.Eq rs mem) >>= fun cond ->
                commit_pred ii >>! cond) ++ fun cond ->
                  M.choiceT cond m1 m2 in
            let mop = fun ma mv mmem ->
              let write_mem a v = rmw_amo_write sz rmw ac a v ii in
              M.aarch64_cas_ok_morello ma mv mmem write_mem in
            M.delay mmem >>= fun (_,mmem) ->
            branch mrs mmem (muncond >>| mmem)
              (mop ma mrt mmem)
              (mrt >>! ())
            >>| write_rs mmem)
          (to_perms "rw" sz)
          (read_reg_ord rn ii)
          (read_reg_data sz rt ii)
          ii

      let ldop op sz rmw rs rt rn ii =
        let open AArch64 in
        let an = rmw_to_read rmw in
        lift_memop Dir.W
          (fun ac ma mv ->
            let noret = match rt with | ZR -> true | _ -> false in
            let op = match op with
            | A_ADD -> Op.Add
            | A_EOR -> Op.Xor
            | A_SET -> Op.Or
            | A_CLR -> Op.AndNot2
            | A_SMAX -> Op.Max
            | A_SMIN -> Op.Min in
            let read_mem =
              if noret then fun sz -> do_read_mem_ret sz NoRet Exp ac
              else fun sz -> rmw_amo_read sz rmw ac
            and write_mem = fun sz -> rmw_amo_write sz rmw ac in
            M.amo_strict (Act.is_physical ac) op
              (append_commit ma ii)
              (fun a -> read_mem sz a ii) mv
              (fun a v -> write_mem sz a v ii)
              (fun w ->
                if noret then M.unitT ()
                else write_reg_sz_non_mixed sz rt w ii))
          (to_perms "rw" sz)
          (read_reg_ord rn ii)
          (read_reg_data sz rs ii)
          an ii

(******************************)
(* Move constant instructions *)
(******************************)

      let movz sz rd k os ii =
        let open AArch64Base in
        assert (MachSize.is_imm16 k);
        begin match sz, os with
        | V32, S_NOEXT | V64, S_NOEXT ->
          (* Or'ing zero with value should zero out what's left *)
          M.unitT (V.intToV k)
        | V32, S_LSL(0|16 as s)
        | V64, S_LSL((0|16|32|48 as s)) ->
          M.op1 (Op.LeftShift s) (V.intToV k)
        | _, S_LSL(_) | _, _ ->
            Warn.fatal
              "illegal instruction %s"
              (dump_instruction (I_MOVZ (sz, rd, k, os)))
        end
          >>= (fun v -> write_reg rd v ii)
          >>! B.Next
      let m_movk msk v1 v2 =
        M.op Op.AndNot2 v2 msk >>= M.op Op.Or v1
        
      let movk var rd k os ii =
        let open AArch64Base in
        let msk =
          let v = V.op1 (Op.LeftShift 16) V.one in
          V.op Op.Sub v V.one in
        assert (MachSize.is_imm16 k);
        let sz = tr_variant var in
        begin match var, os with
        | V32, S_NOEXT | V64, S_NOEXT ->
            read_reg_data sz rd ii >>= m_movk msk (V.intToV k)
        | V32, S_LSL(0|16 as s)
        | V64, S_LSL((0|16|32|48 as s)) ->
            let msk = V.op1 (Op.LeftShift s) msk in
            let v1 = V.op1 (Op.LeftShift s) (V.intToV k) in
            read_reg_data sz rd ii >>= m_movk msk v1
        | _, S_LSL(n) ->
          Warn.fatal
            "illegal shift immediate %d in %s instruction movk"
            n
            (pp_variant var)
        | _, s ->
          Warn.fatal
            "illegal shift operand %s in %s instruction movk"
            (pp_barrel_shift "," s pp_imm)
            (pp_variant var)
        end
          >>= (fun v -> write_reg rd v ii)
          >>! B.Next

      let csel_op op v =
        let open AArch64Base in
        match op with
        | Cpy -> M.unitT v
        | Inc -> M.op Op.Add v V.one
        | Neg -> M.op Op.Sub V.zero v
        | Inv -> M.op1 Op.Inv v

(********************)
(* Main entry point *)
(********************)
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
        | I_LDR(var,rd,rs,kr,s) ->
            let sz = tr_variant var in
            ldr sz rd rs kr s ii
        | I_LDRBH (bh, rd, rs, kr) ->
            let sz = bh_to_sz bh in
            ldr sz rd rs kr S_NOEXT ii
        | I_LDR_P(var,rd,rs,k) ->
            assert (k >= -256 && k <= 255);
            let sz = tr_variant var in
            ldr_p sz rd rs k ii
        | I_LDUR(var,rd,rs,k) ->
            let sz = tr_variant var in
            let k = AArch64.K (match k with Some k -> k | None -> 0) in
            ldr sz rd rs k AArch64.S_NOEXT ii
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
            check_memtag "STZG" ;
            begin
              (read_reg_data MachSize.Quad rt ii >>= tag_extract) >>|
              get_ea rn kr AArch64.S_NOEXT ii
            end >>= fun (v,a) ->
              (M.op1 Op.TagLoc a >>| loc_extract a) >>= fun (atag,loc) ->
                (do_write_tag atag v ii >>| do_write_mem quad AArch64.N aexp Act.A_VIR loc V.zero ii) >>! B.Next

        | I_STG(rt,rn,kr) ->
            check_memtag "STG" ;
            begin
              (read_reg_data quad rt ii >>= tag_extract) >>|
              get_ea rn kr S_NOEXT ii
            end >>= fun (v,a) ->
              M.op1 Op.TagLoc a  >>= fun a ->
                do_write_tag a v ii >>! B.Next

        | I_LDG (rt,rn,kr) ->
            check_memtag "LDG" ;
            get_ea rn kr S_NOEXT ii  >>=
            fun a -> M.op1 Op.TagLoc a >>=
              fun atag -> do_read_tag atag ii
                  >>= fun tag ->
                    M.op Op.SetTag a tag >>= fun v ->
                      write_reg rt v ii >>! B.Next

        | I_STXR(var,t,rr,rs,rd) ->
            stxr (tr_variant var) t rr rs rd ii
        | I_STXRBH(bh,t,rr,rs,rd) ->
            stxr (bh_to_sz bh) t rr rs rd ii

        (* Morello instructions *)
        | I_ALIGND(rd,rn,kr) ->
            check_morello ii ;
            (read_reg_ord_sz MachSize.S128 rn ii >>= match kr with
            | K k -> fun v -> M.op Op.Alignd v (V.intToV k)
            | _ -> assert false
            ) >>= fun v -> write_reg_sz MachSize.S128 rd v ii >>! B.Next
        | I_ALIGNU(rd,rn,kr) ->
            check_morello ii ;
            (read_reg_ord_sz MachSize.S128 rn ii >>= match kr with
            | K k -> fun v -> M.op Op.Alignu v (V.intToV k)
            | _ -> assert false
            ) >>= fun v -> write_reg_sz MachSize.S128 rd v ii >>! B.Next
        | I_BUILD(rd,rn,rm) ->
            check_morello ii ;
            begin
              read_reg_ord_sz MachSize.S128 rn ii >>|
              read_reg_ord_sz MachSize.S128 rm ii
            end >>= fun (a,b) ->
            M.op Op.Build a b >>= fun v ->
            write_reg_sz MachSize.S128 rd v ii >>! B.Next
        | I_CHKEQ(rn,rm) ->
            check_morello ii ;
            begin
              read_reg_ord_sz MachSize.S128 rn ii >>|
              read_reg_ord_sz MachSize.S128 rm ii
            end >>= fun (v1,v2) ->
            M.op Op.Eq v1 v2 >>= fun v -> M.op1 (Op.LeftShift 2) v >>= fun v ->
            write_reg NZP v ii >>! B.Next
        | I_CHKSLD(rn) ->
            check_morello ii ;
            read_reg_ord_sz MachSize.S128 rn ii >>= fun v ->
            M.op1 Op.CheckSealed v >>= fun v -> write_reg NZP v ii >>! B.Next
        | I_CHKTGD(rn) ->
            check_morello ii ;
            read_reg_ord_sz MachSize.S128 rn ii >>= fun v ->
            M.op1 Op.CapaGetTag v >>= fun v -> M.op1 (Op.LeftShift 1) v >>= fun v ->
            write_reg NZP v ii >>! B.Next
        | I_CLRTAG(rd,rn) ->
            check_morello ii ;
            read_reg_ord_sz MachSize.S128 rn ii >>= fun (v) ->
            M.op Op.CapaSetTag v V.zero >>= fun v ->
            write_reg_sz MachSize.S128 rd v ii >>! B.Next
        | I_CPYTYPE(rd,rn,rm) ->
            check_morello ii ;
            begin
              read_reg_ord_sz MachSize.S128 rn ii >>|
              read_reg_ord_sz MachSize.S128 rm ii
            end >>= fun (v1,v2) -> M.op Op.CpyType v1 v2 >>= fun v ->
            write_reg_sz MachSize.S128 rd v ii >>! B.Next
        | I_CPYVALUE(rd,rn,rm) ->
            check_morello ii ;
            begin
              read_reg_ord_sz MachSize.S128 rn ii >>|
              read_reg_ord_sz MachSize.S128 rm ii
            end >>= fun (v1,v2) -> M.op Op.SetValue v1 v2 >>= fun v ->
            write_reg_sz MachSize.S128 rd v ii >>! B.Next
        | I_CSEAL(rd,rn,rm) ->
            check_morello ii ;
            begin
              read_reg_ord_sz MachSize.S128 rn ii >>|
              read_reg_ord_sz MachSize.S128 rm ii
            end >>= fun (v1,v2) ->
            M.op Op.CSeal v1 v2 >>= fun v ->
            write_reg_sz MachSize.S128 rd v ii >>= fun _ ->
            (* TODO: PSTATE overflow flag would need to be conditionally set *)
            write_reg NZP M.A.V.zero ii >>! B.Next
        | I_GC(op,rd,rn) ->
            check_morello ii ;
            read_reg_ord_sz MachSize.S128 rn ii >>= begin fun c -> match op with
            | CFHI -> M.op1 (Op.LogicalRightShift 64) c
            | GCFLGS -> M.op1 (Op.AndK "0xff00000000000000") c
            | GCPERM -> M.op1 (Op.LogicalRightShift 110) c
            | GCSEAL -> M.op1 (Op.LeftShift 18) c >>= fun v ->
                M.op1 (Op.LogicalRightShift 113) v >>= fun v -> is_not_zero v
            | GCTAG -> M.op1 Op.CapaGetTag c
            | GCTYPE -> M.op1 (Op.LeftShift 18) c >>= fun v ->
                M.op1 (Op.LogicalRightShift 113) v
            | GCVALUE -> M.op1 (Op.Mask MachSize.Quad) c
            end >>= fun v -> write_reg_sz MachSize.Quad rd v ii >>! B.Next
        | I_SC(op,rd,rn,rm) ->
            check_morello ii ;
            begin
              read_reg_ord_sz MachSize.S128 rn ii >>|
              read_reg_ord_sz MachSize.Quad rm ii
            end >>=
            begin fun (cn, xm) -> match op with
              | CLRPERM -> M.op Op.ClrPerm cn xm
              | CTHI -> M.op Op.Cthi cn xm
              | SCFLGS ->
                begin
                  M.op1 (Op.AndK "0x00ffffffffffffff") cn >>|
                  M.op1 (Op.AndK "0xff00000000000000") xm
                end >>= fun (v,k) -> M.op Op.Or v k >>= fun v -> M.op Op.SetValue cn v
              | SCTAG -> M.op1 (Op.ReadBit 0) xm >>= fun cond ->
                  M.op Op.CapaSetTag cn cond
              | SCVALUE -> M.op Op.SetValue cn xm
            end >>= fun v ->
            write_reg_sz MachSize.S128 rd v ii >>! B.Next
        | I_SEAL(rd,rn,rm) ->
            check_morello ii ;
            begin
              read_reg_ord_sz MachSize.S128 rn ii >>|
              read_reg_ord_sz MachSize.S128 rm ii
            end >>= fun (a,b) ->
            M.op Op.Seal a b >>= fun v ->
            write_reg_sz MachSize.S128 rd v ii >>! B.Next
        | I_STCT(rt,rn) ->
            check_morello ii ;
            (* NB: only 1 access implemented out of the 4 *)
            lift_morello
              (fun _ac ma mv ->
                do_insert_commit
                  (ma >>| mv)
                  (fun (a,v) -> do_write_morello_tag a v ii >>! B.Next)
                  ii)
              (to_perms "tw" MachSize.S128)
              (read_reg_ord rn ii)
              (read_reg_data MachSize.Quad rt ii)
              ii
        | I_LDCT(rt,rn) ->
            check_morello ii ;
            (* NB: only 1 access implemented out of the 4 *)
            lift_morello
              (fun _ac ma _mv ->
                M.delay_kont "LDCT" ma
                  (fun _a ma ->
                    do_insert_commit ma
                      (fun a -> (* Why cheeck permissions again ? *)
                        M.op (Op.CheckPerms "tr_c") a M.A.V.zero >>= fun v ->
                        M.choiceT v
                          (do_read_morello_tag a ii)
                          mzero
                        >>= fun tag ->
                        write_reg_sz quad rt tag ii >>! B.Next)
                      ii))
              (to_perms "r" MachSize.S128)
              (read_reg_ord rn ii)
              mzero
              ii
        | I_UNSEAL(rd,rn,rm) ->
            check_morello ii ;
            begin
              read_reg_ord_sz MachSize.S128 rn ii >>|
              read_reg_ord_sz MachSize.S128 rm ii
            end >>= fun (a,b) ->
            M.op Op.Unseal a b >>= fun v ->
            write_reg_sz MachSize.S128 rd v ii >>! B.Next

        (* Operations *)
        | I_MOV(var,r,K k) ->
            (mask32 var
               (fun k -> write_reg r k ii)
               (V.intToV k))
              >>! B.Next
        | I_MOV(var,r1,RV (_,r2)) ->
            let sz = tr_variant var in
            read_reg_ord_sz sz r2 ii >>= fun v -> write_reg r1 v ii >>! B.Next
        | I_MOVZ(var,rd,k,os) ->
            movz var rd k os ii
        | I_MOVK(var,rd,k,os) ->
            movk var rd k os ii

        | I_ADDR (r,lbl) ->
            write_reg r (V.nameToV lbl) ii >>! B.Next
        | I_SXTW(rd,rs) ->
            (read_reg_ord_sz MachSize.Word rs ii) >>=
             sxtw_op >>= fun v -> write_reg rd v ii >>! B.Next 

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
                read_reg_ord_sz sz rn ii >>= fun v1 ->
                  (* if present, apply an optional inline barrel shift *)
                  begin match os with
                  | S_NOEXT -> M.unitT (v1,v1)
                  | s ->
                      check_and_shift op ty s v1
                      >>= fun v2 -> M.unitT (v1,v2)
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
              begin match ty with
              | V128 ->
                  check_morello ii ;
                  begin match op with
                  | ADD -> fun (v1,v2) -> M.op Op.CapaAdd v1 v2
                  | SUB -> fun (v1,v2) -> M.op Op.CapaSub v1 v2
                  | SUBS -> fun (v1,v2) -> M.op Op.CapaSubs v1 v2
                  | _ ->
                      Warn.fatal
                        "Operation '%s' is not available in morello mode"
                        (AArch64.pp_op op)
                  end
              | _ ->
                  begin match op with
                  | ADD|ADDS -> fun (v1,v2) -> M.add v1 v2
                  | EOR -> fun (v1,v2) -> M.op Op.Xor v1 v2
                  | ORR -> fun (v1,v2) -> M.op Op.Or v1 v2
                  | SUB|SUBS -> fun (v1,v2) -> M.op Op.Sub v1 v2
                  | AND|ANDS -> fun (v1,v2) -> M.op Op.And v1 v2
                  | ASR -> fun (v1, v2) -> M.op Op.ASR v1 v2
                  | LSR -> fun (v1,v2) -> M.op Op.Lsr v1 v2
                  | LSL -> fun (v1,v2) -> M.op Op.ShiftLeft v1 v2
                  | BIC|BICS -> fun (v1,v2) -> M.op Op.AndNot2 v1 v2
                  end
              end >>=
              (let m v =
                 (write_reg rd v ii) >>|
                   (match op with
                    | ADDS|SUBS|ANDS|BICS
                      -> is_zero v >>= fun v -> write_reg NZP v ii
                    | ADD|EOR|ORR|AND|SUB|ASR|LSR|LSL|BIC
                      -> M.unitT ()) in
               mask32 ty m) >>! B.Next

      (* Barrier *)
        | I_FENCE b ->
            (create_barrier b ii) >>! B.Next
              (* Conditional selection *)
        | I_CSEL (var,r1,r2,r3,c,op) ->
            let sz = tr_variant var in
            let mask = match op with
            | Cpy -> fun m -> m
            | Inc|Inv|Neg -> mask32 var in
            if not (C.variant Variant.NotWeakPredicated) then
              read_reg_ord NZP ii >>= tr_cond c >>*= fun v ->
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
            (* TODO: unify cas functions *)
            let cas = if morello then cas_morello else cas in
            cas (tr_variant v) rmw rs rt rn ii >>! B.Next
        | I_CASBH (v,rmw,rs,rt,rn) ->
            (* TODO: unify cas functions *)
            let cas = if morello then cas_morello else cas in
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
        | (I_RBIT _|I_MRS _|I_LDP _|I_STP _|I_IC _
        | I_BL _|I_BLR _|I_BR _|I_RET _) as i ->
            Warn.fatal "illegal instruction: %s" (AArch64.dump_instruction i)
        | I_LD1 _ | I_LD1M _ | I_LD1R _ | I_LD2 _ | I_LD2M _ | I_LD2R _
        | I_LD3 _ | I_LD3M _ | I_LD3R _ | I_LD4 _ | I_LD4M _ | I_LD4R _
        | I_LDP_SIMD _ | I_LDP_P_SIMD _ | I_LDR_SIMD _ | I_LDR_P_SIMD _ | I_LDUR_SIMD _
        | I_ST1 _ | I_ST1M _ | I_ST2 _ | I_ST2M _ | I_ST3 _ | I_ST3M _ | I_ST4 _ | I_ST4M _
        | I_STP_SIMD _ | I_STP_P_SIMD _ | I_STR_SIMD _ | I_STR_P_SIMD _ | I_STUR_SIMD _
        | I_MOV_S _ | I_MOV_V _ | I_MOV_VE _ | I_MOV_TG _ | I_MOV_FG _ | I_MOVI_S _ | I_MOVI_V _
        | I_EOR_SIMD _ | I_ADD_SIMD _ | I_ADD_SIMD_S _ ->
            Warn.fatal "Neon instructions are not currently supported"
        )
    end
  end
