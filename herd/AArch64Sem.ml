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
    (TopConf:AArch64Sig.Config)
    (V:Value.AArch64 with type Cst.Instr.t = AArch64Base.instruction)
    =
  struct
    module C = TopConf.C
    module ConfLoc = SemExtra.ConfigToArchConfig(C)
    module AArch64 = AArch64Arch_herd.Make(ConfLoc)(V)
    module Act = MachAction.Make(ConfLoc)(AArch64)
    include SemExtra.Make(C)(AArch64)(Act)

    let cache_type = match TopConf.cache_type with
      | None -> CacheType.default
      | Some ct -> ct
    let dirty = match TopConf.dirty with | None -> DirtyBit.soft | Some d -> d
    let mixed = AArch64.is_mixed
    (* We need to be aware of endianness for 128-bit Neon LDR/STR, because
     * these are always little endian *)
    let endian = AArch64.endian
    let memtag = C.variant Variant.MemTag
    let morello = C.variant Variant.Morello
    let neon = C.variant Variant.Neon
    let is_deps = C.variant Variant.Deps
    let kvm = C.variant Variant.VMSA
    let is_branching = kvm && not (C.variant Variant.NoPteBranch)
    let pte2 = kvm && C.variant Variant.PTE2
    let do_cu = C.variant Variant.ConstrainedUnpredictable
    let self = C.variant Variant.Self

    let check_memtag ins =
      if not memtag then
        Warn.user_error "%s without -variant memtag" ins

    let check_morello inst =
      if not morello then
        Warn.user_error
          "morello instruction %s require -variant morello"
          (AArch64.dump_instruction inst)

(* Barrier pretty print *)
    let barriers =
      let bs = AArch64Base.do_fold_dmb_dsb false true (fun h t -> h::t) []
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
      let (>>**==) = M.(>>**==)
      let (>>|) = M.(>>|)
      let (>>||) = M.para_atomic
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

      let has_handler ii =
        match ii.A.env.A.fh_code with
        | Some _ -> true
        | None -> false

      let mk_fault a dir annot ii ft msg =
        let fh = has_handler ii in
        M.mk_singleton_es (Act.Fault (ii,A.Location_global a,dir,annot,fh,ft,msg)) ii

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

      let read_reg_neon is_data r ii =
        if not neon then Warn.user_error "Advanced SIMD instructions require -variant neon" ;
        let vr = match r with
        | AArch64Base.SIMDreg _ -> r
        | AArch64Base.Vreg(vr',_) -> (AArch64Base.SIMDreg vr')
        | _ -> assert false in
          let location = A.Location_reg (ii.A.proc,vr) in
          M.read_loc is_data (mk_read MachSize.S128 AArch64.N aexp) location ii

      let neon_getlane cur_val idx esize =
        let mask = V.op1 (Op.LeftShift (idx*esize)) (AArch64.neon_mask esize) in
        M.op Op.And mask cur_val >>= fun masked_val ->
        M.op1 (Op.LogicalRightShift (idx*esize)) masked_val

      let read_reg_neon_elem is_data r idx ii = match r with
      | AArch64Base.Vreg (_,(_,esize)) ->
          read_reg_neon is_data r ii >>= fun cur_val ->
          neon_getlane cur_val idx esize
      | _ -> assert false

      let read_reg_sz sz is_data r ii = match sz with
      | MachSize.S128 -> read_reg_morello is_data r ii
      | MachSize.Quad when not morello || not is_data -> read_reg is_data r ii
      | MachSize.Quad|MachSize.Word|MachSize.Short|MachSize.Byte ->
          read_reg is_data r ii >>= fun v -> M.op1 (Op.Mask sz) v

      let read_reg_ord = read_reg_sz quad false
      let read_reg_ord_sz sz = read_reg_sz sz false
      let read_reg_data sz = read_reg_sz sz true
      let read_reg_tag is_data =  read_reg is_data

(* Fetch of an instruction, i.e., a read from a label *)
      let mk_fetch an loc v =
        let ac = Access.VIR in (* Instruction fetch seen as ordinary, non PTE, access *)
        Act.Access (Dir.R, loc, v, an, AArch64.nexp_annot, MachSize.Word, ac)

(* Basic write, to register  *)
      let mk_write sz an anexp ac v loc =
        Act.Access (Dir.W, loc, v, an, anexp, sz, ac)

      let write_reg r v ii = match r with
      | AArch64.ZR -> M.unitT ()
      | _ ->
          M.write_loc
            (mk_write quad AArch64.N aexp Access.REG v)
            (A.Location_reg (ii.A.proc,r)) ii

      let write_reg_dest r v ii = match r with
        | AArch64.ZR -> M.unitT V.zero
        | _ ->
            write_reg r v ii >>= fun () -> M.unitT v

      let write_reg_morello r v ii =
        if not morello then
          Warn.user_error "capabilities require -variant morello" ;
        M.write_loc
          (mk_write MachSize.S128  AArch64.N aexp Access.REG v)
          (A.Location_reg (ii.A.proc,r)) ii

      let neon_setlane old_val idx esize v =
        let mask = V.op1 (Op.LeftShift (idx*esize)) (AArch64.neon_mask esize) in
        let invert = V.op1 Op.Inv mask in
        M.op1 (Op.LeftShift (idx*esize)) v >>= fun new_val ->
        M.op Op.And invert old_val >>|
        M.op Op.And mask new_val >>= fun (v1,v2) ->
        M.op Op.Or v1 v2

      let rec neon_replicate old_val nelem esize v = match nelem with
      | 0 -> M.unitT old_val
      | _ ->
        neon_setlane old_val (nelem-1) esize v >>= fun old_val ->
        neon_replicate old_val (nelem-1) esize v

      let write_reg_neon_sz sz r v ii =
        if not neon then Warn.user_error "Advanced SIMD instructions require -variant neon" ;
        let vr = match r with
        | AArch64Base.SIMDreg _ -> r
        | AArch64Base.Vreg(vr',_) -> (AArch64Base.SIMDreg vr')
        | _ -> assert false in
          (* Clear unused register bits (zero extend) *)
          M.op1 (Op.Mask sz) v >>= fun v ->
          let location = A.Location_reg (ii.A.proc,vr) in
          M.write_loc (mk_write MachSize.S128 AArch64.N aexp Access.REG v) location ii

      let write_reg_neon = write_reg_neon_sz MachSize.S128

      let write_reg_neon_elem sz r idx v ii = match r with
      | AArch64Base.Vreg (_,(_,esize)) ->
          read_reg_neon false r ii >>=
          fun old_val -> neon_setlane old_val idx esize v >>= fun new_val ->
          write_reg_neon_sz sz r new_val ii
      | _ -> assert false

      let write_reg_neon_rep sz r v ii = match r with
      | AArch64Base.Vreg (_,(nelem,esize)) ->
        neon_replicate v nelem esize v >>= fun new_val -> write_reg_neon_sz sz r new_val ii
      | _ -> assert false

      let do_write_reg_sz op sz r v ii = match r with
      | AArch64.ZR -> M.unitT ()
      | _ -> match sz with
        | MachSize.S128 -> write_reg_morello r v ii
        | MachSize.Quad when not morello -> write_reg r v ii
        | MachSize.Quad|MachSize.Word|MachSize.Short|MachSize.Byte ->
            M.op1 (op sz) v >>= fun v -> write_reg r v ii

      let write_reg_sz = do_write_reg_sz (fun sz -> Op.Mask sz)
      and write_reg_sz_sxt = do_write_reg_sz (fun sz -> Op.Sxt sz)

      let write_reg_sz_non_mixed =
        if mixed then fun _sz -> write_reg
        else write_reg_sz

      and write_reg_sz_non_mixed_sxt =
        if mixed then
          fun sz r v ii ->
          M.op1 (Op.Sxt sz) v >>= fun v -> write_reg r v ii
        else write_reg_sz_sxt

(* Emit commit event *)
      let commit_bcc ii = M.mk_singleton_es (Act.Commit (Act.Bcc,None)) ii
      and commit_pred_txt txt ii =
        M.mk_singleton_es (Act.Commit (Act.Pred,txt)) ii

      let commit_pred ii = commit_pred_txt None ii

(* Fence *)
      let create_barrier b ii = M.mk_singleton_es (Act.Barrier b) ii

(* Page tables and TLBs *)
      let inv_loc op loc ii =
        let oloc = if A.TLBI.inv_all op then None else Some loc in
        M.mk_singleton_es (Act.Inv (op,oloc)) ii

(* Neon size *)
      let neon_esize r = match r with
      | AArch64Base.Vreg (_,(_,esize)) -> esize
      | _ -> assert false

      let neon_nelem r = match r with
      | AArch64Base.Vreg (_,(nelem,_)) -> nelem
      | _ -> assert false

      let neon_sz r =
        let size = match r with
        | AArch64Base.Vreg(_,(0,esize)) -> esize
        | AArch64Base.Vreg(_,(nelem,esize)) -> nelem * esize
        | _ -> assert false in
        match size with
          | 64 -> MachSize.Quad
          | 128 -> MachSize.S128
          | _ -> assert false

      let neon_sz_k var = let open AArch64Base in
      match var with
      | VSIMD8   -> (V.intToV 1)
      | VSIMD16  -> (V.intToV 2)
      | VSIMD32  -> (V.intToV 4)
      | VSIMD64  -> (V.intToV 8)
      | VSIMD128 -> (V.intToV 16)

(******************)
(* Memory Tagging *)
(******************)


(* Decompose tagged location *)
      let tag_extract a = M.op1 Op.TagExtract a
      let loc_extract a = M.op1 Op.LocExtract a

(*  Low level tag access *)
      let do_read_tag a ii =
        M.read_loc false
          (fun loc v -> access_ord Dir.R loc v Access.TAG)
          (A.Location_global a) ii
      and do_read_tag_nexp a ii =
        M.read_loc false
          (fun loc v -> access_anexp AArch64.nexp_annot Dir.R loc v Access.TAG)
          (A.Location_global a) ii
      and do_write_tag a v ii =
        let loc = A.Location_global a in
        M.mk_singleton_es
          (access_ord Dir.W loc v Access.TAG)
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

      let delayed_check_tags a_virt a_phy ma ii m1 m2 =
        let (+++) = M.data_input_union in
        let rtag  =
          read_tag_mem (match a_phy with None -> a_virt | Some a -> a) ii in
        let commit =  commit_pred_txt (Some "color") ii in
        let choice c ma = M.choiceT c (m1 ma) (m2 ma) in
        let m =
          rtag +++ fun tag1 -> tag_extract a_virt +++ fun tag2 ->
          M.op Op.Eq tag1 tag2 in
        let m =
          let (>>=) = match a_phy with None -> (+++) | Some _ -> (>>=) in
          m >>= fun cond -> commit +++ fun _ -> M.unitT cond in
        M.delay_kont "check_tags" m
          (fun c m ->
            let ma = (* NB output to initial ma *)
              ma >>==
                (fun a -> m >>== fun _ ->
                match a_phy with
                | None -> loc_extract a
                | Some _ -> M.unitT a) in
            choice c ma)

      (* Tag checking Morello *)
      let do_append_commit ma txt ii =
        ma >>== fun a -> commit_pred_txt txt ii >>= fun () -> M.unitT a

      let mzero = M.unitT M.A.V.zero

      let check_morello_tag a ma mv mok mfault =
        M.op1 Op.CapaGetTag a >>= fun x ->
        M.op Op.Ne x V.zero >>= fun cond ->
        M.choiceT cond (mok ma mv) (mfault ma mzero)

      let check_morello_sealed a ma mv mok mfault =
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

      let arch_op1 op = M.op1 (Op.ArchOp1 op)

      let extract_af v = arch_op1 AArch64Op.AF v
      let extract_db v = arch_op1 AArch64Op.DB v
      let extract_dbm v = arch_op1 AArch64Op.DBM v
      let extract_valid v = arch_op1 AArch64Op.Valid v
      let extract_el0 v = arch_op1 AArch64Op.EL0 v
      let extract_oa v = arch_op1 AArch64Op.OA v

      let mextract_whole_pte_val an nexp a_pte iiid =
        (M.do_read_loc false
           (fun loc v ->
             Act.Access (Dir.R,loc,v,an,nexp,quad,Access.PTE))
           (A.Location_global a_pte) iiid)

      and write_whole_pte_val an explicit a_pte v iiid =
        M.do_write_loc
          (mk_write quad an explicit Access.PTE v)
          (A.Location_global a_pte) iiid


      let op_of_set = function
        | AArch64.AF -> AArch64Op.SetAF
        | AArch64.DB -> AArch64Op.SetDB
        | AArch64.Other|AArch64.AFDB -> assert false

      let do_test_and_set_bit combine cond set a_pte iiid =
        let nexp = AArch64.NExp set in
        mextract_whole_pte_val AArch64.X nexp a_pte iiid >>= fun pte_v ->
        cond pte_v >>*= fun c ->
        combine c
            (arch_op1 (op_of_set set) pte_v >>= fun v ->
             write_whole_pte_val AArch64.X nexp a_pte v iiid)
            (M.unitT ())

      let test_and_set_bit cond =  do_test_and_set_bit M.choiceT cond
      and test_and_set_bit_succeeds cond =
        do_test_and_set_bit (fun c m _ -> M.assertT c m) cond

      let bit_is_zero op v = arch_op1 op v >>= is_zero
      let bit_is_not_zero op v = arch_op1 op v >>= is_not_zero
      let m_op op m1 m2 = (m1 >>| m2) >>= fun (v1,v2) -> M.op op v1 v2

      let do_set_bit an a_pte pte_v ii =
        let nexp = AArch64.NExp an in
        arch_op1 (op_of_set an) pte_v >>= fun v ->
        write_whole_pte_val AArch64.X nexp a_pte v (E.IdSome ii)

      let set_af = do_set_bit AArch64.AF
      and set_db = do_set_bit AArch64.DB

      let set_afdb a_pte pte_v ii =
        let nexp = AArch64.NExp AArch64.AFDB in
        arch_op1 (AArch64Op.SetAF) pte_v >>= arch_op1 (AArch64Op.SetDB) >>= fun v ->
        write_whole_pte_val AArch64.X nexp a_pte v (E.IdSome ii)

      let cond_af v =
        m_op Op.And
          (bit_is_zero AArch64Op.AF v) (bit_is_not_zero AArch64Op.Valid v)

      let test_and_set_af = test_and_set_bit cond_af AArch64.AF

      and test_and_set_af_succeeds =
        test_and_set_bit_succeeds cond_af AArch64.AF

      and test_and_set_db =
        test_and_set_bit
          (fun v ->
            m_op Op.And
              (bit_is_zero AArch64Op.DB v)
              (bit_is_not_zero AArch64Op.Valid v))
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
        >>= fun (o,(_,ipte)) -> M.add ipte.oa_v o

(************************************************)
(* Add commit events, when commanded by options *)
(************************************************)


      let append_commit ma txt ii =
        if is_branching then do_append_commit ma txt ii else ma

      let do_insert_commit m1 m2 ii =
      (* Notice the complex dependency >>*==
         from branch to instructions events *)
        m1 >>= fun a -> commit_pred ii >>*== fun _ -> m2 a

      let insert_commit m1 m2 ii =
        if is_branching || morello then do_insert_commit m1 m2 ii
        else m1 >>= m2

      let do_insert_commit_to_fault m1 m2 ii =
        (* Dependencies to fault are simple: Rpte -data-> Branch -> Fault *)
        M.bind_data_to_minimals m1
          (fun a -> commit_pred ii >>*= fun () -> m2 a)

      let insert_commit_to_fault m1 m2 ii =
        if is_branching || morello then do_insert_commit_to_fault m1 m2 ii
        else m1 >>*= m2 (* Direct control dependency to fault *)

(******************)
(* Checking flags *)
(******************)

(* With choice operator *)
      let do_check_cond m m_cond k1 k2 =
        M.delay_kont "1"
          (m >>= fun (_,pte_v as p) ->
           m_cond pte_v >>= fun c -> M.unitT (c,p))
          (fun (c,p) m ->
            let m = m >>= fun _ -> M.unitT p in
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

      let mk_pte_fault a ma dir an ii =
        let open FaultType.AArch64 in
        let ft = Some (MMU Permission) in
        insert_commit_to_fault ma
          (fun _ -> mk_fault a dir an ii ft (Some "EL0"))  ii >>! B.Exit

      let an_xpte =
        let open AArch64 in
        function
        | A|XA -> XA
        | Q|XQ -> XQ
        | L|XL -> XL
        | X|N  -> X
        |NoRet|S -> X (* Does it occur? *)

      let an_pte =
        let open AArch64 in
        function
        | A|XA -> A
        | Q|XQ -> Q
        | L|XL -> L
        | X|N -> N
        | NoRet|S -> N


      let check_ptw proc dir updatedb a_virt ma an ii mdirect mok mfault =

        let is_el0  = List.exists (Proc.equal proc) TopConf.procs_user in
        let check_el0 m =
          if is_el0 then
               fun pte_v -> m_op Op.Or (is_zero pte_v.el0_v) (m pte_v)
             else m in

        let open DirtyBit in
        let tthm = dirty.tthm proc
        and ha = dirty.ha proc
        and hd = dirty.hd proc in
        let ha = ha || hd in (* As far as we know hd => ha *)
        let mfault (_,ipte) m =
          let open FaultType.AArch64 in
          (is_zero ipte.valid_v) >>=
            (fun c ->
              M.choiceT c
                (M.unitT (Some (MMU Translation)))
                (if ha then
                   M.unitT (Some (MMU Permission))
                 else begin
                   (is_zero ipte.af_v) >>=
                     (fun c ->
                       M.choiceT c
                         (M.unitT (Some (MMU AccessFlag)))
                         (M.unitT (Some (MMU Permission))))
                   end) >>=
                fun t -> mfault (get_oa a_virt m) a_virt t)
        and mok (pte_v,ipte) a_pte m a =
          let m =
            let msg =
              match dir with
              | Dir.W ->
                 if hd then
                   "valid:1 && (db:1 || dbm:1 && hd)"
                 else if ha then
                   "valid:1 && db:1"
                 else
                   "valid:1 && af:1 && db:1"
              | Dir.R ->
                 if ha then "valid:1"
                 else "valid:1 && af:1" in
            let m = append_commit m (Some msg) ii in
            let add_setbits cond txt set no =
              cond >>= fun c ->
              M.choiceT c
                (m >>**==
                   (fun _ ->
                     commit_pred_txt (Some txt) ii >>*=
                       fun _ -> set a_pte pte_v ii)
                 >>== fun () -> M.unitT (pte_v,ipte))
               no in
            let setbits =
              match dir with
              | Dir.W ->
                 if hd && updatedb then
                   add_setbits
                     (m_op Op.Or (is_zero ipte.af_v) (is_zero ipte.db_v))
                     "af:0 || db:0"
                     set_afdb m
                 else if ha then
                   add_setbits (is_zero ipte.af_v) "af:0" set_af m
                 else m
              | Dir.R ->
                  if ha then
                   add_setbits (is_zero ipte.af_v) "af:0" set_af m
                 else m in
            setbits in
          mok (get_oa a_virt m) a in


(* Action on case of page table access.
   Delay is used so as to have correct dependencies,
   getting content of PTE by anticipation. *)
        let mvirt = begin
          M.delay_kont "3"
            begin
              ma >>= fun _ -> M.op1 Op.PTELoc a_virt >>= fun a_pte ->
              let an,nexp =
                if hd then (* Atomic accesses, tagged with updated bits *)
                  an_xpte an,AArch64.NExp AArch64.AFDB
                else if ha then
                  an_xpte an,AArch64.NExp AArch64.AF
                else
                  (* Ordinary non-explicit access *)
                  an_pte an,AArch64.nexp_annot in
              mextract_whole_pte_val
                an nexp a_pte (E.IdSome ii) >>== fun pte_v ->
              (mextract_pte_vals pte_v) >>= fun ipte ->
              M.unitT ((pte_v,ipte),a_pte)
            end
          (fun (pair_pte,a_pte) ma -> (* now we have PTE content *)
            (* Monad will carry changing internal pte value *)
            let ma = ma >>= fun (p,_) -> M.unitT p in
            (* wrapping of success/failure continuations,
               only pte value may have changed *)
            let mok ma = mok pair_pte a_pte ma a_virt
(* a_virt was (if pte2 then a_virt else pte_v.oa_v), why? *)
            and mno ma =  mfault pair_pte ma in
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
            (if is_el0 then mk_pte_fault a_virt ma dir an ii
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
        do_read_mem_ret sz an anexp ac a ii
        >>= fun v -> write_reg_sz_non_mixed sz rd v ii
        >>= fun () -> B.nextT

      and do_read_mem_sxt sz an anexp ac rd a ii =
        do_read_mem_ret sz an anexp ac a ii
        >>= fun v -> write_reg_sz_non_mixed_sxt sz rd v ii
        >>= fun () -> B.nextT

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
            ((if do_cu (* If CU allowed, write may succeed whatever the address _a_ is *)
              then M.unitT () else M.assign a resa)
             >>| check_mixed_write_mem sz an anexp ac a v ii)
            >>! ())
        a v ii

(* Page tables and TLBs *)
      let do_inv op a ii = inv_loc op (A.Location_global a) ii

(************************)
(* Conditions and flags *)
(************************)

      let tr_cond =
        (* Utils for writing formulas:
           Here we do operations on functions that will generate the
           underlying monadic operations. We first define our fake variables,
           then a few logical operations on those.*)
        (* Variables *)
        let n = M.op1 (Op.ReadBit 3) in
        let z = M.op1 (Op.ReadBit 2) in
        let c = M.op1 (Op.ReadBit 1) in
        let v = M.op1 (Op.ReadBit 0) in
        let true_ = fun _flags -> M.unitT V.one in
        (* Operators *)
        (* Note: I use [!a] as a shortcut for [a == 0],
           and [a] as a shortcut for [a == 1]
           (the expended version is the one written in the ARM ARM). *)
        let ( ! ) f flags = f flags >>= M.op Op.Eq V.zero in
        let make_op op f1 f2 flags =
          f1 flags >>| f2 flags >>= fun (v1, v2) -> M.op op v1 v2
        in
        let ( == ) = make_op Op.Eq in
        let ( || ) = make_op Op.Or in
        let ( && ) = make_op Op.And in
        (* Note : I use [a <> b] as a shortcut for [!a == b]
           (the expended version is the one written in the ARM ARM). *)
        let ( <> ) = make_op Op.Xor in
        let open AArch64Base in
        (* The real operations, as defined by the ARM ARM. *)
        function
        | NE -> !z
        | EQ -> z
        | GE -> n == v
        | GT -> n == v && !z
        (* Note: for LE, the ARM ARM gives [!(!z && n == v)] here,
           but I've applied De Morgan's law. *)
        | LE -> n <> v || z
        | LT -> n <> v
        | CS -> c
        | CC -> !c
        | MI -> n
        | PL -> !n
        | VS -> v
        | VC -> !v
        | HI -> c && !z
        (* Note: for LS, the ARM ARM gives [!(c && !z)] here,
           but I've applied De Morgan's law. *)
        | LS -> !c || z
        | AL -> true_

(* Arithmetic flags handling *)

      let op_set_flags op ty =
        let open AArch64Base in
        (* Utils for writing formulas
           - We use a base functional type, this surely impacts performance,
             but clarity is improved.
           - We surcharge common operators to use our own functional
             types.
           - The main values come from the three arguments passed to every functions
           - The performance cost is never on operations that do not set flags,
             and mainly on ADDS/SUBS. It consists on an extra monad for every
             variable used.
        *)
        (* Main variables *)
        let res v0 _v1 _v2 = M.unitT v0 in
        let x _v0 v1 _v2 = M.unitT v1 in
        let y _v0 _v1 v2 = M.unitT v2 in
        (* Operators on those variables *)
        let make_op op f1 f2 v0 v1 v2 =
          f1 v0 v1 v2 >>| f2 v0 v1 v2 >>= fun (a, b) -> M.op op a b
        in
        let make_op1 fop f v0 v1 v2 = f v0 v1 v2 >>= fop in
        let ( ! ) = make_op1 (M.op1 Op.Inv) in
        let ( & ) = make_op Op.And in
        let ( || ) = make_op Op.Or in
        let ( + ) = make_op Op.Xor in
        let ( === ) f v = make_op1 (M.op Op.Eq v) f in
        let ( << ) f i = make_op1 (M.op1 (Op.LeftShift i)) f in
        let sign_bit = MachSize.nbits (AArch64Base.tr_variant ty) - 1 in
        let read_sign_bit = make_op1 (M.op1 (Op.ReadBit sign_bit)) in
        let ( ---> ) f i = ( read_sign_bit f ) << i in
        (* Computation of nz flags *)
        let compute_nz =
          let compute_z = ( res === V.zero ) << 2 in
          let compute_n = read_sign_bit res << 3 in
          compute_z || compute_n
        in
        (* Operation specific computations
           For specific formulae, see Hacker's Delight, 2-13.*)
        match op with
        | ADD | EOR | ORR | ORN | SUB | AND | ASR | LSR | LSL | BIC -> None
        | ANDS | BICS -> Some compute_nz
        | ADDS ->
            let x = make_op Op.ToInteger x res
            and y = make_op Op.ToInteger y (fun _ _ _ -> mzero) in
            let compute_c = ((x & y) || ((x || y) & !res)) ---> 1 in
            let compute_v = ((res + x) & (res + y)) ---> 0 in
            Some (compute_nz || compute_c || compute_v)
        | SUBS ->
          (*
            This is the formula give by Hacker's Delight for the carry in an
            unsigned subtraction:
              (!x & y) || ((!x || y) & res)
            But I use the formula given by Hacker's Delight for the carry in an
            unsigned addition, with y replaced by !y, as the Arm ARM specifies
            the subtraction as:
              x - y := x + !y + 1
            This gives the following formula, which seems to produce the same
            results as hardware:
          *)
            let x = make_op Op.ToInteger x res
            and y = make_op Op.ToInteger y (fun _ _ _ -> mzero) in
            let compute_c = ((x & !y) || ((x || !y) & !res)) ---> 1 in
            let compute_v = ((x + y) & (res + x)) ---> 0 in
            Some (compute_nz || compute_c || compute_v)

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

(*  memtag faults *)

      let lift_fault_memtag mfault mm dir ii =
        if has_handler ii then
          fun ma -> M.bind_ctrldata ma (fun _ -> mfault >>! B.Fault dir)
        else
          let open Precision in
          match C.precision,dir with
          | (Fatal,_)|(LoadsFatal,(Dir.R)) ->
             fun ma ->  ma >>*= (fun _ -> mfault >>! B.Exit)
          | (Handled,_)|(LoadsFatal,Dir.W) ->
             fun ma ->
             let (>>) = M.bind_ctrl_first_outputs in
             let ma = ma >> (fun a -> mfault >>! a) in
             mm ma >>! B.Next []
          | Skip,_ ->
             fun _ ->
               Warn.fatal "Memtag extension has no 'Skip' fault handling mode"

      let lift_memtag_phy a_virt mop ma dir an ii =
        M.delay_kont "4" ma
          (fun a_phy ma ->
            let mm = mop Access.PHY in
            let ft = Some FaultType.AArch64.TagCheck in
            delayed_check_tags a_virt (Some a_phy) ma ii
              (fun ma -> mm ma >>= M.ignore >>= B.next1T)
              (lift_fault_memtag (mk_fault a_virt dir an ii ft None) mm dir ii))


      let lift_memtag_virt mop ma dir an ii =
        M.delay_kont "5" ma
          (fun a_virt ma  ->
            let mm = mop Access.VIR in
            let ft = Some FaultType.AArch64.TagCheck in
            delayed_check_tags a_virt None ma ii
              (fun ma -> mm ma >>= M.ignore >>= B.next1T)
              (lift_fault_memtag (mk_fault a_virt dir an ii ft None) mm dir ii))

(* KVM mode *)
      let some_ha = dirty.DirtyBit.some_ha || dirty.DirtyBit.some_hd

      let fire_spurious_af dir a m =
        if
          some_ha &&
            (let v = C.variant Variant.PhantomOnLoad in
             match dir with Dir.W -> not v | Dir.R -> v)
        then
          (m >>|
             M.altT (test_and_set_af_succeeds a E.IdSpurious) (M.unitT ())) >>=
            fun (r,_) -> M.unitT r
        else m

      let set_elr_el1 ii =
        let lbl =
          match Label.norm ii.A.labels with
          | Some hd -> hd
          | None -> "undef" in
        let lbl_v = A.V.cstToV (Constant.Label (ii.A.proc, lbl)) in
        write_reg AArch64Base.elr_el1 lbl_v ii

      let lift_kvm dir updatedb mop ma an ii mphy =
        let mfault ma a ft =
          insert_commit_to_fault ma (fun _ -> mk_fault a dir an ii ft None) ii >>|
          set_elr_el1 ii >>! B.Fault dir in
        let maccess a ma =
          check_ptw ii.AArch64.proc dir updatedb a ma an ii
            ((let m = mop Access.PTE ma in
              fire_spurious_af dir a m) >>= M.ignore >>= B.next1T)
            mphy
            mfault in
        M.delay_kont "6" ma
          (if pte2 then maccess
           else
             fun a ma ->
             match Act.access_of_location_std (A.Location_global a) with
             | Access.VIR|Access.PTE -> maccess a ma
             | ac -> mop ac ma >>= M.ignore >>= B.next1T)

      let lift_morello mop perms ma mv dir an ii =
        let mfault msg ma mv =
          let ft = None in (* FIXME *)
          do_insert_commit
            (ma >>| mv)
            (fun (a,_v) -> mk_fault a dir an ii ft (Some msg)) ii  >>! B.Exit in
        M.delay_kont "morello" ma
          (fun a ma ->
            (* Notice: virtual access only, beaause morello # kvm *)
            let mok ma mv = mop Access.VIR ma mv in
            check_morello_tag a ma mv
              (fun ma mv ->
                check_morello_sealed a ma mv
                  (fun ma mv ->
                    check_morello_perms a ma mv perms
                      (fun ma mv -> mok ma mv >>= M.ignore >>= B.next1T)
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

      let is_this_reg_read rA e =
        match E.location_reg_of e with
        | None -> false
        | Some rB -> AArch64.reg_compare rA rB=0

      let lift_memop rA (* Base address register *)
            dir updatedb mop perms ma mv an ii =
        if morello then
          lift_morello mop perms ma mv dir an ii
        else
          let mop = apply_mv mop mv in
          if memtag then
            begin
              if kvm then
                let mphy = (fun ma a -> lift_memtag_phy a mop ma dir an ii) in
                M.short3
                  (is_this_reg_read rA) E.is_commit
                  (lift_kvm dir updatedb mop ma an ii mphy)
              else lift_memtag_virt mop ma dir an ii
            end
          else if kvm then
            let mphy =
              if pte2 then
                fun ma a_virt ->
                M.op1 Op.IsVirtual a_virt >>= fun c ->
                M.choiceT c
                  (mop Access.PHY ma)
                  (fire_spurious_af dir a_virt (mop Access.PHY_PTE ma))
                >>= M.ignore >>= B.next1T
              else
                fun ma _a ->
                mop Access.PHY ma
                >>= M.ignore >>= B.next1T in
            lift_kvm dir updatedb mop ma an ii mphy
          else
            mop Access.VIR ma >>= M.ignore >>= B.next1T

      let do_ldr rA sz an mop ma ii =
(* Generic load *)
        lift_memop rA  Dir.R true
          (fun ac ma _mv -> (* value fake here *)
            if Access.is_physical ac || memtag  then
              M.bind_ctrldata ma (mop ac)
            else
              ma >>= mop ac)
          (to_perms "r" sz)
          ma mzero an ii

(* Generic store *)
      let do_str rA mop sz an ma mv ii =
        lift_memop rA Dir.W true
          (fun ac ma mv ->
            if memtag || (is_branching && Access.is_physical ac) then begin
              (* additional ctrl dep on address *)
              M.bind_ctrldata_data ma mv
                (fun a v -> mop ac a v ii)
            end else if morello then
              (* additional ctrl dep on address and data *)
              do_insert_commit (ma >>| mv)
                (fun (a,v) -> mop ac a v ii)
                ii
            else
              (ma >>| mv) >>= fun (a,v) -> mop ac a v ii)
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
          | S_LSL(n)
          | S_MSL(n)
            -> fun x -> M.op (Op.ShiftLeft) x (V.intToV n)
          | S_LSR(n)  -> fun x -> M.op (Op.ShiftRight) x (V.intToV n)
          | S_ASR(n)  -> fun x -> M.op (Op.ASR) x (V.intToV n)
          | S_SXTW -> sxtw_op
          | S_UXTW -> uxtw_op

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

      let add_size a sz = M.add a (V.intToV (MachSize.nbytes sz))

      let post_kr rA addr kr ii =
        let open AArch64Base in
        let get_k = match kr with
        | K k -> M.unitT (V.intToV k)
        | RV(_,rO) -> read_reg_ord rO ii in
        get_k >>= fun k ->
        if V.is_var_determined k && V.is_zero k then M.unitT() else
        M.add addr k >>= fun new_addr ->
        write_reg rA new_addr ii

      let ldr sz rd rs kr s ii = (* load *)
        do_ldr rs sz AArch64.N
          (fun ac a -> do_read_mem sz AArch64.N aexp ac rd a ii)
          (get_ea rs kr s ii) ii

      and ldp sz rd1 rd2 rs kr ii =
        do_ldr rs sz AArch64.N
          (fun ac a ->
            do_read_mem sz AArch64.N aexp ac rd1 a ii >>|
            begin
              add_size a sz >>=
              fun a -> do_read_mem sz AArch64.N aexp ac rd2 a ii
            end)
          (get_ea_noext rs kr ii) ii

      and ldpsw rd1 rd2 rs kr ii =
        let mem_sz =  MachSize.Word in
        do_ldr rs MachSize.Word AArch64.N
          (fun ac a ->
            begin
              do_read_mem_sxt mem_sz AArch64.N aexp ac rd1 a ii >>|
              begin
                add_size a mem_sz >>=
                fun a -> do_read_mem_sxt mem_sz AArch64.N aexp ac rd2 a ii
              end
            end)
          (get_ea_noext rs kr ii) ii

      and ldxp sz t rd1 rd2 rs ii =
        let open AArch64 in
        let an = match t with XP -> X | AXP -> XA in
        do_ldr rs sz an
          (fun ac a ->
            read_mem_reserve sz an aexp ac rd1 a ii >>||
            begin
              add_size a sz >>= fun a ->
              do_read_mem sz an aexp ac rd2 a ii
            end)
          (read_reg_ord rs ii) ii

      (* Load literal *)
      and ldr_lit _ rd lbl ii =
        (* We do not use the reg size as we load a word *)
        let open AArch64Base in
        M.unitT (ii.A.addr2v lbl)
        >>= fun a -> write_reg rd a ii
        >>= B.next1T

      and ldar sz t rd rs ii =
        let open AArch64 in
        let an = match t with
        | XX -> AArch64.X
        | AA -> AArch64.A
        | AX -> AArch64.XA
        | AQ -> AArch64.Q in
        do_ldr rs sz an
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
            do_ldr rs sz AArch64.N
              (fun ac a ->
                read_mem_postindexed a_virt sz AArch64.N aexp ac rd rs k a ii)
              ma ii)

      and str sz rs rd kr s ii =
        do_str rd
          (fun ac a _ ii ->
            M.data_input_next
              (read_reg_data sz rs ii)
              (fun v -> do_write_mem sz AArch64.N aexp ac a v ii))
          sz AArch64.N
          (get_ea rd kr s ii)  (M.unitT V.zero) ii

      (* Store - post-indexed write *)
      and str_post sz rs rd k ii =
        M.delay_kont "str_post"
          (read_reg_ord rd ii)
          (fun a_virt ma ->
            do_str rd
              (fun ac a _ ii ->
                M.add a_virt (V.intToV k) >>= fun b -> write_reg rd b ii
        >>|
                M.data_input_next
                  (read_reg_data sz rs ii)
                  (fun v -> do_write_mem sz AArch64.N aexp ac a v ii))
              sz AArch64.N
              ma (M.unitT V.zero) ii)

      and stp =
        let (>>>) = M.data_input_next in
        fun sz rs1 rs2 rd kr ii ->
        do_str rd
          (fun ac a _ ii ->
            (read_reg_data sz rs1 ii >>> fun v ->
             do_write_mem sz AArch64.N aexp ac a v ii) >>|
              (add_size a sz >>= fun a ->
               read_reg_data sz rs2 ii >>> fun v ->
               do_write_mem sz AArch64.N aexp ac a v ii))
          sz AArch64.N
          (get_ea_noext rd kr ii)
          (M.unitT V.zero)
          ii
      (* Load signed - sign extends to either 32 or 64 bit value*)
      let ldrs sz var rd rs kr s ii =
        (* load either 8 or 16 bit value (sz),
        then sign extend based on register size (var) *)
        do_ldr rs sz AArch64.N
          (fun ac a -> do_read_mem_ret sz AArch64.N aexp ac a ii
            >>= M.op1 (Op.Sxt sz)
            >>= fun v2 -> write_reg_sz_non_mixed
              (AArch64.tr_variant var) rd v2 ii)
          (get_ea rs kr s ii) ii

      and stlr sz rs rd ii =
        do_str rd (do_write_mem sz AArch64.L aexp) sz AArch64.L
          (read_reg_ord rd ii) (read_reg_data sz rs ii) ii

      and do_stxr ms mw sz t rr rd ii  =
        let open AArch64Base in
        let an = match t with
          | YY -> AArch64.X
          | LY -> AArch64.XL in
        lift_memop rd Dir.W true
          (fun ac ma mv ->
            let must_fail =
              begin
                let open AArch64 in
                match ii.env.lx_sz with
                | None -> true (* No LoadExcl at all. always fail *)
                | Some szr ->
                   (* Some, must fail when size differ and cu is disallowed *)
                   not (do_cu || MachSize.equal szr sz)
              end in
            M.aarch64_store_conditional must_fail
              (read_reg_ord ResAddr ii)
              mv ma
              (write_reg ResAddr V.zero ii)
              (fun v -> write_reg rr v ii)
              (mw an ac))
              (to_perms "w" sz)
              (read_reg_ord rd ii)
              ms an ii

      let stxr sz t rr rs rd ii =
        do_stxr
          (read_reg_data sz rs ii)
          (fun an ac ea resa v  -> write_mem_atomic sz an aexp ac ea v resa ii)
          sz t rr rd ii

      let stxp sz t rr rs1 rs2 rd ii =
        let (>>>) = M.data_input_next in
        do_stxr
          (M.unitT (V.zero))
          (fun an ac ea resa _ ->
            begin
              (read_reg_data sz rs1 ii >>> fun v ->
               write_mem_atomic sz an aexp ac ea v resa ii)
              >>||
                (add_size ea sz >>= fun a ->
                 read_reg_data sz rs2 ii >>> fun v ->
                 check_morello_for_write
                   (fun a ->
                     check_mixed_write_mem sz an aexp ac a v ii) a v ii)
            end >>!  ())
        sz t rr rd ii

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
        lift_memop r3 Dir.W true (* swp is a write for the purpose of DB *)
          (fun ac ma mv ->
            let r2 = mv
            and w2 v = write_reg_sz_non_mixed sz r2 v ii
            and r1 a = rmw_amo_read sz rmw ac a ii
            and w1 a v = rmw_amo_write sz rmw ac a v ii in
            M.swp
              (Access.is_physical ac)
              ma
              r1 r2 w1 w2)
          (to_perms "rw" sz)
          (read_reg_ord r3 ii)
          (read_reg_data sz r1 ii)
          (rmw_to_read rmw)
          ii

      let cas sz rmw rs rt rn ii =
        let an = rmw_to_read rmw in
        let read_rs = read_reg_data sz rs ii
        and write_rs v = write_reg_sz_non_mixed sz rs v ii in
        lift_memop rn Dir.W true
           (* mv is read new value from reg, not important
              as this code is not executed in morello mode *)
          (fun ac ma mv ->
            let is_phy = Access.is_physical ac in
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
          Dir.W (rmw_to_read rmw) ii

      let ldop op sz rmw rs rt rn ii =
        let open AArch64 in
        let an = rmw_to_read rmw in
        lift_memop rn Dir.W true
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
            M.amo_strict (Access.is_physical ac) op
              ma
              (fun a -> read_mem sz a ii) mv
              (fun a v -> write_mem sz a v ii)
              (fun w ->
                if noret then M.unitT ()
                else write_reg_sz_non_mixed sz rt w ii))
          (to_perms "rw" sz)
          (read_reg_ord rn ii)
          (read_reg_data sz rs ii)
          an ii

      (* Utility that performes an `N`-bit load as two independent `N/2`-bit
       * loads. Used by 128-bit Neon LDR.
       *
       * This instruction is endianness-aware (i.e. performing an 128-bit
       * load as two 64-bit loads will take the endianness of the CPU into
       * account). This is, indeed, a difference in semantics with Neon
       * with respect to, e.g., `LDR` vs `LD1`. *)
      let do_read_mem_2_ops_ret sz an anexp ac addr1 ii =
        let open MachSize in
        assert (sz != MachSize.Byte);
        let sz_half = MachSize.pred sz in
        do_read_mem_ret sz_half an anexp ac addr1 ii >>|
        begin
          M.add addr1 (V.intToV (nbytes sz_half)) >>= fun addr2 ->
          do_read_mem_ret sz_half an anexp ac addr2 ii
        end >>= fun (v1, v2) ->
        let v_lo, v_hi = match endian with
          (* Because an 128-bit Neon STR is allowed to be broken up into
           * 64-bit loads, this means that we do need to take endianness into
           * account *)
          | Endian.Little -> v1, v2
          | Endian.Big -> v2, v1 in
        M.op1 (Op.LeftShift (nbits sz_half)) v_hi >>= fun v_hi_shifted ->
        M.op Op.Or v_lo v_hi_shifted

      (* Utility that performes an `N`-bit store as two independent `N/2`-bit
       * stores. Used by Neon instructions. *)
      let write_mem_2_ops sz anexp ac addr1 v ii =
        let open MachSize in
        assert (sz != MachSize.Byte);
        let sz_half = MachSize.pred sz in
        let comp_lo = M.op1 (Op.Mask sz_half) v in
        let comp_hi = M.op1 (Op.LogicalRightShift (nbits sz_half)) v in
        let (comp_v1, comp_v2) = match endian with
          (* Because an 128-bit Neon STR is allowed to be broken up into
           * 64-bit loads, this means that we do need to take endianness into
           * account *)
          | Endian.Little -> comp_lo, comp_hi
          | Endian.Big -> comp_hi, comp_lo in
        begin
          comp_v1 >>= fun v1 ->
          write_mem sz_half anexp ac addr1 v1 ii
        end >>|
        begin
          M.add addr1 (V.intToV (nbytes sz_half)) >>|
          comp_v2 >>= fun (addr2, v2) ->
          write_mem sz_half anexp ac addr2 v2 ii
        end

      (* Neon extension, memory accesses return B.Next, as they cannot fail *)
      let simd_ldr sz addr rd ii =
        (* 128-bit Neon LDR/STR and friends are split into two 64-bit
         * single-copy atomic accesses. *)
        let mem_op = begin
          if sz == MachSize.S128 then do_read_mem_2_ops_ret else do_read_mem_ret
        end in
        mem_op sz AArch64.N aexp Access.VIR addr ii >>= fun v ->
        write_reg_neon_sz sz rd v ii

      let simd_str sz rs rd kr s ii =
        get_ea rs kr s ii >>|
        read_reg_neon true rd ii >>= fun (addr, v) ->
        if sz == MachSize.S128 then
          write_mem_2_ops sz aexp Access.VIR addr v ii >>= B.next2T
        else
          write_mem sz aexp Access.VIR addr v ii >>= B.next1T

      let simd_str_p sz rs rd k ii =
        read_reg_ord rs ii >>|
        read_reg_neon true rd ii >>= fun (addr, v) ->
        if sz == MachSize.S128 then
          (* 128-bit Neon LDR/STR and friends are split into two 64-bit
           * single-copy atomic accesses. *)
          write_mem_2_ops sz aexp Access.VIR addr v ii >>|
          post_kr rs addr k ii >>= B.next3T
        else
          write_mem sz aexp Access.VIR addr v ii >>|
          post_kr rs addr k ii >>= B.next2T

      let simd_ldp var addr1 rd1 rd2 ii =
        let open AArch64Base in
        let sz = tr_simd_variant var in
        simd_ldr sz addr1 rd1 ii >>|
        begin
          M.add addr1 (neon_sz_k var) >>= fun addr2 ->
          simd_ldr sz addr2 rd2 ii
        end >>= B.next2T

      let simd_stp var addr1 rd1 rd2 ii =
        let open AArch64Base in
        let sz = tr_simd_variant var in
        if sz == MachSize.S128 then
          (* 128-bit Neon LDR/STR are not single-copy atomic, but they
           * are single-copy atomic for each of the two 64-bit quantities
           * they access. This means that a 2x128-bit LDP/STP with Neon
           * registers results in 4 single-copy atomic accesses. *)
          begin
            read_reg_neon true rd1 ii >>= fun v1 ->
            write_mem_2_ops sz aexp Access.VIR addr1 v1 ii
          end >>|
          begin
            M.add addr1 (neon_sz_k var) >>|
            read_reg_neon true rd2 ii >>= fun (addr2, v2) ->
            write_mem_2_ops sz aexp Access.VIR addr2 v2 ii
          end >>= fun ((a, b), (c, d)) -> B.next4T (((a, b), c), d)
        else
          begin
            read_reg_neon true rd1 ii >>= fun v1 ->
            write_mem sz aexp Access.VIR addr1 v1 ii
          end >>|
          begin
            M.add addr1 (neon_sz_k var) >>|
            read_reg_neon true rd2 ii >>= fun (addr2, v2) ->
            write_mem sz aexp Access.VIR addr2 v2 ii
          end >>= B.next2T

      let movi_v r k shift ii =
        let open AArch64Base in
        let sz = neon_sz r and
        esize = neon_esize r in
        begin match esize, shift with
        | 8, S_NOEXT | 16, S_NOEXT | 32, S_NOEXT | 64, S_NOEXT | 128, S_NOEXT ->
          M.unitT (V.intToV k)
        | 8, S_LSL(0 as amount)
        | 16, S_LSL(0|8 as amount)
        | 32, S_LSL(0|8|16|24 as amount)
        | 32, S_MSL(8|16 as amount) ->
          M.op1 (Op.LeftShift amount) (V.intToV k)
        | _, S_LSL(n) ->
          Warn.fatal
            "illegal shift immediate %d in %d-bit instruction movi"
            n
            esize
        | _, s ->
          Warn.fatal
            "illegal shift operand %s in %d-bit instruction movi"
            (pp_barrel_shift "," s pp_imm)
            esize
        end
          >>= (fun v ->  write_reg_neon_rep sz r v ii)


      let movi_s var r k ii =
        let open AArch64Base in
        begin match var with
        | VSIMD64 ->
          M.unitT (V.intToV k)
        | _ ->
          Warn.fatal
          "illegal scalar register size in instruction movi"
        end
          >>= (fun v -> write_reg_neon_sz (tr_simd_variant var) r v ii)


      let simd_op op sz r1 r2 r3 ii =
        read_reg_neon false r3 ii >>|
        read_reg_neon false r2 ii >>=
        begin match op with
        | AArch64.ADD -> fun (v1,v2) -> M.add v1 v2
        | AArch64.EOR -> fun (v1,v2) -> M.op Op.Xor v1 v2
        | _ -> Warn.fatal "unsupported Neon operations"
        end >>=
        fun v -> write_reg_neon_sz sz r1 v ii

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
              (AArch64.dump_instruction (I_MOVZ (sz, rd, k, os)))
        end
        >>= fun v -> write_reg_dest rd v ii

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
        >>= fun v -> write_reg_dest rd v ii

      let csel_op op v =
        let open AArch64Base in
        match op with
        | Cpy -> M.unitT v
        | Inc -> M.op Op.Add v V.one
        | Neg -> M.op Op.Sub V.zero v
        | Inv -> M.op1 Op.Inv v

      let load_elem sz i r addr ii =
        let access_size = AArch64.simd_mem_access_size [r] in
        do_read_mem_ret access_size AArch64.N aexp Access.VIR addr ii >>= fun v ->
        write_reg_neon_elem sz r i v ii

      let load_elem_rep sz r addr ii =
        let access_size = AArch64.simd_mem_access_size [r] in
        do_read_mem_ret access_size AArch64.N aexp Access.VIR addr ii >>= fun v ->
        write_reg_neon_rep sz r v ii

      let store_elem i r addr ii =
        let access_size = AArch64.simd_mem_access_size [r] in
        read_reg_neon_elem true r i ii >>= fun v ->
        write_mem access_size aexp Access.VIR addr v ii

     (* Single structure memory access *)
      let mem_ss memop addr rs ii =
        let op r o = M.add o addr >>= fun addr -> memop r addr ii in
        let os = List.mapi (fun i r -> V.intToV (i * neon_esize r / 8)) rs in
        List.fold_right (>>::) (List.map2 op rs os) (M.unitT [()])

      let rec reduce_ord l =
        match l with
        | [] -> M.unitT ()
        | h::t -> h >>= fun () -> reduce_ord t

      let neon_memops memop addr idx rs ii =
        let calc_offset i r =
          (V.intToV ((idx*(List.length rs)+i) * neon_esize r / 8)) in
        let op r o = M.add o addr >>= fun addr -> memop idx r addr ii in
        List.map2 op rs (List.mapi calc_offset rs)

      let load_m addr rlist ii =
        let op i =
          let ops = neon_memops (load_elem MachSize.S128) addr i rlist ii in
          reduce_ord ops in
        let ops = List.map op (Misc.interval 0 (neon_nelem (List.hd rlist))) in
        reduce_ord ops

      let store_m addr rlist ii =
        let op i =
          let ops = neon_memops store_elem addr i rlist ii in
          List.fold_right (>>::) ops (M.unitT [()]) in
        let ops = List.map op (Misc.interval 0 (neon_nelem (List.hd rlist))) in
        List.fold_right (>>::) ops (M.unitT [[()]])

      (* Data cache operations *)
      let dc_loc op a ii =
        let mk_act loc = Act.DC (op,Some loc) in
        let loc = A.Location_global a in
        M.mk_singleton_es (mk_act loc) ii

      let do_dc op rd ii =
        if AArch64Base.DC.sw op then
          M.mk_singleton_es (Act.DC (op, None)) ii >>= B.next1T
        else begin
            (* TODO: The size for DC should be a cache line *)
            let mop _ac a = dc_loc op a ii in
            let dir = match op.AArch64Base.DC.funct with
              | AArch64Base.DC.I -> Dir.W
              | _ -> Dir.R in
            lift_memop rd dir false
              (fun ac ma _mv -> (* value fake here *)
                if Access.is_physical ac then
                  M.bind_ctrldata ma (mop ac)
                else
                  ma >>= mop ac)
              (to_perms "r" MachSize.Word)
              (read_reg_ord rd ii) mzero AArch64.N ii
          end

      let do_ic op rd ii =
        if AArch64Base.IC.all op then (* IC IALLU *)
          M.mk_singleton_es (Act.IC (op, None)) ii >>= B.next1T
        else
        begin (* IC IVAU *)
          read_reg_ord rd ii
          >>= fun a ->
            let loc = A.Location_global a in
            let act = Act.IC (op,Some loc) in
            M.mk_singleton_es act ii
          >>= B.next1T
        end

(*********************)
(* Instruction fetch *)
(*********************)

      let make_label_value proc lbl_str =
        A.V.cstToV (Constant.Label (proc, lbl_str))

      let read_loc_instr a ii =
        M.read_loc false (mk_fetch AArch64.N) a ii

(*********************)
(* Branches *)
(*********************)

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
              if Label.Full.Set.is_empty lbls then
                Warn.fatal
                  "Could find no potential target for indirect branch %s \
                   (potential targets are statically known labels)" (AArch64.dump_instruction i)
              else
                B.indirectBranchT v lbls bds
        | _ -> Warn.fatal
            "illegal argument for the indirect branch instruction %s \
            (must be a label)" (AArch64.dump_instruction i)

(********************)
(* Main entry point *)
(********************)
      (*
         Additonal type checking, control over discarded values.
         Namely, discarded value cannot be of type B.t, this would
         mean discarding a control flow result and replacing it
         systematically by B.Next. That way, some exit to end
         of code instructions would be ignored. See issue #287.
       *)

      let (!!!!) (m1:(unit list list * unit) M.t) =
        m1 >>= M.ignore >>= B.next1T
      let (!!!) (m1:(unit list * unit) M.t) =
        m1 >>= M.ignore >>= B.next1T
      let (!!) (m1:(unit * unit) M.t) = m1 >>= B.next2T
      let (!) (m1:unit M.t) = m1 >>= B.next1T
      let nextSet = B.nextSetT
      (* And now, just forget about >>! *)
      let (>>!) (_:unit) (_:unit) = ()

      let do_build_semantics test inst ii =
        let open AArch64Base in
        match inst with
        | I_NOP ->(* Instructions nop and branch below do not generate events, use a placeholder *)
           !(M.mk_singleton_es (Act.NoAction) ii)
        (* Branches *)
        | I_B l ->
           M.mk_singleton_es (Act.NoAction) ii
           >>= fun () -> M.unitT (B.Jump (tgt2tgt ii l,[]))
        | I_BC(c,l)->
           read_reg_ord NZCV ii
           >>= tr_cond c
           >>= fun v -> commit_bcc ii
           >>= fun () -> M.unitT (B.CondJump (v,tgt2tgt  ii l))
        | I_BL l ->
           let v_ret = V.intToV (ii.A.addr + 4) in
           write_reg AArch64Base.linkreg v_ret ii
           >>= fun () -> M.unitT (B.Jump (tgt2tgt ii l,[AArch64Base.linkreg,v_ret]))

        | I_BR r as i ->
            read_reg_ord r ii >>= do_indirect_jump test [] i

        | I_BLR r as i ->
           let v_ret = V.intToV (ii.A.addr + 4) in
           write_reg AArch64Base.linkreg v_ret ii
           >>= fun () -> read_reg_ord r ii
           >>= do_indirect_jump test [AArch64Base.linkreg,v_ret] i

        | I_RET ro as i ->
            let r = match ro with
            | None -> AArch64Base.linkreg
            | Some r -> r in
            read_reg_ord r ii
            >>= do_indirect_jump test [] i

        | I_ERET ->
           let eret_to_addr = function
             | M.A.V.Val(Constant.Label (_, l)) -> B.faultRetT l
             | _ ->
                Warn.fatal "Cannot determine ERET target" in
           let commit_eret ii =
             M.mk_singleton_es (Act.Commit (Act.ExcReturn,None)) ii in
           commit_eret ii >>=
             fun () -> read_reg_ord AArch64.elr_el1 ii >>=
             eret_to_addr

        | I_CBZ(_,r,l) ->
            (read_reg_ord r ii)
              >>= is_zero
              >>= fun v -> commit_bcc ii
              >>= fun () -> M.unitT (B.CondJump (v,tgt2tgt ii l))

        | I_CBNZ(_,r,l) ->
            (read_reg_ord r ii)
              >>= is_not_zero
              >>= fun v -> commit_bcc ii
              >>= fun () -> M.unitT (B.CondJump (v,tgt2tgt ii l))

        | I_TBZ(_,r,k,l) ->
            (read_reg_ord r ii)
              >>= M.op1 (Op.ReadBit k)
              >>= is_zero
              >>= fun v -> commit_bcc ii
              >>= fun () -> M.unitT (B.CondJump (v,tgt2tgt ii l))
        | I_TBNZ(_,r,k,l) ->
            (read_reg_ord r ii)
              >>= M.op1 (Op.ReadBit k)
              >>= is_not_zero
              >>= fun v -> commit_bcc ii
              >>= fun () -> M.unitT (B.CondJump (v,tgt2tgt ii l))

                      (* Load and Store *)
        | I_LDR(var,rd,rs,kr,s) ->
            let sz = tr_variant var in
            ldr sz rd rs kr s ii
        | I_LDRBH (bh, rd, rs, kr, s) ->
            let sz = bh_to_sz bh in
            ldr sz rd rs kr s ii
        | I_LDR_L(var,rd,BranchTarget.Lbl lbl) ->
            ldr_lit var rd lbl ii
        | I_LDRS (v, bh, rd, rs) ->
            let sz = bh_to_sz bh in
            ldrs sz v rd rs (AArch64.K 0) S_NOEXT ii
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

        | I_STR(var,rs,rd,kr,os) ->
            str (tr_variant var) rs rd kr os ii

        | I_STR_P(var, rs, rd, k) ->
            str_post (tr_variant var) rs rd k ii

        | I_STRBH(bh,rs,rd,kr, s) ->
            str (bh_to_sz bh) rs rd kr s ii

        | I_STLR(var,rs,rd) ->
            stlr (tr_variant var) rs rd ii

        | I_STLRBH(bh,rs,rd) ->
            stlr (bh_to_sz bh) rs rd ii

        | I_STZG(rt,rn,kr) ->
            check_memtag "STZG" ;
            !!(begin
              (read_reg_data MachSize.Quad rt ii >>= tag_extract) >>|
              get_ea rn kr AArch64.S_NOEXT ii
            end >>= fun (v,a) ->
              (M.op1 Op.TagLoc a >>| loc_extract a) >>= fun (atag,loc) ->
                (do_write_tag atag v ii
                 >>| do_write_mem quad AArch64.N aexp Access.VIR loc V.zero ii))

        | I_STG(rt,rn,kr) ->
            check_memtag "STG" ;
            !(begin
              (read_reg_data quad rt ii >>= tag_extract) >>|
              get_ea rn kr S_NOEXT ii
            end >>= fun (v,a) ->
              M.op1 Op.TagLoc a  >>= fun a ->
                do_write_tag a v ii)

        | I_LDG (rt,rn,kr) ->
            check_memtag "LDG" ;
            !(get_ea rn kr S_NOEXT ii  >>=
            fun a -> M.op1 Op.TagLoc a >>=
              fun atag -> do_read_tag atag ii
                  >>= fun tag ->
                    M.op Op.SetTag a tag >>= fun v ->
                      write_reg rt v ii)

        | I_STXR(var,t,rr,rs,rd) ->
            stxr (tr_variant var) t rr rs rd ii
        | I_STXRBH(bh,t,rr,rs,rd) ->
            stxr (bh_to_sz bh) t rr rs rd ii

        (* Neon operations *)
        | I_MOV_VE(r1,i1,r2,i2) ->
            !(read_reg_neon_elem false r2 i2 ii >>=
              fun v -> write_reg_neon_elem MachSize.S128 r1 i1 v ii)
        | I_MOV_FG(r1,i,var,r2) ->
            !(let sz = tr_variant var  in
              read_reg_ord_sz sz r2 ii >>=
              fun v -> write_reg_neon_elem MachSize.S128 r1 i v ii)
        | I_MOV_TG(_,r1,r2,i) ->
            !(read_reg_neon_elem false r2 i ii >>=
              fun v -> write_reg r1 v ii)
        | I_MOV_V(r1,r2) ->
            !(read_reg_neon false r2 ii >>=
              fun v -> write_reg_neon r1 v ii)
        | I_MOV_S(var,r1,r2,i) ->
            !(let sz = tr_simd_variant var in
              read_reg_neon_elem false r2 i ii >>=
              fun v -> write_reg_neon_sz sz r1 v ii)
        | I_MOVI_V(r,k,shift) ->
            !(movi_v r k shift ii)
        | I_MOVI_S(var,r,k) ->
            !(movi_s var r k ii)
        | I_EOR_SIMD(r1,r2,r3) ->
            let size = neon_sz r1 in
            !(simd_op EOR size r1 r2 r3 ii)
        | I_ADD_SIMD(r1,r2,r3) ->
            let size = neon_sz r1 in
            !(simd_op ADD size r1 r2 r3 ii)
        | I_ADD_SIMD_S(r1,r2,r3) ->
            !(simd_op ADD MachSize.Quad r1 r2 r3 ii)

        (* Neon loads and stores *)
        | I_LD1(r1,i,rA,kr) ->
            !!(read_reg_ord rA ii >>= fun addr ->
            (load_elem MachSize.S128 i r1 addr ii >>|
            post_kr rA addr kr ii))
        | I_LD2(rs,i,rA,kr)
        | I_LD3(rs,i,rA,kr)
        | I_LD4(rs,i,rA,kr) ->
            !!!(read_reg_ord rA ii >>= fun addr ->
            (mem_ss (load_elem MachSize.S128 i) addr rs ii >>|
            post_kr rA addr kr ii))
        | I_LD1R(r1,rA,kr) ->
            !!(read_reg_ord rA ii >>= fun addr ->
            (load_elem_rep MachSize.S128 r1 addr ii >>|
            post_kr rA addr kr ii))
        | I_LD2R(rs,rA,kr)
        | I_LD3R(rs,rA,kr)
        | I_LD4R(rs,rA,kr) ->
            !!!(read_reg_ord rA ii >>= fun addr ->
            (mem_ss (load_elem_rep MachSize.S128) addr rs ii >>|
            post_kr rA addr kr ii))
        | I_LD1M([_] as rs,rA,kr)
        | I_LD2M(rs,rA,kr)
        | I_LD3M(rs,rA,kr)
        | I_LD4M(rs,rA,kr) ->
            !!(read_reg_ord rA ii >>= fun addr ->
            (load_m addr rs ii >>|
            post_kr rA addr kr ii))
        | I_ST1(r1,i,rA,kr) ->
            !!(read_reg_ord rA ii >>= fun addr ->
            (store_elem i r1 addr ii >>|
            post_kr rA addr kr ii))
        | I_ST2(rs,i,rA,kr)
        | I_ST3(rs,i,rA,kr)
        | I_ST4(rs,i,rA,kr) ->
            !!!(read_reg_ord rA ii >>= fun addr ->
            (mem_ss (store_elem i) addr rs ii >>|
            post_kr rA addr kr ii))
        | I_ST1M([_] as rs,rA,kr)
        | I_ST2M(rs,rA,kr)
        | I_ST3M(rs,rA,kr)
        | I_ST4M(rs,rA,kr) ->
            !!!!(read_reg_ord rA ii >>= fun addr ->
            (store_m addr rs ii >>|
            post_kr rA addr kr ii))

        | I_LDR_SIMD(var,r1,rA,kr,s) ->
            let access_size = tr_simd_variant var in
            get_ea rA kr s ii >>= fun addr ->
            simd_ldr access_size addr r1 ii >>= B.next1T
        | I_LDR_P_SIMD(var,r1,rA,k) ->
            let access_size = tr_simd_variant var in
            read_reg_ord rA ii >>= fun addr ->
            simd_ldr access_size addr r1 ii >>|
            post_kr rA addr (K k) ii >>= B.next2T
        | I_LDUR_SIMD(var,r1,rA,k) ->
            let access_size = tr_simd_variant var and
            k = K (match k with Some k -> k | None -> 0) in
            (get_ea rA k S_NOEXT ii >>= fun addr ->
            simd_ldr access_size addr r1 ii) >>= B.next1T
        | I_STR_SIMD(var,r1,rA,kr,s) ->
            let access_size = tr_simd_variant var in
            simd_str access_size rA r1 kr s ii
        | I_STR_P_SIMD(var,r1,rA,k) ->
            let access_size = tr_simd_variant var in
            simd_str_p access_size rA r1 (K k) ii
        | I_STUR_SIMD(var,r1,rA,k) ->
            let access_size = tr_simd_variant var and
            k = K (match k with Some k -> k | None -> 0) in
            simd_str access_size rA r1 k S_NOEXT ii
        | I_LDP_SIMD(_,var,r1,r2,r3,k) ->
            get_ea r3 k S_NOEXT ii >>= fun addr ->
            simd_ldp var addr r1 r2 ii
        | I_LDP_P_SIMD(_,var,r1,r2,r3,k) ->
            read_reg_ord r3 ii >>= fun addr ->
            (simd_ldp var addr r1 r2 ii >>|
            post_kr r3 addr (K k) ii) >>=
            fun (b,()) -> M.unitT b
        | I_STP_SIMD(_,var,r1,r2,r3,k) ->
            get_ea r3 k S_NOEXT ii >>= fun addr ->
            simd_stp var addr r1 r2 ii
        | I_STP_P_SIMD(_,var,r1,r2,r3,k) ->
            read_reg_ord r3 ii >>= fun addr ->
            simd_stp var addr r1 r2 ii >>|
            post_kr r3 addr (K k) ii >>=
            fun (b,()) -> M.unitT b

        (* Morello instructions *)
        | I_ALIGND(rd,rn,kr) ->
            check_morello inst ;
            !((read_reg_ord_sz MachSize.S128 rn ii >>= match kr with
            | K k -> fun v -> M.op Op.Alignd v (V.intToV k)
            | _ -> assert false
            ) >>= fun v -> write_reg_sz MachSize.S128 rd v ii)
        | I_ALIGNU(rd,rn,kr) ->
            check_morello inst ;
            !((read_reg_ord_sz MachSize.S128 rn ii >>= match kr with
            | K k -> fun v -> M.op Op.Alignu v (V.intToV k)
            | _ -> assert false
            ) >>= fun v -> write_reg_sz MachSize.S128 rd v ii)
        | I_BUILD(rd,rn,rm) ->
            check_morello inst ;
            !(begin
              read_reg_ord_sz MachSize.S128 rn ii >>|
              read_reg_ord_sz MachSize.S128 rm ii
            end >>= fun (a,b) ->
            M.op Op.Build a b >>= fun v ->
            write_reg_sz MachSize.S128 rd v ii)
        | I_CHKEQ(rn,rm) ->
            check_morello inst ;
            !(begin
              read_reg_ord_sz MachSize.S128 rn ii >>|
              read_reg_ord_sz MachSize.S128 rm ii
            end >>= fun (v1,v2) ->
            M.op Op.Eq v1 v2 >>= fun v -> M.op1 (Op.LeftShift 2) v >>= fun v ->
            write_reg NZCV v ii)
        | I_CHKSLD(rn) ->
            check_morello inst ;
            !(read_reg_ord_sz MachSize.S128 rn ii >>= fun v ->
            M.op1 Op.CheckSealed v >>= fun v -> write_reg NZCV v ii)
        | I_CHKTGD(rn) ->
            check_morello inst ;
            !(read_reg_ord_sz MachSize.S128 rn ii >>= fun v ->
              M.op1 Op.CapaGetTag v >>= fun v -> M.op1 (Op.LeftShift 1) v
              >>= fun v -> write_reg NZCV v ii)
        | I_CLRTAG(rd,rn) ->
            check_morello inst ;
            !(read_reg_ord_sz MachSize.S128 rn ii >>= fun (v) ->
            M.op Op.CapaSetTag v V.zero >>= fun v ->
            write_reg_sz MachSize.S128 rd v ii)
        | I_CPYTYPE(rd,rn,rm) ->
            check_morello inst ;
            !(begin
              read_reg_ord_sz MachSize.S128 rn ii >>|
              read_reg_ord_sz MachSize.S128 rm ii
            end >>= fun (v1,v2) -> M.op Op.CpyType v1 v2 >>= fun v ->
            write_reg_sz MachSize.S128 rd v ii)
        | I_CPYVALUE(rd,rn,rm) ->
            check_morello inst ;
            !(begin
              read_reg_ord_sz MachSize.S128 rn ii >>|
              read_reg_ord_sz MachSize.S128 rm ii
            end >>= fun (v1,v2) -> M.op Op.SetValue v1 v2 >>= fun v ->
            write_reg_sz MachSize.S128 rd v ii)
        | I_CSEAL(rd,rn,rm) ->
            check_morello inst ;
            !(begin
              read_reg_ord_sz MachSize.S128 rn ii >>|
              read_reg_ord_sz MachSize.S128 rm ii
            end >>= fun (v1,v2) ->
            M.op Op.CSeal v1 v2 >>= fun v ->
            write_reg_sz MachSize.S128 rd v ii >>= fun _ ->
            (* TODO: PSTATE overflow flag would need to be conditionally set *)
            write_reg NZCV M.A.V.zero ii)
        | I_GC(op,rd,rn) ->
            check_morello inst ;
            !(read_reg_ord_sz MachSize.S128 rn ii >>= begin fun c -> match op with
            | CFHI -> M.op1 (Op.LogicalRightShift 64) c
            | GCFLGS -> M.op1 (Op.AndK "0xff00000000000000") c
            | GCPERM -> M.op1 (Op.LogicalRightShift 110) c
            | GCSEAL -> M.op1 (Op.LeftShift 18) c >>= fun v ->
                M.op1 (Op.LogicalRightShift 113) v >>= fun v -> is_not_zero v
            | GCTAG -> M.op1 Op.CapaGetTag c
            | GCTYPE -> M.op1 (Op.LeftShift 18) c >>= fun v ->
                M.op1 (Op.LogicalRightShift 113) v
            | GCVALUE -> M.op1 (Op.Mask MachSize.Quad) c
            end >>= fun v -> write_reg_sz MachSize.Quad rd v ii)
        | I_SC(op,rd,rn,rm) ->
            check_morello inst ;
            !(begin
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
            write_reg_sz MachSize.S128 rd v ii)
        | I_SEAL(rd,rn,rm) ->
            check_morello inst ;
            !(begin
              read_reg_ord_sz MachSize.S128 rn ii >>|
              read_reg_ord_sz MachSize.S128 rm ii
            end >>= fun (a,b) ->
            M.op Op.Seal a b >>= fun v ->
            write_reg_sz MachSize.S128 rd v ii)
        | I_STCT(rt,rn) ->
            check_morello inst ;
            (* NB: only 1 access implemented out of the 4 *)
            lift_morello
              (fun _ac ma mv ->
                do_insert_commit
                  (ma >>| mv)
                  (fun (a,v) -> !(do_write_morello_tag a v ii))
                  ii)
              (to_perms "tw" MachSize.S128)
              (read_reg_ord rn ii)
              (read_reg_data MachSize.Quad rt ii)
              Dir.W AArch64.N ii
        | I_LDCT(rt,rn) ->
            check_morello inst ;
            (* NB: only 1 access implemented out of the 4 *)
            lift_morello
              (fun _ac ma _mv ->
                M.delay_kont "LDCT" ma
                  (fun _a ma ->
                    do_insert_commit ma
                      (fun a -> (* Why check permissions again ? *)
                        M.op (Op.CheckPerms "tr_c") a M.A.V.zero >>= fun v ->
                        M.choiceT v
                          (do_read_morello_tag a ii)
                          mzero
                        >>= fun tag -> !(write_reg_sz quad rt tag ii))
                      ii))
              (to_perms "r" MachSize.S128)
              (read_reg_ord rn ii)
              mzero
              Dir.R AArch64.N
              ii
        | I_UNSEAL(rd,rn,rm) ->
            check_morello inst ;
            !(begin
              read_reg_ord_sz MachSize.S128 rn ii >>|
              read_reg_ord_sz MachSize.S128 rm ii
            end >>= fun (a,b) ->
            M.op Op.Unseal a b >>= fun v ->
            write_reg_sz MachSize.S128 rd v ii)

        (* Operations *)
        | I_MOV(var,r,K k) ->
            mask32 var
               (fun k ->
                 write_reg_dest r k ii
                 >>= nextSet r)
               (V.intToV k)
        | I_MOV(var,r1,RV (_,r2)) ->
            let sz = tr_variant var in
            read_reg_ord_sz sz r2 ii
            >>= fun v -> write_reg_dest r1 v ii
            >>= nextSet r1
        | I_MOVZ(var,rd,k,os) ->
           movz var rd k os ii >>= nextSet rd
        | I_MOVK(var,rd,k,os) ->
            movk var rd k os ii >>= nextSet rd
        | I_ADR (r,tgt) ->
           let v =
             let open BranchTarget in
             match tgt with
             | Lbl lbl -> ii.A.addr2v lbl
             | Offset o -> V.intToV (ii.A.addr + o) in
           write_reg_dest r v ii >>= nextSet r

        | I_SXTW(rd,rs) ->
            read_reg_ord_sz MachSize.Word rs ii
            >>=  sxtw_op
            >>= fun v -> write_reg_dest rd v ii
            >>= nextSet rd

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
            (begin match kr with
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
            begin
              let with_correct_size f arg = f arg >>= mask32 ty M.unitT in
              let do_write_reg v = write_reg_dest rd v ii in
              let write_reg_no_flags f arg =
                let without_flags v = M.unitT (v, None) in
                with_correct_size f arg >>= do_write_reg >>= without_flags
              in
              match ty with
              | V128 ->
                  check_morello inst;
                  write_reg_no_flags
                    (match op with
                    | ADD -> fun (v1, v2) -> M.op Op.CapaAdd v1 v2
                    | SUB -> fun (v1, v2) -> M.op Op.CapaSub v1 v2
                    | SUBS -> fun (v1, v2) -> M.op Op.CapaSubs v1 v2
                    | _ ->
                        Warn.fatal
                          "Operation '%s' is not available in morello mode"
                          (AArch64.pp_op op))
              | _ -> (
                  let get_res =
                    match op with
                    | ADD | ADDS -> fun (v1, v2) -> M.add v1 v2
                    | EOR -> fun (v1, v2) -> M.op Op.Xor v1 v2
                    | ORR -> fun (v1, v2) -> M.op Op.Or v1 v2
                    | ORN -> fun (v1, v2) -> M.op1 Op.Inv v2
                      >>= fun v2 -> M.op Op.Or v1 v2
                    | SUB | SUBS -> fun (v1, v2) -> M.op Op.Sub v1 v2
                    | AND | ANDS -> fun (v1, v2) -> M.op Op.And v1 v2
                    | ASR -> fun (v1, v2) -> M.op Op.ASR v1 v2
                    | LSR -> fun (v1, v2) -> M.op Op.Lsr v1 v2
                    | LSL -> fun (v1, v2) -> M.op Op.ShiftLeft v1 v2
                    | BIC | BICS -> fun (v1, v2) -> M.op Op.AndNot2 v1 v2
                  in
                  match op_set_flags op ty with
                  | None -> write_reg_no_flags get_res
                  | Some get_flags ->
                      let do_write_flags flags = write_reg_dest NZCV flags ii in
                      let return_flags flags = M.unitT (Some flags) in
                      let compute_and_write_flags res v1 v2 =
                        get_flags res v1 v2 >>= do_write_flags >>= return_flags
                      in
                      fun (v1, v2) ->
                        with_correct_size get_res (v1, v2) >>= fun res ->
                        do_write_reg res >>| compute_and_write_flags res v1 v2)
            end)
            >>= fun (v,wo) ->
            begin match wo with
            | None -> B.nextSetT rd v
            | Some w ->
                M.unitT (B.Next [rd,v; NZCV,w])
            end
      (* Barrier *)
        | I_FENCE b ->
            !(create_barrier b ii)
              (* Conditional selection *)
        | I_CSEL (var,r1,r2,r3,c,op) ->
            let sz = tr_variant var in
            let mask = match op with
            | Cpy -> fun m -> m
            | Inc|Inv|Neg -> mask32 var in
            !(if not (C.variant Variant.NotWeakPredicated) then
              read_reg_ord NZCV ii >>= tr_cond c >>*= fun v ->
                M.choiceT v
                  (read_reg_data sz r2 ii >>= fun v -> write_reg r1 v ii)
                  (read_reg_data sz r3 ii >>=
                     csel_op op >>= mask (fun v ->  write_reg r1 v ii))
            else
              begin
                (read_reg_ord NZCV ii >>= tr_cond c) >>|  read_reg_data sz r2 ii >>| read_reg_data sz r3 ii
              end >>= fun ((v,v2),v3) ->
              M.condPredT v
                (M.unitT ())
                (write_reg r1 v2 ii)
                (csel_op op v3 >>= mask (fun v ->  write_reg r1 v ii)))

        (* Swap *)
        | I_SWP (v,rmw,r1,r2,r3) -> swp (tr_variant v) rmw r1 r2 r3 ii
        | I_SWPBH (v,rmw,r1,r2,r3) -> swp (bh_to_sz v) rmw r1 r2 r3 ii
(* Compare & Swap *)
        | I_CAS (v,rmw,rs,rt,rn) ->
            (* TODO: unify cas functions *)
            let cas = if morello then cas_morello else cas in
            cas (tr_variant v) rmw rs rt rn ii
        | I_CASBH (v,rmw,rs,rt,rn) ->
            (* TODO: unify cas functions *)
            let cas = if morello then cas_morello else cas in
            cas (bh_to_sz v) rmw rs rt rn ii
(* Fetch and Op *)
        | I_STOP (op,v,w,rs,rn) ->
            ldop op (tr_variant v) (w_to_rmw w) rs ZR rn ii
        | I_LDOP (op,v,rmw,rs,rt,rn) ->
            ldop op (tr_variant v) rmw rs rt rn ii
        | I_STOPBH (op,v,w,rs,rn) ->
            ldop op (bh_to_sz v) (w_to_rmw w) rs ZR rn ii
        | I_LDOPBH (op,v,rmw,rs,rt,rn) ->
            ldop op (bh_to_sz v) rmw rs rt rn ii
(* Page tables and TLBs *)
        | I_TLBI (op, rd) ->
            !(read_reg_ord rd ii >>= fun a -> do_inv op a ii)
(* Data cache instructions *)
        | I_DC (op,rd) -> do_dc op rd ii
(* Instruction-cache maintenance instruction *)
        | I_IC (op,rd) -> do_ic op rd ii
(* Load/Store pairs *)
        | I_LDP (TT,v,r1,r2,r3,kr) ->
            ldp (tr_variant v) r1 r2 r3 kr ii
        | I_LDPSW (r1,r2,r3,kr) ->
            ldpsw r1 r2 r3 kr ii
        | I_STP (TT,v,r1,r2,r3,kr) ->
            stp (tr_variant v) r1 r2 r3 kr ii
        | I_LDXP (v,t,r1,r2,r3) ->
            ldxp (tr_variant v) t r1 r2 r3 ii
        | I_STXP (v,t,r1,r2,r3,r4) ->
            stxp (tr_variant v) t r1 r2 r3 r4 ii
(*
 * Read/Write system registers.
 * Notice thar NZCV is special:
 * Our NZCV register is a direct representation of
 * PSTATE.<N,Z,C,V>, while SYS_NZCV is here only
 * as an argument to the MRS and MSR instructions.
 *)
        | I_MSR (sreg,xt) ->
           read_reg_ord_sz MachSize.Quad xt ii
           >>=
             begin
               match sreg with
               | SYS_NZCV ->
                  fun v -> M.op1 (Op.LogicalRightShift 28) v
                  >>= M.op1 (Op.AndK "0b1111")
                  >>= fun v -> write_reg_dest NZCV v ii
                  >>= nextSet NZCV
               | _ ->
                  fun v -> write_reg_dest (SysReg sreg) v ii
                  >>= nextSet (SysReg sreg)
             end
        | I_MRS (xt,sreg) ->
          begin
            match sreg with
            | SYS_NZCV ->
               read_reg_ord NZCV ii
               >>= M.op1 (Op.LeftShift 28)
            | _ ->
               read_reg_ord_sz MachSize.Quad (SysReg sreg) ii
          end
          >>= fun v -> write_reg_dest xt v ii
          >>= nextSet (SysReg sreg)
(*  Cannot handle *)
        | (I_RBIT _|I_LDP _|I_STP _|I_LDR_L _
        (* | I_BL _|I_BLR _|I_BR _|I_RET _ *)
        | I_LD1M _|I_ST1M _) as i ->
            Warn.fatal "illegal instruction: %s" (AArch64.dump_instruction i)

(* Compute a safe set of instructions that can
 * overwrite another. By convention, those are
 * instructions pointed to by "exported" labels.
 *)
      let get_overwriting_instrs lbls test =
        let init_instrs =
          AArch64.state_fold
            (fun _ v k ->
              match v with
              | V.Val (Constant.Instruction i) -> i::k
              | _ -> k)
            test.Test_herd.init_state []
        and  code_instrs =
          List.map
            (fun (_,i) -> i)
            (AArch64.from_labels lbls test.Test_herd.nice_prog) in
        init_instrs@code_instrs


(* Test all possible instructions, when appropriate *)
      let check_self test ii =
        let module InstrSet = AArch64.V.Cst.Instr.Set in
        let inst = ii.A.inst in
        if not (AArch64Base.is_overwritable inst) then
          do_build_semantics test inst ii
        else
          let lbls = get_exported_labels test in
          let is_exported =
            Label.Set.exists
              (fun lbl ->
                Label.Full.Set.exists
                  (fun (_,lbl0) -> Misc.string_eq lbl lbl0)
                  lbls)
              ii.A.labels in
          if is_exported then
            match Label.norm ii.A.labels with
            | None -> assert false
            | Some hd ->
                let insts =
                  InstrSet.of_list
                    (get_overwriting_instrs lbls test) in
                let insts =
                  InstrSet.add inst
                    (InstrSet.filter AArch64.can_overwrite insts) in
                if false then
                  Printf.eprintf "%s: {%s}\n%!"
                    (Label.pp hd)
                    (String.concat ","
                       (List.map AArch64.V.Cst.Instr.pp
                          (InstrSet.elements insts))) ;
                (* Shadow default control sequencing operator *)
                let(>>*=) = M.bind_control_set_data_input_first in
                let a_v = make_label_value ii.A.fetch_proc hd in
                let a = (* Normalised address of instruction *)
                  A.Location_global a_v in
                read_loc_instr a ii
                  >>= fun actual_val ->
                    InstrSet.fold
                      (fun inst k ->
                        M.op Op.Eq actual_val (V.instructionToV inst) >>==
                        fun cond -> M.choiceT cond
                            (commit_pred ii >>*=
                             fun () -> do_build_semantics test inst ii)
                            k)
                      insts
                      begin
(* Anything else than a legit instruction is a failure *)
                        let (>>!) = M.(>>!) in
                        let m_fault =
                          mk_fault
                            a_v Dir.R AArch64.N ii
                            (Some FaultType.AArch64.IllegalInstruction)
                            (Some "Invalid") in
                        commit_pred ii
                          >>*= fun () -> m_fault >>| set_elr_el1 ii
                            >>! B.Fault Dir.R
                      end
          else do_build_semantics test inst ii

      let build_semantics test ii =
        M.addT (A.next_po_index ii.A.program_order_index)
          begin
            if self then check_self test ii
            else do_build_semantics test ii.A.inst ii
          end

      let spurious_setaf v = test_and_set_af_succeeds v E.IdSpurious

    end

  end
