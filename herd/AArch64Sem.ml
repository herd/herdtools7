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


(* Basic read, from register *)
      let mk_read sz an loc v = Act.Access (Dir.R, loc, v, an, sz)
      let mk_read_std = mk_read MachSize.Quad AArch64.N
      let mk_fault a ii msg =
        M.mk_singleton_es (Act.Fault (ii,A.Location_global a,msg)) ii

      let read_loc v is_data = M.read_loc is_data (mk_read v AArch64.N)

      let read_reg is_data r ii = match r with
      | AArch64.ZR -> M.unitT V.zero
      | _ ->
          M.read_loc is_data (mk_read MachSize.Quad AArch64.N) (A.Location_reg (ii.A.proc,r)) ii

      let read_reg_sz sz is_data r ii = match sz with
      | MachSize.Quad -> read_reg is_data r ii
      | MachSize.Word|MachSize.Short|MachSize.Byte ->
          read_reg is_data r ii >>= fun v -> M.op1 (Op.Mask sz) v

      let read_reg_ord = read_reg_sz MachSize.Quad false
      let read_reg_ord_sz sz = read_reg_sz sz false
      let read_reg_data sz = read_reg_sz sz true
      let read_reg_tag is_data =  read_reg is_data

(* Basic write, to register  *)
      let mk_write sz an loc v = Act.Access (Dir.W, loc, v, an, sz)
      let write_loc sz an loc v ii = M.mk_singleton_es (mk_write sz an loc v) ii
      let write_reg r v ii = match r with
      | AArch64.ZR -> M.unitT ()
      | _ ->
          write_loc MachSize.Quad AArch64.N (A.Location_reg (ii.A.proc,r)) v ii

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

(******************)
(* Memory Tagging *)
(******************)

(* Decompose tagged location *)
      let tag_extract a = M.op1 Op.TagExtract a
      let loc_extract a = M.op1 Op.LocExtract a


(*  Low level tag access *)
      let do_read_tag a ii =
        M.read_loc false (fun loc v -> Act.TagAccess (Dir.R,loc,v))
          (A.Location_global a) ii
      and do_write_tag a v ii =
        let loc = A.Location_global a in
        M.mk_singleton_es (Act.TagAccess (Dir.W,loc,v)) ii

(* Read tag from memory *)
      let read_tag_mem a ii =
        M.op1 Op.TagLoc a >>= fun atag ->
          M.read_loc false (fun loc v -> Act.TagAccess (Dir.R,loc,v))
            (A.Location_global atag) ii


(* For checking tags *)
      let get_both_tags a ii = tag_extract a >>| read_tag_mem a ii
      let do_check atag patag = M.op Op.Eq patag atag

(*******************)
(* Memory accesses *)
(*******************)


(*
  Implementation of accesses depends upon the appropriate variants,
  NB: for now, mixed and memtag do not combine.
 *)

(* Read *)
      let check_tags a ii m1 m2 =
        get_both_tags a ii >>= fun (atag,patag) ->
        do_check atag patag  >>= fun cond ->
        commit_pred ii >>*= fun () ->
        M.choiceT cond m1 m2

      let delayed_check_tags ma ii m1 m2 =
        let (++) = M.bind_ctrl_avoid ma in
        M.check_tags
          ma (fun a -> read_tag_mem a ii)
          (fun a tag1 -> tag_extract a  >>= fun tag2 -> M.op Op.Eq tag1 tag2)
          (commit_pred ii)  ++ fun cond ->  M.choiceT cond m1 m2

      let do_checked_read sz an rd a ii =
        check_tags a ii
          (loc_extract a >>= fun a ->
           M.read_loc false (mk_read sz an) (A.Location_global a) ii >>= fun v ->
           write_reg_sz sz rd v ii >>! B.Next)
          (mk_fault a ii (Some "MTE read") >>! B.Exit)


(* Old read_mem that returns value read *)
      let old_do_read_mem sz an a ii =
        if mixed then begin
          Mixed.read_mixed false sz (fun sz -> mk_read sz an) a ii
        end else
          M.read_loc false (mk_read sz an) (A.Location_global a) ii

      let do_read_mem sz an rd a ii =
        old_do_read_mem sz an a ii >>= fun v ->  write_reg_sz_non_mixed sz rd v ii

      let read_mem sz = do_read_mem sz AArch64.N
      let read_mem_acquire sz = do_read_mem sz AArch64.A
      let read_mem_acquire_pc sz = do_read_mem sz AArch64.Q
      let read_mem_noreturn sz = do_read_mem sz AArch64.NoRet

      let read_mem_reserve sz an rd a ii =
        (write_reg AArch64.ResAddr a ii >>| do_read_mem sz an rd a ii) >>! ()


(* Write *)
      let do_write_mem sz an a v ii =
        if mixed then begin
          Mixed.write_mixed sz (fun sz -> mk_write sz an) a v ii
        end else write_loc sz an (A.Location_global a) v ii

      let write_mem sz = do_write_mem sz AArch64.N
      let write_mem_release sz = do_write_mem sz AArch64.L
      let write_mem_amo sz = do_write_mem sz AArch64.X
      let write_mem_amo_release sz = do_write_mem sz AArch64.XL

(* Write atomic *)
      let write_mem_atomic an sz a v resa ii =
        if mixed then begin
          (M. assign a resa >>|
           Mixed.write_mixed sz (fun sz -> mk_write sz an)  a v ii) >>! ()
        end else
          let eq = [M.VC.Assign (a,M.VC.Atom resa)] in
          M.mk_singleton_es_eq
            (Act.Access (Dir.W, A.Location_global a, v,an, sz)) eq ii

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

(***********************)
(* Memory instructions *)
(***********************)

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

      let lift_memop mop ma ii =
        if memtag then
          M.delay ma >>= fun (_,ma) ->
          let  mm = mop (ma >>= fun a -> loc_extract a) in
          delayed_check_tags ma ii
            (mm  >>! B.Next)
            (let mfault = ma >>= fun a -> mk_fault a ii (Some "MTE lift_memop") in
            if C.precision then  mfault >>! B.Exit
            else (mfault >>| mm) >>! B.Next)
        else
          mop ma >>! B.Next

      let do_str sz an rs ma ii =
        lift_memop
          (fun ma ->
            (ma >>| read_reg_data sz rs ii) >>= fun (a,v) ->
            do_write_mem sz an a v ii)
          ma ii

      let ldr sz rd rs kr s ii =
        lift_memop
          (fun ma -> ma >>= fun a ->
           old_do_read_mem sz AArch64.N a ii >>= fun v ->
           write_reg_sz_non_mixed sz rd v ii )
          (get_ea rs kr s ii) ii

      (* Post-Indexed load immediate *)
      and ldr_p sz rd rs k ii =
        assert (k >= -256 && k <= 255);
        let open AArch64Base in
          (read_reg_ord rs ii)
          >>= fun a1 ->
              (read_mem sz rd a1 ii)
              >>|
              (M.add a1 (V.intToV k)
                >>= fun v -> write_reg rs v ii)
          >>! B.Next

      and str sz rs rd kr ii = do_str sz AArch64.N rs (get_ea rd kr AArch64.S_NOEXT ii) ii

      and stlr sz rs rd ii = do_str sz AArch64.L rs (read_reg_ord rd ii) ii

      and ldar sz t rd rs ii =
        let open AArch64 in
        lift_memop
          (fun ma ->
            ma >>= fun a ->
              match t with
              | XX ->
                  read_mem_reserve sz AArch64.X rd a ii
              | AA ->
                  read_mem_acquire sz rd a ii
              | AX ->
                  read_mem_reserve sz AArch64.XA rd a ii
              | AQ ->
                  read_mem_acquire_pc sz rd a ii)
          (read_reg_ord rs ii) ii

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

      and stxr sz t rr rs rd ii =
        let open AArch64Base in
        lift_memop
         (fun ma ->
           M.riscv_store_conditional
             (read_reg_ord ResAddr ii)
             (read_reg_data sz rs ii)
             ma
             (write_reg ResAddr V.zero ii)
             (fun v -> write_reg rr v ii)
             (fun ea resa v -> match t with
             | YY -> write_mem_atomic AArch64.X sz ea v resa ii
             | LY -> write_mem_atomic AArch64.XL sz ea v resa ii))
          (read_reg_ord rd ii)
          ii

      let csel_op op v =
        let open AArch64Base in  match op with
        | Cpy -> M.unitT v
        | Inc -> M.op Op.Add v V.one
        | Neg -> M.op Op.Sub V.zero v
        | Inv -> Warn.fatal "size dependent inverse not implemented"

      let rmw_amo_read rmw sz = let open AArch64 in match rmw with
      | RMW_A|RMW_AL -> old_do_read_mem sz XA
      | RMW_L|RMW_P  -> old_do_read_mem sz X

      and rmw_amo_write rmw sz = let open AArch64 in match rmw with
      | RMW_L|RMW_AL -> do_write_mem sz XL
      | RMW_P|RMW_A  -> do_write_mem sz X

      let swp sz rmw r1 r2 r3 ii =
        let open AArch64Base in
        match r2 with
        | ZR ->
            let write_mem = match rmw with
            | RMW_L|RMW_AL -> write_mem_release
            | RMW_P|RMW_A  -> write_mem in
            lift_memop
              (fun ma ->
                (read_reg_data sz r1 ii >>| ma) >>= fun (v,a) ->
                 write_mem sz a v ii)
              (read_reg_ord r3 ii)
              ii
        |  _ ->
            let read_mem = rmw_amo_read rmw
            and write_mem =  rmw_amo_write rmw in
            lift_memop
              (fun ma ->
                let r2 = read_reg_data sz r1 ii
                and w2 v = write_reg r2 v ii (* no sz since alread masked *)
                and r1 a = read_mem sz a ii
                and w1 a v = write_mem sz a v ii in
                M.swp ma r1 r2 w1 w2)
              (read_reg_ord r3 ii)
              ii

      let cas sz rmw rs rt rn ii =
        lift_memop
          (fun ma ->
            let open AArch64 in
            let read_rs = read_reg_data sz rs ii in
            M.altT
              (ma >>= fun a ->
               (read_rs >>|
               begin let read_mem sz = match rmw with
               | RMW_A|RMW_AL -> old_do_read_mem sz A
               | RMW_L|RMW_P  -> old_do_read_mem sz N in
               read_mem sz a ii >>=
               fun v -> write_reg_sz_non_mixed sz rs v ii >>! v end) >>=
               fun (cv,v) -> M.neqT cv v >>! ())
              (let read_rt =  read_reg_data sz rt ii
              and read_mem a = rmw_amo_read rmw sz  a ii
              and write_mem a v = rmw_amo_write rmw sz a v ii
              and write_rs v =  write_reg rs v ii in (* no sz, argument masked *)
              M.aarch64_cas_ok
                ma read_rs read_rt write_rs read_mem write_mem M.eqT))
          (read_reg_ord rn ii)
          ii

      let ldop op sz rmw rs rt rn ii =
        lift_memop
          (fun ma ->
            let open AArch64 in
            let noret = match rt with | ZR -> true | _ -> false in
            let op = match op with
            | A_ADD -> Op.Add
            | A_EOR -> Op.Xor
            | A_SET -> Op.Or
            | A_CLR -> Op.AndNot2
            | A_SMAX -> Op.Max
            | A_SMIN -> Op.Min in
            let read_mem = if noret then fun sz -> old_do_read_mem sz NoRet else rmw_amo_read rmw
            and write_mem = rmw_amo_write rmw in
            M.amo_strict op
              ma
              (fun a -> read_mem sz a ii) (read_reg_data sz rs ii)
              (fun a v -> write_mem sz a v ii)
              (fun w ->if noret then M.unitT () else write_reg_sz_non_mixed sz rt w ii))
          (read_reg_ord rn ii)
          ii

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
            let sz = tr_variant var in
            ldr_p sz rd rs k ii
        | I_LDUR(var,rd,rs,k) ->
            let sz = tr_variant var in
            ldr sz rd rs (AArch64.K (Option.value k ~default:0)) AArch64.S_NOEXT ii

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
              get_ea rn kr AArch64.S_NOEXT ii
            end >>= fun (v,a) ->
            (M.op1 Op.TagLoc a >>| loc_extract a) >>= fun (atag,loc) ->
            (do_write_tag atag v ii >>| do_write_mem MachSize.Quad AArch64.N loc V.zero ii) >>! B.Next

        | I_STG(rt,rn,kr) ->
            if not memtag then Warn.user_error "STG without -variant memtag" ;
            begin
              (read_reg_data MachSize.Quad rt ii >>= tag_extract) >>|
              get_ea rn kr S_NOEXT ii
            end >>= fun (v,a) ->
            M.op1 Op.TagLoc a  >>= fun a ->
            do_write_tag a v ii >>! B.Next

        | I_LDG (rt,rn,kr) ->
            if not memtag then Warn.user_error "LDG without -variant memtag" ;
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

        | I_MOVZ(var,rd,k,os) ->
            movz var rd k os ii

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
            end >>=
            (let m =  (fun v ->
              (write_reg rd v ii) >>|
              (match op with
              | ADDS|SUBS|ANDS -> is_zero v >>= fun v -> write_reg NZP v ii
              | ADD|EOR|ORR|AND|SUB|ASR -> M.unitT ())) in
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
              read_reg_ord NZP ii >>= tr_cond c >>*= fun v ->
                (*commit_bcc ii >>= fun () ->*)
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
(*  Cannot handle *)
        | (I_RBIT _|I_MRS _|I_LDP _|I_STP _|I_IC _|I_DC _|I_BL _|I_BLR _|I_BR _|I_RET _) as i ->
            Warn.fatal "illegal instruction: %s"
              (AArch64.dump_instruction i)
       )
    end
end
