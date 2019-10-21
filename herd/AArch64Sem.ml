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
module Make (C:Sem.Config)(V:Value.S)
    =
  struct
    module ConfLoc = SemExtra.ConfigToArchConfig(C)
    module AArch64 =
      AArch64Arch_herd.Make
        (struct
          let moreedges = C.moreedges
          include ConfLoc
        end)(V)

    module Act = MachAction.Make(ConfLoc)(AArch64)
    include SemExtra.Make(C)(AArch64)(Act)
    let mixed = C.variant Variant.Mixed
    let memtag = C.variant Variant.MemTag

    let _ = B.Next
(* Barrier pretty print *)
    let barriers =
      let bs = AArch64Base.do_fold_dmb_dsb C.moreedges (fun h t -> h::t) []
      in List.map
        (fun b ->
          { barrier = b;
            pp = Misc.lowercase (AArch64Base.pp_barrier b)})
        bs
    let isync = Some { barrier = AArch64Base.ISB;pp = "isb";}

(* Semantics proper *)
    let (>>=) = M.(>>=)
    let (>>==) = M.(>>==)
    let (>>*=) = M.(>>*=)
    let (>>|) = M.(>>|)
    let (>>!) = M.(>>!)
    let (>>::) = M.(>>::)

    let tr_variant = let open AArch64Base in
    function
      | V32 -> MachSize.Word
      | V64 -> MachSize.Quad

    let mask32 ty m =
      let open AArch64Base in
      match ty with
      | V32 -> fun v -> M.op1 (Op.Mask MachSize.Word) v >>= m
      | V64 -> m


    let add_variant v a = (a,tr_variant v)

    let mk_read sz an loc v = Act.Access (Dir.R, loc, v, an, sz)
    let mk_read_std = mk_read MachSize.Quad AArch64.N
    let mk_fault ii loc = Act.Fault (ii,loc)

    let read_loc sz is_data = M.read_loc is_data (mk_read sz AArch64.N)

    let read_reg is_data r ii = match r with
    | AArch64Base.ZR -> M.unitT V.zero
    | _ -> M.read_loc is_data mk_read_std (A.Location_reg (ii.A.proc,r)) ii

    let read_reg_sz sz is_data r ii = match sz with
    | MachSize.Quad -> read_reg is_data r ii
    | MachSize.Word|MachSize.Short|MachSize.Byte ->
        read_reg is_data r ii >>= fun v -> M.op1 (Op.Mask sz) v

    let read_reg_ord = read_reg_sz MachSize.Quad false
    let read_reg_ord_sz sz = read_reg_sz sz false

    let read_reg_data sz = read_reg_sz sz true

(* Memory Tagging *)
(*
  get_tag x  read_loc x.tag
  |            |
  ---equality---
  |
  read_loc x

 *)
    let commit ii = M.mk_singleton_es (Act.Commit true) ii

    let mk_write sz an loc v = Act.Access (Dir.W, loc, v, an, sz)

    let write_loc sz an loc v ii = M.mk_singleton_es (mk_write sz an loc v) ii

    let write_reg r v ii = write_loc MachSize.Quad AArch64.N (A.Location_reg (ii.A.proc,r)) v ii

    let tag_extract a = M.op1 Op.TagExtract a
    let loc_extract a = M.op1 Op.LocExtract a

    let read_tag_mem a ii =
      M.op1 Op.TagLoc a >>= fun atag ->
        M.read_loc false (fun loc v -> Act.TagAccess (Dir.R,loc,v))
          (A.Location_global atag) ii

     let get_both_tags a ii = tag_extract a >>| read_tag_mem a ii

     let do_check atag patag = M.op Op.Eq patag atag

     let mixed_read sz an rd a ii =
       M.read_mixed false sz (fun sz -> mk_read sz an) a ii
              >>= (fun v -> write_reg rd v ii)

    let do_read_mem sz an a ii =
      if mixed then
        M.read_mixed false sz (fun sz -> mk_read sz an) a ii
      else
        M.read_loc false (mk_read sz an) (A.Location_global a) ii

    let do_checked_read sz an rd a ii =
      get_both_tags a ii >>= fun (atag,patag) ->
        do_check atag patag >>= fun cond -> commit ii
            >>*=
          fun _ ->
            M.choiceT cond
              (loc_extract a >>= fun a ->
               do_read_mem sz an a ii >>= fun v -> write_reg rd v ii >>! B.Next)
              (M.mk_singleton_es (mk_fault ii (A.Location_global a)) ii >>! B.Exit)



    let read_mem sz a ii = do_read_mem sz AArch64.N a ii
    let checked_read_mem sz rd a ii = do_checked_read sz AArch64.N rd a ii
    let read_mem_acquire sz a ii = do_read_mem sz AArch64.A a ii
    let checked_read_mem_acquire sz a ii = do_checked_read sz AArch64.A a ii
    let read_mem_acquire_pc sz a ii = do_read_mem sz AArch64.Q a ii
    let checked_read_mem_acquire_pc sz a ii = do_checked_read sz AArch64.Q a ii
    let read_mem_atomic sz a ii = do_read_mem sz AArch64.X a ii
    let checked_read_mem_atomic sz a ii = do_checked_read sz AArch64.X a ii
    let read_mem_atomic_acquire sz a ii = do_read_mem sz AArch64.XA a ii
    let read_mem_noreturn sz a ii = do_read_mem sz AArch64.NoRet a ii
    let checked_read_mem_atomic_acquire sz a ii = do_checked_read sz AArch64.XA a ii

    let do_write_mem sz an a v ii =
      if mixed then M.write_mixed sz (fun sz -> mk_write sz an) a v ii
      else write_loc sz an (A.Location_global a) v ii

    let do_checked_write sz an a rd v ii =
      get_both_tags a ii >>= fun (atag,patag) ->
      do_check atag patag >>= fun cond -> commit ii >>= fun _ ->
      M.choiceT cond
        (loc_extract a >>= fun a -> do_write_mem sz an a v ii >>= fun _ -> M.unitT () >>! B.Next)
        (M.mk_singleton_es (mk_fault ii (A.Location_global a)) ii >>! B.Exit)

    let write_mem sz a v ii = do_write_mem sz AArch64.N a v ii
    let checked_write_mem sz a rd ii = do_checked_write sz AArch64.N rd a ii
    let write_mem_release sz a v ii = do_write_mem sz AArch64.L a v ii
    let checked_write_mem_release sz a ii = do_checked_write sz AArch64.A a ii

    let write_mem_amo sz a v ii = do_write_mem sz AArch64.X a v ii
    let write_mem_amo_release sz a v ii = do_write_mem sz AArch64.XL a v ii

    let do_write_mem_atomic an sz a v resa ii =
      if mixed then
        (M.assign a resa >>|
        M.write_mixed sz (fun sz -> mk_write sz an)  a v ii) >>! ()
      else
        let eq = [M.VC.Assign (a,M.VC.Atom resa)] in
        M.mk_singleton_es_eq
          (Act.Access (Dir.W, A.Location_global a, v,an, sz)) eq ii

    let write_mem_atomic = do_write_mem_atomic AArch64.X
    and write_mem_atomic_release = do_write_mem_atomic AArch64.XL

    let create_barrier b ii =
      M.mk_singleton_es (Act.Barrier b) ii

    let flip_flag v = M.op Op.Xor v V.one
    let is_zero v = M.op Op.Eq v V.zero
    let is_not_zero v = M.op Op.Ne v V.zero

    let csel_op op v =
      let open AArch64Base in  match op with
      | Cpy -> M.unitT v
      | Inc -> M.op Op.Add v V.one
      | Neg -> M.op Op.Sub V.zero v
      | Inv ->
          Warn.fatal "size dependent inverse not implemented"

    let atomic_pair_allowed _ _ = true

    let load_sem sz rd a ii =
      begin
        if memtag then
          checked_read_mem sz rd a ii
        else
          read_mem sz a ii >>= fun v -> write_reg rd v ii >>! B.Next
      end

    let store_sem sz rd a v ii =
      begin
        if memtag then
          checked_write_mem sz rd a v ii
        else
          write_mem sz a v ii >>! B.Next
      end

    let ldr sz rd rs kr ii =
      let open AArch64Base in
      begin match kr with
      | K k ->
          (read_reg_ord rs ii)
            >>= (fun v -> M.add v (V.intToV k))
      | RV(_,r) ->
          (read_reg_ord rs ii >>| read_reg_ord r ii)
            >>= (fun (v1,v2) -> M.add v1 v2)
      end
        >>= fun a -> load_sem sz rd a ii

    and str sz rs rd kr ii =
      let open AArch64Base in
      begin read_reg_data sz rs ii >>|
      match kr with
      | K k ->
          read_reg_ord rd ii >>= fun v -> M.add v (V.intToV k)
      | RV(_,r) ->
          (read_reg_ord rd ii >>| read_reg_ord r ii)
            >>= fun (v1,v2) -> M.add v1 v2
      end >>= fun (v,a) -> store_sem sz rd a v ii

    and ldar sz t rd rs ii =
      let open AArch64 in
      (read_reg_ord rs ii)
        >>= fun a -> begin match t with
        | XX ->
            (write_reg ResAddr a ii >>|
            (read_mem_atomic sz a ii
               >>= (fun v -> (write_reg rd v ii))))
              >>! B.Next
        | AA ->
            (read_mem_acquire sz a ii)
              >>= (fun v -> (write_reg rd v ii))
              >>! B.Next
        | AX ->
            (write_reg ResAddr a ii
               >>| (read_mem_atomic_acquire sz a ii
                      >>= (fun v -> write_reg rd v ii)))
              >>! B.Next
        | AQ ->
            (read_mem_acquire_pc sz a ii)
              >>= (fun v -> (write_reg rd v ii))
              >>! B.Next
        end

    and stxr sz t rr rs rd ii =
      let open AArch64Base in
      M.riscv_store_conditional
        (read_reg_ord ResAddr ii)
        (read_reg_data sz rs ii)
        (read_reg_ord rd ii)
        (write_reg ResAddr V.zero ii)
        (fun v -> write_reg rr v ii)
        (fun ea resa v -> match t with
        | YY -> write_mem_atomic sz ea v resa ii
        | LY -> write_mem_atomic_release sz ea v resa ii)
        >>! B.Next


    let rmw_amo_read rmw = let open AArch64Base in match rmw with
    | RMW_A|RMW_AL -> read_mem_atomic_acquire
    | RMW_L|RMW_P  -> read_mem_atomic
    and rmw_amo_write rmw = let open AArch64Base in match rmw with
    | RMW_L|RMW_AL -> write_mem_amo_release
    | RMW_P|RMW_A  -> write_mem_amo

    let swp sz rmw r1 r2 r3 ii =
      let open AArch64Base in
      match r2 with
      | ZR ->
          let write_mem = match rmw with
          | RMW_L|RMW_AL -> write_mem_release
          | RMW_P|RMW_A  -> write_mem in
          (read_reg_data sz r1 ii >>| read_reg_ord r3 ii) >>=
          fun (v,a) -> write_mem sz a v ii
      |  _ ->
          let read_mem = rmw_amo_read rmw
          and write_mem =  rmw_amo_write rmw in
          read_reg_ord r3 ii >>=
          fun a ->
            let r1 = read_reg_data sz r1 ii
            and w1 = fun v -> write_reg r2 v ii
            and r2 = read_mem sz a ii
            and w2 = fun v -> write_mem sz a v ii in
            M.exch r1 r2 w1 w2 >>! ()

    let cas sz rmw rs rt rn ii =
      let open AArch64Base in
      let read_rn = read_reg_ord rn ii
      and read_rs = read_reg_ord_sz sz rs ii in
      M.altT
        (read_rn >>= fun a ->
          (read_rs >>|
          begin let read_mem = match rmw with
          | RMW_A|RMW_AL -> read_mem_acquire
          | RMW_L|RMW_P  -> read_mem in
          read_mem sz a ii >>=
          fun v -> write_reg rs v ii >>! v end) >>=
          fun (cv,v) -> M.neqT cv v >>! ())
        (let read_rt =  read_reg_data sz rt ii
        and read_mem a = rmw_amo_read rmw sz  a ii
        and write_mem a v = rmw_amo_write rmw sz a v ii
        and write_rs v =  write_reg rs v ii in
        M.aarch64_cas_ok
          read_rn read_rs read_rt write_rs read_mem write_mem M.eqT)

    let ldop op sz rmw rs rt rn ii =
      let open AArch64Base in
      let noret = match rt with | ZR -> true | _ -> false in
      let op = match op with
      | A_ADD -> Op.Add
      | A_EOR -> Op.Xor
      | A_SET -> Op.Or
      | A_CLR -> Op.AndNot2
      | A_SMAX -> Op.Max
      | A_SMIN -> Op.Min in
      let read_mem = if noret then read_mem_noreturn else rmw_amo_read rmw
      and write_mem = rmw_amo_write rmw in
      M.amo op
        (read_reg_ord rn ii) (read_reg_data sz rs ii)
        (fun a -> read_mem sz a ii) (fun a v -> write_mem sz a v ii)
        >>= fun w ->if noret then M.unitT () else write_reg rt w ii

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
          let cond = match c with
          | NE -> is_zero
          | EQ -> is_not_zero
          in
          (read_reg_ord NZP ii)
            >>= cond
            >>= fun v -> commit ii
                >>= fun () -> B.bccT v l

      | I_CBZ(_,r,l) ->
          (read_reg_ord r ii)
            >>= is_zero
            >>= fun v -> commit ii
                >>= fun () -> B.bccT v l

      | I_CBNZ(_,r,l) ->
          (read_reg_ord r ii)
            >>= is_not_zero
            >>= fun v -> commit ii
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
          let sz = tr_variant var in
          (read_reg_ord rd ii >>| read_reg_data sz rs ii)
            >>= (fun (a,v) -> write_mem_release sz a v ii)
            >>! B.Next
      | I_STLRBH(bh,rs,rd) ->
          let sz = bh_to_sz bh in
          (read_reg_ord rd ii >>| read_reg_data sz rs ii)
            >>= (fun (a,v) -> write_mem_release sz a v ii)
            >>! B.Next

      | I_STG(rt,rn,kr) ->
          let open AArch64Base in
          begin
            match kr with
            | K k ->
                read_reg_ord rn ii >>= fun v -> M.add v (V.intToV k)
            | RV(_,r) ->
                (read_reg_ord rn ii >>| read_reg_ord r ii)
                  >>= fun (v1,v2) -> M.add v1 v2
          end
            >>= fun a ->
              (read_reg_ord rt ii
                 >>| M.get_alloc_tag a)
                >>= fun (patag,atag) -> M.set_tag (fun sz -> mk_write sz AArch64.T) atag patag ii
                    >>! B.Next

      | I_LDG (rt,rn,kr) ->
          let open AArch64Base in
          begin
            match kr with
            | K k ->
                (read_reg_ord rn ii)
                  >>= (fun v -> M.add v (V.intToV k))
            | RV(_,r) ->
                (read_reg_ord rn ii >>| read_reg_ord r ii)
                  >>= (fun (v1,v2) -> M.add v1 v2)
          end
            >>= fun a -> M.get_alloc_tag_val (fun sz -> mk_read sz AArch64.T) a ii
                >>= fun v -> (*let rt = List.assoc rt AArch64Base.xregs in
                               let patag = AArch64Base.Symbolic_tag rt in  *)
                  (*begin
                    match rt with Ireg rt ->
                    let patag = AArch64Base.Tag rt in
                    write_loc Word AArch64.T (A.Location_reg (ii.A.proc,patag)) v ii (*Quad?*)
                    end*)
                  write_reg rt v ii
                    >>! B.Next

      | I_STXR(var,t,rr,rs,rd) ->
          stxr (tr_variant var) t rr rs rd ii
      | I_STXRBH(bh,t,rr,rs,rd) ->
          stxr (bh_to_sz bh) t rr rs rd ii

            (* Operations *)
      | I_MOV(_,r,K k) ->
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
          end >>=
          (let m =  (fun v ->
            (write_reg rd v ii) >>|
            (match op with
            | ADDS|SUBS|ANDS -> is_zero v >>= fun v -> write_reg NZP v ii
            | ADD|EOR|ORR|AND|SUB -> M.unitT ())) in
          mask32 ty m) >>!
          B.Next
            (* Barrier *)
      | I_FENCE b ->
          (create_barrier b ii) >>! B.Next
            (* Conditional selection *)
      | I_CSEL (var,r1,r2,r3,c,op) ->
          let sz = tr_variant var in
          let cond = match c with
          | NE -> is_not_zero
          | EQ -> is_zero in
          if C.variant Variant.WeakPredicated then
            read_reg_ord NZP ii >>= cond >>= fun v ->
              M.choiceT v
                (read_reg_data sz r2 ii >>= fun v -> write_reg r1 v ii)
                (read_reg_data sz r3 ii >>=
                 csel_op op >>= mask32 var (fun v ->  write_reg r1 v ii))
                >>! B.Next
          else
            begin
              (read_reg_ord NZP ii >>= cond) >>|  read_reg_data sz r2 ii >>| read_reg_data sz r3 ii
            end >>= fun ((v,v2),v3) ->
              M.choiceT v
                (write_reg r1 v2 ii)
                (csel_op op v3 >>= mask32 var (fun v ->  write_reg r1 v ii))
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
