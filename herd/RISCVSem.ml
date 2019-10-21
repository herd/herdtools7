(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Semantics of RISC-V instructions *)

module Make (C:Sem.Config)(V:Value.S)
    =
  struct
    module RISCV = RISCVArch_herd.Make(SemExtra.ConfigToArchConfig(C))(V)
    module Act = MachAction.Make(C.PC)(RISCV)
    include SemExtra.Make(C)(RISCV)(Act)

    let mixed = C.variant Variant.Mixed

(* Barrier pretty print *)
    let barriers =
      RISCV.do_fold_fence
        (fun f k -> {barrier=f; pp=RISCV.pp_barrier_dot f;}::k)
        []
    let isync = Some {barrier=RISCV.FenceI; pp="fenceI";}

    let nat_sz = V.Cst.Scalar.machsize

    let atomic_pair_allowed _ _ = true

(* Semantics proper *)
    module Mixed(SZ:ByteSize.S) = struct

      module Mixed = M.Mixed(SZ)

      let (>>=) = M.(>>=)
      let (>>*=) = M.(>>*=)
      let (>>|) = M.(>>|)
      let (>>!) = M.(>>!)
      let (>>::) = M.(>>::)

      let unimplemented op = Warn.user_error "RISCV operation %s is not implemented (yet)" op

      let tr_opi op = match op with
      | RISCV.ADDI ->  Op.Add
      | RISCV.SLTI -> Op.Lt
      | RISCV.ANDI -> Op.And
      | RISCV.ORI -> Op.Or
      | RISCV.XORI -> Op.Xor
      | RISCV.SLLI -> Op.ShiftLeft
      | RISCV.SLTIU|RISCV.SRAI|RISCV.SRLI
        -> unimplemented (RISCV.pp_opi op)

      let tr_op op = match op  with
      | RISCV.ADD -> Op.Add
      | RISCV.SLT -> Op.Lt
      | RISCV.AND -> Op.And
      | RISCV.OR -> Op.Or
      | RISCV.XOR -> Op.Xor
      | RISCV.SLL -> Op.ShiftLeft
      | RISCV.SUB -> Op.Sub
      | RISCV.SLTU|RISCV.SRA|RISCV.SRL
        -> unimplemented (RISCV.pp_op op)

      let tr_opiw op = match op with
      | RISCV.ADDIW ->  Op.Add
      | RISCV.SLLIW -> Op.ShiftLeft
      | RISCV.SRLIW|RISCV.SRAIW
        -> unimplemented (RISCV.pp_opiw op)

      let tr_opw op = match op with
      | RISCV.ADDW ->  Op.Add
      | RISCV.SLLW -> Op.ShiftLeft
      | RISCV.SUBW -> Op.Sub
      | RISCV.SRLW|RISCV.SRAW
        -> unimplemented (RISCV.pp_opw op)

      let tr_opamo op = match op with
      | RISCV.AMOSWAP -> assert false
      | RISCV.AMOADD -> Op.Add
      | RISCV.AMOAND -> Op.And
      | RISCV.AMOOR -> Op.Or
      | RISCV.AMOXOR -> Op.Xor
      | RISCV.AMOMAX -> Op.Max
      | RISCV.AMOMIN -> Op.Min
      | RISCV.AMOMAXU|RISCV.AMOMINU ->
          unimplemented (RISCV.pp_opamo op)

      let tr_cond cond = match cond with
      | RISCV.EQ -> Op.Eq
      | RISCV.NE -> Op.Ne
      | RISCV.LT -> Op.Lt
      | RISCV.GE -> Op.Ge
      | RISCV.LTU|RISCV.GEU ->  unimplemented (RISCV.pp_bcc cond)

      let mk_read sz ato loc v = Act.Access (Dir.R, loc, v, ato, sz)

      let plain = RISCV.(P Rlx)

      let read_reg is_data r ii = match r with
      | RISCV.Ireg RISCV.X0 -> M.unitT V.zero
      | _ ->
          M.read_loc is_data (mk_read nat_sz plain)
            (A.Location_reg (ii.A.proc,r)) ii

      let read_reg_ord = read_reg false
      let read_reg_data = read_reg true

      let read_mem_annot sz an a ii =
        if mixed then
          Mixed.read_mixed false sz (fun sz a v -> mk_read sz an a v)
            a ii
        else
          M.read_loc false (mk_read sz an) (A.Location_global a) ii

      let read_mem sz mo = read_mem_annot sz (RISCV.P mo)
      let read_mem_atomic sz mo = read_mem_annot sz (RISCV.X mo)

      let write_loc_annot sz an loc v ii =
        M.mk_singleton_es (Act.Access (Dir.W, loc, v, an, sz)) ii

      let do_write_reg mk r v ii = match r with
      | RISCV.Ireg RISCV.X0 -> M.unitT ()
      | _ ->
          mk
            (Act.Access (Dir.W, (A.Location_reg (ii.A.proc,r)), v, plain, nat_sz)) ii

      let write_reg = do_write_reg M.mk_singleton_es

      let write_reg_success =
        do_write_reg
          (if O.variant Variant.Success then
            M.mk_singleton_es_success else M.mk_singleton_es)

      let do_write_mem sz an a v ii  =
        if mixed then
          Mixed.write_mixed sz
            (fun sz a v -> Act.Access (Dir.W, a, v, an, sz))
            a v ii
        else
          M.mk_singleton_es
            (Act.Access (Dir.W, A.Location_global a, v, an, sz)) ii

      let write_mem sz an = do_write_mem sz (RISCV.P an)

      let lrscdiffok = C.variant Variant.LrScDiffOk

      let write_mem_conditional sz an a v resa ii =
        if  lrscdiffok then
          (M.mk_singleton_es_eq
             (Act.Access (Dir.W, A.Location_global a, v, RISCV.X an,sz)) [] ii >>|
             M.neqT resa V.zero) >>! () (* resa = zero <-> no matching load reserve *)
        else
          let eq = [M.VC.Assign (a,M.VC.Atom resa)] in
          M.mk_singleton_es_eq
            (Act.Access (Dir.W, A.Location_global a, v, RISCV.X an,sz)) eq ii

      let write_mem_atomic sz an = do_write_mem sz (RISCV.X an)

      let create_barrier b ii = M.mk_singleton_es (Act.Barrier b) ii

      let commit ii = M.mk_singleton_es (Act.Commit true) ii

(* Compute amo semantics anotations from syntactic  ones,
   Notice that Sc is exclusively semantics, cf. assert false below *)
(* RMW events *)
      let rmw_events = not (C.variant Variant.SplittedRMW)
      let specialX0 = C.variant Variant.SpecialX0
      let asfence =  C.variant Variant.AcqRelAsFence

      let read_amo  =
        let open RISCV in
        if specialX0 then fun mo -> match mo with
        | Rlx|Acq|AcqRel -> mo
        | Rel -> Rlx
        | Sc -> assert false
        else fun mo -> match mo with
        | Rlx|Acq -> mo
        | Rel -> Rlx
        | AcqRel -> Sc (* Compatibility, may disappear in future *)
        | Sc -> assert false

      and write_amo =
        let open RISCV in
        if specialX0 then fun mo -> match mo with
        | Rlx|Rel|AcqRel -> mo
        | Acq -> Rlx
        | Sc -> assert false
        else fun mo -> match mo with
        | Rlx|Rel -> mo
        | Acq -> Rlx
        | AcqRel -> Sc (* Compatibility, may disappear in future *)
        | Sc -> assert false

      let amo sz op an rd rv ra ii =
        let open RISCV in
        if rmw_events then
          let ra = read_reg_ord ra ii
          and rv = read_reg_data rv ii in
          match op with
          | AMOSWAP ->
              (ra >>| rv) >>=
              (fun (loc,vstore) ->
                M.read_loc false
                  (fun loc v -> Act.Amo (loc,v,vstore,X an,sz))
                  (A.Location_global loc) ii) >>= fun r -> write_reg rd r ii
          | _ ->
              (ra >>| rv) >>=
              (fun (loc,v) ->
                M.fetch (tr_opamo op) v
                  (fun v vstored -> Act.Amo (A.Location_global loc,v,vstored,RISCV.X an,sz))
                  ii)  >>=  fun v -> write_reg rd v ii
        else match specialX0,op,rd,rv with
        | true,AMOSWAP,Ireg X0,_ ->
            (read_reg_data rv ii >>| read_reg_ord ra ii) >>=
            fun (d,a) -> write_mem sz (write_amo an) a d ii
        | true,(AMOOR|AMOADD),_,Ireg X0 ->
            read_reg_ord ra ii >>=
            fun a -> read_mem sz (read_amo an) a ii >>=
              fun v -> write_reg rd v ii
        | _ ->
            let amo an =
              let ra = read_reg_ord ra ii
              and rv = read_reg_data rv ii
              and rmem = fun loc -> read_mem_atomic sz (read_amo an) loc ii
              and wmem = fun loc v -> write_mem_atomic sz (write_amo an) loc v ii in
              (match op with
              | AMOSWAP -> M.linux_exch | _ -> M.amo (tr_opamo op))
                ra rv rmem wmem >>= fun r -> write_reg rd r ii in
            amo an

(* Entry point *)
      let tr_sz = RISCV.tr_width

      let build_semantics ii =
        M.addT (A.next_po_index ii.A.program_order_index)
          begin match ii.A.inst with
          | RISCV.OpI (op,r1,r2,k) ->
              read_reg_ord r2 ii >>=
              fun v -> M.op (tr_opi op) v (V.intToV k) >>=
                fun v -> write_reg r1 v ii >>! B.Next
          | RISCV.OpIW (op,r1,r2,k) ->
              read_reg_ord r2 ii >>=
              fun v -> M.op (tr_opiw op) v (V.intToV k) >>=
                fun v -> write_reg r1 v ii >>! B.Next
          | RISCV.Op (op,r1,r2,r3) ->
              (read_reg_ord r2 ii >>|  read_reg_ord r3 ii) >>=
              (fun (v1,v2) -> M.op (tr_op op) v1 v2) >>=
              (fun v -> write_reg r1 v ii) >>! B.Next
          | RISCV.OpW (op,r1,r2,r3) ->
              (read_reg_ord r2 ii >>|  read_reg_ord r3 ii) >>=
              (fun (v1,v2) -> M.op (tr_opw op) v1 v2) >>=
              (fun v -> write_reg r1 v ii) >>! B.Next

          | RISCV.J lbl -> B.branchT lbl
          | RISCV.Bcc (cond,r1,r2,lbl) ->
              (read_reg_ord r1 ii >>| read_reg_ord r2 ii) >>=
              fun (v1,v2) -> M.op (tr_cond cond) v1 v2 >>=
                fun v -> commit ii >>= fun () -> B.bccT v lbl
          | RISCV.Load (sz,_s,mo,r1,k,r2) ->
              let mk_load mo =
                read_reg_ord r2 ii >>=
                (fun a -> M.add a (V.intToV k)) >>=
                (fun ea -> read_mem (tr_sz sz) mo ea ii) >>=
                (fun v -> write_reg r1 v ii) in
              if specialX0 then mk_load mo >>! B.Next
              else if asfence then
                let open RISCV in
                let ld =  match mo with
                | AcqRel ->
                    create_barrier (Fence (RW,RW)) ii >>*= fun () -> mk_load Rlx
                | Rel|Acq|Rlx -> mk_load Rlx
                | Sc -> assert false in
                let ld = match mo with
                |Acq|AcqRel ->
                    ld >>*= fun () -> create_barrier (Fence (R,RW)) ii
                | Rlx|Rel -> ld
                | Sc -> assert false in
                ld >>! B.Next
              else mk_load mo >>! B.Next

          | RISCV.Store (sz,mo,r1,k,r2) ->
              let mk_store mo =
                (read_reg_data r1 ii >>| read_reg_ord r2 ii) >>=
                (fun (d,a) ->
                  (M.add a (V.intToV k)) >>=
                  (fun ea -> write_mem (tr_sz sz) mo ea d ii)) in
              if specialX0 then mk_store mo >>! B.Next
              else if asfence then
                let open RISCV in
                let sd () =  mk_store Rlx in
                let sd = match mo with
                | Rel -> create_barrier (Fence (RW,W)) ii >>*= sd
                | AcqRel -> create_barrier (Fence (RW,RW)) ii >>*= sd
                | Acq|Rlx -> sd ()
                | Sc -> assert false in
                sd >>! B.Next
              else  mk_store mo >>! B.Next
          | RISCV.LoadReserve  ((RISCV.Double|RISCV.Word as sz),mo,r1,r2) ->
              read_reg_ord r2 ii >>=
              (fun ea ->
                write_reg RISCV.RESADDR ea ii >>|
                (read_mem_atomic (tr_sz sz) mo ea ii >>= fun v -> write_reg r1 v ii)) >>! B.Next
          | RISCV.StoreConditional ((RISCV.Double|RISCV.Word as sz),mo,r1,r2,r3) ->
              M.riscv_store_conditional
                (read_reg_ord RISCV.RESADDR ii)
                (read_reg_data r2 ii)
                (read_reg_ord r3 ii)
                (write_reg RISCV.RESADDR V.zero ii)
                (fun v -> write_reg_success r1 v ii)
                (fun ea resa v -> write_mem_conditional (tr_sz sz) mo ea v resa ii) >>!
              B.Next
          | RISCV.Amo (op,sz,mo,r1,r2,r3) ->
              amo (tr_sz sz) op mo r1 r2 r3 ii >>! B.Next
          | RISCV.FenceIns b ->
              create_barrier b ii >>! B.Next
          | ins -> Warn.fatal "RISCV, instruction '%s' not handled" (RISCV.dump_instruction ins)
          end
    end
  end
