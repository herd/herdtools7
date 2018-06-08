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
    let (>>*=) = M.(>>*=)
    let (>>|) = M.(>>|)
    let (>>!) = M.(>>!)
    let (>>::) = M.(>>::)

    let tr_variant = function
      | AArch64Base.V32 -> MachSize.Word
      | AArch64Base.V64 -> MachSize.Quad

    let add_variant v a = (a,tr_variant v)

    let mk_read sz an loc v = Act.Access (Dir.R, loc, v, an, sz)

    let read_loc v is_data = M.read_loc is_data (mk_read v AArch64.N)

    let read_reg is_data r ii = match r with
    | AArch64Base.ZR -> M.unitT V.zero
    | _ ->
        M.read_loc is_data (mk_read MachSize.Quad AArch64.N) (A.Location_reg (ii.A.proc,r)) ii

    let read_reg_ord = read_reg false
    let read_reg_data = read_reg true

    let do_read_mem sz an a ii =
      if mixed then
        M.read_mixed false sz (fun sz -> mk_read sz an) a ii
      else
        let a = A.Location_global a in
        M.read_loc false (mk_read sz an) a ii

    let read_mem sz a ii = do_read_mem sz AArch64.N a ii
    let read_mem_acquire sz a ii = do_read_mem sz AArch64.A a ii
    let read_mem_acquire_pc sz a ii = do_read_mem sz AArch64.Q a ii
    let read_mem_atomic sz a ii = do_read_mem sz AArch64.X a ii
    let read_mem_atomic_acquire sz a ii = do_read_mem sz AArch64.XA a ii


    let mk_write sz an loc v = Act.Access (Dir.W, loc, v, an, sz)

    let write_loc sz an loc v ii = M.mk_singleton_es (mk_write sz an loc v) ii

    let write_reg r v ii = write_loc MachSize.Quad AArch64.N (A.Location_reg (ii.A.proc,r)) v ii

    let do_write_mem sz an a v ii =
      if mixed then M.write_mixed sz (fun sz -> mk_write sz an)  a v ii
      else  write_loc sz an (A.Location_global a) v ii

    let write_mem sz a v ii = do_write_mem sz AArch64.N a v ii

    let write_mem_release sz a v ii = do_write_mem sz AArch64.L a v ii

        (* TODO MIXED SIZE *)
    let do_write_mem_atomic an sz a v resa ii =
      let eq = [M.VC.Assign (a,M.VC.Atom resa)] in
      M.mk_singleton_es_eq
        (Act.Access (Dir.W, A.Location_global a, v,an, sz)) eq ii

    let write_mem_atomic = do_write_mem_atomic AArch64.X
    and write_mem_atomic_release = do_write_mem_atomic AArch64.XL

    let create_barrier b ii =
      M.mk_singleton_es (Act.Barrier b) ii

    let commit ii =
      M.mk_singleton_es (Act.Commit true) ii

    let flip_flag v = M.op Op.Xor v V.one
    let is_zero v = M.op Op.Eq v V.zero
    let is_not_zero v = M.op Op.Ne v V.zero

    let atomic_pair_allowed _ _ = true

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
        >>= (fun a -> read_mem sz a ii)
        >>= (fun v -> write_reg rd v ii)
        >>! B.Next

    and str sz rs rd kr ii =
      let open AArch64Base in
      begin read_reg_data rs ii >>|
      match kr with
      | K k ->
          read_reg_ord rd ii >>= fun v -> M.add v (V.intToV k)
      | RV(_,r) ->
          (read_reg_ord rd ii >>| read_reg_ord r ii)
          >>= fun (v1,v2) -> M.add v1 v2 end
        >>= (fun (v,a) -> write_mem sz a v ii)
        >>! B.Next

    let build_semantics ii =
      M.addT (A.next_po_index ii.A.program_order_index)
        AArch64Base.(
      match ii.A.inst with
        (* Branches *)
      | I_B l ->
          B.branchT l

      | I_BC(c,l)->
          let cond = match c with
          | NE -> is_not_zero
          | EQ -> is_zero
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
          let var = tr_variant var in
          (read_reg_ord rs ii)
            >>= fun a -> begin match t with
            | XX ->
                (write_reg ResAddr a ii >>|
                (read_mem_atomic var a ii
                   >>= (fun v -> (write_reg rd v ii))))
                  >>! B.Next
            | AA ->
                (read_mem_acquire var a ii)
                  >>= (fun v -> (write_reg rd v ii))
                  >>! B.Next
            | AX ->
                (write_reg ResAddr a ii
                   >>| (read_mem_atomic_acquire var a ii
                          >>= (fun v -> write_reg rd v ii)))
                  >>! B.Next
            | AQ ->
                (read_mem_acquire_pc var a ii)
                  >>= (fun v -> (write_reg rd v ii))
                  >>! B.Next
            end

      | I_STR(var,rs,rd,kr) ->
          str (tr_variant var) rs rd kr ii

      | I_STRBH(bh,rs,rd,kr) ->
          str (bh_to_sz bh) rs rd kr ii

      | I_STLR(var,rs,rd) ->
          (read_reg_ord rd ii >>| read_reg_data rs ii)
            >>= (fun (a,v) -> write_mem_release (tr_variant var) a v ii)
            >>! B.Next

      | I_STXR(var,t,rr,rs,rd) ->
          let var = tr_variant var in
          M.riscv_store_conditional
            (read_reg_ord ResAddr ii)
            (read_reg_data rs ii)
            (read_reg_ord rd ii)
            (write_reg ResAddr V.zero ii)
            (fun v -> write_reg rr v ii)
            (fun ea resa v -> match t with
            | YY -> write_mem_atomic var ea v resa ii
            | LY -> write_mem_atomic_release var ea v resa ii)
            >>! B.Next

            (* Operations *)
      | I_MOV(_,r,K k) ->
          write_reg r (V.intToV k) ii >>! B.Next

      | I_MOV(_,r1,RV (_,r2)) ->
          read_reg_ord r2 ii >>= fun v -> write_reg r1 v ii >>! B.Next

      | I_SXTW(rd,rs) ->
          (read_reg_ord rs ii)            >>= fun v -> (write_reg rd v ii)
              >>! B.Next

      | I_OP3(_,op,rd,rn,kr) ->
          (read_reg_ord rn ii >>|
          match kr with
          | K k -> M.unitT (V.intToV k)
          | RV(_,r) -> read_reg_ord r ii
     ) >>=
          begin match op with
          | ADD|ADDS -> fun (v1,v2) -> M.add v1 v2
          | EOR -> fun (v1,v2) -> M.op Op.Xor v1 v2
          | ORR -> fun (v1,v2) -> M.op Op.Or v1 v2
          | SUB|SUBS -> fun (v1,v2) -> M.op Op.Sub v1 v2
          | AND|ANDS -> fun (v1,v2) -> M.op Op.And v1 v2
          end
            >>= (fun v -> (write_reg rd v ii)
                >>| (match op with ADDS|SUBS|ANDS -> write_reg NZP v ii | ADD|EOR|ORR|AND|SUB -> M.unitT ()))
            >>! B.Next

            (* Barrier *)
      | I_FENCE b ->
          (create_barrier b ii) >>! B.Next
            (* Conditional selection *)
      | I_CSEL (_v,r1,r2,r3,c,op) ->
          let cond = match c with
          | NE -> is_not_zero
          | EQ -> is_zero in
          (read_reg_ord NZP ii)
            >>= cond
            >>= fun v ->
              M.choiceT v
                (read_reg_data r2 ii >>= fun v -> write_reg r1 v ii)
                (read_reg_data r3 ii >>=
                 fun v ->
                   let mop = match op with
                   | Cpy -> M.unitT v
                   | Inc -> M.op Op.Add v V.one
                   | Neg -> M.op Op.Sub V.zero v
                   | Inv ->
                       Warn.fatal "size dependent inverse not implemented" in
                   mop >>= fun v ->  write_reg r1 v ii)
                >>! B.Next

                (*  Cannot handle *)
      | (I_LDP _|I_STP _) as i ->
          Warn.fatal "illegal instruction: %s"
            (AArch64.dump_instruction i)
     )
  end
