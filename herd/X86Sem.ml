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

(** Semantics of X86 instructions *)

module Make (C:Sem.Config)(V : Value.S)
    =
  struct
    module X86 = X86Arch_herd.Make(SemExtra.ConfigToArchConfig(C))(V)
    module Act = MachAction.Make(C.PC)(X86)
    include SemExtra.Make(C)(X86)(Act)

(* barrier pretty print *)
    let mfence = {barrier=X86.Mfence; pp="mfence";}
    let sfence = {barrier=X86.Sfence; pp="sfence";}
    let lfence = {barrier=X86.Lfence; pp="lfence";}
    let barriers = [mfence; lfence;sfence;]
    let isync = None
    let nat_sz = V.Cst.Scalar.machsize
    let is_global = A.is_global

    let atomic_pair_allowed e1 e2 = match e1.E.iiid, e2.E.iiid with
    | Some i1,Some i2 -> i1 == i2
    | _,_ -> false

(********************)
(* semantics proper *)
(********************)

    module Mixed(SZ:ByteSize.S) = struct

      let (>>=) = M.(>>=)
      let (>>*=) = M.(>>*=)
      let (>>|) = M.(>>|)
      let (>>!) = M.(>>!)

      let mk_read sz ato loc v = Act.Access (Dir.R, loc, v, ato, sz)

      let read_loc sz is_d = M.read_loc is_d (mk_read sz false)

      let mk_read_choose_atomic sz loc = mk_read sz (is_global loc) loc

      let read_reg is_data r ii =
        M.read_loc is_data (mk_read nat_sz false) (A.Location_reg (ii.A.proc,r)) ii

      let read_mem sz a ii  =
        M.read_loc false (mk_read sz false) (A.Location_global a) ii
      let read_mem_atomic sz a ii =
        M.read_loc false (mk_read sz true) (A.Location_global a) ii

      let read_loc_atomic sz is_d = M.read_loc is_d (mk_read_choose_atomic sz)

      let read_loc_gen sz data locked loc ii = match loc with
      |  A.Location_global _ ->
          M.read_loc data (mk_read sz locked) loc ii
      | _ ->
          M.read_loc data (mk_read nat_sz false) loc ii


      let write_loc_gen sz locked loc v ii = match loc with
      |  A.Location_global _ ->
          M.mk_singleton_es (Act.Access (Dir.W, loc, v, locked, sz)) ii
      | _ ->
          M.mk_singleton_es (Act.Access (Dir.W, loc, v, locked, nat_sz)) ii

      let write_loc sz loc v ii =
        M.mk_singleton_es (Act.Access (Dir.W, loc, v, false, sz)) ii

      let write_reg r v ii =
        M.mk_singleton_es (Act.Access (Dir.W, (A.Location_reg (ii.A.proc,r)), v, false, nat_sz)) ii
      let write_mem sz a v ii  =
        M.mk_singleton_es (Act.Access (Dir.W, A.Location_global a, v, false, sz)) ii
      let write_mem_atomic sz a v ii =
        M.mk_singleton_es (Act.Access (Dir.W, A.Location_global a, v, true, sz)) ii

      let write_loc_atomic sz loc v ii =
        M.mk_singleton_es (Act.Access (Dir.W, loc, v, (is_global loc), sz)) ii

      let write_flag r o v1 v2 ii =
        M.addT
          (A.Location_reg (ii.A.proc,r))
          (M.op o v1 v2) >>= (fun (loc,v) -> write_loc nat_sz loc v ii)

      let create_barrier b ii =
        M.mk_singleton_es (Act.Barrier b) ii

      let lval_ea ea ii = match ea with
      | X86.Effaddr_rm32 (X86.Rm32_reg r)->
          M.unitT (X86.Location_reg (ii.X86.proc,r))
      | X86.Effaddr_rm32 (X86.Rm32_deref r)     ->
          read_reg false r ii >>=
          fun vreg -> M.unitT (X86.Location_global vreg)
      | X86.Effaddr_rm32 (X86.Rm32_abs v)->
          M.unitT (X86.maybev_to_location v)

      let rval_ea sz locked ea ii = lval_ea ea ii >>=  fun loc -> read_loc sz locked loc ii

      let rval_op sz locked op ii = match op with
      | X86.Operand_effaddr ea -> rval_ea sz locked ea ii
      | X86.Operand_immediate s -> M.unitT (V.intToV s)

      let flip_flag v = M.op Op.Xor v V.one
          (* Set flags by comparing v1 v2 *)
      let write_zf v1 v2 ii =  write_flag X86.ZF Op.Eq v1 v2 ii
      let write_sf v1 v2 ii =  write_flag X86.SF Op.Gt v1 v2 ii

      let write_all_flags v1 v2 ii =
        (write_zf v1 v2 ii >>| write_sf v1 v2 ii >>|
        write_flag X86.CF Op.Eq V.zero V.one ii) (* Carry was always zero! *)
          >>! ()

(* Exchange *)
(*
  let xchg ea1 ea2 ii =
  (lval_ea ea1 ii >>| lval_ea ea2 ii) >>=
  fun (l1,l2) ->
  (read_loc l1 ii >>| read_loc l2 ii) >>=
  fun (v1,v2) ->
  (write_loc l1 v2 ii >>| write_loc l2 v1 ii) >>! B.Next
 *)

      let xchg sz ea1 ea2 ii =
        (lval_ea ea1 ii >>| lval_ea ea2 ii) >>=
        (fun (l1,l2) ->
          let r1 = read_loc_atomic sz true l1 ii
          and r2 = read_loc_atomic sz true l2 ii
          and w1 = fun v -> write_loc_atomic sz l1 v ii
          and w2 = fun v -> write_loc_atomic sz l2 v ii in
          M.exch r1 r2 w1 w2) >>! B.Next

      let do_op sz locked o ea op ii =
        (lval_ea ea ii >>=
         fun loc ->
           M.addT loc (read_loc_gen sz true locked loc ii) >>| rval_op sz locked op ii)
          >>=
        fun ((loc,v_ea),v_op) ->
          M.op o v_ea v_op >>=
          fun v_result ->
            (write_loc_gen sz locked loc v_result ii >>|
            write_all_flags v_result V.zero ii) >>! B.Next

      let build_semantics ii =
        let rec build_semantics_inner locked ii =
          match ii.A.inst with
          |  X86.I_NOP -> M.unitT B.Next
          |  X86.I_XOR (ea,op) -> do_op nat_sz locked Op.Xor ea op ii
          |  X86.I_OR (ea,op) -> do_op nat_sz locked Op.Or ea op ii
          |  X86.I_ADD (ea,op) -> do_op nat_sz locked Op.Add ea op ii
          |  X86.I_MOV (ea,op)|X86.I_MOVB (ea,op)|X86.I_MOVW (ea,op)|X86.I_MOVL (ea,op)|X86.I_MOVQ (ea,op)
          |  X86.I_MOVT (ea,op) as i ->
              let sz = match i with
              | X86.I_MOV _|X86.I_MOVL _ -> MachSize.Word
              | X86.I_MOVB _ ->  MachSize.Byte
              | X86.I_MOVW _ ->  MachSize.Short
              | X86.I_MOVQ _ -> MachSize.Quad
              | _ -> assert false in
              (lval_ea ea ii >>| rval_op sz locked op ii) >>=
              fun (loc,v_op) ->
                write_loc_gen sz locked loc v_op ii >>! B.Next
          |  X86.I_READ (op) ->
              rval_op nat_sz locked op ii >>! B.Next
          |  X86.I_DEC (ea) ->
              lval_ea ea ii >>=
              fun loc -> read_loc_gen nat_sz true locked loc ii >>=
                fun v ->
                  M.op Op.Sub v V.one >>=
                  fun v ->
                    (write_loc_gen nat_sz locked loc v ii >>|
                    write_sf v V.zero ii >>|
                    write_zf v V.zero ii) >>! B.Next
          | X86.I_INC (ea) ->
              lval_ea ea ii >>=
              fun loc -> read_loc_gen  nat_sz true locked loc ii >>=
                fun v ->
                  M.add v V.one >>=
                  fun v ->
                    (write_loc_gen  nat_sz locked loc v ii >>|
                    write_sf v V.zero ii >>|
                    write_zf v V.zero ii) >>! B.Next
          |  X86.I_CMP (ea,op) ->
              (rval_ea nat_sz locked ea ii >>| rval_op nat_sz locked op ii) >>=
              fun (v_ea,v_op) ->
                write_all_flags v_ea v_op ii >>! B.Next
          | X86.I_CMOVC (r,ea) ->
              read_reg false X86.CF ii >>*=
              (fun vcf ->
                M.choiceT vcf
                  (rval_ea  nat_sz locked ea ii >>= fun vea -> write_reg r vea ii >>! B.Next)
                  (M.unitT B.Next))
          |  X86.I_JMP lbl -> M.unitT (B.Jump lbl)

(* Conditional branZch, I need to look at doc for
   interpretation of conditions *)
          |  X86.I_JCC (X86.C_LE,lbl) ->
              read_reg false X86.SF ii >>=
              (* control, data ? no event generated after this read anyway *)
              fun sf -> (* LE simply is the negation of GT, given by sign flag *)
                flip_flag sf >>=
                fun v -> B.bccT v lbl
          | X86.I_JCC (X86.C_LT,lbl) ->
              (read_reg false X86.ZF ii >>|
              (read_reg false X86.SF ii >>= flip_flag)) >>=
              fun (v1,v2) ->
                M.op Op.Or v1 v2 >>=
                fun v -> B.bccT v lbl
          | X86.I_JCC (X86.C_GE,lbl) ->
              (read_reg false X86.ZF ii >>| read_reg false X86.SF ii) >>=
              fun (v1,v2) ->
                M.op Op.Or v1 v2 >>=
                fun v -> B.bccT v lbl
          | X86.I_JCC (X86.C_GT,lbl) ->
              read_reg false X86.SF ii >>=
              fun v -> B.bccT v lbl
          | X86.I_JCC (X86.C_EQ,lbl) ->
              read_reg false X86.ZF ii >>=
              fun v -> B.bccT v lbl
          | X86.I_JCC (X86.C_NE,lbl) ->
              read_reg false X86.ZF ii >>= flip_flag >>=
              fun v -> B.bccT v lbl
          | X86.I_JCC (X86.C_S,lbl) ->
              read_reg false X86.SF ii >>=
              fun v -> B.bccT v lbl
          | X86.I_JCC (X86.C_NS,lbl) ->
              read_reg false X86.SF ii >>= flip_flag >>=
              fun v -> B.bccT v lbl

          | X86.I_LOCK inst -> begin
              let open X86 in
              match inst with
              | I_XCHG _ |  I_XCHG_UNLOCKED _
              | I_ADD _  | I_XOR _
              | I_DEC _ | I_INC _ ->
                  build_semantics_inner true {ii with A.inst = inst}
              | _ ->
                  Warn.user_error "Illegal lock prefix on instruction %s"
                    (dump_instruction inst)

          end
          | X86.I_SETNB (ea) ->
              (lval_ea ea ii >>| read_reg false X86.CF ii) >>=
              fun (loc,cf) ->
                flip_flag cf >>=
                fun v -> write_loc nat_sz loc v ii >>! B.Next
          | X86.I_XCHG (ea1,ea2) ->
              xchg  nat_sz  ea1 ea2 ii
          | X86.I_XCHG_UNLOCKED (ea1,ea2) ->
              xchg  nat_sz ea1 ea2 ii
          | X86.I_CMPXCHG (_,_) -> Warn.fatal "I_CMPXCHG not implemented"
          | X86.I_LFENCE ->
              create_barrier X86.Lfence ii >>! B.Next
          | X86.I_SFENCE ->
              create_barrier X86.Sfence ii >>! B.Next
          | X86.I_MFENCE ->
              create_barrier X86.Mfence ii >>! B.Next
          |  X86.I_MOVSD -> Warn.fatal "I_MOVSD not implemented"
        in
        M.addT
          (A.next_po_index ii.A.program_order_index)
          (build_semantics_inner false ii)
    end
  end
