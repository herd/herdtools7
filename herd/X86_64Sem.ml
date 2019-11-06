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

(** Semantics of X86_64 instructions *)

module Make (C:Sem.Config)(V : Value.S)
  =
  struct
    module X86_64 = X86_64Arch_herd.Make(SemExtra.ConfigToArchConfig(C))(V)
    module Act = MachAction.Make(C.PC)(X86_64)
    include SemExtra.Make(C)(X86_64)(Act)
    let mixed = C.variant Variant.Mixed
    (* barrier pretty print *)
    let mfence = {barrier=X86_64.Mfence; pp="mfence";}
    let barriers = [mfence]
    let isync = None
    let nat_sz = V.Cst.Scalar.machsize
    let is_global = A.is_global

    let atomic_pair_allowed e1 e2 = match e1.E.iiid, e2.E.iiid with
      | Some i1,Some i2 -> i1 == i2
      | _,_ -> false


    (* semantics proper *)
    module Mixed(SZ : ByteSize.S) = struct
      module Mixed = M.Mixed(SZ)

      let (>>=) = M.(>>=)
      let (>>*=) = M.(>>*=)
      let (>>|) = M.(>>|)
      let (>>!) = M.(>>!)

      let inst_size_to_mach_size = X86_64.inst_size_to_mach_size

      let reg_size_to_mach_size sz = match sz with
        | X86_64.R8bL | X86_64.R8bH -> MachSize.Byte
        | X86_64.R16b -> MachSize.Short
        | X86_64.R32b -> MachSize.Word
        | X86_64.R64b -> MachSize.Quad

      let mk_read sz ato loc v = Act.Access (Dir.R, loc, v, ato, sz)

      let read_loc sz is_d = M.read_loc is_d (mk_read sz false)

      let mk_read_choose_atomic sz loc = mk_read sz (is_global loc) loc

      let mask_from_reg_part = function
        | X86_64.R8bH -> Printf.printf "8H\n";fun w -> M.op1 (Op.LogicalRightShift 8) w >>=
                                                         fun v -> M.op1 (Op.UnSetXBits (56, 8)) v
        | X86_64.R8bL -> Printf.printf "8L\n"; fun v -> M.op1 (Op.UnSetXBits (56, 8)) v
        | X86_64.R16b -> Printf.printf "16\n"; fun v -> M.op1 (Op.UnSetXBits (48, 16)) v
        | X86_64.R32b -> Printf.printf "32\n"; fun v -> M.op1 (Op.UnSetXBits (32, 32)) v
        | X86_64.R64b -> Printf.printf "64\n"; fun v -> M.op1 (Op.UnSetXBits (0, 0)) v

      let inst_size_to_reg_size = function
        | X86_64.I8b  -> X86_64.R8bL
        | X86_64.I16b -> X86_64.R16b
        | X86_64.I32b -> X86_64.R32b
        | X86_64.I64b |X86_64.INSb -> X86_64.R64b

      let get_inst_size inst =
        let open X86_64 in
           match inst with
           | I_NOP | I_MFENCE | I_LOCK _ | I_JMP _ | I_JCC _ -> INSb
           | I_EFF_OP (_, sz, _, _) | I_EFF (_, sz, _) | I_EFF_EFF (_, sz, _, _)
             | I_CMPXCHG (sz, _, _) | I_CMOVC (sz, _, _) -> sz

      let read_reg is_data r ii =
        Printf.printf "READ_REG\n";
        if is_data then
          match r with
          | X86_64.Ireg (_, p) ->
             let sz = reg_size_to_mach_size p in
             read_loc sz is_data (A.Location_reg (ii.A.proc,r)) ii >>= mask_from_reg_part p
          | _ -> read_loc nat_sz is_data (A.Location_reg (ii.A.proc,r)) ii
        else
          read_loc nat_sz is_data (A.Location_reg (ii.A.proc,r)) ii

      let read_mem sz data an a ii =
        if mixed then
          Mixed.read_mixed data sz (fun sz -> mk_read sz an) a ii
        else
          let a = A.Location_global a in
          M.read_loc data (mk_read sz an) a ii

      let read_mem_atomic sz a ii = read_mem sz false true a ii

      let read_loc_atomic sz is_d = M.read_loc is_d (mk_read_choose_atomic sz)

      let read_loc_gen sz data locked loc ii = begin
        match loc with
        | A.Location_global l -> read_mem sz data locked l ii
        | A.Location_reg (_, reg) -> read_reg data reg ii
        | _ -> M.read_loc data (mk_read sz false) loc ii
        end >>= mask_from_reg_part (if data
                                    then (inst_size_to_reg_size (get_inst_size ii.X86_64.inst))
                                    else X86_64.R64b)

      let mk_write sz an loc v = Act.Access (Dir.W, loc, v, an, sz)

      let write_loc sz an loc v ii =
        M.mk_singleton_es (mk_write sz an loc v) ii

      let write_mem sz an a v ii =
        if mixed then Mixed.write_mixed sz (fun sz -> mk_write sz an) a v ii
        else write_loc sz an (A.Location_global a) v ii

      let write_reg r v ii =
        (* Spec from intel manual :
           - 64-bit operands generate a 64-bit result in the destination
             general-purpose register.
           - 32-bit operands generate a 32-bit result, zero-extended to a 64-bit
             result in the destination general-purpose register.
           - 8-bit and 16-bit operands generate an 8-bit or 16-bit result.
             The upper 56 bits or 48 bits (respectively) of the destination
             general-purpose register are not be modified by the operation.
         *)
        let normalize_register_and_value = function
          | X86_64.R8bH -> fun a -> M.op1 (Op.UnSetXBits (8, 8)) a
          | X86_64.R8bL -> fun a -> M.op1 (Op.UnSetXBits (8, 0)) a
          | X86_64.R16b -> fun a -> M.op1 (Op.UnSetXBits (16, 0)) a
          | X86_64.R32b -> fun a -> M.op1 (Op.UnSetXBits (64, 0)) a
          | X86_64.R64b -> fun a -> M.op1 (Op.UnSetXBits (64, 0)) a
        in
        match r with
        | X86_64.Ireg (_, p) ->
           let sz = reg_size_to_mach_size p in
           read_reg false r ii >>=
             normalize_register_and_value p >>=
             fun nr -> M.op1 (Op.LeftShift (if p = X86_64.R8bH then 8 else 0)) v >>=
             fun nv -> M.op Op.Or nr nv >>=
             fun w -> write_loc sz false (A.Location_reg (ii.A.proc,r)) w ii
        | _ -> write_loc nat_sz false (A.Location_reg (ii.A.proc,r)) v ii

      let write_loc_gen sz locked loc v ii = match loc with
        | A.Location_global l -> write_mem sz locked l v ii
        | A.Location_reg (_, reg) -> write_reg reg v ii
        | _ -> write_loc sz locked loc v ii

      let write_mem_atomic sz a v ii = write_mem sz true a v ii

      let write_loc_atomic sz loc v ii =
        write_loc sz (is_global loc) loc v ii

      let write_flag r o v1 v2 ii =
        M.addT (A.Location_reg (ii.A.proc,r)) (M.op o v1 v2) >>=
          (fun (loc,v) ->
            write_loc (reg_size_to_mach_size (X86_64.get_reg_size r)) false loc v ii)

      let create_barrier b ii =
        M.mk_singleton_es (Act.Barrier b) ii

      let lval_ea ea ii = match ea with
        | X86_64.Effaddr_rm64 (X86_64.Rm64_reg r)->
           M.unitT (X86_64.Location_reg (ii.A.proc,r))
        | X86_64.Effaddr_rm64 (X86_64.Rm64_deref (r,o)) ->
           read_reg false r ii >>=
             fun v -> M.add v (V.intToV o) >>=
             fun vreg -> M.unitT (X86_64.Location_global vreg)
        | X86_64.Effaddr_rm64 (X86_64.Rm64_abs v)->
           M.unitT (X86_64.maybev_to_location v)

      let rval_ea sz locked ea ii = lval_ea ea ii >>= fun loc -> read_loc_gen sz true locked loc ii

      let rval_op sz locked op ii = match op with
        | X86_64.Operand_effaddr ea  -> rval_ea sz locked ea ii
        | X86_64.Operand_immediate s -> M.unitT (V.intToV s)

      let flip_flag v = M.op Op.Xor v V.one
      (* Set flags by comparing v1 v2 *)
      let write_zf v1 v2 ii =  write_flag (X86_64.Flag X86_64.ZF) Op.Eq v1 v2 ii
      let write_sf v1 v2 ii =  write_flag (X86_64.Flag X86_64.SF) Op.Gt v1 v2 ii

      let write_all_flags v1 v2 ii =
        (write_zf v1 v2 ii >>| write_sf v1 v2 ii >>|
           write_flag (X86_64.Flag X86_64.CF) Op.Eq V.zero V.one ii) (* Carry was always zero! *)
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
          | X86_64.I_NOP -> M.unitT B.Next
          | X86_64.I_EFF_OP (X86_64.I_CMP, sz, ea, op) ->
             let sz = inst_size_to_mach_size sz in
             (rval_ea sz locked ea ii >>| rval_op sz locked op ii) >>=
               fun (v_ea,v_op) ->
               write_all_flags v_ea v_op ii >>! B.Next
          | X86_64.I_EFF_OP (X86_64.I_MOV, sz, ea, op) ->
             let sz = inst_size_to_mach_size sz in
             (lval_ea ea ii >>| rval_op sz locked op ii) >>=
               fun (loc,v_op) ->
               write_loc_gen sz locked loc v_op ii >>! B.Next
          | X86_64.I_EFF_OP (_, sz, ea, op) ->
             let sz = inst_size_to_mach_size sz in
             do_op sz locked Op.Xor ea op ii (* Problem, it's not always xor but the parameter of I_EFF_OP *)
          | X86_64.I_EFF (X86_64.I_SETNB, sz, ea) ->
             let sz = inst_size_to_mach_size sz in
             (lval_ea ea ii >>| read_reg false (X86_64.Flag X86_64.CF) ii) >>=
               fun (loc,cf) ->
               flip_flag cf >>=
               fun v -> write_loc sz false loc v ii >>! B.Next
          | X86_64.I_EFF (inst, sz, ea) ->
             let sz = inst_size_to_mach_size sz in
             lval_ea ea ii >>=
               fun loc -> read_loc_gen sz true locked loc ii >>=
               fun v -> begin
                   if inst = X86_64.I_DEC
                   then M.op Op.Sub v V.one
                   else M.add v V.one
                 end >>=
               fun v ->
               (write_loc_gen sz locked loc v ii >>|
                  write_sf v V.zero ii >>|
                  write_zf v V.zero ii) >>! B.Next
          | X86_64.I_CMOVC (sz,r,ea) ->
             let sz = inst_size_to_mach_size sz in
             read_reg false (X86_64.Flag X86_64.CF) ii >>*=
               (fun vcf ->
                 M.choiceT vcf
                   (rval_ea  sz locked ea ii >>= fun vea -> write_reg r vea ii >>! B.Next)
                   (M.unitT B.Next))
          |  X86_64.I_JMP lbl -> M.unitT (B.Jump lbl)

          (* Conditional branZch, I need to look at doc for
   interpretation of conditions *)
          |  X86_64.I_JCC (X86_64.C_LE,lbl) ->
              read_reg false (X86_64.Flag X86_64.SF) ii >>=
                (* control, data ? no event generated after this read anyway *)
                fun sf -> (* LE simply is the negation of GT, given by sign flag *)
                flip_flag sf >>=
                fun v -> B.bccT v lbl
          | X86_64.I_JCC (X86_64.C_LT,lbl) ->
             (read_reg false (X86_64.Flag X86_64.ZF) ii >>|
                (read_reg false (X86_64.Flag X86_64.SF) ii >>= flip_flag)) >>=
               fun (v1,v2) ->
               M.op Op.Or v1 v2 >>=
               fun v -> B.bccT v lbl
          | X86_64.I_JCC (X86_64.C_GE,lbl) ->
             (read_reg false (X86_64.Flag X86_64.ZF) ii >>| read_reg false (X86_64.Flag X86_64.SF) ii) >>=
               fun (v1,v2) ->
               M.op Op.Or v1 v2 >>=
               fun v -> B.bccT v lbl
          | X86_64.I_JCC (X86_64.C_GT,lbl) ->
             read_reg false (X86_64.Flag X86_64.SF) ii >>=
               fun v -> B.bccT v lbl
          | X86_64.I_JCC (X86_64.C_EQ,lbl) ->
             read_reg false (X86_64.Flag X86_64.ZF) ii >>=
               fun v -> B.bccT v lbl
          | X86_64.I_JCC (X86_64.C_NE,lbl) ->
             read_reg false (X86_64.Flag X86_64.ZF) ii >>= flip_flag >>=
               fun v -> B.bccT v lbl
          | X86_64.I_JCC (X86_64.C_S,lbl) ->
             read_reg false (X86_64.Flag X86_64.SF) ii >>=
               fun v -> B.bccT v lbl
          | X86_64.I_JCC (X86_64.C_NS,lbl) ->
             read_reg false (X86_64.Flag X86_64.SF) ii >>= flip_flag >>=
               fun v -> B.bccT v lbl

          | X86_64.I_LOCK inst -> begin
              let open X86_64 in
              match inst with
              | I_EFF_EFF _
                | I_EFF_OP ((I_ADD | I_XOR), _, _, _)
                | I_EFF ((I_DEC | I_INC),  _, _) ->
                 build_semantics_inner true {ii with A.inst = inst}
              | _ ->
                 Warn.user_error "Illegal lock prefix on instruction %s"
                   (dump_instruction inst)
            end
          | X86_64.I_EFF_EFF (_,sz,ea1,ea2) ->
             let sz = inst_size_to_mach_size sz in
             xchg  sz ea1 ea2 ii
          | X86_64.I_CMPXCHG (_,_,_) -> Warn.fatal "I_CMPXCHG not implemented"
          | X86_64.I_MFENCE ->
             create_barrier X86_64.Mfence ii >>! B.Next
        in
        M.addT
          (A.next_po_index ii.A.program_order_index)
          (build_semantics_inner false ii)
    end
  end
