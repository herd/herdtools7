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

module
  Make
    (C:Sem.Config)
    (V:Value.S with type Cst.Instr.exec = X86_64Base.instruction)
  =
  struct
    module X86_64 = X86_64Arch_herd.Make(SemExtra.ConfigToArchConfig(C))(V)
    module Act = MachAction.Make(C.PC)(X86_64)
    include SemExtra.Make(C)(X86_64)(Act)
    let mixed = C.variant Variant.Mixed
    (* barrier pretty print *)
    let barriers =
      let mfence = {barrier=X86_64.MFENCE; pp="mfence";}
      and sfence = {barrier=X86_64.SFENCE; pp="sfence";}
      and lfence = {barrier=X86_64.LFENCE; pp="lfence";} in
      [mfence; sfence; lfence;]
    let isync = None
    let nat_sz = V.Cst.Scalar.machsize
    let is_global = A.is_global

    let atomic_pair_allowed e1 e2 = match e1.E.iiid, e2.E.iiid with
      | E.IdSome i1,E.IdSome i2 -> i1 == i2
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

      let mk_read sz an loc v =
        let ac = Act.access_of_location_std loc in
        Act.Access (Dir.R, loc, v, an, (), sz, ac)

      let read_loc port sz = M.read_loc port (mk_read sz X86_64.Plain)

      let plain = X86_64.Plain
      and atomic = X86_64.Atomic

      let atomic_when_global loc = if is_global loc then atomic else plain

      let mk_read_choose_atomic sz loc =
        mk_read sz (atomic_when_global loc) loc

      let mask_from_reg_part = function
        | X86_64.R8bH -> fun w -> M.op1 (Op.LogicalRightShift 8) w >>=
                                    fun v -> M.op1 (Op.UnSetXBits (56, 8)) v
        | X86_64.R8bL -> fun v -> M.op1 (Op.UnSetXBits (56, 8)) v
        | X86_64.R16b -> fun v -> M.op1 (Op.UnSetXBits (48, 16)) v
        | X86_64.R32b -> fun v -> M.op1 (Op.UnSetXBits (32, 32)) v
        | X86_64.R64b -> M.unitT

      let inst_size_to_reg_size = function
        | X86_64.I8b  -> X86_64.R8bL
        | X86_64.I16b -> X86_64.R16b
        | X86_64.I32b -> X86_64.R32b
        | X86_64.I64b |X86_64.INSb -> X86_64.R64b

      let get_inst_size inst =
        let open X86_64 in
           match inst with
           | I_NOP | I_RET | I_FENCE _ | I_LOCK _ | I_JMP _ | I_JCC _
           | I_MOVNTDQA _ | I_CLFLUSH _ -> INSb
           | I_EFF_OP (_, sz, _, _) | I_EFF (_, sz, _) | I_EFF_EFF (_, sz, _, _)
           | I_CMPXCHG (sz, _, _) | I_CMOVC (sz, _, _)
           | I_MOVNTI (sz,_,_) | I_MOVD (sz,_,_) -> sz

      let read_reg port r ii =
        match r with
        | X86_64.Ireg (_, p) ->
            let sz = reg_size_to_mach_size p in
            read_loc port sz
              (A.Location_reg (ii.A.proc,r)) ii >>= mask_from_reg_part p
        | _ ->
            read_loc port nat_sz (A.Location_reg (ii.A.proc,r)) ii

      let read_reg_ord = read_reg Port.No
      and read_reg_data = read_reg Port.Data
      and read_reg_addr = read_reg Port.Addr

      let read_mem port sz an a ii =
        if mixed then
          Mixed.read_mixed port sz (fun sz -> mk_read sz an) a ii
        else
          let a = A.Location_global a in
          M.read_loc port (mk_read sz an) a ii

      let read_mem_atomic port sz a ii = read_mem port sz X86_64.Atomic a ii

      let read_loc_gen port sz locked loc ii =
        begin
          match loc with
          | A.Location_global l -> read_mem port sz locked l ii
          | A.Location_reg (_, reg) -> read_reg port reg ii
        end
        >>= mask_from_reg_part
              (match port with
               | Port.(No|Data) ->
                   inst_size_to_reg_size (get_inst_size ii.X86_64.inst)
               | Port.Addr -> X86_64.R64b)

      let read_loc_atomic sz is_d loc ii =
        read_loc_gen sz is_d (atomic_when_global loc) loc ii

      let mk_write sz an loc v =
        let ac = Act.access_of_location_std loc in
        Act.Access (Dir.W, loc, v, an, (), sz, ac)

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
          | X86_64.R32b
          | X86_64.R64b -> fun _a -> M.unitT V.zero
        in
        match r with
        | X86_64.Ireg (_, p) ->
           let sz = reg_size_to_mach_size p in
           read_reg_ord r ii >>=
             normalize_register_and_value p >>=
             fun nr -> M.op1 (Op.LeftShift (if p = X86_64.R8bH then 8 else 0)) v >>=
             fun nv -> M.op Op.Or nr nv >>=
             fun w -> write_loc sz plain (A.Location_reg (ii.A.proc,r)) w ii
        | _ -> write_loc nat_sz plain (A.Location_reg (ii.A.proc,r)) v ii

      let write_loc_gen sz an loc v ii = match loc with
        | A.Location_global l -> write_mem sz an l v ii
        | A.Location_reg (_, reg) -> write_reg reg v ii

      let write_mem_atomic sz a v ii = write_mem sz atomic a v ii

      let write_loc_atomic sz loc v ii =
        write_loc_gen sz (atomic_when_global loc) loc v ii

      let write_flag r o v1 v2 ii =
        M.addT (A.Location_reg (ii.A.proc,r)) (M.op o v1 v2) >>=
          (fun (loc,v) ->
            write_loc (reg_size_to_mach_size (X86_64.get_reg_size r)) plain loc v ii)

      let create_barrier b ii =
        M.mk_singleton_es (Act.Barrier b) ii

      let lval_port =
        let open X86_64 in
        function
        | Effaddr_rm64 (Rm64_reg _|Rm64_abs _) -> Port.No
        | Effaddr_rm64 (Rm64_deref _|Rm64_scaled _) -> Port.Addr

      let check_data =
        let open Port in
        fun ea -> match lval_port ea with Addr -> Data | No -> No | Data -> assert false

      let lval_ea ea ii = match ea with
        | X86_64.Effaddr_rm64 (X86_64.Rm64_reg r)->
           M.unitT (X86_64.Location_reg (ii.A.proc,r))
        | X86_64.Effaddr_rm64 (X86_64.Rm64_deref (r,o)) ->
           read_reg_addr  r ii >>=
             fun v -> M.add v (V.intToV o) >>=
             fun vreg -> M.unitT (X86_64.Location_global vreg)
        | X86_64.Effaddr_rm64 (X86_64.Rm64_scaled (o1,r1,r2,o2)) ->
           (read_reg_addr r1 ii >>= fun v -> M.add v (V.intToV o1)) >>|
           (read_reg_addr r2 ii >>= fun v -> M.op Op.Mul v (V.intToV o2))
          >>= fun (vreg,a) -> M.add vreg a
          >>= fun vreg -> M.unitT (X86_64.Location_global vreg)
        | X86_64.Effaddr_rm64 (X86_64.Rm64_abs v)->
           M.unitT (X86_64.maybev_to_location v)

      let rval_ea port sz locked ea ii =
        lval_ea ea ii >>= fun loc -> read_loc_gen port sz locked loc ii

      let rval_op port sz locked op ii = match op with
        | X86_64.Operand_effaddr ea  -> rval_ea port sz locked ea ii
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

      let xchg sz ea1 ea2 ii =
        (lval_ea ea1 ii >>| lval_ea ea2 ii) >>=
          (fun (l1,l2) ->
            let r1 = read_loc_atomic (check_data ea2) sz l1 ii
            and r2 = read_loc_atomic (check_data ea1) sz l2 ii
            and w1 = fun v -> write_loc_atomic sz l1 v ii
            and w2 = fun v -> write_loc_atomic sz l2 v ii in
            M.exch r1 r2 w1 w2) >>= B.next2T

      let cmpxchg sz locked ea r ii =
        lval_ea ea ii
        >>= fun loc_ea -> read_loc_gen Port.No sz locked loc_ea ii
        >>=
          fun v_ea -> lval_ea (X86_64.Effaddr_rm64 (X86_64.Rm64_reg (X86_64.Ireg (X86_64.AX,X86_64.R64b)))) ii >>=
          fun loc_ra -> read_loc_gen Port.Data sz locked loc_ra ii >>=
          fun v_ra -> write_zf v_ea v_ra ii >>=
          fun _ -> rval_ea Port.No sz locked (X86_64.Effaddr_rm64 (X86_64.Rm64_reg r)) ii >>=
          fun v_r -> M.op Op.Eq v_ea v_ra >>=
          (fun vcf ->
            M.choiceT vcf
              (write_loc_gen sz locked loc_ea v_r ii)
              (write_loc_gen sz locked loc_ra v_ea ii))
                     >>= B.next1T

      let do_op sz locked x86_op ea op ii =
        let module A = X86_64 in
        let o = match x86_op with
          | A.I_ADD -> Op.Add
          | A.I_XOR -> Op.Xor
          | A.I_OR  -> Op.Or
          | A.I_AND -> Op.And
          | A.I_SHL -> Op.ShiftLeft
          | (A.I_MOV|A.I_CMP) -> assert false in
        (lval_ea ea ii >>=
           fun loc ->
           let port = check_data ea in
           M.addT loc (read_loc_gen port sz locked loc ii)
           >>| rval_op port sz locked op ii)
        >>=
          fun ((loc,v_ea),v_op) ->
          M.op o v_ea v_op >>=
          fun v_result ->
          (write_loc_gen sz locked loc v_result ii >>|
             write_all_flags v_result V.zero ii) >>= B.next2T

      let clflush opt ea ii =
        lval_ea ea ii >>= fun a ->
        M.mk_singleton_es
          (Act.Arch (X86_64.ArchAction.ClFlush (opt,a))) ii

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
                M.unitT () >>! B.Exit
              else
                B.indirectBranchT v lbls bds
        | _ -> Warn.fatal
            "illegal argument for the indirect branch instruction %s \
            (must be a label)" (X86_64.dump_instruction i)

      let build_semantics test ii =
        let rec build_semantics_inner locked ii =
          match ii.A.inst with
          | X86_64.I_NOP -> B.nextT
          | X86_64.I_RET as i when C.variant Variant.Telechat ->
            read_reg_ord X86_64.RIP ii
            >>= do_indirect_jump test [] i
          | X86_64.I_EFF_OP (X86_64.I_CMP, sz, ea, op) ->
             let sz = inst_size_to_mach_size sz in
             (rval_ea Port.No sz locked ea ii >>|
              rval_op Port.No sz locked op ii) >>=
               fun (v_ea,v_op) ->
               write_all_flags v_ea v_op ii >>= B.next1T
          | X86_64.I_EFF_OP (X86_64.I_MOV, sz, ea, op) ->
             let sz = inst_size_to_mach_size sz in
             (lval_ea ea ii >>| rval_op (check_data ea) sz locked op ii) >>=
               fun (loc,v_op) ->
               write_loc_gen sz locked loc v_op ii >>= B.next1T
(* TODO add NTI annotation, at movnti is an ordinary store *)
          | X86_64.I_MOVNTI (sz,ea,r) ->
              let sz = inst_size_to_mach_size sz in
              (lval_ea ea ii >>| read_reg (check_data ea) r ii) >>=
              fun (loc,v) ->
              write_loc_gen sz X86_64.NonTemporal loc v ii >>= B.next1T
          | X86_64.I_EFF_OP (x86_op, sz, ea, op) ->
             let sz = inst_size_to_mach_size sz in
             do_op sz locked x86_op ea op ii (* Problem, it's not always xor but the parameter of I_EFF_OP *)
          | X86_64.I_EFF (X86_64.I_SETNB, sz, ea) ->
             let sz = inst_size_to_mach_size sz in
             (lval_ea ea ii >>|
              read_reg (check_data ea) (X86_64.Flag X86_64.CF) ii) >>=
               fun (loc,cf) ->
               flip_flag cf >>=
               fun v -> write_loc sz plain loc v ii >>= B.next1T
          | X86_64.I_EFF (inst, sz, ea) ->
             let sz = inst_size_to_mach_size sz in
             lval_ea ea ii >>=
               fun loc -> read_loc_gen (check_data ea) sz locked loc ii >>=
               fun v ->
                 begin
                   match inst with
                   | X86_64.I_DEC -> M.op Op.Sub v V.one
                   | X86_64.I_INC -> M.op Op.Add v V.one
                   | _ -> assert false
                 end >>=
               fun v ->
               (write_loc_gen sz locked loc v ii >>|
                  write_sf v V.zero ii >>|
                  write_zf v V.zero ii) >>= B.next3T
          | X86_64.I_CMOVC (sz,r,ea) ->
             let sz = inst_size_to_mach_size sz in
             read_reg_ord (X86_64.Flag X86_64.CF) ii >>*=
               (fun vcf ->
                 M.choiceT vcf
                   (rval_ea Port.No sz locked ea ii
                    >>= fun vea -> write_reg r vea ii >>= B.next1T)
                   B.nextT)
          |  X86_64.I_JMP lbl -> B.branchT lbl

          (* Conditional branch, I need to look at doc for
             interpretation of conditions *)
          |  X86_64.I_JCC (X86_64.C_LE,lbl) ->
              read_reg_ord (X86_64.Flag X86_64.SF) ii >>=
                (* control, data ? no event generated after this read anyway *)
                fun sf -> (* LE simply is the negation of GT, given by sign flag *)
                flip_flag sf >>=
                fun v -> B.bccT v lbl
          | X86_64.I_JCC (X86_64.C_LT,lbl) ->
             (read_reg_ord (X86_64.Flag X86_64.ZF) ii >>|
                (read_reg_ord (X86_64.Flag X86_64.SF) ii >>= flip_flag)) >>=
               fun (v1,v2) ->
               M.op Op.Or v1 v2 >>=
               fun v -> B.bccT v lbl
          | X86_64.I_JCC (X86_64.C_GE,lbl) ->
             (read_reg_ord (X86_64.Flag X86_64.ZF) ii >>| read_reg_ord (X86_64.Flag X86_64.SF) ii) >>=
               fun (v1,v2) ->
               M.op Op.Or v1 v2 >>=
               fun v -> B.bccT v lbl
          | X86_64.I_JCC (X86_64.C_GT,lbl) ->
             read_reg_ord (X86_64.Flag X86_64.SF) ii >>=
               fun v -> B.bccT v lbl
          | X86_64.I_JCC (X86_64.C_EQ,lbl) ->
             read_reg_ord (X86_64.Flag X86_64.ZF) ii >>=
               fun v -> B.bccT v lbl
          | X86_64.I_JCC (X86_64.C_NE,lbl) ->
             read_reg_ord (X86_64.Flag X86_64.ZF) ii >>= flip_flag >>=
               fun v -> B.bccT v lbl
          | X86_64.I_JCC (X86_64.C_S,lbl) ->
             read_reg_ord (X86_64.Flag X86_64.SF) ii >>=
               fun v -> B.bccT v lbl
          | X86_64.I_JCC (X86_64.C_NS,lbl) ->
             read_reg_ord (X86_64.Flag X86_64.SF) ii >>= flip_flag >>=
               fun v -> B.bccT v lbl

          | X86_64.I_LOCK inst -> begin
              let open X86_64 in
              match inst with
              | I_EFF_EFF _
              | I_EFF_OP ((I_ADD | I_XOR), _, _, _)
              | I_EFF ((I_DEC | I_INC),  _, _)
              | I_CMPXCHG _ ->
                 build_semantics_inner atomic {ii with A.inst = inst}
              | _ ->
                 Warn.user_error "Illegal lock prefix on instruction %s"
                   (dump_instruction inst)
            end
          | X86_64.I_EFF_EFF (_,sz,ea1,ea2) ->
             let sz = inst_size_to_mach_size sz in
             xchg  sz ea1 ea2 ii
          | X86_64.I_CMPXCHG (sz,ea,r) ->
             let sz = inst_size_to_mach_size sz in
             cmpxchg sz locked ea r ii
          | X86_64.I_FENCE f ->
              create_barrier f ii >>= B.next1T
          | X86_64.I_CLFLUSH (opt,ea) ->
              clflush opt ea ii >>= B.next1T
          | X86_64.I_MOVD _
          | X86_64.I_RET
          | X86_64.I_MOVNTDQA _ as i ->
              Warn.fatal "X86_64Sem.ml: Instruction %s not implemented" (X86_64.dump_instruction i)
        in
        M.addT
          (A.next_po_index ii.A.program_order_index)
          (build_semantics_inner plain ii)

    let spurious_setaf _ = assert false

    end

  end
