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

open Code

module Make(Cfg:CompileCommon.Config) : XXXCompile_gen.S =
  struct

    open MachSize

    let mach_size =
      let open TypBase in
      match Cfg.typ with
      | Std (_,Quad) -> Quad
      | Int | Std (_,Word) -> Word
      | Std (_,Short) -> Short
      | Std (_,Byte) -> Byte

    let size_to_inst_size =
      let open X86_64Base in
      function
      | Byte -> I8b
      | Short -> I16b
      | Word -> I32b
      | Quad -> I64b

    let size_to_reg_size =
      let open X86_64Base in
      function
      | Byte -> R8bL
      | Short -> R16b
      | Word -> R32b
      | Quad -> R64b

    let size_reg_part = size_to_reg_size mach_size

    let size = size_to_inst_size mach_size

    let naturalsize = TypBase.get_size Cfg.typ
    module X86_64 = X86_64Arch_gen.Make
        (struct
          let naturalsize = naturalsize
          let fullmixed = Cfg.variant Variant_gen.FullMixed
        end)
    include CompileCommon.Make(Cfg)(X86_64)
    open X86_64

    let inst_to_reg_size = function
      | I8b -> R8bL
      | I16b -> R16b
      | I32b | INSb -> R32b
      | I64b -> R64b

    (******)
    let ppo _f k = k
    (******)

    let next_reg x = alloc_reg x

    let mov r i =
      let r = change_size_reg r size_reg_part in
      I_EFF_OP (I_MOV, size, Effaddr_rm64 (Rm64_reg r), Operand_immediate i)
    let mov_mixed sz r i =
      let sz = size_to_inst_size sz in
      let r = change_size_reg r (inst_to_reg_size sz) in
      I_EFF_OP (I_MOV, sz,
                Effaddr_rm64 (Rm64_reg r), Operand_immediate i)

    let mov_reg r1 r2 =
      let r1 = change_size_reg r1 size_reg_part
      and r2 = change_size_reg r2 size_reg_part in
      I_EFF_OP (I_MOV, size, Effaddr_rm64 (Rm64_reg r1),
                Operand_effaddr (Effaddr_rm64 (Rm64_reg r2)))
    let mov_reg_mixed sz r1 r2 =
      let sz = size_to_inst_size sz in
      let r1 = change_size_reg r1 (inst_to_reg_size sz)
      and r2 = change_size_reg r2 (inst_to_reg_size sz) in
      I_EFF_OP (I_MOV, sz,
                Effaddr_rm64 (Rm64_reg r1),
                Operand_effaddr (Effaddr_rm64 (Rm64_reg r2)))

    module Extra = struct
      let use_symbolic = false
      type reg = X86_64.reg
      type instruction = X86_64.pseudo

      let mov r i = Instruction (mov r i)
      let mov_mixed sz r i = Instruction (mov_mixed sz r i)
      let mov_reg r1 r2 = Instruction (mov_reg r1 r2)
      let mov_reg_mixed sz r1 r2 = Instruction (mov_reg_mixed sz r1 r2)
    end

    module U = GenUtils.Make(Cfg)(A)(Extra)

    let pseudo = List.map (fun i -> X86_64.Instruction i)

    let emit_store_ins sz _ rB v =
      I_EFF_OP (I_MOV, sz,
                Effaddr_rm64 rB,
                Operand_immediate v)

    let emit_store_ins_reg sz _ rB rC =
      I_EFF_OP (I_MOV, sz,
                Effaddr_rm64 rB,
                Operand_effaddr (Effaddr_rm64 (Rm64_reg rC)))

    let emit_store_nti_ins_reg sz o rB rC =
      I_MOVNTI (sz,Effaddr_rm64 (Rm64_deref (rB,o)),rC)


    let emit_store_mixed sz o st p init addr v =
      let isz = size_to_inst_size sz in
      let rB,init,st =
        if o <> 0 then
          let r,i,s = U.next_init st p init addr in
          let r = change_size_reg r R64b in
          Rm64_deref (r,o),i,s
        else
          Rm64_abs (ParsedConstant.nameToV addr),init,st in
      let r_opt,init,st = U.emit_const st p init v in
      match r_opt with
      | None ->
          init,pseudo [emit_store_ins isz o rB v],st
      | Some rC ->
          let rC = change_size_reg rC (size_to_reg_size sz) in
          init,pseudo [emit_store_ins_reg isz o rB rC],st

    let emit_store st p init addr v =
      emit_store_mixed mach_size 0 st p init addr v

    let emit_store_nti_mixed  sz o st p init addr v =
      let rA,init,st = U.next_init st p init addr in
      let rA = change_size_reg rA R64b in
      let rV,init,cv,st = U.emit_mov st p init v in
      let rV = change_size_reg rV (size_to_reg_size sz) in
      init,cv@pseudo [emit_store_nti_ins_reg (size_to_inst_size sz) o rA rV],st


    let emit_store_nti st p init addr v = emit_store_nti_mixed  mach_size 0 st p init addr v

    let emit_movntdqa_ins xmm rA =
      I_MOVNTDQA (xmm,Effaddr_rm64 (Rm64_deref (rA,0)))

    let emit_movd_ins sz r xmm = I_MOVD (sz,r,xmm)

    let emit_load_nti sz st p init addr =
      let rA,init,st = U.next_init st p init addr in
      let r64,st = next_reg st in
      let r = change_size_reg r64 (size_to_reg_size sz) in
      let xmm,st = alloc_special st in
      let c = [emit_movntdqa_ins xmm rA;emit_movd_ins (size_to_inst_size sz) r xmm] in
      r64,init,pseudo c,st


    let emit_sta sz addr r v =
      let rsz = size_to_reg_size sz
      and isz = size_to_inst_size sz in
      let r = change_size_reg r rsz in
      [
        I_EFF_OP
          (I_MOV, isz,
           Effaddr_rm64 (Rm64_reg r), Operand_immediate v);
        I_EFF_EFF
          (I_XCHG, isz,
           Effaddr_rm64 (Rm64_abs (ParsedConstant.nameToV addr)),
           Effaddr_rm64 (Rm64_reg r))
      ]

    let emit_sta_mixed sz o st p init addr v =
      let rsz = size_to_reg_size sz
      and isz = size_to_inst_size sz in
      let r_opt,init,st = U.emit_const st p init v in
      let r64,st = next_reg st in
      let r = change_size_reg r64 rsz in
      let imov =
        match r_opt with
        | None ->
            I_EFF_OP
              (I_MOV, isz, Effaddr_rm64 (Rm64_reg r), Operand_immediate v)
        | Some rc ->
            let rc = change_size_reg rc rsz in
            I_EFF_OP
              (I_MOV, isz, Effaddr_rm64 (Rm64_reg r),
               Operand_effaddr (Effaddr_rm64 (Rm64_reg rc))) in

      let init,iexch,st =
        match o with
        | 0 ->
            let iexch =
              I_EFF_EFF
              (I_XCHG, isz,
               Effaddr_rm64 (Rm64_abs (ParsedConstant.nameToV addr)),
               Effaddr_rm64 (Rm64_reg r)) in
          init,iexch,st
      | _ ->
          let rbase,init,st = U.next_init st p init addr in
          let iexch =
            I_EFF_EFF
              (I_XCHG, isz,
               Effaddr_rm64 (Rm64_deref (rbase,o)),
               Effaddr_rm64 (Rm64_reg r)) in
          init,iexch,st in
      r64,init,pseudo [imov;iexch;],st

    let emit_cmp_zero_ins r =
      let r = change_size_reg r size_reg_part in
      I_EFF_OP
        (I_CMP, size, Effaddr_rm64 (Rm64_reg r), Operand_immediate 0)

    and emit_cmp_one_ins r =
      let r = change_size_reg r size_reg_part in
      I_EFF_OP
        (I_CMP, size, Effaddr_rm64 (Rm64_reg r), Operand_immediate 1)

    and emit_cmp_int_ins r i =
      let r = change_size_reg r size_reg_part in
      I_EFF_OP
        (I_CMP, size, Effaddr_rm64 (Rm64_reg r), Operand_immediate i)

    and emit_je_ins lab =
      I_JCC (C_EQ,lab)

    and emit_jne_ins lab =
      I_JCC (C_NE,lab)

    let emit_load_ins sz _ rm r =
      let r = change_size_reg r (inst_to_reg_size (size_to_inst_size sz)) in
      I_EFF_OP
        (I_MOV, size_to_inst_size  sz,
         Effaddr_rm64 (Rm64_reg r),
         Operand_effaddr (Effaddr_rm64 rm))

    let emit_load_mixed sz o st p init x =
      let rA,st = next_reg st in
      let rB,init,st =
        if o <> 0 then
          let r,i,s = U.next_init st p init x in
          let r = change_size_reg r R64b in
          Rm64_deref (r,o),i,s
        else
          Rm64_abs (ParsedConstant.nameToV x),init,st in
      rA,init,pseudo [emit_load_ins sz o rB rA],st

    let emit_load st p init x =
      emit_load_mixed mach_size 0 st p init x

    let emit_load_not_zero st _p init x =
      let rA,st = next_reg st in
      let rB = Rm64_abs (ParsedConstant.nameToV x) in
      let lab = Label.next_label "L" in
      rA,init,
      Label (lab,Nop)::
        pseudo
          [emit_load_ins mach_size 0 rB rA ;
           emit_cmp_zero_ins rA ;
           emit_je_ins lab],
      st

    let emit_load_one st _p init x =
      let rA,st = next_reg st in
      let rB = Rm64_abs (ParsedConstant.nameToV x) in
      let lab = Label.next_label "L" in
      rA,init,
      Label (lab,Nop)::
        pseudo
          [emit_load_ins mach_size 0 rB rA ;
           emit_cmp_one_ins rA ;
           emit_jne_ins lab],
      st

    let emit_load_not  _st _p _init _x _cmp =
      Warn.fatal "Loop observers not implemented for X86_64"

    let emit_load_not_eq  st =  emit_load_not st
    let emit_load_not_value  st = emit_load_not st

    let emit_obs _ = emit_load_mixed naturalsize 0
    let emit_obs_not_value = emit_load_not_value
    let emit_obs_not_eq = emit_load_not_eq
    let emit_obs_not_zero = emit_load_not_zero

    let emit_joker st init = None,init,[],st

    let emit_access st _p init e = match e.C.dir with
      | None -> Warn.fatal "TODO"
      | Some d ->
         begin match e.C.loc with
         | Data loc ->
            begin match d with
            | R ->
               begin match e.C.atom with
               | None|Some (Plain,None) ->
                  let r,init,cs,st = emit_load st _p init loc  in
                  Some r,init, cs,st
               | Some (Atomic,_) ->
                  Warn.fatal "No atomic load for X86_64"
               | Some (Plain,Some (sz, o)) ->
                  let r,init,cs,st = emit_load_mixed sz o st _p init loc  in
                  Some r,init,cs,st
               | Some (NonTemporal,None) ->
                   let r,init,cs,st = emit_load_nti mach_size st _p init loc  in
                   Some r,init,cs,st
               | Some (NonTemporal,Some (sz,0)) ->
                   let r,init,cs,st = emit_load_nti sz st _p init loc  in
                   Some r,init,cs,st
               | Some (NonTemporal,Some _) ->
                   Warn.fatal "Illegal non-temporal load"
               end
            | W ->
               begin match e.C.atom with
               | None|Some (Plain,None) ->
                  let init,cs,st = emit_store st _p init loc e.C.v in
                  None,init,cs,st
               | Some (NonTemporal,None) ->
                   let init,cs,st = emit_store_nti st _p init loc e.C.v in
                   None,init,cs,st
               | Some (Atomic,None) ->
                   let rX,st = next_reg st in
                   Some rX,init,pseudo (emit_sta mach_size loc rX e.C.v), st
               | Some (NonTemporal,Some (sz,o)) ->
                   let init,cs,st =
                     emit_store_nti_mixed sz o st _p init loc e.C.v in
                   None,init,cs,st
               | Some (Atomic,Some (sz,o)) ->
                   let r,init,cs,st =
                     emit_sta_mixed sz o st _p init loc e.C.v in
                  Some r,init,cs,st
               | Some (Plain,Some (sz, o)) ->
                  let init,cs,st = emit_store_mixed sz o st _p init loc e.C.v in
                  None,init,cs,st
               end
            | J -> emit_joker st init
            end
         | Code _ -> Warn.fatal "No code location for X86_64"
         end

    let emit_exch st _p init er ew =
      let rA,st = next_reg st in
      rA,init,
      pseudo  (emit_sta mach_size (Code.as_data er.C.loc) rA ew.C.v),
      st

    let emit_rmw () st p init er ew  =
      let rR,init,cs,st = emit_exch st p init er ew in
      Some rR,init,cs,st

    let emit_access_dep _st _p _init _e _r1 =
      Warn.fatal "Dependent access is irrelevant for X86_64"

    let emit_exch_dep _st =
      Warn.fatal "Dependent access is irrelevant for X86_64"

    let emit_rmw_dep () =  emit_exch_dep

    let emit_fence st _ init _ f = init,[X86_64.Instruction (I_FENCE f)],st

    let full_emit_fence = (*GenUtils.to_full*) emit_fence

    let stronger_fence = MFENCE

    (* Check load *)
    let do_check_load p st r e =
      let lab = Label.exit p (current_label st) in
      (fun k ->
        Instruction (emit_cmp_int_ins r e.C.v)::
          Instruction (emit_jne_ins lab)::
            k),
      next_label_st st

    let check_load  p r e init st =
      let cs,st = do_check_load p st r e in
      init,cs,st

    (* Postlude *)

    let does_jump lab cs =
      List.exists
        (fun i -> match i with
                  | Instruction (I_JMP lab0|I_JCC (_,lab0)) ->
                     (lab0:string) = lab
                  | _ -> false)
        cs

    let does_exit p cs st =  does_jump (Label.exit p (current_label st)) cs

    let list_of_exit_labels p st =
      let rec do_rec i k =
        match i with
        | 0 -> k
        | n -> let k' = Label (Label.exit p n,Nop)::k
               in do_rec (i-1) k'
      in
      do_rec (current_label st) []

    let postlude st p init cs =
      if does_exit p cs st then
        init,cs@(list_of_exit_labels p st),st
      else init,cs,st

    let get_xstore_results _ = []

(* Info computation, compute extra alignement constraints *)
    let add_info n k =
      let e = n.C.evt in
      match e.C.loc,e.C.dir,e.C.atom with
      | Data x,Some R,Some (NonTemporal,_) ->
          let v =
            try
              let old = StringMap.find x k in
              max 16 old
            with Not_found -> 16 in
          StringMap.add x v k
      | _,_,_ -> k

    let get_archinfo n =
      let i = C.fold add_info n StringMap.empty in
      let i =
        StringMap.fold (fun x a k -> Printf.sprintf "%s:%i" x a::k) i [] in
      ["Align",String.concat "," i;]
  end
