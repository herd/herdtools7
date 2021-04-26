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
module Make (C:Arch_herd.Config) (V:Value.S) =
  struct

    include
      MakeAArch64Base.Make
        (struct let is_morello = C.variant Variant.Morello end)

    let is_kvm = C.variant Variant.Kvm


    let is_amo _ = false
    let pp_barrier_short = pp_barrier
    let reject_mixed = true

    type annot = A | XA | L | XL | X | N | Q | NoRet | T | S
    type nexp =  AF|DB|Other
    type explicit = Exp | NExp of nexp
    type lannot = annot

    let empty_annot = N
    let tag_annot = T
    let exp_annot = Exp
    let nexp_annot = NExp Other

    let is_barrier b1 b2 = barrier_compare b1 b2 = 0

    let is_speculated = function
      | S -> true
      | _ -> false

    let _is_atomic = function
      | XA | XL | X | NoRet -> true
      | _ -> false

    let is_atomic = _is_atomic

    let is_noreturn = function
      | NoRet -> true
      | _ -> false

    let is_acquire = function
      | A | XA -> true
      | _ -> false

    let is_acquire_pc = function
      | Q -> true
      | _ -> false

    let is_release = function
      | L | XL -> true
      | _ -> false

    let is_tag = function
      | T -> true
      | _ -> false

    let is_explicit = function
      | Exp -> true
      | _ -> false
    and is_not_explicit = function
      | NExp _-> true
      | _ -> false
    and is_af = function (* Setting of access flag *)
      | NExp AF-> true
      | _ -> false
    and is_db = function (* Setting of dirty bit flag *)
      | NExp DB -> true
      | _ -> false

    let is_out_of_the_box = is_af

    let barrier_sets =
      do_fold_dmb_dsb true
        (fun b k ->
          let tag = pp_barrier_dot b in
          (tag,is_barrier b)::k)
        ["ISB",is_barrier ISB]

    let annot_sets = [
      "X", is_atomic;
      "A",  is_acquire;
      "Q",  is_acquire_pc;
      "L",  is_release;
      "NoRet", is_noreturn;
      "T", is_tag;
      "S", is_speculated;
    ]

    let explicit_sets = [
      "Exp", is_explicit;
      "NExp", is_not_explicit;
      "AF", is_af;
      "DB", is_db;
    ]

    let is_isync = is_barrier ISB
    let pp_isync = "isb"

    let pp_annot a = match a with
      | XA -> "Acq*"
      | A -> "Acq"
      | Q -> "AcqPc"
      | XL -> "Rel*"
      | L -> "Rel"
      | X -> "*"
      | N -> ""
      | NoRet -> "NoRet"
      | T -> "Tag"
      | S -> "^s"

    let pp_explicit = function
      | Exp -> if is_kvm && C.verbose > 2 then "Exp" else ""
      | NExp Other-> "NExp"
      | NExp AF-> "NExpAF"
      | NExp DB-> "NExpDB"
    module V = V

    let neon_mask esize =
      let mask = match esize with
      | 8 -> "0xff"
      | 16 -> "0xffff"
      | 32 -> "0xffffffff"
      | 64 -> "0xffffffffffffffff"
      | _ -> assert false in
      V.stringToV mask

    let neon_getlane cur_val esize idx =
      let mask = V.op1 (Op.LeftShift (idx*esize)) (neon_mask esize) in
      let masked_val = V.op Op.And mask cur_val in
      V.op1 (Op.LogicalRightShift (idx*esize)) masked_val

    let neon_getvec nelem esize v =
      let get_concrete v = match v with
      | V.Val c -> c
      | _ -> assert false in
      let rec get_rec idx  =
        if idx < nelem then
          get_concrete (neon_getlane v esize idx)::
            get_rec (idx+1)
        else [] in
      let vs = get_rec 0 in
      V.Val (Constant.ConcreteVector(nelem, vs))

    let simd_mem_access_size rs = match List.hd rs with
    | Vreg (_,(_,8)) -> MachSize.Byte
    | Vreg (_,(_,16)) -> MachSize.Short
    | Vreg (_,(_,32)) -> MachSize.Word
    | Vreg (_,(_,64)) -> MachSize.Quad
    | _ -> assert false (* Unsupported arrangement specifier *)

    let mem_access_size = function
      | I_LDR (v,_,_,_,_) | I_LDP (_,v,_,_,_,_)
      | I_LDUR (v,_,_,_)  | I_LDR_P(v,_,_,_)
      | I_STR (v,_,_,_) | I_STLR (v,_,_) | I_STXR (v,_,_,_,_)
      | I_STP (_,v,_,_,_,_)
      | I_CAS (v,_,_,_,_) | I_SWP (v,_,_,_,_)
      | I_LDOP (_,v,_,_,_,_) | I_STOP (_,v,_,_,_) ->
          Some (tr_variant v)
      | I_LDR_SIMD (v,_,_,_,_) | I_LDR_P_SIMD (v,_,_,_)
      | I_LDP_SIMD (_,v,_,_,_,_) | I_LDP_P_SIMD (_,v,_,_,_,_)
      | I_STR_SIMD (v,_,_,_,_) | I_STR_P_SIMD (v,_,_,_)
      | I_STP_SIMD (_,v,_,_,_,_) | I_STP_P_SIMD (_,v,_,_,_,_)
      | I_LDUR_SIMD (v,_,_,_) | I_STUR_SIMD (v,_,_,_) ->
          Some (tr_simd_variant v)
      | I_LD1 (r,_,_,_) | I_LD1R (r,_,_) | I_ST1 (r,_,_,_) ->
          Some (simd_mem_access_size [r])
      | I_LD1M (rs,_,_) | I_ST1M (rs,_,_)
      | I_LD2 (rs,_,_,_) | I_LD2R (rs,_,_) | I_ST2 (rs,_,_,_)
      | I_LD2M (rs,_,_) | I_ST2M (rs,_,_)
      | I_LD3 (rs,_,_,_) | I_LD3R (rs,_,_) | I_ST3 (rs,_,_,_)
      | I_LD3M (rs,_,_) | I_ST3M (rs,_,_)
      | I_LD4 (rs,_,_,_) | I_LD4R (rs,_,_) | I_ST4 (rs,_,_,_)
      | I_LD4M (rs,_,_) | I_ST4M (rs,_,_) ->
          Some (simd_mem_access_size rs)
      | I_LDRBH (v,_,_,_) | I_LDARBH (v,_,_,_)
      | I_STRBH (v,_,_,_) | I_STLRBH (v,_,_) | I_STXRBH (v,_,_,_,_)
      | I_CASBH (v,_,_,_,_) | I_SWPBH (v,_,_,_,_)
      | I_LDOPBH (_,v,_,_,_,_) | I_STOPBH (_,v,_,_,_) ->
          Some (bh_to_sz v)
      | I_NOP|I_B _|I_BR _|I_BC (_, _)|I_CBZ (_, _, _)
      | I_CBNZ (_, _, _)|I_BL _|I_BLR _|I_RET _|I_LDAR (_, _, _, _)
      | I_TBNZ(_,_,_,_) | I_TBZ (_,_,_,_) | I_MOVZ (_,_,_,_) | I_MOVK(_,_,_,_)
      |I_MOV (_, _, _)|I_SXTW (_, _)|I_OP3 (_, _, _, _, _, _)
      | I_ADDR (_, _)|I_RBIT (_, _, _)|I_FENCE _
      | I_CSEL (_, _, _, _, _, _)|I_IC (_, _)|I_DC (_, _)|I_MRS (_, _)
      | I_STG _ | I_STZG _ | I_LDG _
      | I_ALIGND _| I_ALIGNU _|I_BUILD _|I_CHKEQ _|I_CHKSLD _|I_CHKTGD _
      | I_CLRTAG _|I_CPYTYPE _|I_CPYVALUE _|I_CSEAL _|I_GC _|I_LDCT _|I_SEAL _
      | I_STCT _|I_UNSEAL _
      | I_SC _
      | I_TLBI (_,_)
      | I_MOV_V _ | I_MOV_VE _ | I_MOV_S _ | I_MOV_TG _ | I_MOV_FG _
      | I_MOVI_S _ | I_MOVI_V _
      | I_EOR_SIMD _ | I_ADD_SIMD _ | I_ADD_SIMD_S _
          -> None

    include ArchExtra_herd.Make(C)
        (struct
          module V = V
          let endian = endian

          type arch_reg = reg
          let pp_reg = pp_reg
          let reg_compare = reg_compare

          type arch_instruction = instruction
          let fromto_of_instr _ = None

          let get_val reg v = match reg with
          | AArch64Base.Vreg(_,(nelem,esize)) -> neon_getvec nelem esize v
          | _ -> v

        end)

  end
