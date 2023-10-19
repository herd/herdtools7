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

module Types = struct
  type annot = AArch64Annot.t
  type nexp =  AF|DB|AFDB|Other
  type explicit = Exp | NExp of nexp
  type lannot = annot
end

module Make (C:Arch_herd.Config)(V:Value.AArch64) =
  struct

    include
      MakeAArch64Base.Make
        (struct let is_morello = C.variant Variant.Morello end)

    let is_kvm = C.variant Variant.VMSA


    let is_amo _ = false
    let pp_barrier_short = pp_barrier
    let reject_mixed = true

    include Types

    let empty_annot = AArch64Annot.N
    let exp_annot = Exp
    let nexp_annot = NExp Other

    let is_atomic = AArch64Annot.is_atomic

    let is_explicit_annot = function
      | Exp -> true
      | NExp _ -> false

    and is_not_explicit_annot = function
      | NExp _ -> true
      | Exp -> false

    let is_barrier b1 b2 = barrier_compare b1 b2 = 0

    let is_af = function (* Setting of access flag *)
      | NExp (AF|AFDB)-> true
      | NExp (DB|Other)|Exp -> false

    and is_db = function (* Setting of dirty bit flag *)
      | NExp (DB|AFDB) -> true
      | NExp (AF|Other)|Exp -> false

    module CMO = struct
      type t = | DC of AArch64Base.DC.op | IC of AArch64Base.IC.op

      let pp cmo loc =
        let loc = (Misc.pp_opt_arg Fun.id loc) in
        match cmo with
        | DC op ->
           Printf.sprintf "DC(%s%s)" (AArch64Base.DC.pp_op op) loc
        | IC op ->
           Printf.sprintf "IC(%s%s)" (AArch64Base.IC.pp_op op) loc
    end

    (* Holds of an instruction iff modifying it or with it while it is being
    * fetched is subject to special restrictions.
    * Returns false iff its argument is any of:
    *     B, B.cond, BL, BRK, CBNZ, CBZ, HVC, ISB, NOP, SMC, SVC, TBNZ and TBZ
    * For the other instructions, a concurrent modification and an execution
    * represent a conflict. The list is taken from:
    *   Arm ARM B2.2.5 "Concurrent modification and execution of instructions" 
    *)
    let is_cmodx_restricted_instruction = function
    | I_B _| I_BL _| I_CBNZ _| I_CBZ _| I_FENCE ISB | I_NOP | I_TBNZ _| I_TBZ _
      -> false
    | I_ADD_SIMD _| I_ADD_SIMD_S _| I_ADR _| I_ALIGND _| I_ALIGNU _| I_BC _
    | I_BLR _| I_BR _| I_BUILD _| I_CAS _| I_CASBH _| I_CASP _| I_CHKEQ _| I_CHKSLD _
    | I_CHKTGD _| I_CLRTAG _| I_CPYTYPE _| I_CPYVALUE _| I_CSEAL _| I_CSEL _| I_DC _
    | I_EOR_SIMD _| I_ERET| I_FENCE _| I_GC _| I_IC _| I_LD1 _| I_LD1M _| I_LD1R _
    | I_LD2 _| I_LD2M _| I_LD2R _| I_LD3 _| I_LD3M _| I_LD3R _| I_LD4 _| I_LD4M _
    | I_LD4R _| I_LDAR _| I_LDARBH _| I_LDCT _| I_LDG _| I_LDOP _| I_LDOPBH _
    | I_LDP _| I_LDP_P_SIMD _| I_LDP_SIMD _| I_LDPSW _| I_LDR _| I_LDR_P _| I_LDR_P_SIMD _
    | I_LDR_SIMD _| I_LDRBH _| I_LDRS _| I_LDUR _| I_LDUR_SIMD _| I_LDXP _| I_MOV _
    | I_MOV_FG _| I_MOV_S _| I_MOV_TG _| I_MOV_V _| I_MOV_VE _| I_MOVI_S _
    | I_MOVI_V _| I_MOVK _| I_MOVZ _| I_MOVN _| I_MRS _| I_MSR _| I_OP3 _| I_RBIT _| I_RET _
    | I_SBFM _| I_SC _| I_SEAL _| I_ST1 _| I_ST1M _| I_ST2 _| I_ST2M _| I_ST3 _
    | I_ST3M _| I_ST4 _| I_ST4M _| I_STCT _| I_STG _| I_STLR _| I_STLRBH _| I_STOP _
    | I_STOPBH _| I_STP _| I_STP_P_SIMD _| I_STP_SIMD _| I_STR _| I_STR_P _
    | I_STR_P_SIMD _| I_STR_SIMD _| I_STRBH _| I_STUR_SIMD _| I_STXP _| I_STXR _
    | I_STXRBH _| I_STZG _| I_SWP _| I_SWPBH _| I_SXTW _| I_TLBI _| I_UBFM _
    | I_UDF _| I_UNSEAL _
      -> true

    let is_cmodx_restricted_value = 
      let open Constant in
      function
      | V.Val Instruction i -> is_cmodx_restricted_instruction i
      | V.Val
           (Symbolic _|Concrete _|ConcreteVector _|ConcreteRecord _|
            Label _|Tag _|PteVal _|Frozen _)
      | V.Var _ -> false

    let ifetch_value_sets = [("Restricted-CMODX",is_cmodx_restricted_value)]

    let barrier_sets =
      do_fold_dmb_dsb false true
        (fun b k ->
          let tag = pp_barrier_dot b in
          (tag,is_barrier b)::k)
        ["ISB",is_barrier ISB]

    let cmo_sets =
      DC.fold_op
        (fun op1 k ->
          let tag = DC.pp_dot op1 in
          let p = function
            | CMO.DC op2 -> DC.equal op1 op2
            | _ -> false in
          (tag,p)::k)
        (IC.fold_op
        (fun op1 k ->
          let tag = IC.pp_dot op1 in
          let p = function
            | CMO.IC op2 -> IC.equal op1 op2
            | _ -> false in
          (tag,p)::k) [])


    let annot_sets = AArch64Annot.sets

    let explicit_sets = [
      "AF", is_af;
      "DB", is_db;
    ]

    let pteval_sets =
      if is_kvm then
        let open AArch64PteVal in
        [
          "PTEINV",(fun p -> p.valid=0);
          "PTEV",(fun p -> p.valid=1);
          "PTEAF0",(fun p -> p.af=0);
          "PTEDB0",(fun p -> p.db=0);
        ]
      else []

    let dirty_sets =

         let read_only =
           (fun t p ->
             let open DirtyBit in
             let open AArch64PteVal in
             let af = p.af=1
             and db = p.db=1
             and dbm = p.dbm=1 in
             (af || not af && t.my_ha ()) &&
             (db && (not (t.my_hd ()) || dbm))) in
             [ "ReadOnly",read_only; ]

    let is_isync = is_barrier ISB
    let pp_isync = "isb"

    let pp_annot = AArch64Annot.pp

    let pp_explicit = function
      | Exp -> if is_kvm && C.verbose > 2 then "Exp" else ""
      | NExp Other-> "NExp"
      | NExp AF-> "NExpAF"
      | NExp DB-> "NExpDB"
      | NExp AFDB-> "NExpAFDB"

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
      V.Val (Constant.ConcreteVector vs)

    let simd_mem_access_size rs = match List.hd rs with
    | Vreg (_,(_,8)) -> MachSize.Byte
    | Vreg (_,(_,16)) -> MachSize.Short
    | Vreg (_,(_,32)) -> MachSize.Word
    | Vreg (_,(_,64)) -> MachSize.Quad
    | _ -> assert false (* Unsupported arrangement specifier *)

    let mem_access_size = function
      | I_LDPSW _ -> Some (tr_variant V32)
      | I_LDR (v,_,_,_,_) | I_LDP (_,v,_,_,_,_,_) | I_LDXP (v,_,_,_,_)
      | I_LDUR (v,_,_,_)  | I_LDR_P(v,_,_,_)
      | I_STR (v,_,_,_,_) | I_STLR (v,_,_) | I_STXR (v,_,_,_,_)
      | I_STR_P (v,_,_,_)
      | I_STP (_,v,_,_,_,_,_) | I_STXP (v,_,_,_,_,_)
      | I_CAS (v,_,_,_,_) | I_CASP (v,_,_,_,_,_,_) | I_SWP (v,_,_,_,_)
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
      | I_LDRBH (v,_,_,_,_) | I_LDARBH (v,_,_,_) | I_LDRS (_,v,_,_)
      | I_STRBH (v,_,_,_,_) | I_STLRBH (v,_,_) | I_STXRBH (v,_,_,_,_)
      | I_CASBH (v,_,_,_,_) | I_SWPBH (v,_,_,_,_)
      | I_LDOPBH (_,v,_,_,_,_) | I_STOPBH (_,v,_,_,_) ->
          Some (bh_to_sz v)
      | I_NOP|I_B _|I_BR _|I_BC (_, _)|I_CBZ (_, _, _)
      | I_CBNZ (_, _, _)|I_BL _|I_BLR _|I_RET _|I_ERET|I_LDAR (_, _, _, _)
      | I_TBNZ(_,_,_,_) | I_TBZ (_,_,_,_) | I_MOVZ (_,_,_,_) | I_MOVK(_,_,_,_)
      | I_MOVN _
      | I_MOV (_, _, _)|I_SXTW (_, _)|I_OP3 (_, _, _, _, _, _)
      | I_ADR (_, _)|I_RBIT (_, _, _)|I_FENCE _
      | I_SBFM (_,_,_,_,_) | I_UBFM (_,_,_,_,_)
      | I_CSEL (_, _, _, _, _, _)|I_IC (_, _)|I_DC (_, _)|I_MRS (_, _)|I_MSR (_, _)
      | I_STG _ | I_STZG _ | I_LDG _
      | I_ALIGND _| I_ALIGNU _|I_BUILD _|I_CHKEQ _|I_CHKSLD _|I_CHKTGD _
      | I_CLRTAG _|I_CPYTYPE _|I_CPYVALUE _|I_CSEAL _|I_GC _|I_LDCT _|I_SEAL _
      | I_STCT _|I_UNSEAL _
      | I_SC _
      | I_TLBI (_,_)
      | I_MOV_V _ | I_MOV_VE _ | I_MOV_S _ | I_MOV_TG _ | I_MOV_FG _
      | I_MOVI_S _ | I_MOVI_V _
      | I_EOR_SIMD _ | I_ADD_SIMD _ | I_ADD_SIMD_S _
      | I_UDF _
          -> None

    let all_regs =
      all_gprs@vregs (* Should be enough, only those are tracked *)

    let opt_env = true

    let killed_idx r = function
      | Idx -> Misc.identity
      | PostIdx|PreIdx -> fun k -> r::k

    let killed i =
      match i with
      | I_LDP (_,_,r1,r2,ra,_,idx) |I_LDPSW (r1,r2,ra,_,idx) ->
          killed_idx ra idx [r1; r2;]
      | I_STP (_,_,_,_,ra,_,idx) ->
          killed_idx ra idx []
      | I_B _| I_BR _
      | I_BC _ | I_CBZ _ | I_CBNZ _
      | I_STR _ | I_STLR _
      | I_STRBH _ | I_STLRBH _
      | I_STOP _ | I_STOPBH _
      | I_FENCE _
      | I_IC _|I_DC _|I_TLBI _
      | I_NOP|I_TBZ _|I_TBNZ _
      | I_BL _ | I_BLR _ | I_RET _ | I_ERET | I_UDF _
        -> [] (* For -variant self only ? *)
      | I_LDR (_,r,_,_,_)|I_LDRBH (_,r,_,_,_)
      | I_LDRS (_,_,r,_)
      | I_LDUR (_,r,_,_)
      | I_LDAR (_,_,r,_) |I_LDARBH (_,_,r,_)
      | I_SWP (_,_,_,r,_) | I_SWPBH (_,_,_,r,_)
      | I_STXR (_,_,r,_,_) | I_STXP (_,_,r,_,_, _) | I_STXRBH (_,_,r,_,_)
      | I_CAS (_,_,r,_,_) | I_CASBH (_,_,r,_,_)
      | I_LDOP (_,_,_,_,r,_) | I_LDOPBH (_,_,_,_,r,_)
      | I_MOV (_,r,_) | I_MOVZ (_,r,_,_) | I_MOVN (_,r,_,_) | I_MOVK (_,r,_,_)
      | I_SXTW (r,_)
      | I_OP3 (_,_,r,_,_,_)
      | I_ADR (r,_)
      | I_RBIT (_,r,_)
      | I_CSEL (_,r,_,_,_,_)
      | I_MRS (r,_)
      | I_STR_P (_,_,r,_)
      | I_UBFM (_,r,_,_,_) | I_SBFM (_,r,_,_,_)
        -> [r]
      | I_MSR (sr,_)
        -> [(SysReg sr)]
      | I_LDR_P (_,r1,r2,_)
      | I_LDXP (_,_,r1,r2,_)
        -> [r1;r2;]
      | I_LD1 _|I_LD1M _|I_LD1R _|I_LD2 _
      | I_LD2M _|I_LD2R _|I_LD3 _|I_LD3M _
      | I_LD3R _|I_LD4 _|I_LD4M _|I_LD4R _
      | I_ST1 _|I_ST1M _|I_ST2 _|I_ST2M _
      | I_ST3 _|I_ST3M _|I_ST4 _|I_ST4M _
      | I_LDP_P_SIMD _|I_STP_P_SIMD _
      | I_LDP_SIMD _|I_STP_SIMD _
      | I_LDR_SIMD _|I_LDR_P_SIMD _
      | I_STR_SIMD _|I_STR_P_SIMD _
      | I_LDUR_SIMD _|I_STUR_SIMD _|I_MOV_VE _
      | I_MOV_V _|I_MOV_TG _|I_MOV_FG _
      | I_MOV_S _|I_MOVI_V _|I_MOVI_S _
      | I_EOR_SIMD _|I_ADD_SIMD _|I_ADD_SIMD_S _
      | I_ALIGND _|I_ALIGNU _
      | I_BUILD _|I_CHKEQ _|I_CHKSLD _|I_CHKTGD _|I_CLRTAG _
      | I_CPYTYPE _|I_CPYVALUE _|I_CSEAL _|I_GC _
      | I_LDCT _|I_SC _|I_SEAL _|I_STCT _
      | I_UNSEAL _|I_STG _|I_STZG _|I_LDG _
      | I_CASP _
        ->
         all_regs (* safe approximation *)

    let get_lx_sz = function
      | I_LDAR (var,(XX|AX),_,_)|I_LDXP (var,_,_,_,_) -> MachSize.Ld (tr_variant var)
      | I_LDARBH (bh,(XX|AX),_,_) -> MachSize.Ld (bh_to_sz bh)
      | I_LDRS (var,_,_,_) -> MachSize.Ld (tr_variant var)
      | I_STXR _|I_STXRBH _ | I_STXP _ -> MachSize.St
      | I_LDAR (_, (AA|AQ), _, _)|I_LDARBH (_, (AA|AQ), _, _)
      | I_NOP|I_B _|I_BR _|I_BC _|I_CBZ _|I_CBNZ _
      | I_TBNZ _|I_TBZ _|I_BL _|I_BLR _|I_RET _|I_ERET
      | I_UBFM _ | I_SBFM _
      | I_LDR _|I_LDUR _|I_LD1 _
      | I_LD1M _|I_LD1R _|I_LD2 _|I_LD2M _
      | I_LD2R _|I_LD3 _|I_LD3M _|I_LD3R _
      | I_LD4 _|I_LD4M _|I_LD4R _|I_ST1 _
      | I_ST1M _|I_ST2 _|I_ST2M _|I_ST3 _
      | I_ST3M _|I_ST4 _|I_ST4M _
      | I_LDP_P_SIMD _|I_STP_P_SIMD _
      | I_LDP_SIMD _|I_STP_SIMD _
      | I_LDR_SIMD _|I_LDR_P_SIMD _
      | I_STR_SIMD _|I_STR_P_SIMD _
      | I_LDUR_SIMD _|I_STUR_SIMD _|I_MOV_VE _
      | I_MOV_V _|I_MOV_TG _|I_MOV_FG _
      | I_MOV_S _|I_MOVI_V _|I_MOVI_S _
      | I_EOR_SIMD _|I_ADD_SIMD _|I_ADD_SIMD_S _
      | I_LDR_P _|I_LDP _|I_LDPSW _|I_STP _
      | I_STR_P _
      | I_STR _|I_STLR _|I_ALIGND _|I_ALIGNU _
      | I_BUILD _|I_CHKEQ _|I_CHKSLD _|I_CHKTGD _|I_CLRTAG _
      | I_CPYTYPE _|I_CPYVALUE _|I_CSEAL _|I_GC _
      | I_LDCT _|I_SC _|I_SEAL _|I_STCT _
      | I_UNSEAL _|I_LDRBH _|I_STRBH _
      | I_STLRBH _|I_CAS _|I_CASBH _
      | I_CASP _
      | I_SWP _|I_SWPBH _|I_LDOP _
      | I_LDOPBH _|I_STOP _|I_STOPBH _
      | I_MOV _|I_MOVZ _|I_MOVN _|I_MOVK _|I_SXTW _
      | I_OP3 _|I_ADR _|I_RBIT _|I_FENCE _
      | I_CSEL _|I_IC _|I_DC _|I_TLBI _|I_MRS _|I_MSR _
      | I_STG _|I_STZG _|I_LDG _|I_UDF _
        -> MachSize.No

    include ArchExtra_herd.Make(C)
        (struct
          module V = V
          let endian = endian

          type arch_reg = reg
          let pp_reg = pp_reg
          let reg_compare = reg_compare

          let fromto_of_instr _ = None

          let get_val reg v = match reg with
          | AArch64Base.Vreg(_,(nelem,esize)) -> neon_getvec nelem esize v
          | _ -> v

          module FaultType = FaultType.AArch64
        end)

    module MemType = MemoryType.No

    module NoConf = struct
      type v = V.v
      type loc = location
      type value_set = V.ValueSet.t
      type solution = V.solution
      type arch_lannot = lannot
      type arch_explicit = explicit
    end

    module ArchAction = ArchAction.No(NoConf)

    module Barrier = AllBarrier.No(struct type a = barrier end)

  end
