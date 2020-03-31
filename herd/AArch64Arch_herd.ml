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
    include AArch64Base

    let is_amo _ = false
    let pp_barrier_short = pp_barrier
    let reject_mixed = true

    type annot = A | XA | L | XL | X | N | Q | NoRet | T  | S | NExp
    type lannot = annot

    let empty_annot = N
    let tag_annot = T

    let wrap_is is_fun a = is_fun a

    let is_barrier b1 b2 = barrier_compare b1 b2 = 0

    let is_speculated = function 
      | S -> true
      | _ -> false

    let _is_atomic = function
      | XA | XL | X | NoRet -> true
      | _ -> false

    let is_atomic = wrap_is _is_atomic

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

    let is_not_explicit = function
      | NExp -> true
      | _ -> false

    let barrier_sets =
      do_fold_dmb_dsb true
        (fun b k ->
          let tag = pp_barrier_dot b in
          (tag,is_barrier b)::k)
        ["ISB",is_barrier ISB]

    let annot_sets = [
      "X", is_atomic;
      "A",  wrap_is is_acquire;
      "Q",  wrap_is is_acquire_pc;
      "L",  wrap_is is_release;
      "NoRet", wrap_is is_noreturn;
      "T", wrap_is is_tag;
      "S", is_speculated;
      "NExp", wrap_is is_not_explicit
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
      | NExp -> "NExp"

    module V = V

    let mem_access_size = function
      | I_LDR (v,_,_,_) | I_LDP (_,v,_,_,_,_)
      | I_STR (v,_,_,_) | I_STLR (v,_,_) | I_STXR (v,_,_,_,_)
      | I_STP (_,v,_,_,_,_)
      | I_CAS (v,_,_,_,_) | I_SWP (v,_,_,_,_)
      | I_LDOP (_,v,_,_,_,_) | I_STOP (_,v,_,_,_) ->
          Some (tr_variant v)
      | I_LDRBH (v,_,_,_) | I_LDARBH (v,_,_,_)
      | I_STRBH (v,_,_,_) | I_STLRBH (v,_,_) | I_STXRBH (v,_,_,_,_)
      | I_CASBH (v,_,_,_,_) | I_SWPBH (v,_,_,_,_)
      | I_LDOPBH (_,v,_,_,_,_) | I_STOPBH (_,v,_,_,_) ->
          Some (bh_to_sz v)
      | I_NOP|I_B _|I_BR _|I_BC (_, _)|I_CBZ (_, _, _)
      | I_CBNZ (_, _, _)|I_BL _|I_BLR _|I_RET _|I_LDAR (_, _, _, _)
      | I_MOV (_, _, _)|I_SXTW (_, _)|I_OP3 (_, _, _, _, _)
      | I_ADDR (_, _)|I_RBIT (_, _, _)|I_FENCE _
      | I_CSEL (_, _, _, _, _, _)|I_IC (_, _)|I_DC (_, _)|I_MRS (_, _)
      | I_STG _ | I_LDG _
      | I_TLBI (_,_)
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

        end)

  end
