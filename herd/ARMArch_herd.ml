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

(** Define ARM architecture *)

module Make (C:Arch_herd.Config) (V:Value.S) =
  struct
    include ARMBase
    let is_amo _ = false
    let pp_barrier_short = pp_barrier
    let reject_mixed = false

    type lannot = bool (* atomicity *)
    let get_machsize _ = V.Cst.Scalar.machsize (* No mixed size instruction *)

    let empty_annot = false

    let is_barrier b1 b2 = barrier_compare b1 b2 = 0
    let is_atomic annot = annot

    let barrier_sets =
      [
       "DMB",is_barrier (DMB SY);
       "DSB",is_barrier (DSB SY);
       "DMB.ST",is_barrier (DMB ST);
       "DSB.ST",is_barrier (DSB ST);
       "ISB", is_barrier ISB;
     ]
    let annot_sets = ["X",is_atomic]

    let is_isync = is_barrier ISB
    let pp_isync = "isb"

    let pp_annot annot =
      if annot then "*" else ""

    module V = V

    let mem_access_size = function
      | I_NOP | I_ADD _ | I_ADD3 _ | I_SUB _ | I_SUB3 _ | I_AND _
      | I_B _ | I_BEQ _ | I_BNE _ | I_CB _ | I_CMPI _  | I_CMP _
      | I_MOVI _ | I_MOV _ | I_XOR _ | I_DMB _ | I_DSB _ | I_ISB
      | I_SADD16 _ | I_SEL _
        -> None
      | I_LDR _ | I_LDREX _ | I_LDR3 _ | I_STR _ | I_STREX _ | I_STR3 _
        -> Some MachSize.Word

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
