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

(** Define X86 architecture *)

module Make (C:Arch_herd.Config)(V:Value.S) =
  struct
    include X86Base
    let is_amo = function
      |I_LOCK _ | I_XCHG _ -> true
      |I_NOP|I_LFENCE|I_SFENCE|I_MFENCE|I_MOVSD|I_JMP _|I_JCC _|I_READ _
      |I_ADD _|I_XOR _|I_OR _|I_MOV _|I_DEC _|I_CMP _|I_CMOVC _
      |I_INC _|I_XCHG_UNLOCKED _|I_CMPXCHG _|I_SETNB _
      |I_MOVB _|I_MOVW _|I_MOVL _|I_MOVQ _|I_MOVT _
          -> false

    let pp_barrier_short = pp_barrier
    let reject_mixed = false

    type lannot = bool (* atomicity *)
    let get_machsize _ = V.Cst.Scalar.machsize
    let empty_annot = false
    let is_atomic annot = annot
    let is_barrier b1 b2 = barrier_compare b1 b2 = 0

    let barrier_sets =
      [
       "MFENCE",is_barrier Mfence;
       "SFENCE",is_barrier Sfence;
       "LFENCE",is_barrier Lfence;
     ]

    let annot_sets = ["X",is_atomic]

    let is_isync _ = false
    let pp_isync = "???"

    let pp_annot annot = if annot then "*" else ""

    module V = V

(* Technically wrong, but it does not matter as there is no mixed-size *)
    let mem_access_size _ = None

    include ArchExtra_herd.Make
        (C)(struct
          module V = V
          let endian = endian

          type arch_reg = reg
          let pp_reg = pp_reg
          let reg_compare = reg_compare

          type arch_instruction = instruction
          let fromto_of_instr _ = None

        end)
  end
