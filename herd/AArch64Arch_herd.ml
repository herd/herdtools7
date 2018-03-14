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
module Make (C:sig include Arch_herd.Config val moreedges : bool end) (V:Value.S) =
  struct
    include AArch64Base
    open MachSize

    let is_amo _ = false
    let pp_barrier_short = pp_barrier
    let reject_mixed = true

    type annot = A | XA | L | XL | X | N | Q
    type lannot = annot * MachSize.sz

    let get_machsize (_,sz) = sz

    let empty_annot = N,Quad

    let wrap_is is_fun ((a,sz):lannot) = is_fun a

    let is_barrier b1 b2 = barrier_compare b1 b2 = 0

    let _is_atomic = function
      | XA | XL | X -> true
      | _ -> false

    let is_atomic = wrap_is _is_atomic

    let is_acquire = function
      | A | XA -> true
      | _ -> false

    let is_acquire_pc = function
      | Q -> true
      | _ -> false

    let is_release = function
      | L | XL -> true
      | _ -> false

    let barrier_sets =
      do_fold_dmb_dsb C.moreedges
        (fun b k ->
          let tag = pp_barrier_dot b in
          (tag,is_barrier b)::k)
        ["ISB",is_barrier ISB]

    let annot_sets = [
      "X", is_atomic;
      "A",  wrap_is is_acquire;
      "Q",  wrap_is is_acquire_pc;
      "L",  wrap_is is_release
    ]

    let is_isync = is_barrier ISB
    let pp_isync = "isb"

    let pp_annot (a,_) = match a with
      | XA -> "Acq*"
      | A -> "Acq"
      | Q -> "AcqPc"
      | XL -> "Rel*"
      | L -> "Rel"
      | X -> "*"
      | N -> ""

    module V = V

    include ArchExtra_herd.Make(C)
        (struct
          module V = V

          type arch_reg = reg
          let pp_reg = pp_reg
          let reg_compare = reg_compare

          type arch_instruction = instruction
          let fromto_of_instr _ = None
        end)

  end
