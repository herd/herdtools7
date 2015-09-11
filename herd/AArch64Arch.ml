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
module Make (C:Arch.Config) (V:Value.S) =
  struct
    include AArch64Base

    type lannot = A | XA | L | XL | X | N
					 
    let empty_annot = N

    let is_barrier b1 b2 = barrier_compare b1 b2 = 0
						     
    let is_atomic = function
      | XA | XL | X -> true
      | _ -> false

    let is_acquire = function
      | A | XA -> true
      | _ -> false

    let is_release = function
      | L | XL -> true
      | _ -> false

    let barrier_sets = 
      do_fold_dmb_dsb
        (fun b k ->
          let tag = pp_barrier_dot b in
          (tag,is_barrier b)::k)
        ["ISB",is_barrier ISB]

    let annot_sets = [
      "X", is_atomic;
      "A", is_acquire;
      "L", is_release
    ]

    let is_isync = is_barrier ISB
    let pp_isync = "isb"

    let pp_annot = function
      | XA -> "Acq*"
      | A -> "Acq"
      | XL -> "Rel*"
      | L -> "Rel"
      | X -> "*"
      | N -> ""

    module V = V

    include ArchExtra.Make(C)
	(struct
	  module V = V 

	  type arch_reg = reg
	  let pp_reg = pp_reg
	  let reg_compare = reg_compare

	  type arch_instruction = instruction
          let fromto_of_instr _ = None
	end)
	  
  end
