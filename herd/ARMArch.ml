(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Define ARM architecture *)

module Make (C:Arch.Config) (V:Value.S) =
  struct
    include ARMBase

    type lannot = bool (* atomicity *)

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
