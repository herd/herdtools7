(*********************************************************************)
(*                        herd                                      *)
(*                                                                   *)
(*               Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Define MIPS architecture *)

module Make (C:Arch.Config) (V:Value.S) =
  struct
    include MIPSBase

    type lannot = bool (* atomicity *)

    let empty_annot = false
    let is_atomic annot = annot 

    let barrier_sets = ["SYNC",(function Sync -> true);]
    let annot_sets = ["X", is_atomic]

    let is_isync _ = false
    let pp_isync = "???"

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
