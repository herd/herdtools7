(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Define MIPS architecture *)

module Make (C:Arch_herd.Config) (V:Value.S) =
  struct
    include MIPSBase
    let is_amo _ = false
    let pp_barrier_short = pp_barrier
    let reject_mixed = false

    type lannot = bool (* atomicity *)
    let get_machsize _ = V.Cst.Scalar.machsize
        
    let empty_annot = false
    let is_atomic annot = annot 

    let barrier_sets = ["SYNC",(function Sync -> true);]
    let annot_sets = ["X", is_atomic]

    let is_isync _ = false
    let pp_isync = "???"

    let pp_annot annot = 
      if annot then "*" else ""

    module V = V

(* Technically wrong, but it does not matter as there is no mixed-size *)
    let mem_access_size _ = None

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
