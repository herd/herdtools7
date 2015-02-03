(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Define PTX architecture *)

module Make (C:Arch.Config) (V:Value.S) = struct
    include GPU_PTXBase

    let is_barrier b1 b2 = barrier_compare b1 b2 = 0

    let arch_fences =
      [
       "membar.cta",is_barrier (Membar CTA_bar);
       "membar.gl",is_barrier (Membar GL_bar);
       "membar.sys",is_barrier (Membar SYS_bar);
     ]


(* Now global locations, that include reservations *)
    module V = V

    include ArchExtra.Make(C)        
	(struct
	  module V = V 

	  type arch_reg = reg
	  let pp_reg = pp_reg
	  let reg_compare = reg_compare

	  type arch_instruction = instruction
	end)
  end
