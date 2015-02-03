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

(** Define PPC architecture *)

module Make (C:Arch.Config) (V:Value.S)
=
  struct
    include PPCBase

    let is_barrier b1 b2 = barrier_compare b1 b2 = 0

    let arch_sets =
      [
       "SYNC",is_barrier Sync;
       "ISYNC",is_barrier Isync;
       "LWSYNC",is_barrier Lwsync;
       "EIEIO",is_barrier Eieio;
     ]

    let is_isync = is_barrier Isync
    let pp_isync = "isync"

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
    
