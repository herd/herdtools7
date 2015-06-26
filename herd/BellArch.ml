(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Define Bell architecture *)

module Make (C:Arch.Config) (V:Value.S) = struct
  include BellBase

  module V = V

  include ArchExtra.Make(C)        
      (struct
	module V = V 
            
	type arch_reg = reg
	let pp_reg = pp_reg
	let reg_compare = reg_compare
            
	type arch_instruction = instruction
        let fromto_of_instr ins = match ins with
          | Pfence(Fence(_,Some o)) -> Some o
          | _ -> None
      end)
  end
