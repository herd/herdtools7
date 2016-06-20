(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

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
