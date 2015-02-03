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

(** Basic arch, ie with no definition of what a global location is *)

module type Config = sig
  val texmacros : bool
  val hexa : bool
  val brackets : bool
end

module type S =
  sig

    include ArchBase.S

    module V : Value.S

    include ArchExtra.S with module I.V = V
    and type I.arch_reg = reg
    and type I.arch_instruction = instruction
   end
      
