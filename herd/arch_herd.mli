(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Basic arch, ie with no definition of what a global location is *)

module type Config = sig
  val texmacros : bool
  val hexa : bool
  val brackets : bool
  val variant : Variant.t -> bool
  val endian : Endian.t option
end

module type S =
  sig

    include ArchBase.S
    val is_amo : instruction -> bool
    val pp_barrier_short : barrier -> string
    val reject_mixed : bool (* perform a check that rejects mixed-size tests *)
    val mem_access_size : instruction -> MachSize.sz option

    module V : Value.S
    include ArchExtra_herd.S with module I.V = V
    and type I.arch_reg = reg
    and type I.arch_instruction = instruction
   end
      
