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

module type S = sig
  module A : Arch_litmus.S

  val is_ret : A.instruction -> bool
  val is_nop : A.instruction -> bool
  val extract_addrs : A.instruction -> Global_litmus.Set.t
  val stable_regs : A.instruction -> A.RegSet.t
  val emit_loop : A.Out.ins list -> A.Out.ins list
  val compile_ins :
      (Label.t -> string) ->
        A.instruction ->  A.Out.ins list -> A.Out.ins list
end
