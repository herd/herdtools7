(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


(* Some utilities *)

module Hash : functor(O:Warn.Config) ->
  sig
    open Answer
    val mk_hash_info : string -> MiscParser.info  -> hash
    val hash_ok : hash_env -> string -> hash -> bool
  end

module Pseudo : functor(A:Arch_litmus.S) ->
  PseudoAbstract.S with
type ins = A.instruction
and type code = MiscParser.proc * A.pseudo list
