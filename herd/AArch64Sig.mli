(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2022-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Signatures of AArch64 "Sem" modules *)

module type SubConfig = sig
  include GenParser.Config
  include Top_herd.CommonConfig
  include Sem.Config

  val libfind : string -> string
  val byte : MachSize.Tag.t
end

module type Config = sig
  module C : SubConfig
  val dirty : DirtyBit.t option
  val procs_user : Proc.t list
end

module type Semantics =
  Sem.Semantics
  with type A.instruction = AArch64Base.instruction
  and type A.parsedInstruction = AArch64Base.parsedInstruction
  and type A.reg = AArch64Base.reg
  and type 'ins A.kpseudo = 'ins AArch64Base.kpseudo


module type MakeSemantics =
  functor(C:Config) ->
  functor(V:Value.AArch64) ->
  Semantics with module A.V = V
