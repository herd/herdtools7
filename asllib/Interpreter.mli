(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
(* Authors:                                                                 *)
(* Hadrien Renaud, University College London, UK.                           *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(* Jade Alglave, Arm Ltd and UCL, UK.                                       *)
(****************************************************************************)

module type S = sig
  module B : Backend.S

  val run_env : (AST.identifier * B.value) list -> B.ast -> B.value B.m
  (** [run env0 ast] runs the function main of the ast,
      in an environment build from the ast. However, the (global)
      identifiers listed in the A-list [env0] will take their
      initial values from [env0]  and _not_ from [ast]. *)

  val run : B.ast -> B.value B.m
  (** Shorthand for [run [] ast] *)

  val run_typed : B.ast -> StaticEnv.env -> B.value B.m
  (** [run_typed ast env] runs the function main of the typed-checked [ast], in
      typed-checking environment [env]. *)
end

module type Config = sig
  module Instr : Instrumentation.SEMINSTR

  val type_checking_strictness : Typing.strictness
  (** The strictness of type-checking. *)

  val unroll : int
  (** Loop unrolling threshold *)
end

module Make (B : Backend.S) (C : Config) : S with module B = B
