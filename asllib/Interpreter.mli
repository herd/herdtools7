(******************************************************************************)
(*                           the diy toolsuite                                *)
(*                                                                            *)
(* Jade Alglave, University College London, UK.                               *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
(*                                                                            *)
(* Copyright 2015-present Institut National de Recherche en Informatique et   *)
(* en Automatique and the authors. All rights reserved.                       *)
(*                                                                            *)
(* This software is governed by the CeCILL-B license under French law and     *)
(* abiding by the rules of distribution of free software. You can use,        *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B   *)
(* license as circulated by CEA, CNRS and INRIA at the following URL          *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.              *)
(******************************************************************************)
(* Authors:                                                                   *)
(* Hadrien Renaud, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
(* Jade Alglave, Arm Ltd and UCL, UK.                                         *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

(** Reference interpreter for ASL. *)

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
