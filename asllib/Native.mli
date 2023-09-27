(******************************************************************************)
(*                                ASLRef                                      *)
(*                                                                            *)
(* Copyright (c) 2022-present, Arm Limited or its affiliates.                 *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(* SPDX-License-Identifier: Apache-2.0                                        *)
(******************************************************************************)
(* Authors:                                                                   *)
(* Hadrien Renaud, University College London, UK.                             *)
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

(* Sequential ASL interpreter using straight OCaml values as backend. *)

type native_value =
  | NV_Literal of AST.literal
  | NV_Vector of native_value list
  | NV_Record of native_value ASTUtils.IMap.t

module NativeBackend :
  Backend.S with type value = native_value and type 'a m = 'a

module NativePrimitives : sig
  val primitives : NativeBackend.primitive AST.t
end

module NativeInterpreter (C : Interpreter.Config) :
  Interpreter.S with module B = NativeBackend

val interprete :
  Typing.strictness ->
  ?instrumentation:bool ->
  ?static_env:StaticEnv.env ->
  NativeBackend.primitive AST.t ->
  int * Instrumentation.semantics_rule list
