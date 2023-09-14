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
(****************************************************************************)

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
