(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)

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
