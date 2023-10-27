(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)

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
