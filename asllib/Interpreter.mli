(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
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
  module IEnv : Env.S with type v = B.value and module Scope = B.Scope

  type value_read_from = B.value * AST.identifier * B.Scope.t

  type 'a maybe_exception =
    | Normal of 'a
    | Throwing of (value_read_from * AST.ty) option * IEnv.env

  val eval_expr :
    IEnv.env -> AST.expr -> (B.value * IEnv.env) maybe_exception B.m

  val run_typed_env :
    (AST.identifier * B.value) list -> StaticEnv.global -> AST.t -> B.value B.m
  (** [run env0 tenv ast] runs the function main of the ast,
      in the typing environment [tenv]. However, the (global)
      identifiers listed in the A-list [env0] will take their
      initial values from [env0]  and _not_ from [ast]. *)

  val run_typed : StaticEnv.global -> AST.t -> B.value B.m
  (** [run_typed ast env] runs the function main of the typed-checked [ast], in
      typed-checking environment [env]. *)
end

module type Config = sig
  module Instr : Instrumentation.SEMINSTR

  val unroll : int
  (** Loop unrolling threshold *)

  val error_handling_time : Error.error_handling_time
  (** When are error filed. *)

  val log_nondet_choice : bool
  (** Log to stderr non-deterministic choices. *)
end

module Make (B : Backend.S) (C : Config) : S with module B = B
