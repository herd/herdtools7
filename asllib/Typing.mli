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

(** The Typing module is yet a single-entry-point module. It only exports the
    function [annotate_ast] which fills type-annotations holes in the AST.
    It should provide enough information to disambiguate any type-dependent
    behaviour. *)

(** Possible strictness of type-checking. *)
type strictness = Silence | Warn | TypeCheck | TypeCheckNoWarn

(** Overriding modes *)
type override_mode =
  | Permissive  (** Allow both `impdef` and `implementation` *)
  | NoImplementations  (** Warn if there are any `implementation`s *)
  | AllImpdefsOverridden  (** Warn if an `impdef` has no `implementation` *)

module type ANNOTATE_CONFIG = sig
  val check : strictness
  val output_format : Error.output_format
  val print_typed : bool
  val use_field_getter_extension : bool
  val use_conflicting_side_effects_extension : bool
  val override_mode : override_mode
end

module type S = sig
  val type_check_ast : AST.t -> AST.t * StaticEnv.global

  val type_check_ast_in_env :
    StaticEnv.global -> AST.t -> AST.t * StaticEnv.global
end

module Annotate : functor (C : ANNOTATE_CONFIG) -> S
module TypeCheckDefault : S

val type_and_run :
  ?instrumentation:bool -> AST.t -> int * Instrumentation.semantics_rule list
