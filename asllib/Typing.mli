(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)

(** The Typing module is yet a single-entry-point module. It only exports the 
    function [annotate_ast] which fills type-annotations holes in the AST.
    It should provide enough information to disambiguate any type-dependent
    behaviour. *)

val infer_value : AST.literal -> AST.type_desc

type strictness = [ `Silence | `Warn | `TypeCheck ]
(** Possible strictness of type-checking. *)

val type_check_ast :
  strictness -> 'p AST.t -> StaticEnv.env -> 'p AST.t * StaticEnv.env
(** Typechecks the AST, and returns an AST with type inference holes filled.

    @raise Error.ASLException if the AST does not type-checks.
*)
