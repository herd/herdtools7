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

(** Type Algebra *)

(**
   Types are defined as {!type:AST.ty}. This should map pretty-well with the current
   version of the Language Reference Manual.
*)

open AST

type env = StaticEnv.env

(** {1 Predicates on types} *)

val is_builtin : ty -> bool
val is_builtin_singular : ty -> bool
val is_builtin_aggregate : ty -> bool

(** Note that a builtin type is either builtin aggregate or builtin singular. *)

val is_singular : env -> ty -> bool
val is_aggregate : env -> ty -> bool

(** Note that a type is either singular or aggregate. *)

val is_named : ty -> bool
(** Types declared using the [type] syntax. *)

val is_anonymous : ty -> bool
(** Those not declared using †he [type] syntax. *)

(** Note that a type is either builtin, named or anonymous. *)

val is_primitive : ty -> bool
(** Types that only use the builtin types. *)

val is_non_primitive : ty -> bool
(** Types that are named types or which make use of named types.

    Usually for all [ty]:
    {[
      is_non_primitive ty = not (is_primitive ty)
    ]}
*)

(** {1 Relations on types} *)

(** {2 Type transformations} *)

val make_anonymous : env -> ty -> ty
(** Replace any named type by its declared type in the environment. *)

val get_structure : env -> ty -> ty
(** The structure of a type is the primitive type that can hold the same
    values. *)

val parameterized_ty : loc:'a annotated -> identifier -> ty
(** Builds an parameterized integer type from a declared variable. *)

val to_well_constrained : ty -> ty
(** Transform a parameterized integer type into a well-constrained integer equal
    to the parameter that have this type, and leave the other types (such as
    well-constrained integers) as they are. *)

val get_well_constrained_structure : env -> ty -> ty
(** [get_well_constrained_structure env ty] is equivalent to
    [get_structure env ty |> to_well_constrained]. *)

(** {2 Orders on types} *)

val subtypes : env -> ty -> ty -> bool
(** [subtypes env t1 t2] is true if and only if [t1] is a declared subtype of [t2]. *)

val subtypes_names : env -> identifier -> identifier -> bool
(** [subtypes_names env s1 s2] is true if and only if the type named [s1] is a
    declared subtype of the type named [s2].

    Equivalent to [subtypes env (T_Named s1 |> here) (T_Named s2 |> here)].
*)

val subtype_satisfies : env -> ty -> ty -> bool
(** Subtype-satisfaction test. *)

val type_satisfies : env -> ty -> ty -> bool
(** Type-satisfaction test. *)

val type_clashes : env -> ty -> ty -> bool
(** Type-clashing relation.

    Notes:
      - T subtype-satisfies S implies T and S type-clash
      - This is an equivalence relation
*)

val subprogram_clashes : env -> func -> func -> bool
(** Subprogram clashing relation. *)

val lowest_common_ancestor : loc:'a annotated -> env -> ty -> ty -> ty option
(** Lowest common ancestor. *)

val type_equal : env -> ty -> ty -> bool
(** A conservative type equivalence test for types in env. *)
