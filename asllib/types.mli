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

(** {2 Domains} *)

(** The domain of a type is the set of values which storagbe element of that type may hold. *)
module Domain : sig
  type t
  (** Abstract value set. *)

  val pp : Format.formatter -> t -> unit
  (** A printer for the domain type. *)

  val of_type : env -> ty -> t
  (** Construct the domain of a type. *)

  val mem : AST.literal -> t -> bool
  (** [mem v d] is true if and only if [v] is in [d]. *)

  val equal : t -> t -> bool
  (** Wheather two domains are equal. *)

  val compare : t -> t -> int option
  (** The inclusion order on domains.

      It is a partial order. *)
end

(** {2 Orders on types} *)

val subtype_satisfies : env -> ty -> ty -> bool
(** Subtype-satisfation as per Definition TRVR. *)

val domain_subtype_satisfies : env -> ty -> ty -> bool
val structural_subtype_satisfies : env -> ty -> ty -> bool

val type_satisfies : env -> ty -> ty -> bool
(** Type-satisfation as per Rule FMXK. *)

val type_clashes : env -> ty -> ty -> bool
(** Type-clashing relation.

    Notes:
      - T subtype-satisfies S implies T and S type-clash
      - This is a equivalence relation

    As par Definition VPZZ.
*)

val subprogram_clashes : env -> 'a func -> 'b func -> bool
(** Subprogram clashing relation.

    As per Definition BTBR.
*)

val lowest_common_ancestor : env -> ty -> ty -> ty option
(** Lowest common ancestor.

    As per Rule YZHM.
*)

val base_value : 'a annotated -> env -> ty -> expr
(** [base_value env ty] is a base value of [ty]. *)
