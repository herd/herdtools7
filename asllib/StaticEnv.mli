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

open AST
open ASTUtils

(** Static Environment used for type-checking (cf {!Typing}. *)

type global = {
  declared_types : (ty * SideEffect.TimeFrame.t) IMap.t;
      (** Maps a type name t to its declaration and its time-frame.

        As expressions on which a type depends need to be statically evaluable,
        the only effects allowed in a type are statically evaluable, so need to
        be reading immutable (global) storage elements. This makes it possible
        only to store the time-frame of the type, and not the whole side-effect
        set.
    *)
  constant_values : literal Storage.t;
      (** Maps a global constant name to its value. *)
  storage_types : (ty * global_decl_keyword) IMap.t;
      (** Maps global declared storage elements to their types. *)
  subtypes : identifier IMap.t;
      (** Maps an identifier s to its parent in the subtype relation. *)
  subprograms : (AST.func * SideEffect.SES.t) IMap.t;
      (** Maps each subprogram runtime name to its signature and the
          side-effects inferred for it. *)
  overloaded_subprograms : ISet.t IMap.t;
      (** Maps the name of each declared subprogram to the equivalence class of
          all the subprogram runtime names that were declared with this name. *)
  expr_equiv : expr IMap.t;
      (** Maps every expression to a reduced immutable form. *)
}
(** Store all the global environment information at compile-time. *)

type local = {
  constant_values : literal Storage.t;
      (** Maps a local constant to its value. *)
  storage_types : (ty * local_decl_keyword) IMap.t;
      (** Maps an locally declared names to their type. *)
  expr_equiv : expr IMap.t;
      (** Maps immutable storage to their oldest equivalent expression. *)
  return_type : ty option;
      (** Local return type, [None] for procedures, global constants, or setters. *)
}
(** Store all the local environment information at compile-time. *)

type env = { global : global; local : local }
(** The static environment type. *)

val pp_env : Format.formatter -> env -> unit
val pp_global : Format.formatter -> global -> unit
val pp_local : Format.formatter -> local -> unit
val empty_global : global
val empty_local : local
val empty_local_return_type : ty option -> local
val empty : env
val with_empty_local : global -> env

val lookup_constant : env -> identifier -> literal
(** [lookup x env] is the value of x as defined in environment.

      @raise Not_found if it is not defined inside. *)

val lookup_constant_opt : env -> identifier -> literal option

val type_of : env -> identifier -> ty
(** [type_of env "x"] is the type of ["x"] in the environment [env]. *)

val type_of_opt : env -> identifier -> ty option
val lookup_immutable_expr : env -> identifier -> expr
val lookup_immutable_expr_opt : env -> identifier -> expr option
val mem_constants : env -> identifier -> bool
val add_subprogram : identifier -> AST.func -> SideEffect.SES.t -> env -> env
val set_renamings : identifier -> ISet.t -> env -> env

val add_global_storage :
  identifier -> ty -> global_decl_keyword -> global -> global

val add_type : identifier -> ty -> SideEffect.TimeFrame.t -> env -> env
val add_global_constant : identifier -> literal -> global -> global
val add_local_constant : identifier -> literal -> env -> env

val add_local_immutable_expr : identifier -> expr -> env -> env
(** [add_local_immutable_expr x e env] binds [x] to [e] in [env].
    [x] is assumed to name an immutable local storage element.
    [e] is supposed to be the oldest expression corresponding to [x].
*)

val add_global_immutable_expr : identifier -> expr -> env -> env
(** [add_global_immutable_expr x e env] binds [x] to [e] in [env].
    [x] is assumed to name an immutable global storage element.
    [e] is supposed to be the oldest expression corresponding to [x].
*)

val add_local : identifier -> ty -> local_decl_keyword -> env -> env
val add_subtype : identifier -> identifier -> env -> env
val is_local_undefined : identifier -> local -> bool
val is_global_undefined : identifier -> global -> bool
val is_undefined : identifier -> env -> bool
val is_subprogram : identifier -> env -> bool
