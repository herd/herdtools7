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

type func_sig = unit AST.func
(** Type signature for functions, some kind of an arrow type, with added
    informations. *)

type global = {
  declared_types : ty IMap.t;  (** Maps a type name t to its declaration. *)
  constants_values : literal IMap.t;
      (** Maps a global constant name to its value. *)
  storage_types : (ty * global_decl_keyword) IMap.t;
      (** Maps global declared storage elements to their types. *)
  subtypes : identifier IMap.t;
      (** Maps an identifier s to its parent in the subtype relation. *)
  subprograms : func_sig IMap.t;
      (** Maps each subprogram runtime name to its signature. *)
  subprogram_renamings : ISet.t IMap.t;
      (** Maps each subprogram declared name to the equivalence class of all
          the subprogram runtime names that were declared with this name. *)
}
(** Store all the global environment information at compile-time. *)

type local = {
  constants_values : literal IMap.t;  (** Maps a local constant to its value. *)
  storage_types : (ty * local_decl_keyword) IMap.t;
      (** Maps an locally declared names to their type. *)
  return_type : ty option (** Local return type, [None] for procedures, global constants, or setters. *)
}
(** Store all the local environment information at compile-time. *)

type env = { global : global; local : local }
(** The static environment type. *)

val ast_func_to_func_sig : 'a AST.func -> func_sig
val pp_env : Format.formatter -> env -> unit
val pp_global : Format.formatter -> global -> unit
val empty_global : global
val empty_local : local
val empty_local_return_type : ty option -> local
val empty : env
val lookup_constants : env -> identifier -> literal
val type_of : env -> identifier -> ty
val type_of_opt : env -> identifier -> ty option
val mem_constants : env -> identifier -> bool
val add_subprogram : identifier -> func_sig -> env -> env
val set_renamings : identifier -> ISet.t -> env -> env
val add_global_storage : identifier -> ty -> global_decl_keyword -> env -> env
val add_type : identifier -> ty -> env -> env
val add_global_constant : identifier -> literal -> env -> env
val add_local_constant : identifier -> literal -> env -> env
val add_local : identifier -> ty -> local_decl_keyword -> env -> env
val add_subtype : identifier -> identifier -> env -> env
val is_undefined : identifier -> env -> bool
