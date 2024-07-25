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

(** The runtime environment used by {!Interpreter}. *)

module type RunTimeConf = sig
  type v
  (** Stored elements of the environment. *)
end

module RunTime (C : RunTimeConf) : sig
  (** Internal representation for subprograms. *)

  (* -------------------------------------------------------------------------*)
  (** {2 Types and constructors.} *)

  type func = int ref * AST.func
  (** A function has an index that keeps a unique calling index. *)

  type global = {
    static : StaticEnv.global;  (** References the static environment. *)
    storage : C.v Storage.t;  (** Binds global variables to their names. *)
    funcs : func IMap.t;
        (** Declared subprograms, maps called identifier to their code. *)
  }
  (** The global part of an environment. *)

  type local
  (** The local part of an environment. *)

  type env = { global : global; local : local }
  (** The environment type. *)

  val empty_local : local
  (** An empty local environment. *)

  val to_static : env -> StaticEnv.env
  (** Builds a static environment, with an empty local part. *)

  val empty_scoped : scope -> local
  (** [empty_scoped scope] is an empty local environment in the scope [scope].
  *)

  (* -------------------------------------------------------------------------*)
  (** {2 Accessors} *)

  type 'a env_result =
    | Local of 'a
    | Global of 'a
    | NotFound
        (** Indicates if the value returned was bound in the global or local namespace. *)

  val find : identifier -> env -> C.v env_result
  (** Fetches an identifier from the environment. *)

  val mem : identifier -> env -> bool
  (** [mem x env] is true iff [x] is bound in [env]. *)

  val declare_local : identifier -> C.v -> env -> env
  (** [declare_local x v env] is [env] where [x] is now bound to [v]. This
      binding will be discarded by the call to [pop_scope] corresponding to the
      last call to [push_scope] before this declaration. *)

  val assign_local : identifier -> C.v -> env -> env
  (** [assign_local x v env] is [env] where [x] is now bound to [v]. It is
      assumed to be already bound in [env]. *)

  val declare_global : identifier -> C.v -> env -> env
  (** [declare_global x v env] is [env] where [x] is now bound to [v]. It is
      supposed that [x] is not bound in [env]. *)

  val assign_global : identifier -> C.v -> env -> env
  (** [assign_global x v env] is [env] where [x] is now bound to [v]. It is
      assumed to be already bound in [env]. *)

  val remove_local : identifier -> env -> env
  (** [remove_local x env] is [env] where [x] is not bound. *)

  val assign : identifier -> C.v -> env -> env env_result
  (** [assign x v env] assigns [x] to [v] in [env], and returns if [x] was
      declared as a local or global identifier. *)

  (* -------------------------------------------------------------------------*)
  (** {2 Loop unrolling} *)

  val tick_push : env -> env
  (** Push a new unrolling counter on the stack. The associated loop will be
      unrolled [C.unroll] times. *)

  val tick_push_bis : env -> env
  (** Push a new unrolling counter on the stack. The associated loop will be
      unrolled [C.unroll - 1] times. *)

  val tick_pop : env -> env
  (** Discards the last unrolling counter of the stack. *)

  val tick_decr : env -> bool * env
  (** [tick_decr env] is [(stop, env')] where
    - if the last counter is lower or equal to 1, [stop] is true and the last
      counter is discarded.
    - if the last counter is bigger or equal to 2, [stop] is false and the
      counter is decremented of 1.
    *)

  (* -------------------------------------------------------------------------*)
  (** {2 Scope handling} *)

  val get_scope : env -> scope
  (** Returns the local scope of that environment. *)

  val same_scope : env -> env -> bool
  (** [same_scope env1 env2] is [true] iff [env1]'s scope and [env2]'scope are equal. *)

  val push_scope : env -> env
  (** Push a new scope on the declaration stack. Variables declared here will
      be stored until the corresponding [pop_scope]. *)

  val pop_scope : env -> env -> env
  (** [pop_scope old new] restores the variable bindings of [old], with the
      updated values of [new]. *)
end
