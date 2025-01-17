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

module type RunTimeConf = sig
  module Scope : Backend.SCOPE

  type v

  val unroll : int
end

module type S = sig
  type v

  module Scope : Backend.SCOPE

  type global = {
    static : StaticEnv.global;
    storage : v Storage.t;
    stack_size : Z.t IMap.t;
  }

  type local
  type env = { global : global; local : local }

  val to_static : env -> StaticEnv.env
  val local_empty_scoped : ?storage:v Storage.t -> Scope.t -> local
  val global_from_static : ?storage:v Storage.t -> StaticEnv.global -> global

  type 'a env_result = Local of 'a | Global of 'a | NotFound

  val find : identifier -> env -> v env_result
  val mem : identifier -> env -> bool
  val declare_local : identifier -> v -> env -> env
  val assign_local : identifier -> v -> env -> env
  val declare_global : identifier -> v -> env -> env
  val assign_global : identifier -> v -> env -> env
  val remove_local : identifier -> env -> env
  val assign : identifier -> v -> env -> env env_result
  val tick_push : env -> env
  val tick_push_bis : env -> env
  val tick_pop : env -> env
  val tick_decr : env -> bool * env
  val get_scope : env -> Scope.t
  val push_scope : env -> env
  val pop_scope : env -> env -> env
  val get_stack_size : identifier -> env -> Z.t
  val incr_stack_size : identifier -> global -> global
  val decr_stack_size : identifier -> global -> global
end

module RunTime (C : RunTimeConf) = struct
  module Scope = C.Scope

  type v = C.v

  type global = {
    static : StaticEnv.global;
    storage : C.v Storage.t;
    stack_size : Z.t IMap.t;
  }

  type int_stack = int list

  type local = {
    storage : C.v Storage.t;
    scope : Scope.t;
    unroll : int_stack;
    declared : identifier list;
  }

  type env = { global : global; local : local }

  let local_empty_scoped ?(storage = Storage.empty) scope =
    { scope; storage; unroll = []; declared = [] }

  let global_from_static ?(storage = Storage.empty) static =
    { static; storage; stack_size = IMap.empty }

  let get_scope env = env.local.scope

  let to_static env =
    let global = env.global.static in
    StaticEnv.{ empty with global }

  (* --------------------------------------------------------------------------*)
  (* Loop unrolling controls. *)

  let set_unroll env unroll = { env with local = { env.local with unroll } }

  (** [tick_push env] is [env] with [C.unroll] pushed on its unrolling stack. *)
  let tick_push env = set_unroll env (C.unroll :: env.local.unroll)

  (** [tick_push_bis env] is [env] with [C.unroll -1] pushed on its unrolling
      stack. *)
  let tick_push_bis env = set_unroll env ((C.unroll - 1) :: env.local.unroll)

  (** [tick_pop env] is [env] with removed the unrolling stack first element. *)
  let tick_pop env =
    match env.local.unroll with
    | [] -> assert false
    | _ :: unroll -> set_unroll env unroll

  (** [tick_decr env] decrements the unrolling stack of env and returns
      wheather it has poped something or not. *)
  let tick_decr env =
    match env.local.unroll with
    | [] -> assert false
    | x :: xs ->
        let x = x - 1 in
        if x <= 0 then (true, set_unroll env xs)
        else (false, set_unroll env (x :: xs))

  (* --------------------------------------------------------------------------*)
  (* Retrieval utils *)

  type 'a env_result = Local of 'a | Global of 'a | NotFound

  (* Begin SemanticsRule.EnvFind *)
  let find x env =
    try Local (Storage.find x env.local.storage)
    with Not_found -> (
      try Global (Storage.find x env.global.storage)
      with Not_found -> NotFound)
  (* End *)

  let mem x env =
    Storage.mem x env.local.storage || Storage.mem x env.global.storage

  (* --------------------------------------------------------------------------*)
  (* Assignments utils *)

  let declare_local x v env =
    {
      env with
      local =
        {
          env.local with
          storage = Storage.add x v env.local.storage;
          declared = x :: env.local.declared;
        };
    }

  let assign_local x v env =
    {
      env with
      local = { env.local with storage = Storage.assign x v env.local.storage };
    }

  let declare_global x v env =
    {
      env with
      global = { env.global with storage = Storage.add x v env.global.storage };
    }

  let assign_global x v env =
    {
      env with
      global =
        { env.global with storage = Storage.assign x v env.global.storage };
    }

  let remove_local x env =
    {
      env with
      local =
        {
          env.local with
          storage = Storage.remove x env.local.storage;
          declared =
            List.filter (fun s -> not (String.equal s x)) env.local.declared;
        };
    }

  let assign x v env =
    try Local (assign_local x v env)
    with Not_found -> (
      try Global (assign_global x v env) with Not_found -> NotFound)

  (* --------------------------------------------------------------------------*)
  (* Scope swapping utils *)

  let push_scope env = { env with local = { env.local with declared = [] } }

  let pop_scope parent child =
    let local_storage =
      Storage.patch_mem ~t_env:parent.local.storage ~t_mem:child.local.storage
        child.local.declared
    in
    { child with local = { parent.local with storage = local_storage } }

  let get_stack_size name env =
    try IMap.find name env.global.stack_size with Not_found -> Z.zero

  let set_stack_size name value global =
    let stack_size = IMap.add name value global.stack_size in
    { global with stack_size }

  let incr_stack_size name global =
    let prev =
      try IMap.find name global.stack_size with Not_found -> Z.zero
    in
    set_stack_size name (Z.succ prev) global

  let decr_stack_size name global =
    let prev =
      try IMap.find name global.stack_size with Not_found -> assert false
    in
    assert (Z.sign prev > 0);
    set_stack_size name (Z.pred prev) global
end
