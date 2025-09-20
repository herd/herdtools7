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

  type symbolic_choice = {
    description : string;
    decision : bool;
    location : unit annotated;
  }

  type global = {
    static : StaticEnv.global;
    storage : v IMap.t;
    pending_calls : Z.t IMap.t;
    call_stack : identifier annotated list;
    symbolic_path : symbolic_choice list;
  }

  type local
  type env = { global : global; local : local }

  val to_static : env -> StaticEnv.env
  val local_empty_scoped : ?storage:v IMap.t -> Scope.t -> local
  val global_from_static : ?storage:v IMap.t -> StaticEnv.global -> global

  type 'a env_result = Local of 'a | Global of 'a | NotFound

  val find : identifier -> env -> v env_result
  val find_global : identifier -> env -> v
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
  val get_pending_calls : identifier -> env -> Z.t
  val incr_pending_calls : pos:'a annotated -> identifier -> global -> global
  val decr_pending_calls : identifier -> global -> global
  val push_symbolic_choice : symbolic_choice -> env -> env
end

module RunTime (C : RunTimeConf) = struct
  module Scope = C.Scope

  type v = C.v

  type symbolic_choice = {
    description : string;
    decision : bool;
    location : unit annotated;
  }

  type global = {
    static : StaticEnv.global;
    storage : C.v IMap.t;
    pending_calls : Z.t IMap.t;
    call_stack : identifier annotated list;
    symbolic_path : symbolic_choice list;
  }

  type int_stack = int list

  type local = {
    storage : C.v IMap.t;
    scope : Scope.t;
    unroll : int_stack;
    declared : identifier list;
  }

  type env = { global : global; local : local }

  let local_empty_scoped ?(storage = IMap.empty) scope =
    { scope; storage; unroll = []; declared = [] }

  let global_from_static ?(storage = IMap.empty) static =
    {
      static;
      storage;
      pending_calls = IMap.empty;
      call_stack = [];
      symbolic_path = [];
    }

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
    try Local (IMap.find x env.local.storage)
    with Not_found -> (
      try Global (IMap.find x env.global.storage) with Not_found -> NotFound)
  (* End *)

  let find_global x env = IMap.find x env.global.storage
  let mem x env = IMap.mem x env.local.storage || IMap.mem x env.global.storage

  (* --------------------------------------------------------------------------*)
  (* Assignments utils *)

  let declare_local x v env =
    {
      env with
      local =
        {
          env.local with
          storage = IMap.add x v env.local.storage;
          declared = x :: env.local.declared;
        };
    }

  let _assign x v =
    IMap.update x (function None -> raise Not_found | Some _ -> Some v)

  let _declare x v =
    IMap.update x (function
      | None -> Some v
      | Some _ ->
          let () =
            Printf.eprintf "Storage element %s already declared in env.\n%!" x
          in
          assert false)

  let assign_local x v env =
    {
      env with
      local = { env.local with storage = _assign x v env.local.storage };
    }

  let declare_global x v env =
    {
      env with
      global = { env.global with storage = _declare x v env.global.storage };
    }

  let assign_global x v env =
    {
      env with
      global = { env.global with storage = _assign x v env.global.storage };
    }

  let remove_local x env =
    {
      env with
      local =
        {
          env.local with
          storage = IMap.remove x env.local.storage;
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

  let get_scope env = env.local.scope
  let push_scope env = { env with local = { env.local with declared = [] } }

  (** [patch_mem ~t_env ~t_mem to_avoid] is the storage formed with the bindings
    of [t_env], the memory of [t_mem] except for the cells bound to the
    variables in [to_avoid]. *)
  let patch_mem ~t_env ~t_mem to_avoid =
    List.fold_left (fun t x -> IMap.remove x t) t_env to_avoid
    |> IMap.mapi (fun x _ -> IMap.find x t_mem)

  let pop_scope parent child =
    let local_storage =
      patch_mem ~t_env:parent.local.storage ~t_mem:child.local.storage
        child.local.declared
    in
    { child with local = { parent.local with storage = local_storage } }

  (* --------------------------------------------------------------------------*)
  (* Call stack utils *)

  let get_pending_calls name env =
    try IMap.find name env.global.pending_calls with Not_found -> Z.zero

  let _incr_pending_calls name =
    IMap.update name @@ function
    | None -> Some Z.one
    | Some z -> Some (Z.succ z)

  let _decr_pending_calls name =
    IMap.update name @@ function
    | None -> assert false
    | Some z -> Some (Z.pred z)

  let _push_call_stack ~pos name call_stack =
    ASTUtils.add_pos_from pos name :: call_stack

  let _pop_call_stack = function [] -> assert false | _ :: t -> t

  let incr_pending_calls ~pos name global =
    {
      global with
      pending_calls = _incr_pending_calls name global.pending_calls;
      call_stack = _push_call_stack ~pos name global.call_stack;
    }

  let decr_pending_calls name global =
    {
      global with
      pending_calls = _decr_pending_calls name global.pending_calls;
      call_stack = _pop_call_stack global.call_stack;
    }

  let push_symbolic_choice choice env =
    {
      env with
      global =
        { env.global with symbolic_path = choice :: env.global.symbolic_path };
    }
end
