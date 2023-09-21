(******************************************************************************)
(*                           the diy toolsuite                                *)
(*                                                                            *)
(* Jade Alglave, University College London, UK.                               *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
(*                                                                            *)
(* Copyright 2015-present Institut National de Recherche en Informatique et   *)
(* en Automatique and the authors. All rights reserved.                       *)
(*                                                                            *)
(* This software is governed by the CeCILL-B license under French law and     *)
(* abiding by the rules of distribution of free software. You can use,        *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B   *)
(* license as circulated by CEA, CNRS and INRIA at the following URL          *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.              *)
(******************************************************************************)
(* Authors:                                                                   *)
(* Hadrien Renaud, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
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

(** This module produces an map like interface to model memory storage. *)

open AST

(**

  {1 Introduction to scopes, mutability and blocks}

  The usual implementation and semantics of variables is based on "maps", i.e.
   functions from variables to values with a finite domain. We note [env] such
   a map.

  As in ASL, to evaluate a program, we distinguish between expressions and
   statements:
  + function [eval_expr] takes as argument an environment and an expression and
     returns a value. For instance, here is the evaluation of a variable:
     {[
       eval_expr env = function
         | E_Var x -> Map.find x env
     ]}
  + function [eval_stmt] takes as argument an environment and a statement and
   returns an environment. For instance here is the execution of a variable
   declaration:
     {[
       eval_stmt env = function
         | S_Decl (x, e) ->
           let v = eval_expr x e in
           Map.add x v env
     ]}

  In a simplified setting, the above scheme suffices to implement mutable
   variables, mostly because the [eval_stmt] returns the environment, as
   illustrated by the rule for assignment, which is the same as the declaration
   rule.
     {[
       eval_stmt env = function
         | S_Assign (x, e) ->
           let v = eval_expr env e in
           Map.add x v env
     ]}
  Notice that the new binding overwrites the old one, which is no longer accessible.

  The scheme still works in case all variables are global and when expression
   can perform side effects. Function [eval_expr] will now return a pair of a
   value and of an environment, For instance, assume expression [E_Incr x] that
   increments the contents of [x] and return the new value:
     {[
       eval_expr env = function
         | E_Incr x ->
           let v = Map.find x env in
           let v = v + 1 in
           (v, Map.add x v env)
     ]}

  Things get more complicated in the presence of scopes. The root reason is
   what happens to bindings at scope end. Consider the following ASL code:
     {v
 var s = 0;
 var i = 0;
 while i < n do
   var c = i*i ;
   s = s + c;
   i = i + 1;
 end
 // 'c' is mo longer available, 's' is.
     v}
  According to ASL semantics the variable [c] exists from its declaration to
   the end of its enclosing block. One simple implementation for executing a
   block is as follows:
     {[
       let eval_stmt env = function
         | S_Block stmt ->
           ignore (eval_stmt env stmt);
           env
     ]}
  All extensions performed by the statement [stmt] from inside the block are
   discarded.

  However, some variable defined before the block can be assigned inside the
   block (this is the case of [s] above), and those modifications have to
   survive the end of the block... This would lead to the following
   contradictory implementation:
     {[
       let eval_stmt env = function
         | S_Block stmt -> eval_stmt env stmt
     ]}

  One solution is adding an indirection in environments. En environment is now
   made of two maps: one for bindings from variables to {i pointers} and another
   for the "heap" from pointers to values.
     {[
       type env = { bds : pointer VarMap.t; heap : value PointerMap.t; }
     ]}
   The rule for blocks can now discard the bindings and retain the heap:
     {[
       let eval_stmt old_env = function
         | S_Block stmt ->
           let new_env =  eval_stmt env stmt in
           { bds = old_env.bds; heap = new_env.heap; }
     ]}
  One may notice that the heap can be purged of the slots associated to the
    discarded bindings, some kind of garbage collection. This can be done
    easily by an additional field in environment 'declared' that records the
    variables declared in a block - this technique is used in the interpreter.

  The rule for evaluating a variable now performs two successive searches, one
   for retrieving the pointer and then the value:
    {[
      let eval_expr env = function
        | E_Var x ->
          let pointer = VarMap.find x env.bds in
          PointerMap.find pointer env.heap
    ]}

  The rules for declarations and assignments also change, there are now different:
    + The declaration {i allocates} a fresh pointer. That way if a binding for
      the same variable exists the underlying heap slot is preserved.
    + The assignment retrieve the pointer associated to the variable, which
      must exist.

  {1 Storage module}

  This module implements the storage type presented above, and present a simple
   interface to this.

   *)

type 'v t
(** The type [t] stores an association between names and ['v] values. *)

val empty : 'a t
(** An empty storage. *)

val mem : identifier -> 'a t -> bool
(** [mem x t] is true iff [x] is bound in [t]. *)

val add : identifier -> 'a -> 'a t -> 'a t
(** [add x v t] is [t] with [x] bound to [v], over-riding previous bind but not
    deleting values from memory. *)

val assign : identifier -> 'a -> 'a t -> 'a t
(** [assign x v t] is [t] with [x] bound to [v], replacing the previous value
    bound for [x]. If [x] is not bound in [t], raise [Not_found]. *)

val declare : identifier -> 'a -> 'a t -> 'a t
(** [declare x v t] is [t] with [x] bound to [v], creating a new memory cell
    for [v]. *)

val find : identifier -> 'a t -> 'a
(** [find x t] is [v] if [x] is bound to [v] in [t], raising [Not_found]
    otherwise. *)

val find_opt : identifier -> 'a option t -> 'a option
(** [find_opt x t] is [Some v] if [x] is bound to [v] in [t], [None] otherwise.
*)

val remove : identifier -> 'a t -> 'a t
(** [remove x t] is [t] with [x] not bound to anything. Memory is freed. *)

val patch_mem : t_env:'a t -> t_mem:'a t -> identifier list -> 'a t
(** [patch_mem ~t_env ~t_mem to_avoid] is the storage formed with the bindings
    of [t_env], the memory of [t_mem] except for the cells bound to the
    variables in [to_avoid]. *)
