(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Constraint solver interface used by architectures. *)

module type S = sig
  module V : Value.S

  (* Architecture-specific predicate type and helpers. *)
  type arch_pred

  (* Return whether an equality of two syntactically different constants may
   * hold modulo a predicate. This may represent a hash or random generator
   * collision. *)
  val eq_satisfiable : V.Cst.v -> V.Cst.v -> arch_pred option
  val compare_predicate : arch_pred -> arch_pred -> int
  val enforce_no_collision : arch_pred -> arch_pred
  val pp_predicate : arch_pred -> string

  val fold_predicate_vars : (V.v -> 'a -> 'a) -> arch_pred -> 'a -> 'a
  val map_predicate : (V.v -> V.v) -> arch_pred -> arch_pred
  val pred_assume_collision : V.v -> V.v -> arch_pred
  val pred_assume_no_collision : V.v -> V.v -> arch_pred

  (* Functions to interact with a constraint solver. This constraint solver must
   * support at least equalities/inequalities (modulo the solver's theory), plus
   * resolution of a set of predicates given by the architecture. For example on
   * AArch64, the solver must support equalities/disequalities of Pointer
   * Authentication Codes (hash collisions).
   *)
  type arch_solver_state
  val empty_solver : arch_solver_state

  (** The [add_predicate] function returns the new solver state if the new
   * constraint is satisfiable in the current environment, and `None` otherwise.
   *)
  val add_predicate : arch_pred -> arch_solver_state -> arch_solver_state option

  (* Some constraint solvers may support normalisation. For example, if the
   * solver contains a Union-Find data structure, then it is possible to
   * normalize the elements of an equivalence class using the representative
   * of their class. *)
  val normalize : V.Cst.v -> arch_solver_state -> V.Cst.v

  (* The comparison is used in sets/maps that track solver states, so it must
   * be a total order over the full solver state. *)
  val compare_solver_state : arch_solver_state -> arch_solver_state -> int

  (* Render the solver state for pretty printing. None means there is nothing
   * relevant to display for this architecture. *)
  type finalized_solver_state
  val render_solver_state : arch_solver_state -> finalized_solver_state option
  val pp_solver_state : finalized_solver_state -> string
  (* Comparison used for ordering printable solver states. *)
  val compare_finalized_solver_state :
    finalized_solver_state -> finalized_solver_state -> int
end

module No (V : Value.S) : S with module V = V

module Pac (V : Value.S) : S with module V = V
