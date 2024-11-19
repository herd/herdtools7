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

(** Basic arch, ie with no definition of what a global location is *)

module type Config = ArchExtra_herd.Config


module type S =
  sig

    include ArchBase.S

    module V : Value.S with type Cst.Instr.t = instruction

    val is_amo : instruction -> bool
    val pp_barrier_short : barrier -> string
    val reject_mixed : bool (* perform a check that rejects mixed-size tests *)
    val mem_access_size : instruction -> MachSize.sz option

    val opt_env : bool (* environemnt optimisation is available *)
    val killed : instruction -> reg list
    val get_lx_sz : instruction -> MachSize.lr_sc
    (* Those register are to be initialised to the default value explicitly *)
    val reg_defaults : reg list

(* Return if an equality of two syntactically different constants may be equals
 * modulo a predicate. This may represent a hash or random generator collision *)
    val eq_satisfiable : V.Cst.v -> V.Cst.v -> V.arch_pred option

(* Functions to interact with a constraint solver, this constraint solver must
 * support at least the computation of equalities/inequalities (modulo the
 * theory represented by the solver), plus the resolution of a set of predicates
 * given by the architecture. As example for AArch64, the solver must sopport
 * the equalities/disequalities of Pointer Authentication Codes (hash
 * collisions).
 *
 * The `add_predicate` function return the new solver state if the new
 * constraint is satisfiable in the current environment. And `None` otherwise.
 *
 * In addition the compare function must only compare the part of the solver
 * state we print in `pp_solver_state`, as example the PAC solver of `AArch64`
 * only show it's equalities.
 *)
    type solver_state
    val empty_solver : solver_state

    val add_predicate : V.arch_pred -> solver_state -> solver_state option

    (* Some constraint solvers may support normalisation, for example if the
     * solver contain an Union-Find data structure, then it's possible to
     * normalize the elements of an equivalence class using the representant
     * of their class... *)
    val normalize : V.Cst.v -> solver_state -> V.Cst.v

    (* Pretty print the current solver state, must verify
     * `pp_solver_state empty_solver = ""` otherwise the final result may be
     * invalid if "-debug pred-solver" is not set *)
    val pp_solver_state : solver_state -> string

    (* The comparison is used to store the final state in a Map before pretty
     * printing, but not before doing any resolution of predicates. So the
     * `compare` function may be imprecise. As example if we don't show the
     * inequalities at pretty-printing, then the comparison may return that
     * two solver are equals even if they contains different inequalities. *)
    val compare_solver_state : solver_state -> solver_state -> int

    include ArchExtra_herd.S with module I.V = V
    and type I.solver_state = solver_state
    and type I.arch_reg = reg

(* Levels are abstract, for AArch64, they are E0 to E3 *)
    type level
    val levels : level list
    val pp_level : level -> string

    module TLBI :
    sig
      type op
      val pp_op : op -> string
      val is_at_level : level -> op -> bool
      val inv_all : op -> bool
      val sets : (string * (op -> bool)) list
    end

    val convert_if_imm_branch : int -> int -> int Label.Map.t -> int Label.Map.t -> instruction -> instruction

    module MemType:MemoryType.S

    module Barrier:AllBarrier.S with type a = barrier

    module CMO:Cmo.S
  end
