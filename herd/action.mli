(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** An architecture-independent interface for actions *)

module type S = sig

  module A : Arch.S

  type action

  val mk_init_write : A.location -> A.V.v -> action

  val pp_action : action -> string

(* Some architecture-specific sets, and their definitions
   e.g. ["rmw", is_rmw; "ls", is_successful_lock]
 *)
  val arch_sets : (string * (action -> bool)) list
(* architecture specific fences *)
  val arch_fences : (string * (action -> bool)) list
(* control fence *)
  val is_isync : action -> bool
  val pp_isync : string

(**************************************)
(* Access to sub_components of events *)
(**************************************)

  val value_of : action -> A.V.v option
  val read_of : action -> A.V.v option
  val written_of : action -> A.V.v option
  val location_of   : action -> A.location option


(************************)
(* Predicates on events *)
(************************)

(* relative to memory *)
  val is_mem_store : action -> bool
  val is_mem_load : action ->  bool
  val is_mem : action -> bool
  val is_atomic : action -> bool
  val get_mem_dir : action -> Dir.dirn

(* relative to the registers of the given proc *)
  val is_reg_store : action -> A.proc -> bool
  val is_reg_load : action -> A.proc -> bool
  val is_reg : action -> A.proc -> bool

(* Reg events, proc not specified *)
  val is_reg_store_any : action -> bool
  val is_reg_load_any : action -> bool
  val is_reg_any : action -> bool

(* Store/Load to memory or register *)
  val is_store : action -> bool
  val is_load : action -> bool

(* Barriers *)
  val is_barrier : action -> bool
  val barrier_of : action -> A.barrier option
  val same_barrier_id : action -> action -> bool

(* Commits *)
  val is_commit : action -> bool

(* Local/Global Fences *)
  val is_local_fence : action -> bool
  val is_global_fence : action -> bool

(* Mutex operations *)
  val is_mutex_action : action -> bool

  val is_sc_action : action -> bool

(********************)
(* Equation solving *)
(********************)

  val undetermined_vars_in_action : action -> A.V.ValueSet.t
  val simplify_vars_in_action : A.V.solution -> action -> action

(************************)
(* Parallel composition *)
(************************)

  val make_action_atomic : action -> action

end
