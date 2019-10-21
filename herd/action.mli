(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** An architecture-independent interface for actions *)

module type S = sig

  module A : Arch_herd.S

  type action

  val mk_init_write : A.location -> MachSize.sz -> A.V.v -> action

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
  val is_additional_mem_load :  action -> bool (* trylock *)
  val is_mem : action -> bool
  val is_additional_mem : action -> bool (* abstract memory actions, eg locks *)
  val is_atomic : action -> bool
  val get_mem_dir : action -> Dir.dirn
  val get_mem_size : action -> MachSize.sz

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

(* Compatible accesses *)
  val compatible_accesses : action -> action -> bool

(* for bell annotations *)
  val annot_in_list : string -> action -> bool

(* Barriers *)
  val is_barrier : action -> bool
  val barrier_of : action -> A.barrier option
  val same_barrier_id : action -> action -> bool

(* Commits *)
  val is_commit_bcc : action -> bool
  val is_commit_pred : action -> bool

(********************)
(* Equation solving *)
(********************)

  val undetermined_vars_in_action : action -> A.V.ValueSet.t
  val simplify_vars_in_action : A.V.solution -> action -> action

end
