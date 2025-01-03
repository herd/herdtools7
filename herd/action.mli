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

(* Some architecture-specific sets and relations, with their definitions *)
  val arch_sets : (string * (action -> bool)) list
  val arch_rels : (string * (action -> action -> bool)) list
(* To be deprecated *)
  val arch_dirty : (string * (DirtyBit.my_t -> action -> bool)) list

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
  val is_ifetch : action -> bool
  val is_tag : action -> bool
  val is_additional_mem : action -> bool (* abstract memory actions, eg locks *)
  val is_atomic : action -> bool
  val is_fault : action -> bool
  val to_fault : action -> A.fault option
  val get_mem_dir : action -> Dir.dirn
  val get_mem_size : action -> MachSize.sz
  val is_pte_access : action -> bool
  val is_explicit : action -> bool
  val is_not_explicit : action -> bool

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
  val is_bcc : action -> bool
  val is_pred : ?cond:string option -> action -> bool
  val is_commit : action -> bool

(* Unrolling control *)
  val cutoff : string -> action
  val is_cutoff : action -> bool
  val as_cutoff : action -> string option

(********************)
(* Equation solving *)
(********************)

  val undetermined_vars_in_action : action -> A.V.ValueSet.t
  val simplify_vars_in_action : A.V.solution -> action -> action

end
