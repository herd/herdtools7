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

(** Utilities used when generating event structures *)

module Make : functor (S: SemExtra.S) -> sig

(* Program order as a relation *)
  val iico : S.event_structure -> S.event_rel
  val po_strict : S.event_structure -> S.event_rel
  val po_iico_data :  S.event_structure -> S.event_rel
  val po_iico :  S.event_structure -> S.event_rel
(* po union iico_data union iico_control *)
  val is_before_strict : S.event_structure -> S.event -> S.event -> bool

(*
 * Set of events ordered by  is_before_strict.
 * Notice it is an error to add unrelated events
 *  to this set.
 *)
  module
    EvtSetByPo :
    functor
      (I:sig val es : S.event_structure end) ->
    sig

      (* Strict ordering, taking iico into account for effects
         from the same instruction. *)
      val is_before_strict : S.event -> S.event -> bool

      include Set.S with type elt = S.event

      (* [find_last_before set e] find maximal effect in [set] before [e] *)
      val find_last_before : t -> S.event -> S.event option

      (* [find_last_before set e] find minimal effect in [set] after [e] *)
      val find_first_after : S.event -> t -> S.event option

    end

(* Fence like relations *)
  val po_fence_po : S.event_rel (* po *) -> (S.event -> bool) -> S.event_rel

(* Lift relations to memory *)
  val trans_close_mem : S.event_rel -> S.event_rel
  val trans_close_mems : S.event_rel list -> S.event_rel list

(* All scope relations *)
  val get_scope_rels :
      S.event_set -> BellInfo.scopes -> (string * S.event_rel) list
  val get_level_rels :
      S.event_set -> BellInfo.scopes -> S.event_rel * (string * S.event_rel) list
  val lift_proc_info :
      S.proc_info -> S.event_set -> (string * S.event_set Lazy.t) list

(* View of a relation by a processor:
   restricted to local events and mem_stores *)
  val proc_view : S.proc -> S.event_rel -> S.event_rel
  val proc_view_event : S.proc -> S.event -> bool

(* Perform operations columnwise *)
 val diff_p :  S.event_rel list -> S.event_rel list -> S.event_rel list
 val union_p :  S.event_rel list -> S.event_rel list -> S.event_rel list
 val unions_p : S.event_rel list list -> S.event_rel list
 val transitive_closure_p :   S.event_rel list -> S.event_rel list


(* Misc, but everywhere... *)
  val find_source :'a S.RFMap.t -> S.event -> 'a
  val rext : S.concrete -> S.event -> bool
  val same_source :  S.concrete -> S.event -> S.event -> bool
  val ext : S.event_rel -> S.event_rel
  val internal : S.event_rel -> S.event_rel

(* RF/FR relations for memory *)
  val make_rf_from_rfmap : S.rfmap -> S.event_rel
  val make_rf : S.concrete -> S.event_rel
  val make_write_mem_finals : S.concrete -> S.event_set
  val make_rf_regs : S.concrete -> S.event_rel

(* make_fr conc ws, where ws is write serialization as
   a relation (ie as a transitive relation, not as a successor
   relation *)
  val make_fr : S.concrete -> S.event_rel -> S.event_rel

(* Mapping from locations, a.k.a. location maps *)
  module LocEnv : MyMap.S with type key = S.location

(* Collect various events, indexed by location *)
  val collect_reg_loads : S.event_structure -> S.event list LocEnv.t
  val collect_reg_stores : S.event_structure -> S.event list LocEnv.t
  val collect_reg_loads_stores : S.event_structure -> (S.event list * S.event list) LocEnv.t
  val collect_mem_loads : S.event_structure -> S.event list LocEnv.t
  val collect_mem_stores : S.event_structure -> S.event list LocEnv.t
  val collect_mem : S.event_structure -> S.event list LocEnv.t
  val collect_mem_non_init : S.event_structure -> S.event list LocEnv.t
  val collect_loads : S.event_structure -> S.event list LocEnv.t
  val collect_stores : S.event_structure -> S.event list LocEnv.t
  val collect_stores_non_spec : S.event_structure -> S.event list LocEnv.t
  val collect_loads_non_spec : S.event_structure -> S.event list LocEnv.t

  (*
   * Collect atomic effects indexed by threads and by locations.
   * When given an event structure as argument, the function
   * returns a pair [(maps,evts)], where:
   *  + [maps] is a list of location maps, one per thread.
   *    The values of this map are the atomic effects
        of the given thread.
   *  + [evts] is the list of spurious (atomic) effects.
   *)

  val collect_atomics :
    S.event_structure -> (Proc.t * S.event list LocEnv.t) list * S.event list

(* Partition by location *)
  val partition_events : S.event_set -> S.event_set list

(* Utilities for relations *)
  val restrict_to_mem_stores : S.event_rel -> S.event_rel
  val remove_spec_from_map : S.event_structure -> S.event list LocEnv.t -> S.event list LocEnv.t

(* Place loads given write serialization *)
val make_load_stores : S.concrete -> S.event_rel -> S.event_rel

(************************************************************)
(* Stop early if we can, final condition invalisation mode. *)
(************************************************************)

(*
  Notice that a runtime option is checked, and
  that can_stop answers true if fast mode is disabled
*)

(* Test on memory is performed by fold_write_serialization_candidates
   below *)

(************************)
(* Fold over candidates *)
(************************)

(* fold a function over write serialization candidates
   'fold_write_serialization_candidates test es env vb kont res'

    - test is test
    - es is concrete event structure
    - vb is a relation that candidates must include.
    - kont of type relation ->'a -> 'a will apply
      to each generated candidate

NOTICE: The generator takes care of placing stores to final state
        correctly *)


  val fold_write_serialization_candidates :
      S.concrete ->
	S.event_rel ->
	  (S.event_rel ->  'a -> 'a) -> 'a -> 'a

(* Apply previous fold, catching cyclic graphs errors *)
  val apply_process_co :
      S.test ->
        S.concrete ->
          (S.event_rel -> 'a -> 'a) -> 'a -> 'a


(*****************************************)
(* Compute write serialization precursor *)
(*****************************************)
(* Iniital pco: account for init stores *)
  val compute_pco_init : S.event_structure -> S.event_rel
(* Assumes complete uniproc and hence may fail (because of Init in rfmap *)
  val compute_pco : S.rfmap -> S.event_rel -> S.event_rel option

(* Alignment check *)
val is_aligned : S.A.type_env -> S.A.size_env -> S.event -> bool

end
