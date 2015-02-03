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

(** Utilities used when generating event structures *)

module Make : functor (S: SemExtra.S) -> sig

(* Program order as a relation *)
  val iico : S.event_structure -> S.event_rel
  val po_strict : S.event_structure -> S.event_rel
  val po_iico_data :  S.event_structure -> S.event_rel
  val po_iico :  S.event_structure -> S.event_rel
(* po union iico_data union iico_control *)
  val is_before_strict : S.event_structure -> S.event -> S.event -> bool
(* Fence like relations *)
  val po_fence_po : S.event_rel (* po *) -> (S.event -> bool) -> S.event_rel

(* Lift relations to memory *)
  val trans_close_mem : S.event_rel -> S.event_rel
  val trans_close_mems : S.event_rel list -> S.event_rel list

(* View of a relation by a processor:
   restricted to local events and mem_stores *)
  val proc_view : S.proc -> S.event_rel -> S.event_rel
  val proc_view_event : S.proc -> S.event -> bool

(* Perform operations columnwise *)
 val diff_p :  S.event_rel list -> S.event_rel list -> S.event_rel list 
 val union_p :  S.event_rel list -> S.event_rel list -> S.event_rel list 
 val unions_p : S.event_rel list list -> S.event_rel list
 val transitive_closure_p :   S.event_rel list -> S.event_rel list


(* Convert the ordered list representation of a total order to a relation *)
  val order_to_rel : S.event list -> S.event_rel
  val order_to_succ_rel :  S.event list -> S.event_rel
(* Convert a cyclic list into a relation *)
  val cycle_to_rel : S.event list -> S.event_rel


(* Misc, but everywhere... *)
  val find_source :'a S.RFMap.t -> S.event -> 'a
  val rext : S.concrete -> S.event -> bool
  val same_source :  S.concrete -> S.event -> S.event -> bool
  val ext : S.event_rel -> S.event_rel
  val internal : S.event_rel -> S.event_rel

  (*scope operations*)
  val ext_scope : 
    AST.scope -> S.event_rel -> GPU_PTXBase.scope_tree -> S.event_rel
  val int_scope : 
    AST.scope -> S.event_rel -> GPU_PTXBase.scope_tree -> S.event_rel
  

(* RF/FR relations for memory *)
  val make_rf_from_rfmap : S.rfmap -> S.event_rel
  val make_rf : S.concrete -> S.event_rel
  val make_rf_regs : S.concrete -> S.event_rel

(* make_fr conc ws, where ws is write serialization as
   a relation (ie as a transitive relation, not as a successor
   relation *)  
  val make_fr : S.concrete -> S.event_rel -> S.event_rel
(* Idem, includes loads from init only *)
  val make_fr_partial : S.concrete -> S.event_rel
(* Separated by barrier in po *)
  val sep :
      (S.event -> bool) ->
        (S.event * S.event -> bool) ->
          S.event_set -> S.event_rel

(* External sub relation *)
  val extract_external :  S.event_rel -> S.event_rel

(* Mapping from locations *)
  module LocEnv : Map.S with type key = S.location

(* Collect various events, indexed by location *)
  val collect_reg_loads : S.event_structure -> S.event list LocEnv.t
  val collect_reg_stores : S.event_structure -> S.event list LocEnv.t
  val collect_mem_loads : S.event_structure -> S.event list LocEnv.t
  val collect_mem_stores : S.event_structure -> S.event list LocEnv.t
  val collect_loads : S.event_structure -> S.event list LocEnv.t
  val collect_stores : S.event_structure -> S.event list LocEnv.t
  val collect_atomics : S.event_structure -> S.event list LocEnv.t
  val collect_mutex_actions : S.event_structure -> S.event list LocEnv.t

(* Utlities for relations *)
  val restrict_to_mem_stores : S.event_rel -> S.event_rel

(* Place loads given write serialization *)
val make_load_stores : S.concrete -> S.event_rel -> S.event_rel

(*************)
(* Atomicity *)
(*************)

val get_canonical_locked_events_of :  S.event_structure -> S.event_set

(*
  'get_atomicity_candidates rel es'
     1. rel is a constraining relation on order over locked events.
        all generated orderings are implied (include) the relation.
     2. es is event structure
  Return a pair of

  1. set of representatives of atomicity class
  2. A list of candidates, each being a pair
  or ordering amongst representative X amongst all locked events.
*)

val get_atomicity_candidates :
    S.event_rel ->
      S.event_structure ->
           S.event_set * (S.event_rel * S.event_rel) list

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

  val fold_mutex_serialization_candidates :
      S.concrete ->
	S.event_rel ->
	  (S.event_rel -> 'a -> 'a) -> 'a -> 'a


(* Apply previous fold, catching cyclic graphs errors *)
  val apply_process_co :
      S.test ->
        S.concrete ->
          (S.event_rel -> 'a -> 'a) -> 'a -> 'a

  val apply_process_lo :
      S.test ->
        S.concrete ->
          (S.event_rel -> 'a -> 'a) -> 'a -> 'a

val apply_process_sc :
    S.test -> S.concrete -> (S.event_rel -> 'a -> 'a) -> 'a -> 'a


val apply_orders :
    S.event_set ->  S.event_rel ->
      (S.event_rel -> 'o) (* kont for failure *) ->
        (S.event_rel -> 'o -> 'o) (* kont for one order *) ->
          'o -> 'o
(* fold over possibilities when saturating memory order wrt atomicity classes.
     'fold_saturated_mem_order es mem_order kont res'
     - es is the event structure for calculation
     - mem_order is the order relation on accesses found so far
     - kont and res are folded function and initial value, 
	kont takes as additional argument the new saturating edges *)
  val fold_saturated_mem_order :
      S.event_structure ->
	S.event_rel ->
	  (S.event_rel -> 'a -> 'a) -> 'a -> 'a



(*****************************************)
(* Compute write serialization precursor *)
(*****************************************)
(* Iniital pco: account for init stores *)
  val compute_pco_init : S.event_structure -> S.event_rel
(* Assumes complete uniproc and hence may fail (because of Init in rfmap *)
  val compute_pco : S.rfmap -> S.event_rel -> S.event_rel option

(***************************)
(* Final state is relevant *)
(***************************)
  val final_is_relevant : S.test -> S.A.state -> bool
end

