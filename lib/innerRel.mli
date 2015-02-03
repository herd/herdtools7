(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Operations on relations *)

module type S =  sig
  type elt0

  module Elts : MySet.S with type elt = elt0
	
  include Rel.S with
  type elt1 = elt0 and type elt2 = elt0
  and module Elts1 = Elts and module Elts2 = Elts

(* All elements related *)
  val nodes : t -> Elts.t

(* Inverse *)
  val inverse : t -> t

(* Set to relation *)
  val set_to_rln : Elts.t -> t

(* Are e1 and e2 related by the transitive closure of relation.
   Does not detect cycles *)
  val mem_transitive : elt1 * elt2 -> t -> bool

(* Nodes reachable from node and set of nodes [argument included in result] *)
  val reachable : elt0 -> t -> Elts.t
  val reachable_from_set : Elts.t -> t -> Elts.t

(* Idem backwards *)
  val up :  elt0 -> t -> Elts.t
  val up_from_set : Elts.t -> t -> Elts.t

(* Does not detect cycles either *)
  val transitive_closure : t -> t

(* Direct cycles *)
  val is_reflexive : t -> bool
  val is_irreflexive : t -> bool

(* Explicit acyclicity check,  cost (one dfs) is neglectible
   w.r.t. transitive closure *)
  val get_cycle : t ->  elt0 list option
  val is_acyclic : t -> bool
  val is_cyclic : t -> bool

(* All toplogical orders, raises Cyclic in case of cycle
   Enhancement: all_topos nodes edges still works
    when edges relates elts not in nodes *)
  
  exception Cyclic
  val topo : Elts.t -> t -> elt0 list
(* Continuation based all_topos (see next function) *)
  val all_topos_kont :  Elts.t -> t -> (elt0 list -> 'a -> 'a) -> 'a -> 'a
(* All toplogical orders, as a list of lists *)
  val all_topos : bool (* verbose *) -> Elts.t -> t -> elt0 list list

(* Remove any transitivity edges
   [LUC: set argument. removed, it is useless, since set = nodes rel is ok]
   remove_transitive_edges [set] rel
      [assumes rel \in set \times set]
      returns rel' \subset rel such that rel' 
        does not have (e1, e2) if it has both (e1, e3) and (e3, e2),
        but transitive_closure rel' = transitive_closure rel
*)
  val remove_transitive_edges : t -> t

(* Sequence composition of relation *)
  val sequence : t-> t -> t

end

module Make:
functor (O:MySet.OrderedType) -> S
with type elt0 = O.t and module Elts = MySet.Make(O)
