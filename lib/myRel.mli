(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Public interafec of relations *)

module type S = sig

  type elt1
  type elt2

  module Elts1 : MySet.S with type elt = elt1
  module Elts2 : MySet.S with type elt = elt2
  
  type t

  (* Old, set of pairs interface *)
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val is_empty : t -> bool
  val mem : (elt1 * elt2) -> t -> bool

  val empty : t
  val singleton : (elt1 * elt2) -> t
  val of_list : (elt1 * elt2) list -> t

  val add : (elt1 * elt2) -> t -> t
  val remove : (elt1 * elt2) -> t -> t
  val choose : t -> (elt1 * elt2)
  val cardinal : t -> int

  val iter : ((elt1 * elt2) -> unit) -> t -> unit
  val fold : ((elt1 * elt2) -> 'a -> 'a) -> t -> 'a -> 'a
  val exists :  ((elt1 * elt2) -> bool) -> t -> bool
  val for_all :  ((elt1 * elt2) -> bool) -> t -> bool
  val to_seq : t -> (elt1 * elt2) Seq.t

  val split3 : t -> t * (elt1 * elt2) * t

  (* Specific *)
  val exists_succ : t -> elt1 -> bool
  val exists_pred : t -> elt2 -> bool

  val succs : t -> elt1 -> Elts2.t
  val preds : t -> elt2 -> Elts1.t

(* Various ways to build a relation *)
  val cartesian : Elts1.t -> Elts2.t -> t
  val of_pred :  Elts1.t -> Elts2.t -> (elt1 -> elt2 -> bool) -> t

(* Extract domain and codomain *)
  val domain : t -> Elts1.t
  val codomain : t -> Elts2.t

(* Restriction of domain/codomain *)
  val restrict_domain : (elt1 -> bool) -> t -> t
  val restrict_codomain : (elt2 -> bool) -> t -> t
  val restrict_domains : (elt1 -> bool) -> (elt2 -> bool) -> t -> t
  val restrict_domains_to_sets : Elts1.t -> Elts2.t -> t -> t
  val restrict_rel : (elt1 -> elt2 -> bool) -> t -> t

  (* Set like operations *)
  val subrel : t -> t -> bool
  val subset : t -> t -> bool
  val union : t -> t -> t
  val union3 : t -> t -> t -> t
  val union4 :  t -> t -> t -> t -> t
  val union5 :  t -> t -> t -> t -> t -> t
  val union6 :  t -> t -> t -> t -> t -> t -> t
  val unions : t list -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
end
