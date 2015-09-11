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

(** Implementation of relations *)

module type S = sig

  type elt1
  type elt2

  include MySet.S with type elt = elt1 * elt2


  module Elts1 : MySet.S with type elt = elt1
  module Elts2 : MySet.S with type elt = elt2
  val exists_succ : t -> elt1 -> bool
  val exists_pred : t -> elt2 -> bool
 
  val succs : t -> elt1 -> Elts2.t
  val preds : t -> elt2 -> Elts1.t

(* Various ways to build a relation *)
  val cartesian : Elts1.t -> Elts2.t -> t
  val of_preds : Elts1.t -> elt2 -> t
  val of_succs : elt1 -> Elts2.t -> t
  val of_pred :
      Elts1.t -> Elts2.t ->
	(elt1 -> elt2 -> bool) -> t

(* Extract domain and codomain *)
  val domain : t -> Elts1.t
  val codomain : t -> Elts2.t

(* Restriction of domain/codomain *)
  val restrict_domain : (elt1 -> bool) -> t -> t
  val restrict_codomain : (elt2 -> bool) -> t -> t
  val restrict_domains : (elt1 -> bool) -> (elt2 -> bool) -> t -> t
  val restrict_rel : (elt1 -> elt2 -> bool) -> t -> t

end

module Make :
functor (O1:MySet.OrderedType) ->
  functor(O2:MySet.OrderedType) ->
    S with  
type elt1 = O1.t and type elt2 = O2.t
and module Elts1 = MySet.Make(O1)
and module Elts2 = MySet.Make(O2)
