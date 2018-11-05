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

(** Additional operations on sets *)

module type OrderedType = Set.OrderedType

module type S = sig
  include Set.S

  (* Iterate with counter *)
  val iteri : (int -> elt -> unit) -> t -> unit
  (* Iterate over  cartesian product *)
  val iter2 : (elt -> elt -> unit) -> t -> t -> unit
  (* Exists on cartesian product *)
  val exists2 : (elt -> elt -> bool) -> t -> t -> bool
  (* Like exists, but returns an elt that satisfy the predicate,
     raises Not_found, if no such elt exists *)
  val fold2 : (elt -> elt -> 'a -> 'a) -> t -> t -> 'a -> 'a
  (* You get the idea I guess *)

  val find : (elt -> bool) -> t -> elt

  (* Check for a singleton *)
  val as_singleton : t -> elt option

 (* union of some sets *)
  val union3 : t -> t -> t -> t
  val union4 : t -> t -> t -> t -> t
  val union5 : t -> t -> t -> t -> t -> t
  val union6 : t -> t -> t -> t -> t -> t -> t

  (* Quite convenient: union of sets given in a list *)
  val unions : t list -> t

  (* Quite convenient: build a set from the elts in a list,
     which need not be pairwise distinct *)
  val of_list : elt list -> t

  (* Should be obvious *)
  val map : (elt -> elt) -> t -> t
  val map_list : (elt -> 'a) -> t -> 'a list
  val map_union : (elt -> t) -> t -> t
  val disjoint : t -> t -> bool

  (* Decomposition, should be efficient an trivial, given
     set implementation as a tree. It is not. *)
  val split3 : t -> t * elt * t

  (* second argument is delimiter (as in String.concat) *)  
  val pp : out_channel -> string -> (out_channel -> elt -> unit) -> t -> unit

 (* As above, but sprintf style instead of fprintf style *)
  val pp_str : string -> (elt -> string) -> t -> string

end

module Make: functor (Ord:OrderedType) -> S with type elt = Ord.t
