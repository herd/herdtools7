(*********************************************************************)
(*                        DIY                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Sets with polymorphic keys, a clone from OCaml stlib *)

module type OrderedType =
  sig
    type 'k t
    val compare: 'k t -> 'k t -> int
  end

module type S =
  sig
    type 'k elt
    type 'k t
    val empty:'k t
    val is_empty:'k t -> bool
    val mem:'k elt ->'k t -> bool
    val add:'k elt ->'k t ->'k t
    val singleton:'k elt ->'k t
    val remove:'k elt ->'k t ->'k t
    val union:'k t ->'k t ->'k t
    val inter:'k t ->'k t ->'k t
    val diff:'k t ->'k t ->'k t
    val compare:'k t ->'k t -> int
    val equal:'k t ->'k t -> bool
    val subset:'k t ->'k t -> bool
    val iter: ('k elt -> unit) ->'k t -> unit
    val fold: ('k elt -> 'a -> 'a) ->'k t -> 'a -> 'a
    val for_all: ('k elt -> bool) ->'k t -> bool
    val exists: ('k elt -> bool) ->'k t -> bool
    val filter: ('k elt -> bool) ->'k t ->'k t
    val partition: ('k elt -> bool) ->'k t ->'k t *'k t
    val cardinal:'k t -> int
    val elements:'k t ->'k elt list
    val min_elt:'k t ->'k elt
    val max_elt:'k t ->'k elt
    val choose:'k t ->'k elt
    val split:'k elt ->'k t ->'k t * bool *'k t
    val split3: 'k t -> 'k t * 'k elt * 'k t
    val find:'k elt ->'k t ->'k elt
    val of_list:'k elt list ->'k t
(* My additions (from MySet) *)
    val pp:
        out_channel -> string ->
      (out_channel -> 'k elt -> unit) -> 'k t -> unit
    val pp_str: string -> ('k elt -> string) -> 'k t -> string
    val unions: 'k t list -> 'k t
  end

module Make: functor (Ord:OrderedType) -> S with type 'k elt = 'k Ord.t
