(*
 * Copyright (C) 2016 David Scott <dave@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** An interval set implementation using diets. *)

module type ELT = sig
  type t
  (** The type of the set elements. *)

  include Set.OrderedType with type t := t

  val zero : t
  (** The zeroth element *)

  val pred : t -> t
  (** Predecessor of an element *)

  val succ : t -> t
  (** Successor of an element *)

  val sub : t -> t -> t
  (** [sub a b] returns [a] - [b] *)

  val add : t -> t -> t
  (** [add a b] returns [a] + [b] *)

  val to_string : t -> string
  (** Display an element. *)
end

module type INTERVAL_SET = sig
  type elt
  (** The type of the set elements *)

  type interval
  (** An interval: a range (x, y) of set values where all the elements from
      x to y inclusive are in the set *)

  module Interval : sig
    val make : elt -> elt -> interval
    (** [make first last] construct an interval describing all the elements from
        [first] to [last] inclusive. *)

    val x : interval -> elt
    (** the starting element of the interval *)

    val y : interval -> elt
    (** the ending element of the interval *)
  end

  type t
  (** The type of sets *)

  val equal : t -> t -> bool
  (** Equality over sets *)

  val compare : t -> t -> int
  (** Comparison over sets *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-print a set *)

  val empty : t
  (** The empty set *)

  val is_empty : t -> bool
  (** Test whether a set is empty or not *)

  val singleton : elt -> t
  (** [singleton x] is the set containing just [x]. *)

  val cardinal : t -> elt
  (** [cardinal t] is the number of elements in the set [t] *)

  val mem : elt -> t -> bool
  (** [mem elt t] tests whether [elt] is in set [t] *)

  val fold : (interval -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f t acc] folds [f] across all the intervals in [t] *)

  val fold_individual : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold_individual f t acc] folds [f] across all the individual elements of [t] *)

  val iter : (interval -> unit) -> t -> unit
  (** [iter f t] iterates [f] across all the intervals in [t] *)

  val add : interval -> t -> t
  (** [add interval t] returns the set consisting of [t] plus [interval] *)

  val remove : interval -> t -> t
  (** [remove interval t] returns the set consisting of [t] minus [interval] *)

  val min_elt : t -> interval
  (** [min_elt t] returns the smallest (in terms of the ordering) interval in
      [t], or raises [Not_found] if the set is empty. *)

  val max_elt : t -> interval
  (** [max_elt t] returns the largest (in terms of the ordering) interval in
      [t], or raises [Not_found] if the set is empty. *)

  val choose : t -> interval
  (** [choose t] returns one interval, or raises Not_found if the set is empty *)

  val take : t -> elt -> (t * t) option
  (** [take n] returns [Some a, b] where [cardinal a = n] and [diff t a = b]
      or [None] if [cardinal t < n] *)

  val union : t -> t -> t
  (** set union *)

  val diff : t -> t -> t
  (** set difference *)

  val inter : t -> t -> t
  (** set intersection *)

  val find_next_gap : elt -> t -> elt
  (** [find_next_gap from t] returns the next element that's
      absent in set [t] and greater than or equal to [from] **)

  (**/**)

  val check_invariants : t -> (unit, string) result
  (** [check_invariants t] returns [Ok ()] if the underlying invariants hold, or
      an error message. *)

  val height : t -> int
  (** [height t] return the height of the corresponding tree. *)
end

module Make (Elt : ELT) : INTERVAL_SET with type elt = Elt.t
module Int : INTERVAL_SET with type elt = int
module Int64 : INTERVAL_SET with type elt = int64
module Z : INTERVAL_SET with type elt = Z.t
