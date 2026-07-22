(****************************************************************************)
(*                           The Diy toolsuite                              *)
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

let verbalize_index (ix : int) : string =
  match ix with
  | 0 -> "first"
  | 1 -> "second"
  | 2 -> "third"
  | 3 -> "fourth"
  | 4 -> "fifth"
  | ix -> Format.sprintf "%dth" (ix + 1)

module NonEmpty : sig
  type 'a t = private { head : 'a; tail : 'a list }
  (** A list with at least one element. *)

  val singleton : 'a -> 'a t
  (** [singleton x] is the non-empty list containing only [x]. *)

  val cons : 'a -> 'a t -> 'a t
  (** [cons x xs] prepends [x] to [xs]. *)

  val uncons : 'a t -> 'a * 'a list

  val rev : 'a t -> 'a t
  (** [rev xs] returns [xs] in reverse order. *)

  val to_list : 'a t -> 'a list
end = struct
  type 'a t = { head : 'a; tail : 'a list }

  let singleton head = { head; tail = [] }
  let cons head { head = old_head; tail } = { head; tail = old_head :: tail }
  let uncons { head; tail } = (head, tail)

  let rev { head; tail } =
    match List.rev (head :: tail) with
    | [] -> assert false
    | head :: tail -> { head; tail }

  let to_list { head; tail } = head :: tail
end

module List : sig
  val uniq : eq:('a -> 'a -> bool) -> 'a list -> 'a list
  (** [uniq ~eq l] returns [l] with duplicate elements removed according to
      [eq].

      When two elements compare equal, the rightmost occurrence is kept and the
      earlier occurrences are dropped. The relative order of the kept elements
      is preserved. *)

  val group_by : eq:('a -> 'a -> bool) -> 'a list -> 'a NonEmpty.t list
  (** [group_by ~eq l] groups adjacent elements of [l] into non-empty runs. *)

  val intersperse : 'a -> 'a list -> 'a list
end = struct
  let rec intersperse x = function
    | [] -> []
    | [ y ] -> [ y ]
    | y :: ys -> y :: x :: intersperse x ys

  let uniq ~(eq : 'a -> 'a -> bool) (l : 'a list) : 'a list =
    let rec uniq eq acc l =
      match l with
      | [] -> List.rev acc
      | x :: xs when List.exists (eq x) xs -> uniq eq acc xs
      | x :: xs -> uniq eq (x :: acc) xs
    in
    uniq eq [] l

  let group_by ~(eq : 'a -> 'a -> bool) (l : 'a list) : 'a NonEmpty.t list =
    match l with
    | [] -> []
    | x :: xs ->
        let rec go prev group acc = function
          | [] -> List.rev (NonEmpty.rev group :: acc)
          | x :: xs ->
              if eq prev x then go x (NonEmpty.cons x group) acc xs
              else go x (NonEmpty.singleton x) (NonEmpty.rev group :: acc) xs
        in
        go x (NonEmpty.singleton x) [] xs
end

module Option : sig
  val keep_some : 'a option list -> 'a list
end = struct
  let keep_some l = Stdlib.List.filter_map Fun.id l
end

let pp_list pp_item =
  let pp_sep fmt () = Format.fprintf fmt "; " in
  Format.pp_print_list ~pp_sep pp_item
