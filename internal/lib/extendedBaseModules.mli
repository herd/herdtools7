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

(** Extending built-in / base modules, either to port future features into
 *  earlier versions of OCaml, or to add new functionality. *)

module Int : sig
  (* TODO: iff we raise our minimum OCaml version to 4.08,
   *       include module type of Int. *)
  type t = int

  (** [compare x y] is a non-polymorphic version of [Pervasives.compare] or
   *  [Stdlib.compare]. It has identical semantics to [Int.compare] from OCaml
   *  >= 4.08. *)
  val compare : int -> int -> int

  (** [to_ocaml_string i] returns an OCaml syntax form of [i].
   *  For example, [to_ocaml_string 1] returns ["1"]. *)
  val to_ocaml_string : int -> string
end

module String : sig
  include module type of String

  (** [to_ocaml_string s] returns an OCaml syntax form of [s].
   *  For example, [to_ocaml_string "a"] returns ["\"a\""]. *)
  val to_ocaml_string : string -> string
end

module List : sig
  include module type of List

  (** [compare c xs ys] compares lists [xs] and [ys], first by length, then by
   *  comparing each pair of elements of [xs] and [ys] with compare function [c]
   *  until a pair differs. *)
  val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int

  (** [to_ocaml_string f xs] returns an OCaml syntax form of [xs], applying [f]
   *  to each element of [xs]. For example,
   *  [to_ocaml_string String.to_ocaml_string ["a"; "b"]] returns
   *  "[\"a\"; \"b\"]". *)
  val to_ocaml_string : ('a -> string) -> 'a list -> string

  module Make : functor (Elt : Compare.S) -> Compare.S with type t = Elt.t list
end
