(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Extending built-in / base modules, either to port future features into
 *  earlier versions of OCaml, or to add extra functionality. *)

module Fun : sig
  (** [Finally_raised e] is raised by [protect ~finally f] if [~finally] raises
   *  an exception [e], to disambiguate it from exceptions raised by [f].
   *  If [Finally_raised] is raised, it is either an unexpected exception (e.g.
   *  [Out_of_memory]), or programmer error. *)
  exception Finally_raised of exn

  (** [negate f] negates the predicate function [f]. *)
  val negate : ('a -> bool) -> ('a -> bool)

  (** [protect ~finally f] calls [f], then calls [~finally]. If [f] raises an
   *  exception [e], it calls [~finally] before re-raising [e]. If [~finally]
   *  raises an exception [e], [e] is re-raised as [Finally_raised e].
   *  It is equivalent to [Fun.protect] from OCaml >= 4.08. *)
  val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a

  (** [open_out_protect f name] applies f to a channel
   *  to file whose name is [name]. Close the file under
      all circumstances. *)
  val open_out_protect : (out_channel -> 'a) -> string -> 'a

end

module List : sig
  include module type of List

  (** [to_ocaml_string f xs] returns an OCaml syntax form of [xs], applying [f]
   *  to each element of [xs]. For example,
   *  [to_ocaml_string String.to_ocaml_string ["a"; "b"]] returns
   *  "[\"a\"; \"b\"]". *)
  val to_ocaml_string : ('a -> string) -> 'a list -> string
end

module String : sig
  include module type of String

  (** [to_ocaml_string s] returns an OCaml syntax form of [s].
   *  For example, [to_ocaml_string "a"] returns ["\"a\""]. *)
  val to_ocaml_string : string -> string
end

module Option : sig
  include module type of Option

  type 'a t = 'a option

  (** [to_ocaml_string f x] returns an OCaml syntax form of [x]. If [x] is
   * [Some x'], [f] is applied to the element [x'].
   * For example, [to_ocaml_string String.to_ocaml_string (Some "hello")]
   * returns ["Some (\"hello\")"]. *)
  val to_ocaml_string : ('a -> string) -> 'a option -> string
end

module Iter : sig
  type 'a t = unit -> 'a option

  val of_list : 'a list -> 'a t
end
