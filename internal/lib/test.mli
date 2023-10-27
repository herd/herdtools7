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

(** Unit-testing utilities. *)

(** Raising [AssertionFailure msg] causes a test to fail, printing the reason
 *  for failing [msg]. *)
exception AssertionFailure of string

(** [run tests] runs every named test in [tests], printing an error message if
 *  a test fails. *)
val run : (string * (unit -> unit)) list -> unit

(** [fail msg] raises an AssertionFailure with error message [msg], causing
 *  the test to fail. *)
val fail : string -> unit


(* Pretty-printing for failure messages. *)

(** [pp_list f xs] prints a list [xs] as though OCaml syntax, applying [f] to
 *  each member. *)
val pp_list : ('a -> string) -> 'a list -> string

(** [pp_int_list xs] prints a list [xs] of ints as though OCaml syntax. For
 *  example, [pp_int_list [1; 2]] returns "[1; 2]". *)
val pp_int_list : int list -> string

(** [pp_string_list xs] prints a list [xs] of strings as though OCaml
 *  syntax. For example, [pp_string_list ["a"; "b"]] returns
 *  "[\"a\"; \"b\"]". *)
val pp_string_list : string list -> string


(* Comparisons. *)

(** [list_compare c xs ys] compares lists [xs] and [ys], first by length,
 *  comparing each pair of elements of [xs] and [ys] with compare function [c]
 *  until a pair differs. *)
val list_compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int

(** [int_list_compare xs ys] compares int lists [xs] and [ys]. *)
val int_list_compare : int list -> int list -> int

(** [string_list_compare xs ys] compares string lists [xs] and [ys]. *)
val string_list_compare : string list -> string list -> int
