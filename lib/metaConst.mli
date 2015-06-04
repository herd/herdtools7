(*********************************************************************)
(*                        Constants in code                          *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Constants in code, can be a meta variable (for jingle) *)

type k = Int of int | Meta of string

val zero : k
val fatal_meta : k -> 'a
val as_int : k -> int

val pp : k -> string

val compare : k -> k -> int


