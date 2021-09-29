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

(** Manage kinds.txt files. *)

exception ParseError of string

(** [t] is an association list of (name, kind). *)
type kind = ConstrGen.kind
type t = (string * kind) list

(** Check actual kinds against reference.
  * Returns pair [diff,miss] where [diff] is a listt of
  * differences and [miss] a list of test names whose kinds
  * are not in reference *)
val check :
  expected:t -> actual:t ->
    (string * kind * kind) list * string list

val compare : t -> t -> int

(* Raises ParseError. *)
val of_file : string -> t

val to_string : t -> string
