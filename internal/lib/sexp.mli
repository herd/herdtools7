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

(** An S-expression type, parser, and Dune-specific parser. *)

exception ParseError of string

type t =
  | Atom of string
  | List of t list

val compare : t -> t -> int

val to_string : t -> string

(** [of_dune_file path] reads the Dune configuration file at [path].
 *  It is a special case of S-expression, because the Dune file itself is an
 *  implicit [List].
 *  [of_dune_file] can raise [ParseError]. *)
val of_dune_file : string -> t
