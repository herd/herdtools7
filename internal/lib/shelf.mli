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

(** An OCaml representation of a shelf.py file. *)

type t = {
  record : string ;

  cats : string list ;
  configs : string list ;
  tests : string list ;
  bells : string list option ;
  compatibilities : string list option ;
}

val compare : t -> t -> int

(** [to_ocaml_string s] returns a string of the OCaml literal representation of
 *  a Shelf [s]. *)
val to_ocaml_string : t -> string

(** [of_file path] reads a Shelf from a shelf.py file.
 *  It can raise [ParseError]. *)
val of_file : string -> t
