(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Utilities for command-line options *)

type 'a tfun = string -> ('a -> unit) -> string -> string * Arg.spec * string
type 'a tref = string -> 'a ref -> string -> string * Arg.spec * string

val parse_bool : bool tref
val parse_int : int tref
val parse_int_opt : int option tref

val parse_float : float tref
val parse_float_opt : float option tref

type pos = float * float
val parse_pos : pos tref
val parse_pos_opt : pos option tref

val parse_string : string tref
val parse_string_opt : string option tref

val parse_stringsetfun : StringSet.t tfun
val parse_stringset : StringSet.t tref
