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

(** Utilities for parsing command line option arguments *)

type 'a tfun = string -> ('a -> unit) -> string -> string * Arg.spec * string
(** Type of generic [parse] functions for handling argument of type ['a]:
   [parse name handle msg]  returns the triple that can serve as an
   option specification of the [Arg] module. Where:
    - [name] is the option name;
    - [handle] handles the argument of type ['a] by side effect;
    - [msg] is the documentation message.
*)


type 'a tref = string -> 'a ref -> string -> string * Arg.spec * string
(** Type of more specific [parse] functions:
   [parse name reference msg] returns the triple that can serve as an
   option specification of the [Arg] module. Where:
    - [name] is the option name;
    - [reference] is a reference to store the parsed argument of type ['a];
    - [msg] is the documentation message.
*)


val parse_bool : bool tref
(** For boolean argument. The initial value of the reference argument
    is the default value and will be added to the documentation message *)

val parse_int : int tref
(** Similar to [parse_bool] above,  for an argument of type int *)

val parse_int_opt : int option tref
(** Optional argument of type int, no default value *)


(** The following functions are similar, only the type varies:
    handling floats, positions (pair of floats) and strings *)
val parse_float : float tref
val parse_float_opt : float option tref

type pos = float * float
val parse_pos : pos tref
val parse_pos_opt : pos option tref

val parse_string : string tref
val parse_string_opt : string option tref


(** Parsing of string sets. The syntax of argument is a comma separated
    list of strings *)
val parse_stringsetfun : StringSet.t tfun
val parse_stringset : StringSet.t tref

type ttag =
  string -> (string -> bool) -> string list
    -> string -> string * Arg.spec * string
(** Type of functions for parsing tags and list of tags.
   [parse name handle tags doc] returns the triple that can serve as an
   option specification of the [Arg] module. Where:
      - [name] is the name of the option;
      - [handle] is a function of type [string -> bool] that checks
        tag validity and handles it by side effect. The function [handle]
        should return [false] on invalid tags. The [parse] function will
        then raise the [Arg.Bad] exception with an appropriate message
        as argument.
      - [tags] is the set of valid tags, part of documentation and of
        the error message above.
      - [doc] is the documentation message.
*)

val parse_tag : ttag
(** Parse a single tag *)

val parse_tags : ttag
(** Parse a list of comma separated tags. The [handle] function
    is iterated on that list. *)
