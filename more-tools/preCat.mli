(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Before actual Cat code *)

type tag = And | Or | Seq

val pp_tag : tag -> string

val get_tag : string list -> tag

type arg = ANone | ASet of string | ARel of string * string

type name =
  | Plus of name
  | Inverse of string
  | Name of string
  | Neg of name

(* Get name inside *)
val get_name : name -> string

(* Pretty print *)
val pp_name : name -> string

(* Map on string inside name *)
val map_name : (string -> string) -> name -> name

(* Expression structure *)
type reduced =
  | Rel of name * (string * string)
  | Set of string * string

val reduce :
 (string list -> name * string array) -> string list -> reduced

type t =
  | Connect of tag * arg * t list * string list
  | Arg of reduced * string list

val pp_trees : out_channel -> t list -> unit
val pp_tree : out_channel -> t -> unit

type d = Def of tag * reduced * t list  * string list

val pp_def : out_channel -> d -> unit

val pp_defs : out_channel -> d list -> unit
