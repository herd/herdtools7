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

val get_tag : string list -> tag

type reduced =
  | Rel of string * (string * string)
  | Set of string * string

val reduce :
 (string list -> string * string array) -> string list -> reduced

type t =
  | Connect of tag * string list * t list
  | Arg of reduced * string list

type d = Def of tag * reduced * string list * t list

val pp_defs : out_channel -> d list -> unit
