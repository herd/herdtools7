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

open Printf

type k = Int of int | Meta of string

let zero = Int 0

let pp = function
  | Int i -> sprintf "%i" i
  | Meta v -> sprintf "&%s" v

let fatal_meta v =
  Warn.fatal "Unexpected meta variable %s" (pp v)
let as_int = function
  | Int i -> i
  | Meta _ as v -> fatal_meta v
      



let compare k1 k2 = match k1,k2 with
| Int i1,Int i2 -> Pervasives.compare i1 i2
| Meta v1,Meta v2 -> String.compare v1 v2
| Int _,Meta _ -> -1
| Meta _,Int _ -> 1


