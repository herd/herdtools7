(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2019-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type t =
  | Self (* Self modifying code *)
  | Precise of Precision.t
  | S128 (* 128 bit signed ints*)
  | Mixed (* Ignored *)

let compare = compare

let tags = "s128"::"self"::"mixed"::Precision.tags

let parse s = match Misc.lowercase s with
| "s128" -> Some S128
| "self" -> Some Self
| "mixed" -> Some Mixed
| tag ->
   Misc.app_opt (fun p -> Precise p) (Precision.parse tag)

let pp = function
  | Self -> "self"
  | Mixed -> "mixed"
  | S128 -> "s128"
  | Precise p -> Precision.pp p

let ok v a = match v,a with
| Self,`AArch64 -> true
| _,_ -> false

let set_precision r tag = match tag with
| Precise p -> r := p ; true
| _ -> false
