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
  | Precise (* Precise exception in kvm mode, ie jump to end of thread code in case of exception *)
let compare = compare

let tags = ["self";"precise";]

let parse s = match Misc.lowercase s with
| "self" -> Some Self
| "precise" -> Some Precise
| _ -> None

let pp = function
  | Self -> "self"
  | Precise -> "precise"

let ok v a = match v,a with
| Self,`AArch64 -> true
| _,_ -> false

let setnow _ = false
