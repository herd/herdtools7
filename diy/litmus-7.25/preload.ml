(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


type c = One | Two
type t =
  | NoPL|RandomPL|CustomPL
  | StaticPL (* Hardwired prefetch *)
  | StaticNPL of c

let tags = ["no";"random";"custom";"static";"static2"]

let parse tag = match Misc.lowercase tag with
| "false"|"no" -> Some NoPL
| "true"|"random" -> Some RandomPL
| "custom" -> Some CustomPL
| "static" -> Some StaticPL
| "static1" -> Some (StaticNPL One)
| "static2" -> Some (StaticNPL Two)
| _ -> None

let pp = function
  | NoPL -> "no"
  | RandomPL -> "random"
  | CustomPL -> "custom"
  | StaticPL -> "static"
  | StaticNPL One -> "static1"
  | StaticNPL Two -> "static2"
