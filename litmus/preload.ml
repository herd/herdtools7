(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


type c = One | Two
type t =
  | NoPL|RandomPL|CustomPL
  | StaticPL (* Hardwired prefetch *)
  | StaticNPL of c

let tags = ["no";"random";"custom";"static";"static2"]

let parse tag = match String.lowercase tag with
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
