(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

type t = False | True | Fast

let tags = ["false";"true";"fast";]

let parse tag = match String.lowercase tag with
| "false" -> Some False
| "true" -> Some True
| "fast" -> Some Fast
| _ -> None

let pp = function
  | False -> "false"
  | True -> "true"
  | Fast -> "fast"
