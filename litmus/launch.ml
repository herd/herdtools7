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



type t = Fixed | Changing

let tags = ["fixed";"changing";]

let parse tag = match String.lowercase tag with
| "fixed" -> Some Fixed
| "changing" -> Some Changing
| _ -> None

let pp = function
  | Fixed -> "fixed"
  | Changing -> "changing"
