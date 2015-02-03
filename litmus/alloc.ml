(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)



type t = Dynamic | Static | Before

let tags = ["dynamic"; "static" ; "before"; ]

let parse tag = match String.lowercase tag with
| "dynamic" -> Some Dynamic
| "static" -> Some  Static
| "before" -> Some Before
| _ ->  None

let pp = function
  | Dynamic -> "dynamic"
  | Static -> "static"
  | Before -> "Before"
