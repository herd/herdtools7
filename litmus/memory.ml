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



type t = Direct | Indirect

let tags = ["direct";"indirect";]

let parse tag  = match String.lowercase tag with
| "direct" -> Some Direct
| "indirect" -> Some Indirect
| _ -> None

let pp = function
  | Direct -> "direct"
  | Indirect -> "indirect"
