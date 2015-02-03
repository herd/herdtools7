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



type t = No | All | Write

let tags = [ "no"; "all"; "write"; ]


let parse tag  = match String.lowercase tag with
| "false"|"no" -> Some No
| "true"|"all" -> Some All
| "write" -> Some Write
| _ -> None 

let pp = function
| No -> "no"
| All -> "all"
| Write -> "write"

