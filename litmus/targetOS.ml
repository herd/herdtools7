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


type t = Linux | Mac |AIX


let tags = ["linux"; "mac"; "aix"; ]

let parse tag = match String.lowercase tag with
| "linux" -> Some Linux
| "mac"|"macos" -> Some Mac
| "aix"|"aix5" -> Some AIX
| _ -> None

let pp = function
  | Linux -> "linux"
  | Mac -> "mac"
  | AIX -> "aix"
