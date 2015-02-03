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



type t = C | Shell | XCode

let tags = ["C";"shell";"xcode";]

let parse tag = match String.lowercase tag with
| "c" -> Some C
| "shell" -> Some Shell
| "xcode" -> Some XCode
| _ -> None

let pp = function
  | C -> "C"
  | XCode -> "xcode"
  | Shell -> "shell"
