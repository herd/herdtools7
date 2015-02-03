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


type t = W32 | W64 |WXX

let tags = ["w32"; "w64"; "wXX";]

let parse tag = match String.lowercase tag with
| "w32" -> Some W32
| "w64" -> Some W64
| "wxx" -> Some WXX
| _ -> None

let pp = function
  | W32 -> "w32"
  | W64 -> "w64"
  | WXX -> "wXX"
