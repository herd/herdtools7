(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

type t = Std | PreSi

let tags = ["std";"presi";]

let parse tag = match String.lowercase tag with
| "std" -> Some Std
| "presi" -> Some PreSi
| _ -> None

let pp = function
  | Std -> "std"
  | PreSi -> "presi"
