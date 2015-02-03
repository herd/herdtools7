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


(* Additional arch specification, not so nice... *)

type t = No | ARMv6K | Cheri

let tags = ["armv6k";"cheri"; "none"]

let parse tag = match String.lowercase tag with
| "none" -> Some No
| "armv6k" -> Some ARMv6K
| "cheri" -> Some Cheri
| _ -> None

let pp = function
  | No -> "none"
  | ARMv6K -> "ARMv6K"
  | Cheri -> "cheri"
