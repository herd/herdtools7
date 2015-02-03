(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Output restriction *)

type t = Observed | NonAmbiguous | No | CondOne

let tags = ["observed"; "nonambiguous"; "condone"; "none"; ]

let parse tag = match String.lowercase tag with
| "none" -> Some No
| "observed" -> Some Observed
| "nonambiguous" -> Some NonAmbiguous
| "condone" -> Some CondOne
| _ -> None

let pp = function
  | No -> "none"
  | Observed -> "observed"
  | NonAmbiguous -> "nonambiguous"
  | CondOne -> "condone"
