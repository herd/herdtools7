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



type t = Local|After|Both

let tags = ["local";"after";"both";]

let parse tag = match String.lowercase tag with
| "local" -> Some Local
| "after" -> Some After
| "both" -> Some Both
| _ ->  None

let pp = function
  | Local -> "local"
  | After -> "after"
  | Both -> "both"
