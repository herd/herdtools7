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


(* SMT numbering convention *)
type t = Seq | End | No

let tags = ["none"; "seq" ; "end";]

let parse tag  = match String.lowercase tag with
  | "none" -> Some No
  | "seq" -> Some Seq
  | "end" -> Some End
  | _ ->  None

let pp = function
  | No -> "none"
  | Seq -> "seq"
  | End -> "end"
