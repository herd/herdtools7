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


(* Stop test as condition is settled *)
type t =
  | NoSpeed (* Don't *)
  | SomeSpeed (* Do for current run *)
  | AllSpeed  (* Do for current run and following runs *)

val tags : string list
val parse : string -> t option
val pp : t -> string
