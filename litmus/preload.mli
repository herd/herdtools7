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


type c = One | Two
type t =
  | NoPL|RandomPL|CustomPL
  | StaticPL       (* Hardwired prefetch *)
  | StaticNPL of c (* Old static preload *)

val tags : string list
val parse : string -> t option
val pp : t -> string
