(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Debug tags *)

type t = {
  solver : bool ;
  lexer : bool ;
  top : bool ;
  mem : bool ;
  barrier : bool ;
  res : bool ;
  rfm : bool  ;
  }

val none : t
val tags : string list
val parse : t -> string -> t option


