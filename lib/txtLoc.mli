(*********************************************************************)
(*                        diy                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Locations for AST *)

type t = {
  loc_start : Lexing.position ;
  loc_end : Lexing.position ;
  loc_ghost : bool ;
  }

val none : t
val make : Lexing.position -> Lexing.position -> t

val pp : out_channel -> t -> unit

