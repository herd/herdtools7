(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Utility functions for lexers *)

(* The Lexing module of standard library defines
     - input buffers for ocamllex lexers [type lexbuf]
     - position in streams being lexed [type position]
*)
open Lexing

module type Config = sig
  val debug : bool
end

module Default : Config

module Make : functor (O:Config) -> sig

(* Build some special lexbuf:   'from_section (pos1,pos2) chan'
   Returns a lexbuf that behave as if the lexed stream starts at
   pos1 in chan (included) and ends at position pos2 (excluded) *)
val from_section : Pos.pos2 -> in_channel -> lexbuf



(* Lexer used elsewhere *)
val skip_comment : lexbuf -> unit
val skip_c_comment : lexbuf -> unit
val skip_c_line_comment : lexbuf -> unit
val skip_string : lexbuf -> unit
end
