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

open Lexing

exception Error of string * position

let error msg lexbuf = raise (Error (msg,lexbuf.lex_curr_p ))

let incr_lineno lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- 
    { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum; }

let init_file name lexbuf =
  lexbuf.lex_curr_p <-
    {pos_fname = name; pos_lnum = 1;
     pos_bol = 0; pos_cnum = 0};
