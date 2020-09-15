(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Lexing

exception Error of string * position

let error msg lexbuf = raise (Error (msg,lexbuf.lex_curr_p ))

let incr_lineno lexbuf =
  let pos = lexbuf.lex_curr_p in
  let n = pos.pos_lnum + 1 in
  lexbuf.lex_curr_p <-
    { pos with
      pos_lnum = n ;
      pos_bol = pos.pos_cnum; }

let init_file name lexbuf =
  lexbuf.lex_curr_p <-
    {pos_fname = name; pos_lnum = 1;
     pos_bol = 0; pos_cnum = 0};
