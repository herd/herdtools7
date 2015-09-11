(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
open Lexing
open Printf

let zyva fname =
  Misc.input_protect
    (fun chan ->
      let lexbuf = Lexing.from_channel chan in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname=fname;};
      let pgm =  GenParser.call_parser "progC" lexbuf CLexer.main CParser.main in
      List.iter
        (fun f ->
          printf "Code de %i: '%s'\n" f.CAst.proc f.CAst.body)
        pgm ;
      exit 0)
    fname


let () =
  try zyva Sys.argv.(1) 
  with Misc.Exit -> exit 2
