(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make(O:ParserConfig.Config) = struct

  open Printf

  module P = Parser.Make(O)

  let zyva oname chan =
    let open Lexing in
    let lexbuf = Lexing.from_channel chan in
    begin
      match oname with
      | Some fname ->
          lexbuf.lex_curr_p <-
            {lexbuf.lex_curr_p with pos_fname = fname}
      | None -> ()
    end ;
    try
      P.defs Lexer.token lexbuf
    with
    | Parsing.Parse_error ->
        let lxm = lexeme lexbuf
        and start_loc = lexeme_start_p lexbuf
        and end_loc = lexeme_end_p lexbuf in
        Warn.user_error "%s: unexpected '%s'" (Pos.str_pos2 (start_loc, end_loc)) lxm 
    | LexMisc.Error (msg,pos) ->
        Warn.user_error
          "%s: Lex error %s\n%!" (Pos.str_pos pos) msg
end
