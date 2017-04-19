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

{
module Make(O:LexUtils.Config) = struct
open Lexing
open ScopeParser
open LexMisc
}


let digit =  [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = alpha (alpha|digit|'_' | '/' | '-')*
let num = "0x"?digit+

rule token = parse
| [' ''\t''\r'] { token lexbuf  }
| '\n'      { incr_lineno lexbuf; token lexbuf  }
| num as x { NUM (int_of_string x) }
| 'P' (num as x)
    { NUM (int_of_string x) }
| '(' { LPAR  }
| ')' { RPAR  }
| name as x
    { NAME x }
| eof { EOF }
| ""  { error "Scope lexer" lexbuf }

{
let token lexbuf =
   let tok = token lexbuf in
   if O.debug then begin
     Printf.eprintf
       "%a: Lexed '%s'\n"
       Pos.pp_pos2
       (lexeme_start_p lexbuf,lexeme_end_p lexbuf)
       (lexeme lexbuf)
   end ;
   tok
 end
}
