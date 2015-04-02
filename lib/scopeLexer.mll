(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

{
module Make(O:LexUtils.Config) = struct
open Lexing
open ScopeParser
open LexMisc
module LU = LexUtils.Make(O)

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
