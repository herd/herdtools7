(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(* Tyler Sorensen, University College London                         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

{
module Make(O:LexUtils.Config) = struct
open Lexing
open LexMisc
open BellParser
module Bell = BellBase
module LU = LexUtils.Make(O)

}


let digit =  [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = alpha (alpha|digit|'_' | '/' | '-')*
let num = "0x"?digit+

rule token = parse
| [' ''\t''\r'] { token lexbuf  }
| '\n'      { incr_lineno lexbuf; token lexbuf  }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf  }
| '-' ? num as x { NUM (int_of_string x) }
| 'P' (num as x)
    { PROC (int_of_string x) }
| ';' { SEMI  }
| '.' { DOT   }
| ',' { COMMA }
| '|' { PIPE  }
| ':' { COLON }
| '(' { LPAR  }
| ')' { RPAR  }
| ']' { RBRAC }
| '[' { LBRAC }
| '{' { LBRACE }
| '}' { RBRACE }
| '+' { PLUS }
| 'r'   { READ }
| 'w'   { WRITE }
| "f"   { FENCE }
| "rmw"  { RMW  }
| "exch" { EXCH }
| "cas"  { CAS }
| "mov"  { MOV }
| "add"  { ADD }
| "and"  { AND }
| "xor"  { XOR }
| "beq"  { BEQ }
| "bne"  { BNE }
| "bal"  { BAL }
| "scopes"  { SCOPES  }
| "regions" { REGIONS }
| name as x
    { 
      match Bell.parse_reg x with
      | Some r -> REG r
      | None ->  NAME x
    }
| eof { EOF }
| ""  { error "Bell lexer" lexbuf }

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
