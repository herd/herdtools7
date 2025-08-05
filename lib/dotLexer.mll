(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2024-present Institut National de Recherche en Informatique et *)
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
open DotParser
open Lexing
open LexMisc
module LU = LexUtils.Make(O)
}

let digit =  [ '0'-'9' ]
let hexadigit = [ '0'-'9' 'a'-'f' 'A'-'F']
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = (alpha|'_'|'.'|'$')+ (alpha|digit|'_' | '/' | '.' | '-')*
let decimal = '-' ? digit+
let hexadecimal = ("0x"|"0X") hexadigit+
let num = decimal | hexadecimal

rule token = parse
| [' ''\t''\r'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "/*"      { LU.skip_c_comment lexbuf ; token lexbuf }
| '"'('\\''"'|[^'"'])*'"' as s { QUOTED_STRING (String.sub s 1 (String.length s - 2)) }
| ';' { SEMI }
| "[" { LBRK }
| "]" { RBRK }
| '{' { LCURLY }
| '}' { RCURLY }
| "=" { EQUAL }
| "," { COMMA }
| "->" { ARROW }
| "digraph" { GRAPH }
| "subgraph" { SUBGRAPH }
| name as name { NAME name }
| eof { EOF }

{
 let token lexbuf =
   let tok = token lexbuf in
   if O.debug then begin
     Printf.eprintf
       "%a: Lexed '%s'\n"
       Pos.pp_pos2
       (lexeme_start_p lexbuf,lexeme_end_p lexbuf)
       (lexeme lexbuf) ;
   end ;
   tok
end
}
