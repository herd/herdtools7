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

{

module Make(O:LexUtils.Config) = struct
open Lexing
open LexMisc
open StateParser
module LU = LexUtils.Make(O)
}

let digit = [ '0'-'9' ]
let hexadigit = [ '0'-'9' 'a'-'f' 'A'-'F']
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = alpha (alpha|digit|'_' | '/' | '.' | '-')*
let decimal = '-' ? digit+
let hexadecimal = ("0x"|"0X") hexadigit+
let num = decimal | hexadecimal

rule token = parse
| [' ''\t''\r'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf }
| num as num
   {NUM num }
| 'P' (decimal as x)
    { PROC (int_of_string x) }
| '%' (name as name) { SYMB_REG name }
| '&' { AMPER }
| ';' { SEMI }
| ':' { COLON }
| '[' { LBRK }
| ']' { RBRK }
| '('  { LPAR }
| ')' { RPAR }
| '=' { EQUAL }
| "==" { EQUALEQUAL }
| "!="|"<>" { NOTEQUAL }
| '+' { PLUS_DISJ }
| "=>" { IMPLIES }
 | "/\\" {AND}
| "\\/" {OR}
| '~'| "not" { NOT }
| "true"     { TRUE }
| "false"     { FALSE }
| "observed"|"Observed"   { OBSERVED }
| "and" { TOKAND }
| "exists"   { EXISTS }
| "forall"   { FORALL }
| "cases"    { CASES }
| "final"    { FINAL }
| "with"     { WITH }
| "locations" { LOCATIONS }
| "filter" { FILTER }
(* Typing *)
| "_Atomic" { ATOMIC }
| "ATOMIC_INIT" { ATOMICINIT }
(*for GPU*)
| ".reg" {PTX_REG_DEC}
| ".s32" as x
| ".b64" as x
| ".b32" as x
| ".u64" as x
| ".u32" as x
| ".pred" as x {PTX_REG_TYPE x}
(* Memory Tagging *)
| ".patag" {PATAG}

| "*" { STAR }
| '$' (digit+|alpha+) as name { DOLLARNAME name }
| name as name { NAME name }
| eof { EOF }
| "<<" { error "<<" lexbuf }
| "" { error "Init lex" lexbuf }

{
 let token lexbuf =
   let tok = token lexbuf in
   if O.debug then begin
     Printf.eprintf
       "%a: Lexed '%s'\n"
       Pos.pp_pos2
       (lexeme_start_p lexbuf,lexeme_end_p lexbuf)
       (lexeme lexbuf) ;
     Printf.eprintf
       "LOC=%a\n" Pos.debug_pos (lexeme_start_p lexbuf)
   end ;
   tok
end
}
