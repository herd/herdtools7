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
let name  = (alpha|'_'|'.'|'$')+ (alpha|digit|'_' | '/' | '.' | '-')*
let decimal = '-' ? digit+
let hexadecimal = ("0x"|"0X") hexadigit+
let num = decimal | hexadecimal

rule token = parse
| "NOP" | "nop"    { TOK_NOP }
| [' ''\t''\r'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf }
| num as num
   {NUM num }
| 'P' (decimal as x)
    { PROC (int_of_string x) }
| '%' (name as name) { SYMB_REG name }
| ',' { COMMA }
| '&' { AMPER }
| ';' { SEMI }
| ':' { COLON }
| '+' { PLUS }
| '[' { LBRK }
| ']' { RBRK }
| '('  { LPAR }
| ')' { RPAR }
| '{' { LCURLY }
| '}' { RCURLY }
| '=' { EQUAL }
| "==" { EQUALEQUAL }
| "!="|"<>" { NOTEQUAL }
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
| "final"    { FINAL }
| "with"     { WITH }
| "locations" { LOCATIONS }
| "filter" { FILTER }
| "fault"|"Fault" { FAULT }
| "tag"|"TAG" { TOK_TAG }
(* Distinguished  descriptor fields *)
| "attrs"|"Attrs" { ATTRS }
| "dt"  { DTYPE }
| "oa" { TOK_OA }
(* Descriptor keywords *)
| "PTE" { TOK_PTE }
| "TTD" { TOK_TTD }
| "PA"  { TOK_PA }
(* Descriptor level *)
| "LV2" { LEVEL_LV2 }
| "LV3" { LEVEL_LV3 }
(* Translation stages - enables future expansion of herd to stage 2 *)
| "S1" { STAGE_S1 }
(* | "S2" { STAGE_S2 } *)
(* PAR_EL1 *)
| "parel1_t"|"PAREL1_T" { TOK_PAR }
(* Typing *)
| "_Atomic" { ATOMIC }
| "ATOMIC_INIT" { ATOMICINIT }
| "instr:" '"' ([^'"']+ as i) '"' { INSTR i }
| "label:" '"' 'P'? (decimal as p) ':' ([^'"']+ as l)  '"' { LABEL ((int_of_string p), l) }
| '`' ([^'`']+ as i) '`' { VALUE i }
(*for GPU*)
| ".reg" {PTX_REG_DEC}
| ".s32" as x
| ".b64" as x
| ".b32" as x
| ".u64" as x
| ".u32" as x
| ".pred" as x {PTX_REG_TYPE x}
(* Memory Tagging *)
| "*" { STAR }
| '$' (digit+|alpha+) as name { DOLLARNAME name }
| "__int128" as name { NAME name }
| "__uint128" as name { NAME name }
| '_' ? name as name { NAME name }
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
