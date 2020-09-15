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
open ARMParser
module ARM = ARMBase
module LU = LexUtils.Make(O)
}
let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = alpha (alpha|digit|'_' | '/' | '.' | '-')*
let num = digit+

rule token = parse
| [' ''\t''\r'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf }
| '-' ? num as x { NUM (int_of_string x) }
| '#' ('-' ? num as x) { NUM (int_of_string x) }
| 'P' (num as x)
    { PROC (int_of_string x) }
| '%' (name as name) { SYMB_REG name }
| '&' (name as name) { META name }
| ';' { SEMI }
| ',' { COMMA }
| '|' { PIPE }
| '[' { LBRK }
| ']' { RBRK }
| ':' { COLON }
| "add" | "ADD"   { I_ADD }
| "adds" | "ADDS"   { I_ADDS }
| "and" | "AND"   { I_AND }
| "ands" | "ANDS"   { I_ANDS }
| "bne" | "BNE"   { I_BNE }
| "beq" | "BEQ"   { I_BEQ }
| "cbz" | "CBZ"   { I_CBZ }
| "cbnz" | "CBNZ"   { I_CBNZ }
| "cmp" | "CMP"   { I_CMP }
| "ldr" | "LDR"   { I_LDR }
| "ldrex" | "LDREX"   { I_LDREX }
| "ldrne" | "LDRNE"   { I_LDRNE }
| "ldreq" | "LDREQ"   { I_LDREQ }
| "str" | "STR"   { I_STR }
| "strne" | "STRNE"   { I_STRNE }
| "streq" | "STREQ"   { I_STREQ }
| "strex" | "STREX" { I_STREX }
| "mov" | "MOV"   { I_MOV }
| "movne" | "MOVNE"   { I_MOVNE }
| "moveq" | "MOVEQ"   { I_MOVEQ }
| "xor" | "XOR"   { I_XOR }
| "eor" | "EOR"   { I_XOR }
| "eors" | "EORS"   { I_XOR }
| "dmb" | "DMB"   { I_DMB }
| "dsb" | "DSB"   { I_DSB }
| "isb" | "ISB"   { I_ISB }
| "b" | "B" { I_B }
| "sy" | "SY" { I_SY }
| "st" | "ST" { I_ST }
| "ish" | "ISH" { I_ISH }
| "ishst" | "ISHST" { I_ISHST }
| "nsh" | "NSH" { I_NSH }
| "nshst" | "NSHST" { I_NSHST }
| "osh" | "OSH" { I_OSH }
| "oshst" | "OSHST" { I_OSHST }
| "codevar:" (name as x) { CODEVAR x }
| name as x
  { match ARM.parse_reg x with
  | Some r -> ARCH_REG r
  | None -> NAME x }
| eof { EOF }
| ""  { error "ARM lexer" lexbuf }

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
