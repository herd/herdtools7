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

let check_name name =
match name with
| "add" | "ADD" -> I_ADD
| "adds" | "ADDS"   -> I_ADDS
| "bx" | "BX" -> I_BX
| "sub" | "SUB"   -> I_SUB
| "subs" | "SUBS" -> I_SUBS
| "and" | "AND"   -> I_AND
| "orr" | "ORR"   -> I_ORR
| "ands" | "ANDS"   -> I_ANDS
| "bne" | "BNE"   -> I_BNE
| "beq" | "BEQ"   -> I_BEQ
| "cbz" | "CBZ"   -> I_CBZ
| "cbnz" | "CBNZ"   -> I_CBNZ
| "cmp" | "CMP"   -> I_CMP
| "ldr" | "LDR"   -> I_LDR
| "ldm" | "LDM"   -> I_LDM
| "ldrd" | "LDRD"   -> I_LDRD
| "ldmib" | "LDMIB"   -> I_LDMIB
| "ldrex" | "LDREX"   -> I_LDREX
| "ldaex" | "LDAEX"   -> I_LDAEX
| "ldrne" | "LDRNE"   -> I_LDRNE
| "ldreq" | "LDREQ"   -> I_LDREQ
| "lda" | "LDA" -> I_LDA
| "str" | "STR"   -> I_STR
| "strne" | "STRNE"   -> I_STRNE
| "streq" | "STREQ"   -> I_STREQ
| "strex" | "STREX" -> I_STREX
| "stlex" | "STLEX" -> I_STLEX
| "stl" | "STL" -> I_STL
| "mov" | "MOV"   -> I_MOV
| "movw" | "MOVW" -> I_MOVW
| "movt" | "MOVT" -> I_MOVT
| "movne" | "MOVNE"   -> I_MOVNE
| "moveq" | "MOVEQ"   -> I_MOVEQ
| "xor" | "XOR"   -> I_XOR
| "eor" | "EOR"   -> I_XOR
| "eors" | "EORS" -> I_XOR
| "dmb" | "DMB"   -> I_DMB
| "dsb" | "DSB"   -> I_DSB
| "isb" | "ISB"   -> I_ISB
| "b" | "B" -> I_B
| "sy" | "SY" -> I_SY
| "st" | "ST" -> I_ST
| "ish" | "ISH" -> I_ISH
| "ishst" | "ISHST" -> I_ISHST
| "nsh" | "NSH" -> I_NSH
| "nshst" | "NSHST" -> I_NSHST
| "osh" | "OSH" -> I_OSH
| "oshst" | "OSHST" -> I_OSHST
| _  -> begin match ARM.parse_reg name with
  | Some r -> ARCH_REG r
  | None -> NAME name
  end

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
| '{' { LPAREN }
| '}' { RPAREN }
| ':' { COLON }
| "codevar:" (name as x) { CODEVAR x }
| name as x { check_name x }
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
