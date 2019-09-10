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
open X86_64Parser
module X86_64 = X86_64Base
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
| '-' ? num as x { NUM x }
| '$' ('-'? num as x) { INTEL_NUM x }
| 'P' (num as x)
    { PROC (int_of_string x) }
| '%' (name as name) { SYMB_REG name }
| ';' { SEMI }
| ',' { COMMA }
| '|' { PIPE }
| '(' { LPAR }
| ')' { RPAR }
| '[' { LBRK }
| ']' { RBRK }
| ':' { COLON }
| "add"|"ADD"   { I_ADD }
| "addb"|"ADDB"   { I_ADDB }
| "addw"|"ADDW"   { I_ADDW }
| "addl"|"ADDL"   { I_ADDL }
| "addq"|"ADDQ"   { I_ADDQ }
| "xor"|"XOR"   { I_XOR }
| "or"|"OR"   { I_OR }
| "mov"|"MOV"   { I_MOV }
| "movb"|"MOVB"   { I_MOVB }
| "movw"|"MOVW"   { I_MOVW }
| "movl"|"MOVL"   { I_MOVL }
| "movq"|"MOVQ"   { I_MOVQ }
| "movt"|"MOVT"   { I_MOVT }
| "movsd"|"MOVSD"   { I_MOVSD }
| "dec"|"DEC"   { I_DEC }
| "cmp"|"CMP"   { I_CMP }
| "cmovc"|"CMOVC"   { I_CMOVC }
| "inc"|"INC"   { I_INC }
| "jmp"|"JMP"   { I_JMP }
| "je"|"JE"    { I_JE }
| "jne"|"JNE"    { I_JNE }
| "lock"|"LOCK"   { I_LOCK }
| "xchg"|"XCHG"   { I_XCHG }
| "cmpxchg"|"CMPXCHG"   { I_CMPXCHG }
| "lfence"|"LFENCE"   { I_LFENCE }
| "sfence"|"SFENCE"   { I_SFENCE }
| "mfence"|"MFENCE"   { I_MFENCE }
| "read"|"READ"       { I_READ }
| "setnb"|"SETNB"       { I_SETNB }
| name as x
  { match X86_64.parse_reg x with
  | Some r -> ARCH_REG r
  | None -> NAME x }
| eof { EOF }
| ""  { error "X86_64" lexbuf }

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
