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
open LexMisc
open AArch64Parser
module A = AArch64Base
module LU = LexUtils.Make(O)

let check_name name = match name with
(* Branch *)
| "b"  | "B"  -> B
| "b.eq" | "B.EQ" -> BEQ
| "b.ne" | "B.NE" -> BNE
| "cbz"  | "CBZ" -> CBZ
| "cbnz"  | "CBNZ" -> CBNZ
(* Memory *)
| "ldr"|"LDR" -> LDR
| "ldp"|"LDP" -> LDP
| "ldnp"|"LDNP" -> LDP
| "stp"|"STP" -> STP
| "stnp"|"STNP" -> STNP
| "ldrb"|"LDRB" -> LDRB
| "ldrh"|"LDRH" -> LDRH
| "ldar"|"LDAR" -> LDAR
| "ldxr"|"LDXR" -> LDXR
| "ldaxr"|"LDAXR" -> LDAXR
| "str"|"STR" -> STR
| "stlr"|"STLR" -> STLR
| "stxr"|"STXR" -> STXR
| "stlxr"|"STLXR" -> STLXR
| "strb"|"STRB" -> STRB
| "strh"|"STRH" -> STRH
(* Operations *)
| "sxtw"|"SXTW" -> SXTW
| "mov"|"MOV" -> MOV
| "add"|"ADD" -> ADD
| "eor"|"EOR" -> EOR
| "and"|"AND" -> AND
| "subs"|"SUBS" -> ADD
| "cmp"|"CMP" -> CMP
(* Fences *)
| "dmb"|"DMB" -> DMB
| "dsb"|"DSB" -> DSB
| "isb"|"ISB" -> ISB
(* Fence Operands *)
| "sy"|"SY" ->SY
| "st"|"ST" -> ST
| "ld"|"LD" -> LD
| "osh"|"OSH" -> OSH
| "oshst"|"OSHST" -> OSHST
| "oshld"|"OSHLD" -> OSHLD
| "ish"|"ISH" -> ISH
| "ishst"|"ISHST" -> ISHST
| "ishld"|"ISHLD" -> ISHLD
| "nsh"|"NSH" -> NSH
| "nshst"|"NSHST" -> NSHST
| "nshld"|"NSHLD" -> NSHLD
| _ ->
    begin match A.parse_wreg name with
    | Some r -> ARCH_WREG r
    | None ->
        begin match A.parse_xreg name with
        | Some r -> ARCH_XREG r
        | None -> NAME name
        end
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
| '#' ('-' ? num as x) { NUM (int_of_string x) }
| 'P' (num as x)
    { PROC (int_of_string x) }
| ['w''W']'%' (name as name) { SYMB_WREG name }
| ['x''X']?'%' (name as name) { SYMB_XREG name }
| ';' { SEMI }
| ',' { COMMA }
| '|' { PIPE }
| '[' { LBRK }
| ']' { RBRK }
| ':' { COLON }
| '&' (name as x) { META x }
| "codevar:" (name as x) { CODEVAR x }
| name as x  { check_name x }
| eof { EOF }
| ""  { error "AArch64 lexer" lexbuf }

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

