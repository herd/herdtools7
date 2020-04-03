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
open PPCParser
module PPC = PPCBase
module LU = LexUtils.Make(O)
}
let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = (alpha | '_') (alpha|digit|'_' | '/' | '.' | '-')*
let num = digit+

rule token = parse
| [' ''\t''\r'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf }
| '-' ? num as x { NUM (int_of_string x) }
| 'P' (num as x)
    { PROC (int_of_string x) }
| "cr" (digit as x) { CRK (int_of_string (String.make 1 x)) }
| '%' (name as name) { SYMB_REG name }
| '&' (name as name) { CSTVAR name }
| '\"' ([^'\"''\n']* as c) '\"' { STRING c}
| ';' { SEMI }
| ',' { COMMA }
| '|' { PIPE }
| ':' { COLON }
| '(' { LPAR }
| ')' { RPAR }
| "codevar:" (name as x) { CODEVAR x }
| "addi" { ADDI }
| "subi" { SUBI }
| "add"  { ADD }
| "add."  { ADDDOT }
| "sub"   { SUB }
| "subf"  { SUBF }
| "sub."  { SUBDOT }
| "subf." { SUBFDOT }
| "cmpwi" { CMPWI }
| "cmpw" {CMPW}
| "li"   { LI }
| "xor" { XOR }
| "xor." { XORDOT }
| "xori" { XORI }
| "and" { AND }
| "and." { ANDDOT }
| "andi." { ANDIDOT }
| "or" { OR }
| "or." { ORDOT }
| "ori" { ORI }
| "mullw" { MULL }
| "mullw." { MULLDOT }
| "mulli" { MULLI }
| "divw" { DIV }
| "divw." { DIVDOT }
| "lbz" { LBZ }
| "lbzx" { LBZX }
| "lhz" { LHZ }
| "lhzx" { LHZX }
| "lwz" { LWZ }
| "lwzu" { LWZU }
| "lwzx" { LWZX }
| "mr" { MR }
| "stb" { STB }
| "stbx" { STBX }
| "sth" { STH }
| "sthx" { STHX }
| "stw" { STW }
| "stwu" { STWU }
| "stwx" { STWX }
| "lwarx" { LWARX }
| "stwcx." { STWCX }
| "std" { STD }
| "ld"  { LD }
| "stdx" { STDX }
| "ldx"  { LDX }
| "sync" { SYNC }
| "eieio" { EIEIO }
| "isync" { ISYNC }
| "lwsync" { LWSYNC }
| "dcbf" { DCBF }
| "b" { B }
| "beq" { BEQ }
| "blt" { BLT }
| "ble" { BLE }
| "bgt" { BGT }
| "bge" { BGE }
| "bne" { BNE }
| "bnl" { BNL }
| "bng" { BNG }

| "nor" { NOR }
| "nor." { NORDOT }
| "neg" { NEG }
| "neg." { NEGDOT }
| "slw" { SLW }
| "srawi" { SRAWI }
| "sraw"  { SRAW }
| "bl"    { BL }
| "blr"   { BLR }
| "mtlr"  { MTLR }
| "mflr"  { MFLR }
| "mfcr"  { MFCR }
| "stmw"  { STMW }
| "lmw"  { LMW }
| "com"   { COMMENT}

| name as x
  { match PPC.parse_reg x with
  | Some r -> ARCH_REG r
  | None -> NAME x }
| eof { EOF }
| ""  { error "PPC lexer" lexbuf }

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

