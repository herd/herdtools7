(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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
| [' ''\t'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf }
| '-' ? num as x { NUM (int_of_string x) }
| 'P' (num as x)
    { PROC (int_of_string x) }
| "cr" (digit as x) { CRK (int_of_string (String.make 1 x)) }
| '%' (name as name) { SYMB_REG name }
| '\"' ([^'\"''\n']* as c) '\"' { STRING c}
| ';' { SEMI }
| ',' { COMMA }
| '|' { PIPE }
| ':' { COLON }
| '(' { LPAR }
| ')' { RPAR }
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
| "lwz" { LWZ }
| "lwzu" { LWZU }
| "lwzx" { LWZX }
| "mr" { MR }
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

