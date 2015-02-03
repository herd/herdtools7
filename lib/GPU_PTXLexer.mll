(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(* John Wickerson, Imperial College London, UK.                      *)
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
open GPU_PTXParser
module GPU_PTX = GPU_PTXBase
module LU = LexUtils.Make(O)
}


let digit =  [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = alpha (alpha|digit|'_' | '/' | '-' )*
let num = "0x"?digit+

rule token = parse
| [' ''\t''\r'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf }
| '-' ? num as x { NUM (int_of_string x) }
| 'P' (num as x)
    { PROC (int_of_string x) }
| ';' { SEMI }
| ',' { COMMA }
| '|' { PIPE }
| ':' { COLON }
| '(' { LPAR }
| ')' { RPAR }
| ']' { RBRAC }
| '[' { LBRAC }
| '{' { LBRACE }
| '}' { RBRACE }
| '@' { AMPERSAT }
| '!' { BANG }
| "membar" { MEMBAR }
| "ld" {LD}
| "st" {ST}

| "atom" {ATOM}
| ".add" {ATOM_ADD}
| ".exch" {ATOM_EXCH}
| ".cas" {ATOM_CAS}
(* Implement the rest of the atomic ops as needed *)

| "mov" {MOV}
| "setp" {SETP}
| "add" {ADD}
| "and" {AND}
| "cvt" {CVT}
| "bra" {BRA}
| ".cta" { BARRIER_SCOPE GPU_PTXBase.CTA_bar }
| ".gl"  { BARRIER_SCOPE GPU_PTXBase.GL_bar }
| ".sys" { BARRIER_SCOPE GPU_PTXBase.SYS_bar }

| ".ca"  {CACHE_OP GPU_PTXBase.CA}
| ".cg"  {CACHE_OP GPU_PTXBase.CG}
| ".cv"  {CACHE_OP GPU_PTXBase.CV}
| ".wb"  {CACHE_OP GPU_PTXBase.WB}
| ".wt"  {CACHE_OP GPU_PTXBase.WT}

| ".eq" {CMP_OP Op.Eq}
| ".ne" {CMP_OP Op.Ne}
| ".lt" {CMP_OP Op.Lt}
| ".gt" {CMP_OP Op.Gt}

| ".s32" {OP_TYPE GPU_PTXBase.S32}
| ".b64" {OP_TYPE GPU_PTXBase.B64}
| ".b32" {OP_TYPE GPU_PTXBase.B32}
| ".u64" {OP_TYPE GPU_PTXBase.U64}
| ".s64" {OP_TYPE GPU_PTXBase.S64}
| ".u32" {OP_TYPE GPU_PTXBase.U32}
| ".pred" {OP_TYPE GPU_PTXBase.PRED}

| ".shared" {STATE_SPACE GPU_PTXBase.Shared}
| ".global" {STATE_SPACE GPU_PTXBase.Global}
| ".volatile" {VOL}
| "scopeTree" { SCOPETREE }
| "global"  { GLOBAL }
| "shared"|"local" { SHARED }
| "kernel" { KERNEL }
| "device" {DEVICE }
| "cta" | "block" | "work_group" { CTA }
| "warp" | "sub_group" { WARP }
| "thread" { THREAD }
| name as x
  { match GPU_PTX.parse_reg x with
  | Some r -> ARCH_REG r
  | None -> NAME x }
| eof { EOF }
| ""  { error "GPU_PTX lexer" lexbuf }

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

