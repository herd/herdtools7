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
open PPCGenParser
module PPCGen = PPCGenBase
module LU = LexUtils.Make(O)

let instruction_table = Hashtbl.create 100
let () =
  List.iter (fun (kwd, tok) -> Hashtbl.add instruction_table kwd tok)
  [
  (* #include "src_power_gen/lexer.gen" *)
  ]
}
let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = (alpha | '_') (alpha|digit|'_' | '/' | '.' | '-')*
let num = digit+
let hexa = "0x" (digit|['a'-'f''A'-'F'])+
rule token = parse
| [' ''\t'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf }
| '-' ? num as x { NUM (int_of_string x) }
| hexa as x { NUM (int_of_string x) }
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
| '*' { TIMES }
| '+' { PLUS }

(* Branch *)
| "b" { B }
| "bl"    { BL }
(* Data cache block flush *)
| "dcbf" { DCBF }
(* Enforce in-order execution of I/O *)
| "eieio" { EIEIO }
(* Instruction synchronize *)
| "isync" { ISYNC }
(* Load word and reserve indexed *)
| "lwarx" { LWARX }
(* Store word condition indexed *)
| "stwcx." { STWCX }
(* Synchronize *)
| "sync" { SYNC }

| "crnand" { CRNAND }
| "crand" { CRAND }

(* Extended mnemonics *)
| "beq" { BEQ }
| "bge" { BGE }
| "bgt" { BGT }
| "ble" { BLE }
| "blr"   { BLR }
| "blt" { BLT }
| "bne" { BNE }
| "bng" { BNG }
| "bnl" { BNL }
| "cmpw" { CMPW }
| "cmpwi" { CMPWI }
| "li"   { LI }
| "lwsync" { LWSYNC }
| "mflr"  { MFLR }
| "mr" { MR }
| "mtlr"  { MTLR }
| "sub"   { SUB }
| "sub."  { SUBDOT }
| "subi" { SUBI }

| "lt" { CRBIT 0 }
| "gt" { CRBIT 1 }
| "eq" { CRBIT 2 }
| "so" { CRBIT 3 }
| "un" { CRBIT 3 }

| "com"   { COMMENT}

| name as x
  {
  try
   (* Generated fixed-point instructions *)
   Hashtbl.find instruction_table x
  with Not_found -> match PPCGen.parse_reg x with
  | Some r -> ARCH_REG r
  | None -> NAME x
  }
| eof { EOF }
| ""  { error "PPCGen lexer" lexbuf }

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

