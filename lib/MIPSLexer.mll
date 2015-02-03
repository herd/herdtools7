(*********************************************************************)
(*                        DIY                                        *)
(*                                                                   *)
(*               Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

{
module Make(O:LexUtils.Config) = struct
open Lexing
open LexMisc
open MIPSParser

module LU = LexUtils.Make(O)

let check_name = function
  | "li" -> LI
  | "lw" -> LW
  | "sw" -> SW
  | "ll" -> LL
  | "sc" -> SC
  | "sync" -> SYNC
(* ADD *)
  | "add" -> ADD
  | "addu" -> ADDU
  | "addi" -> ADDI
  | "addiu" -> ADDIU
(* SUB *)
  | "sub" -> SUB
  | "subu" -> SUBU
  | "subi" -> SUBI
  | "subiu" -> SUBIU
(* SLT *)
  | "slt" -> SLT
  | "sltu" -> SLTU
  | "slti" -> SLTI
  | "sltiu" -> SLTIU
(* AND *)
  | "and" -> AND
  | "andi" -> ANDI
(* OR *)
  | "or" -> OR
  | "ori" -> ORI
(* XOR *)
  | "xor" -> XOR
  | "xori" -> XORI
(* NOR *)
  | "nor" -> NOR
(* JUMPS *)
  | "b" -> B
  | "beq" -> BEQ
  | "bne" -> BNE
  | "blez" -> BLEZ
  | "bgtz" -> BGTZ
  | "bltz" -> BLTZ
  | "bgez" -> BGEZ
  | name -> NAME name

}
let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = alpha (alpha|digit|'_' | '/' | '.' | '-')*
let num = digit+

rule token = parse
| [' ''\t'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf }
| '-' ? num as x { NUM (int_of_string x) }
| 'P' (num as x)
    { PROC (int_of_string x) }
| '%' (name as name) { SYMB_REG name }
| ';' { SEMI }
| ',' { COMMA }
| '|' { PIPE }
| '(' { LPAR }
| ')' { RPAR }
| ':' { COLON }
| '$' (name|num) as x
  { match MIPSBase.parse_reg x with
  | Some r -> ARCH_REG r
  | None -> NAME x }
| name as x
  { check_name x }
| eof { EOF }
| ""  { error "MIPS lexer" lexbuf }

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

