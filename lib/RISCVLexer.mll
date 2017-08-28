(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
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
open RISCVBase
open RISCVParser

module LU = LexUtils.Make(O)

  let check_name = function
    | "addi" -> OPI ADDI
    | "slti" -> OPI SLTI
    | "sltiu" -> OPI SLTIU
    | "andi" -> OPI ANDI
    | "ori" -> OPI ORI
    | "xori" -> OPI XORI
    | "slli" -> OPI SLLI
    | "srli" -> OPI SRLI
    | "srai" -> OPI SRAI
    | "addiw" -> OPIW ADDIW
    | "slliw" -> OPIW SLLIW
    | "srliw" -> OPIW SRLIW
    | "sraiw" -> OPIW SRAIW
    | "add" -> OP ADD
    | "slt" -> OP SLT
    | "sltu" -> OP SLTU
    | "and" -> OP AND
    | "or" -> OP OR
    | "xor" -> OP XOR
    | "sll" -> OP SLL
    | "srl" -> OP SRL
    | "sub" -> OP SUB
    | "sra" -> OP SRA
    | "addw" -> OPW ADDW
    | "sllw" -> OPW SLLW
    | "srlw" -> OPW SRLW
    | "subw" -> OPW SUBW
    | "sraw" -> OPW SRAW

    | "j" -> J
    | "beq" -> BCC EQ
    | "bne" -> BCC NE
    | "blt" -> BCC LT
    | "bltu" -> BCC LTU
    | "bge" -> BCC GE
    | "bgeu" -> BCC GEU

    | "lb" -> LOAD (Byte,Signed)
    | "lh" -> LOAD (Half,Signed)
    | "lw" -> LOAD (Word,Signed)
    | "ld" -> LOAD (Double,Signed)
    | "lbu" -> LOAD (Byte,Unsigned)
    | "lhu" -> LOAD (Half,Unsigned)
    | "lwu" -> LOAD (Word,Unsigned)
    | "sb" -> STORE Byte
    | "sh" -> STORE Half
    | "sw" -> STORE Word
    | "sd"   -> STORE Double

    | "lr.w" -> LR (Word,Rlx)
    | "lr.w.aq" -> LR (Word,Acq)
    | "lr.w.rl" -> LR (Word,Rel)
    | "lr.w.aqrl" -> LR (Word,AcqRel)
    | "lr.d" ->  LR (Double,Rlx)
    | "lr.d.aq" -> LR (Double,Acq)
    | "lr.d.rl" -> LR (Double,Rel)
    | "lr.d.aqrl" -> LR (Double,AcqRel)


    | "sc.w" -> SC (Word,Rlx)
    | "sc.w.aq" -> SC (Word,Acq)
    | "sc.w.rl" -> SC (Word,Rel)
    | "sc.w.aqrl" -> SC (Word,AcqRel)
    | "sc.d" ->  SC (Double,Rlx)
    | "sc.d.aq" -> SC (Double,Acq)
    | "sc.d.rl" -> SC (Double,Rel)
    | "sc.d.aqrl" -> SC (Double,AcqRel)

    | "fence" -> FENCE
    | "fence.i" -> FENCEI
    | name ->
        match parse_reg name with
        | Some r -> ARCH_REG r
        | None ->NAME name

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
| 'P' (num as x)
    { PROC (int_of_string x) }
| '&' (name as x) { META x }
| '%' (name as name) { SYMB_REG name }
| ';' { SEMI }
| ',' { COMMA }
| '|' { PIPE }
| '(' { LPAR }
| ')' { RPAR }
| ':' { COLON }
| name as x
  { check_name x }
| eof { EOF }
| ""  { error "RISCV lexer" lexbuf }

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

