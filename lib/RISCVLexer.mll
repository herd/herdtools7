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
    | "li"  -> LI
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

    | "lb" -> LOAD (Byte,Signed,Rlx)
    | "lh" -> LOAD (Half,Signed,Rlx)
    | "lw" -> LOAD (Word,Signed,Rlx)
    | "ld" -> LOAD (Double,Signed,Rlx)
    | "lbu" -> LOAD (Byte,Unsigned,Rlx)
    | "lhu" -> LOAD (Half,Unsigned,Rlx)
    | "lwu" -> LOAD (Word,Unsigned,Rlx)
    | "sb" -> STORE (Byte,Rlx)
    | "sh" -> STORE (Half,Rlx)
    | "sw" -> STORE (Word,Rlx)
    | "sd"   -> STORE (Double,Rlx)

(* Limited memory order on ordinary load and store *)
    | "lb.aq" -> LOAD (Byte,Signed,Acq)
    | "lh.aq" -> LOAD (Half,Signed,Acq)
    | "lw.aq" -> LOAD (Word,Signed,Acq)
    | "ld.aq" -> LOAD (Double,Signed,Acq)
    | "lbu.aq" -> LOAD (Byte,Unsigned,Acq)
    | "lhu.aq" -> LOAD (Half,Unsigned,Acq)
    | "lwu.aq" -> LOAD (Word,Unsigned,Acq)
    | "sb.rl" -> STORE (Byte,Rel)
    | "sh.rl" -> STORE (Half,Rel)
    | "sw.rl" -> STORE (Word,Rel)
    | "sd.rl"   -> STORE (Double,Rel)
(* Extension: AcqRel for everybody! *)
    | "lb.aq.rl" -> LOAD (Byte,Signed,AcqRel)
    | "lh.aq.rl" -> LOAD (Half,Signed,AcqRel)
    | "lw.aq.rl" -> LOAD (Word,Signed,AcqRel)
    | "ld.aq.rl" -> LOAD (Double,Signed,AcqRel)
    | "lbu.aq.rl" -> LOAD (Byte,Unsigned,AcqRel)
    | "lhu.aq.rl" -> LOAD (Half,Unsigned,AcqRel)
    | "lwu.aq.rl" -> LOAD (Word,Unsigned,AcqRel)
    | "sb.aq.rl" -> STORE (Byte,AcqRel)
    | "sh.aq.rl" -> STORE (Half,AcqRel)
    | "sw.aq.rl" -> STORE (Word,AcqRel)
    | "sd.aq.rl"   -> STORE (Double,AcqRel)

(* Complete memory ordering... *)
    | "lr.w" -> LR (Word,Rlx)
    | "lr.w.aq" -> LR (Word,Acq)
    | "lr.w.rl" -> LR (Word,Rel)
    | "lr.w.aq.rl" -> LR (Word,AcqRel)
    | "lr.d" ->  LR (Double,Rlx)
    | "lr.d.aq" -> LR (Double,Acq)
    | "lr.d.rl" -> LR (Double,Rel)
    | "lr.d.aq.rl" -> LR (Double,AcqRel)
    | "sc.w" -> SC (Word,Rlx)
    | "sc.w.aq" -> SC (Word,Acq)
    | "sc.w.rl" -> SC (Word,Rel)
    | "sc.w.aq.rl" -> SC (Word,AcqRel)
    | "sc.d" ->  SC (Double,Rlx)
    | "sc.d.aq" -> SC (Double,Acq)
    | "sc.d.rl" -> SC (Double,Rel)
    | "sc.d.aq.rl" -> SC (Double,AcqRel)

    | "amoswap.w" -> AMO (AMOSWAP,Word,Rlx)
    | "amoadd.w" ->  AMO (AMOADD,Word,Rlx)
    | "amoand.w" ->  AMO (AMOAND,Word,Rlx)
    | "amoor.w" ->   AMO (AMOOR,Word,Rlx)
    | "amoxor.w" ->  AMO (AMOXOR,Word,Rlx)
    | "amomax.w" ->  AMO (AMOMAX,Word,Rlx)
    | "amomin.w" ->  AMO (AMOMIN,Word,Rlx)
    | "amomaxu.w" -> AMO (AMOMAXU,Word,Rlx)
    | "amominu.w" -> AMO (AMOMINU,Word,Rlx)
    | "amoswap.d" -> AMO (AMOSWAP,Double,Rlx)
    | "amoadd.d" ->  AMO (AMOADD,Double,Rlx)
    | "amoand.d" ->  AMO (AMOAND,Double,Rlx)
    | "amoor.d" ->   AMO (AMOOR,Double,Rlx)
    | "amoxor.d" ->  AMO (AMOXOR,Double,Rlx)
    | "amomax.d" ->  AMO (AMOMAX,Double,Rlx)
    | "amomin.d" ->  AMO (AMOMIN,Double,Rlx)
    | "amomaxu.d" -> AMO (AMOMAXU,Double,Rlx)
    | "amominu.d" -> AMO (AMOMINU,Double,Rlx)
    | "amoswap.w.aq" -> AMO (AMOSWAP,Word,Acq)
    | "amoadd.w.aq" ->  AMO (AMOADD,Word,Acq)
    | "amoand.w.aq" ->  AMO (AMOAND,Word,Acq)
    | "amoor.w.aq" ->   AMO (AMOOR,Word,Acq)
    | "amoxor.w.aq" ->  AMO (AMOXOR,Word,Acq)
    | "amomax.w.aq" ->  AMO (AMOMAX,Word,Acq)
    | "amomin.w.aq" ->  AMO (AMOMIN,Word,Acq)
    | "amomaxu.w.aq" -> AMO (AMOMAXU,Word,Acq)
    | "amominu.w.aq" -> AMO (AMOMINU,Word,Acq)
    | "amoswap.d.aq" -> AMO (AMOSWAP,Double,Acq)
    | "amoadd.d.aq" ->  AMO (AMOADD,Double,Acq)
    | "amoand.d.aq" ->  AMO (AMOAND,Double,Acq)
    | "amoor.d.aq" ->   AMO (AMOOR,Double,Acq)
    | "amoxor.d.aq" ->  AMO (AMOXOR,Double,Acq)
    | "amomax.d.aq" ->  AMO (AMOMAX,Double,Acq)
    | "amomin.d.aq" ->  AMO (AMOMIN,Double,Acq)
    | "amomaxu.d.aq" -> AMO (AMOMAXU,Double,Acq)
    | "amominu.d.aq" -> AMO (AMOMINU,Double,Acq)
    | "amoswap.w.rl" -> AMO (AMOSWAP,Word,Rel)
    | "amoadd.w.rl" ->  AMO (AMOADD,Word,Rel)
    | "amoand.w.rl" ->  AMO (AMOAND,Word,Rel)
    | "amoor.w.rl" ->   AMO (AMOOR,Word,Rel)
    | "amoxor.w.rl" ->  AMO (AMOXOR,Word,Rel)
    | "amomax.w.rl" ->  AMO (AMOMAX,Word,Rel)
    | "amomin.w.rl" ->  AMO (AMOMIN,Word,Rel)
    | "amomaxu.w.rl" -> AMO (AMOMAXU,Word,Rel)
    | "amominu.w.rl" -> AMO (AMOMINU,Word,Rel)
    | "amoswap.d.rl" -> AMO (AMOSWAP,Double,Rel)
    | "amoadd.d.rl" ->  AMO (AMOADD,Double,Rel)
    | "amoand.d.rl" ->  AMO (AMOAND,Double,Rel)
    | "amoor.d.rl" ->   AMO (AMOOR,Double,Rel)
    | "amoxor.d.rl" ->  AMO (AMOXOR,Double,Rel)
    | "amomax.d.rl" ->  AMO (AMOMAX,Double,Rel)
    | "amomin.d.rl" ->  AMO (AMOMIN,Double,Rel)
    | "amomaxu.d.rl" -> AMO (AMOMAXU,Double,Rel)
    | "amominu.d.rl" -> AMO (AMOMINU,Double,Rel)
    | "amoswap.w.aq.rl" -> AMO (AMOSWAP,Word,AcqRel)
    | "amoadd.w.aq.rl" ->  AMO (AMOADD,Word,AcqRel)
    | "amoand.w.aq.rl" ->  AMO (AMOAND,Word,AcqRel)
    | "amoor.w.aq.rl" ->   AMO (AMOOR,Word,AcqRel)
    | "amoxor.w.aq.rl" ->  AMO (AMOXOR,Word,AcqRel)
    | "amomax.w.aq.rl" ->  AMO (AMOMAX,Word,AcqRel)
    | "amomin.w.aq.rl" ->  AMO (AMOMIN,Word,AcqRel)
    | "amomaxu.w.aq.rl" -> AMO (AMOMAXU,Word,AcqRel)
    | "amominu.w.aq.rl" -> AMO (AMOMINU,Word,AcqRel)
    | "amoswap.d.aq.rl" -> AMO (AMOSWAP,Double,AcqRel)
    | "amoadd.d.aq.rl" ->  AMO (AMOADD,Double,AcqRel)
    | "amoand.d.aq.rl" ->  AMO (AMOAND,Double,AcqRel)
    | "amoor.d.aq.rl" ->   AMO (AMOOR,Double,AcqRel)
    | "amoxor.d.aq.rl" ->  AMO (AMOXOR,Double,AcqRel)
    | "amomax.d.aq.rl" ->  AMO (AMOMAX,Double,AcqRel)
    | "amomin.d.aq.rl" ->  AMO (AMOMIN,Double,AcqRel)
    | "amomaxu.d.aq.rl" -> AMO (AMOMAXU,Double,AcqRel)
    | "amominu.d.aq.rl" ->  AMO (AMOMINU,Double,AcqRel)

(* Fences *)
| "fence" -> FENCE
| "fence.i" -> FENCEI
| "fence.tso" -> FENCETSO
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
