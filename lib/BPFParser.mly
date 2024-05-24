%{
(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Copyright (c) 2024 Puranjay Mohan <puranjay@kernel.org>                  *)
(*                                                                          *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module A=BPFBase

%}

%token EOF
%token <BPFBase.reg> ARCH_REG
%token <string> SYMB_REG
%token <int> NUM
%token <string> NAME
%token <int> PROC
%token <BPFBase.signed * BPFBase.width> SIZE

%token SEMI PIPE COLON LPAR RPAR MINUS EQUAL STAR PLUS
%token COMMA

/* Instruction tokens */
%token <BPFBase.op> ALU_OP
%token <BPFBase.op> AMOF
%token <BPFBase.width> AMOXCHGT
%token <BPFBase.width> AMOCMPXCHGT
%token LOCK
%token GOTO
%token IF
%token <BPFBase.cond> COND

%type <MiscParser.proc list * (BPFBase.pseudo) list list> main
%start  main

%%

main:
| semi_opt proc_list iol_list EOF { $2,$3 }

semi_opt:
| { () }
| SEMI { () }

proc_list:
| ps=separated_nonempty_list(PIPE,PROC) SEMI
  { List.map (fun p -> p,None,MiscParser.Main) ps }

iol_list :
|  instr_option_list SEMI
    {[$1]}
|  instr_option_list SEMI iol_list {$1::$3}

instr_option_list :
  | instr_option
      {[$1]}
  | instr_option PIPE instr_option_list
      {$1::$3}

instr_option :
|            { A.Nop }
| NAME COLON instr_option { A.Label ($1,$3) }
| instr      { A.Instruction $1}

reg:
| SYMB_REG { A.Symbolic_reg $1 }
| ARCH_REG { $1 }

k:
| NUM { $1 }
| MINUS NUM { -$2 }
| PLUS NUM { $2 }

instr:
/* ALU OPS */

/* Register operand */
| reg ALU_OP reg
  { A.OP ($2,$1,$3) }
/* Immediate operand */
| reg ALU_OP k
  { A.OPI ($2,$1,$3) }

/* LDX r0 = *(size *)(r1 + 0) */
| reg EQUAL STAR LPAR SIZE STAR RPAR LPAR reg k RPAR
  { let s,w = $5 in
    A.LOAD (w,s,$1,$9,$10) }

/* STX *(size *)(r1 + 0) = r2  */
| STAR LPAR SIZE STAR RPAR LPAR reg k RPAR EQUAL reg
  { let _,w = $3 in
    A.STORE (w,$7,$8,$11) }

/* ST *(size *)(r1 + 0) = imm  */
| STAR LPAR SIZE STAR RPAR LPAR reg k RPAR EQUAL k
  { let _,w = $3 in
    A.STOREI (w,$7,$8,$11) }

/* MOV r0 = r1 */
| reg EQUAL reg
  { A.MOV($1, $3)}

/* MOV r0 = 10 */
| reg EQUAL k
  { A.MOVI($1, $3)}

/* atomic ops with fetch rs = atomic_fetch_or ((u64 *)(rd + offset16), rs)  */
| reg EQUAL AMOF LPAR LPAR SIZE STAR RPAR LPAR reg k RPAR COMMA reg RPAR
  { let op = $3 in
    let _,w = $6 in
   A.AMO(op, w, $10, $11, $14, A.SC, true) }

/* atomic exchange rs = xchg_64 (rd + offset16, rs) */
| reg EQUAL AMOXCHGT LPAR reg k COMMA reg RPAR
  { let sz = $3 in
   A.AMO(A.AMOXCHG, sz, $5, $6, $8, A.SC, true) }

/* atomic compare and exchange r0 = cmpxchg_64 (rd + offset16, r0, rs) */
| reg EQUAL AMOCMPXCHGT LPAR reg k COMMA reg COMMA reg RPAR
  { let sz = $3 in
   A.AMO(A.AMOCMPXCHG, sz, $5, $6, $10, A.SC, true) }

/* atomic operations without fetch lock *(u64 *)(rd + offset16) = rs  */
| LOCK STAR LPAR SIZE STAR RPAR LPAR reg k RPAR ALU_OP reg
  { let _,w = $4 in
      let op = $11 in
    A.AMO(op, w, $8, $9, $12, A.X, false) }

/* Unconditional jump to label */
| GOTO NAME
  { A.GOTO($2) }

/* Conditional jump to label */
| IF reg COND reg GOTO NAME
  { let c=$3 in
    A.JCOND(c, $2, $4, $6)  }
| IF reg COND k GOTO NAME
  { let c=$3 in
    A.JCONDI(c, $2, $4, $6)  }
