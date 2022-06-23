%{
(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module A=MIPSBase

%}

%token EOF
%token <MIPSBase.reg> ARCH_REG
%token <string> SYMB_REG
%token <int> NUM
%token <string> NAME
%token <int> PROC

%token SEMI COMMA PIPE COLON LPAR RPAR

/* Instruction tokens */
%token LI LW SW LL SC SYNC
%token ADD ADDU ADDI ADDIU
%token SUB SUBU SUBI SUBIU
%token SLT SLTU SLTI SLTIU
%token AND ANDI OR ORI
%token XOR XORI NOR
%token B BEQ BNE BLEZ BGTZ BLTZ BGEZ


%type <MiscParser.proc list * (MIPSBase.pseudo) list list> main
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

instr:
/* ADD */
| ADD reg COMMA reg COMMA reg
  { A.OP (A.ADD,$2,$4,$6) }
| ADDI reg COMMA reg COMMA k
  { A.OPI (A.ADD,$2,$4,$6) }
| ADDU reg COMMA reg COMMA reg
  { A.OP (A.ADDU,$2,$4,$6) }
| ADDIU reg COMMA reg COMMA k
  { A.OPI (A.ADDU,$2,$4,$6) }
/* SUB */
| SUB reg COMMA reg COMMA reg
  { A.OP (A.SUB,$2,$4,$6) }
| SUBI reg COMMA reg COMMA k
  { A.OPI (A.SUB,$2,$4,$6) }
| SUBU reg COMMA reg COMMA reg
  { A.OP (A.SUBU,$2,$4,$6) }
| SUBIU reg COMMA reg COMMA k
  { A.OPI (A.SUBU,$2,$4,$6) }
/* SLT */
| SLT reg COMMA reg COMMA reg
  { A.OP (A.SLT,$2,$4,$6) }
| SLTI reg COMMA reg COMMA k
  { A.OPI (A.SLT,$2,$4,$6) }
| SLTU reg COMMA reg COMMA reg
  { A.OP (A.SLTU,$2,$4,$6) }
| SLTIU reg COMMA reg COMMA k
  { A.OPI (A.SLTU,$2,$4,$6) }
/* AND */
| AND reg COMMA reg COMMA reg
  { A.OP (A.AND,$2,$4,$6) }
| ANDI reg COMMA reg COMMA k
  { A.OPI (A.AND,$2,$4,$6) }
/* OR */
| OR reg COMMA reg COMMA reg
  { A.OP (A.OR,$2,$4,$6) }
| ORI reg COMMA reg COMMA k
  { A.OPI (A.OR,$2,$4,$6) }
/* XOR */
| XOR reg COMMA reg COMMA reg
  { A.OP (A.XOR,$2,$4,$6) }
| XORI reg COMMA reg COMMA k
  { A.OPI (A.XOR,$2,$4,$6) }
/* NOR */
| NOR reg COMMA reg COMMA reg
  { A.OP (A.NOR,$2,$4,$6) }
/* Branch */
| B NAME
  { A.B $2 }
| BEQ reg COMMA reg COMMA NAME
  { A.BC (A.EQ,$2,$4,$6) }
| BNE reg COMMA reg COMMA NAME
  { A.BC (A.NE,$2,$4,$6) }
| BLEZ reg COMMA NAME
  { A.BCZ (A.LEZ,$2,$4) }
| BGTZ reg COMMA NAME
  { A.BCZ (A.GTZ,$2,$4) }
| BLTZ reg COMMA NAME
  { A.BCZ (A.LTZ,$2,$4) }
| BGEZ reg COMMA NAME
  { A.BCZ (A.GEZ,$2,$4) }
/* Load and Store */
| LW reg COMMA k LPAR reg RPAR
  { A.LW ($2,$4,$6) }
| LW reg COMMA LPAR reg RPAR
  { A.LW ($2,0,$5) }
| SW reg COMMA k LPAR reg RPAR
  { A.SW ($2,$4,$6) }
| SW reg COMMA LPAR reg RPAR
  { A.SW ($2,0,$5) }
| LL reg COMMA k LPAR reg RPAR
  { A.LL ($2,$4,$6) }
| LL reg COMMA LPAR reg RPAR
  { A.LL ($2,0,$5) }
| SC reg COMMA k LPAR reg RPAR
  { A.SC ($2,$4,$6) }
| SC reg COMMA LPAR reg RPAR
  { A.SC ($2,0,$5) }
/* Misc */
| LI reg COMMA k
  { A.LI ($2,$4) }
| SYNC
  { A.SYNC }
