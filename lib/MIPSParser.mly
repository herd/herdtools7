/*********************************************************************/
/*                        DIY                                        */
/*                                                                   */
/*               Luc Maranget, INRIA Paris-Rocquencourt, France.     */
/*                                                                   */
/*  Copyright 2014 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

%{
open MIPSBase
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


%type <int list * (MIPSBase.pseudo) list list * MiscParser.gpu_data option> main 
%start  main

%nonassoc SEMI
%%

main:
| semi_opt proc_list iol_list EOF { $2,$3,None }

semi_opt:
| { () }
| SEMI { () }

proc_list:
| PROC SEMI
    {[$1]}

| PROC PIPE proc_list  { $1::$3 }

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
|            { Nop }
| NAME COLON instr_option { Label ($1,$3) }
| instr      { Instruction $1}

reg:
| SYMB_REG { Symbolic_reg $1 }
| ARCH_REG { $1 }

k:
| NUM { $1 }

instr:
/* ADD */
| ADD reg COMMA reg COMMA reg 
  { OP (ADD,$2,$4,$6) }
| ADDI reg COMMA reg COMMA k
  { OPI (ADD,$2,$4,$6) }
| ADDU reg COMMA reg COMMA reg 
  { OP (ADDU,$2,$4,$6) }
| ADDIU reg COMMA reg COMMA k
  { OPI (ADDU,$2,$4,$6) }
/* SUB */
| SUB reg COMMA reg COMMA reg 
  { OP (SUB,$2,$4,$6) }
| SUBI reg COMMA reg COMMA k
  { OPI (SUB,$2,$4,$6) }
| SUBU reg COMMA reg COMMA reg 
  { OP (SUBU,$2,$4,$6) }
| SUBIU reg COMMA reg COMMA k
  { OPI (SUBU,$2,$4,$6) }
/* SLT */
| SLT reg COMMA reg COMMA reg 
  { OP (SLT,$2,$4,$6) }
| SLTI reg COMMA reg COMMA k
  { OPI (SLT,$2,$4,$6) }
| SLTU reg COMMA reg COMMA reg 
  { OP (SLTU,$2,$4,$6) }
| SLTIU reg COMMA reg COMMA k
  { OPI (SLTU,$2,$4,$6) }
/* AND */
| AND reg COMMA reg COMMA reg 
  { OP (AND,$2,$4,$6) }
| ANDI reg COMMA reg COMMA k
  { OPI (AND,$2,$4,$6) }
/* OR */
| OR reg COMMA reg COMMA reg 
  { OP (OR,$2,$4,$6) }
| ORI reg COMMA reg COMMA k
  { OPI (OR,$2,$4,$6) }
/* XOR */
| XOR reg COMMA reg COMMA reg 
  { OP (XOR,$2,$4,$6) }
| XORI reg COMMA reg COMMA k
  { OPI (XOR,$2,$4,$6) }
/* NOR */
| NOR reg COMMA reg COMMA reg 
  { OP (NOR,$2,$4,$6) }
/* Branch */
| B NAME
  { B $2 }
| BEQ reg COMMA reg COMMA NAME
  { BC (EQ,$2,$4,$6) }
| BNE reg COMMA reg COMMA NAME
  { BC (NE,$2,$4,$6) }
| BLEZ reg COMMA NAME
  { BCZ (LEZ,$2,$4) }
| BGTZ reg COMMA NAME
  { BCZ (GTZ,$2,$4) }
| BLTZ reg COMMA NAME
  { BCZ (LTZ,$2,$4) }
| BGEZ reg COMMA NAME
  { BCZ (GEZ,$2,$4) }
/* Load and Store */
| LW reg COMMA k LPAR reg RPAR
  { LW ($2,$4,$6) }
| LW reg COMMA LPAR reg RPAR
  { LW ($2,0,$5) }
| SW reg COMMA k LPAR reg RPAR
  { SW ($2,$4,$6) }
| SW reg COMMA LPAR reg RPAR
  { SW ($2,0,$5) }
| LL reg COMMA k LPAR reg RPAR
  { LL ($2,$4,$6) }
| LL reg COMMA LPAR reg RPAR
  { LL ($2,0,$5) }
| SC reg COMMA k LPAR reg RPAR
  { SC ($2,$4,$6) }
| SC reg COMMA LPAR reg RPAR
  { SC ($2,0,$5) }
/* Misc */
| LI reg COMMA k
  { LI ($2,$4) }
| SYNC
  { SYNC }
