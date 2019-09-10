%{
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

module X86_64 = X86_64Base
open X86_64
%}

%token EOF
%token <X86_64Base.reg> ARCH_REG
%token <string> SYMB_REG
%token <string> NUM
%token <string> INTEL_NUM
%token <string> NAME
%token <int> PROC

%token SEMI COMMA PIPE LBRK RBRK
%token LPAR RPAR COLON
/* Instruction tokens */

%token  I_EFF_OP
%token  I_XOR I_OR I_ADD I_ADDB I_ADDW I_ADDL I_ADDQ I_MOV  I_MOVB I_MOVW I_MOVL I_MOVQ I_MOVT I_MOVSD I_DEC  I_CMP  I_CMOVC  I_INC  I_JMP
%token  I_LOCK  I_XCHG   I_LFENCE  I_SFENCE  I_MFENCE
%token  I_READ I_SETNB I_JE I_JNE I_JLE I_JLT I_JGT I_JGE I_JS I_JNS
%token  I_CMPXCHG

%type <int list * (X86_64Base.pseudo) list list> main
%start  main

%nonassoc SEMI
%%
main: semi_opt proc_list iol_list EOF { $2,$3 }


semi_opt:
| { () }
| SEMI { () }

proc_list:
| PROC SEMI  {[$1]}
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
| {Nop}
| NAME COLON instr_option { Label ($1,$3) }
| instr      { Instruction $1}


reg:
| SYMB_REG { Symbolic_reg $1 }
| ARCH_REG { $1 }

k:
| NUM { $1 }

instr:
  | I_OR   effaddr  COMMA  operand
    {I_EFF_OP (I_OR, NO_SIZE, $2,$4)}
  | I_XOR   effaddr  COMMA  operand
    {I_EFF_OP (I_XOR, NO_SIZE, $2,$4)}

  | I_ADD   effaddr  COMMA  operand
    {I_EFF_OP (I_ADD, NO_SIZE, $2,$4)}
  | I_ADDB   effaddr  COMMA  operand
    {I_EFF_OP (I_ADD, B, $2,$4)}
  | I_ADDW   effaddr  COMMA  operand
    {I_EFF_OP (I_ADD, W, $2,$4)}
  | I_ADDL   effaddr  COMMA  operand
    {I_EFF_OP (I_ADD, L, $2,$4)}
  | I_ADDQ   effaddr  COMMA  operand
    {I_EFF_OP (I_ADD, Q, $2,$4)}

  | I_MOV   effaddr  COMMA  operand
    {I_EFF_OP (I_MOV, NO_SIZE, $2,$4)}
  | I_MOVB   effaddr  COMMA  operand
    {I_EFF_OP (I_MOV, B, $2,$4)}
  | I_MOVW   effaddr  COMMA  operand
    {I_EFF_OP (I_MOV, W, $2,$4)}
  | I_MOVL   effaddr  COMMA  operand
    {I_EFF_OP (I_MOV, L, $2,$4)}
  | I_MOVQ   effaddr  COMMA  operand
    {I_EFF_OP (I_MOV, Q, $2,$4)}

  | I_CMP   effaddr COMMA   operand
    {I_EFF_OP (I_CMP, NO_SIZE, $2,$4)}

  | I_JMP  NAME
    {I_JMP $2}

  | I_JE NAME
    {I_JCC(C_EQ, $2)}
  | I_JNE NAME
    {I_JCC(C_NE, $2)}
  | I_JLE NAME
    {I_JCC(C_LE, $2)}
  | I_JLT NAME
    {I_JCC(C_LT, $2)}
  | I_JGE NAME
    {I_JCC(C_GE, $2)}
  | I_JGT NAME
    {I_JCC(C_GT, $2)}
  | I_JS NAME
    {I_JCC(C_S, $2)}
  | I_JNS NAME
    {I_JCC(C_NS, $2)}

  | I_MFENCE
      { I_MFENCE}

effaddr:
  | rm32  {Effaddr_rm64 $1}

rm32:
  |  reg {Rm64_reg $1}
  |  LPAR reg RPAR {Rm64_deref $2}
  |  LBRK reg RBRK {Rm64_deref $2}
  |  LBRK NAME RBRK {Rm64_abs (Constant.Symbolic ($2,0))}
  |  LBRK NUM RBRK {Rm64_abs (Constant.Concrete $2)}

operand:
  | effaddr {Operand_effaddr $1}
  | k {Operand_immediate (Misc.string_as_int $1) }
  | INTEL_NUM {Operand_immediate (Misc.string_as_int $1)} /* enough ? */
