/*********************************************************************/
/*                        Memevents                                  */
/*                                                                   */
/* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     */
/* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         */
/*                                                                   */
/*  Copyright 2010 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

%{
module X86 = X86Base
open X86
%}

%token EOF
%token <X86Base.reg> ARCH_REG
%token <string> SYMB_REG
%token <int> NUM
%token <int> INTEL_NUM
%token <string> NAME
%token <int> PROC

%token SEMI COMMA PIPE LBRK RBRK 
%token LPAR RPAR COLON
/* Instruction tokens */

%token  I_XOR I_ADD  I_MOV  I_MOVQ I_MOVSD I_DEC  I_CMP  I_CMOVC  I_INC  I_JMP
%token  I_LOCK  I_XCHG   I_LFENCE  I_SFENCE  I_MFENCE
%token  I_READ I_SETNB I_JE I_JNE
%token  I_CMPXCHG

%type <int list * (X86Base.pseudo) list list> main 
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
  | I_XOR   effaddr  COMMA  operand
    {I_XOR ($2,$4)}
  | I_ADD   effaddr  COMMA  operand
    {I_ADD ($2,$4)}
  | I_MOV   effaddr  COMMA  operand
    {I_MOV ($2,$4)}
  | I_MOVQ   effaddr  COMMA  operand
    {I_MOVQ ($2,$4)}
  | I_MOVSD
    {I_MOVSD}
  | I_DEC   effaddr
    {I_DEC $2}
  | I_CMP   effaddr COMMA   operand
    {I_CMP ($2,$4)}
  | I_CMOVC reg COMMA  effaddr
    {I_CMOVC ($2, $4)}
  | I_INC   effaddr
    {I_INC $2}
  | I_JMP  NAME
    {I_JMP $2}
  | I_JE NAME
    {I_JCC(C_EQ, $2)}
  | I_JNE NAME
    {I_JCC(C_NE, $2)}
  | I_LOCK semi_opt instr
    {I_LOCK $3 }
  | I_XCHG   effaddr COMMA effaddr
    { I_XCHG ($2,$4)}
  | I_CMPXCHG effaddr COMMA reg
    { I_CMPXCHG ($2,$4)}
  | I_LFENCE
      { I_LFENCE}
  | I_SFENCE
      { I_SFENCE}
  | I_MFENCE
      { I_MFENCE}
  | I_SETNB effaddr {I_SETNB $2 }

effaddr:
  | rm32  {Effaddr_rm32 $1}

rm32:
  |  reg {Rm32_reg $1}
  |  LPAR reg RPAR {Rm32_deref $2}
  |  LBRK reg RBRK {Rm32_deref $2}
  |  LBRK NAME RBRK {Rm32_abs (Constant.Symbolic $2)} 
  |  LBRK NUM RBRK {Rm32_abs (Constant.Concrete $2)} 

operand:
  | effaddr {Operand_effaddr $1}
  | k {Operand_immediate $1 }
  | INTEL_NUM {Operand_immediate $1} /* enough ? */
  

