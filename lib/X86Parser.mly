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

module X86 = X86Base
%}

%token EOF
%token <X86Base.reg> ARCH_REG
%token <string> SYMB_REG
%token <string> NUM
%token <string> INTEL_NUM
%token <string> NAME
%token <int> PROC

%token SEMI COMMA PIPE LBRK RBRK
%token LPAR RPAR COLON
/* Instruction tokens */

%token  I_XOR I_OR I_ADD  I_MOV  I_MOVB I_MOVW I_MOVL I_MOVQ I_MOVT I_MOVSD I_DEC  I_CMP  I_CMOVC  I_INC  I_JMP
%token  I_LOCK  I_XCHG   I_LFENCE  I_SFENCE  I_MFENCE
%token  I_SETNB I_JE I_JNE
%token  I_CMPXCHG

%type <MiscParser.proc list * (X86Base.pseudo) list list> main
%start  main

%%
main: semi_opt proc_list iol_list EOF { $2,$3 }


semi_opt:
| { () }
| SEMI { () }

proc_list:
| PROC SEMI  {[$1,None,MiscParser.Main]}
| PROC PIPE proc_list  { ($1,None,MiscParser.Main)::$3 }

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
| { X86.Nop}
| NAME COLON instr_option { X86.Label ($1,$3) }
| instr      { X86.Instruction $1}


reg:
| SYMB_REG { X86.Symbolic_reg $1 }
| ARCH_REG { $1 }

k:
| NUM { $1 }

instr:
  | I_XOR   effaddr  COMMA  operand
    { X86.I_XOR ($2,$4)}
  | I_OR   effaddr  COMMA  operand
    { X86.I_OR ($2,$4)}
  | I_ADD   effaddr  COMMA  operand
    { X86.I_ADD ($2,$4)}
  | I_MOV   effaddr  COMMA  operand
    { X86.I_MOV ($2,$4)}
  | I_MOVB   effaddr  COMMA  operand
    { X86.I_MOVB ($2,$4)}
  | I_MOVW   effaddr  COMMA  operand
    { X86.I_MOVW ($2,$4)}
  | I_MOVL   effaddr  COMMA  operand
    { X86.I_MOVL ($2,$4)}
  | I_MOVQ   effaddr  COMMA  operand
    { X86.I_MOVQ ($2,$4)}
  | I_MOVT   effaddr  COMMA  operand
    { X86.I_MOVT ($2,$4)}
  | I_MOVSD
    { X86.I_MOVSD}
  | I_DEC   effaddr
    { X86.I_DEC $2}
  | I_CMP   effaddr COMMA   operand
    { X86.I_CMP ($2,$4)}
  | I_CMOVC reg COMMA  effaddr
    { X86.I_CMOVC ($2, $4)}
  | I_INC   effaddr
    { X86.I_INC $2}
  | I_JMP  NAME
    { X86.I_JMP $2}
  | I_JE NAME
    { X86.I_JCC(X86.C_EQ, $2)}
  | I_JNE NAME
    { X86.I_JCC(X86.C_NE, $2)}
  | I_LOCK semi_opt instr
    { X86.I_LOCK $3 }
  | I_XCHG   effaddr COMMA effaddr
    { X86.I_XCHG ($2,$4)}
  | I_CMPXCHG effaddr COMMA reg
    { X86.I_CMPXCHG ($2,$4)}
  | I_LFENCE
      { X86.I_LFENCE}
  | I_SFENCE
      { X86.I_SFENCE}
  | I_MFENCE
      { X86.I_MFENCE}
  | I_SETNB effaddr { X86.I_SETNB $2 }

effaddr:
  | rm32  { X86.Effaddr_rm32 $1 }

rm32:
  |  reg { X86.Rm32_reg $1}
  |  LPAR reg RPAR  { X86.Rm32_deref $2}
  |  LBRK reg RBRK  { X86.Rm32_deref $2}
  |  LBRK NAME RBRK { X86.Rm32_abs (Constant.mk_sym $2) }
  |  LBRK NUM RBRK  { X86.Rm32_abs (Constant.Concrete $2) }

operand:
  | effaddr { X86.Operand_effaddr $1}
  | k { X86.Operand_immediate (Misc.string_as_int $1) }
  | INTEL_NUM { X86.Operand_immediate (Misc.string_as_int $1)} /* enough ? */
