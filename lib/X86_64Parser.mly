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

%token I_XOR I_XORB I_XORW I_XORL I_XORQ
%token I_OR I_ORB I_ORW I_ORL I_ORQ
%token I_ADD I_ADDB I_ADDW I_ADDL I_ADDQ
%token I_MOV I_MOVB I_MOVW I_MOVL I_MOVQ
%token I_CMP I_CMPB I_CMPW I_CMPL I_CMPQ
%token I_DEC I_DECB I_DECW I_DECL I_DECQ
%token I_INC I_INCB I_INCW I_INCL I_INCQ
%token I_XCHG I_XCHGB I_XCHGW I_XCHGL I_XCHGQ
%token I_UXCH I_UXCHB I_UXCHW I_UXCHL I_UXCHQ
%token I_CMPXCHG I_CMPXCHGB I_CMPXCHGW I_CMPXCHGL I_CMPXCHGQ
%token I_CMOVC I_CMOVCB I_CMOVCW I_CMOVCL I_CMOVCQ
%token  I_LOCK  I_JMP  I_MFENCE I_SETNB
%token  I_JE I_JNE I_JLE I_JLT I_JGT I_JGE I_JS I_JNS
%token I_MOVNTI  I_MOVNTIL I_MOVNTIQ

%type <MiscParser.proc list * (X86_64Base.pseudo) list list> main
%start  main

%%
main: semi_opt proc_list iol_list EOF { $2,$3 }


semi_opt:
| { () }
| SEMI { () }

proc_list:
| PROC SEMI  {[$1,None]}
| PROC PIPE proc_list  { ($1,None)::$3 }

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
| {X86_64.Nop}
| NAME COLON instr_option { X86_64.Label ($1,$3) }
| instr      { X86_64.Instruction $1}


reg:
| SYMB_REG { X86_64.Symbolic_reg $1 }
| ARCH_REG { $1 }

k:
| NUM { $1 }

instr:
  | I_OR  operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_OR, X86_64.INSb , $4,$2)}
  | I_ORB  operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_OR, X86_64.I8b, $4,$2)}
  | I_ORW  operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_OR, X86_64.I16b, $4,$2)}
  | I_ORL  operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_OR, X86_64.I32b, $4,$2)}
  | I_ORQ  operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_OR, X86_64.I64b, $4,$2)}

  | I_XOR  operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_XOR, X86_64.INSb, $4,$2)}
  | I_XORB  operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_XOR, X86_64.I8b, $4,$2)}
  | I_XORW  operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_XOR, X86_64.I16b, $4,$2)}
  | I_XORL  operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_XOR, X86_64.I32b, $4,$2)}
  | I_XORQ  operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_XOR, X86_64.I64b, $4,$2)}

  | I_ADD   operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_ADD, X86_64.INSb, $4,$2)}
  | I_ADDB   operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_ADD, X86_64.I8b, $4,$2)}
  | I_ADDW   operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_ADD, X86_64.I16b, $4,$2)}
  | I_ADDL   operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_ADD, X86_64.I32b, $4,$2)}
  | I_ADDQ   operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_ADD, X86_64.I64b, $4,$2)}

  | I_MOV   operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_MOV, X86_64.INSb, $4,$2)}
  | I_MOVB   operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_MOV, X86_64.I8b, $4,$2)}
  | I_MOVW   operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_MOV, X86_64.I16b, $4,$2)}
  | I_MOVL   operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_MOV, X86_64.I32b, $4,$2)}
  | I_MOVQ   operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_MOV, X86_64.I64b, $4,$2)}

  | I_MOVNTI reg COMMA effaddr
      {X86_64.I_MOVNTI (X86_64.INSb,$4,$2)}
  | I_MOVNTIL reg COMMA effaddr
      {X86_64.I_MOVNTI (X86_64.I32b,$4,$2)}
  | I_MOVNTIQ reg COMMA effaddr
      {X86_64.I_MOVNTI (X86_64.I64b,$4,$2)}

  | I_CMP  operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_CMP, X86_64.INSb, $4,$2)}
  | I_CMPB  operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_CMP, X86_64.I8b, $4,$2)}
  | I_CMPW  operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_CMP, X86_64.I16b, $4,$2)}
  | I_CMPL  operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_CMP, X86_64.I32b, $4,$2)}
  | I_CMPQ  operand  COMMA  effaddr
    {X86_64.I_EFF_OP (X86_64.I_CMP, X86_64.I64b, $4,$2)}

  | I_JMP  NAME
    {X86_64.I_JMP $2}

  | I_JE NAME
    {X86_64.I_JCC(X86_64.C_EQ, $2)}
  | I_JNE NAME
    {X86_64.I_JCC(X86_64.C_NE, $2)}
  | I_JLE NAME
    {X86_64.I_JCC(X86_64.C_LE, $2)}
  | I_JLT NAME
    {X86_64.I_JCC(X86_64.C_LT, $2)}
  | I_JGE NAME
    {X86_64.I_JCC(X86_64.C_GE, $2)}
  | I_JGT NAME
    {X86_64.I_JCC(X86_64.C_GT, $2)}
  | I_JS NAME
    {X86_64.I_JCC(X86_64.C_S, $2)}
  | I_JNS NAME
    {X86_64.I_JCC(X86_64.C_NS, $2)}

  | I_MFENCE
      { X86_64.I_MFENCE}

  | I_DEC  effaddr
    {X86_64.I_EFF (X86_64.I_DEC, X86_64.INSb, $2)}
  | I_DECB  effaddr
    {X86_64.I_EFF (X86_64.I_DEC, X86_64.I8b, $2)}
  | I_DECW  effaddr
    {X86_64.I_EFF (X86_64.I_DEC, X86_64.I16b, $2)}
  | I_DECL  effaddr
    {X86_64.I_EFF (X86_64.I_DEC, X86_64.I32b, $2)}
  | I_DECQ  effaddr
    {X86_64.I_EFF (X86_64.I_DEC, X86_64.I64b, $2)}

  | I_INC  effaddr
    {X86_64.I_EFF (X86_64.I_INC, X86_64.INSb, $2)}
  | I_INCB  effaddr
    {X86_64.I_EFF (X86_64.I_INC, X86_64.I8b, $2)}
  | I_INCW  effaddr
    {X86_64.I_EFF (X86_64.I_INC, X86_64.I16b, $2)}
  | I_INCL  effaddr
    {X86_64.I_EFF (X86_64.I_INC, X86_64.I32b, $2)}
  | I_INCQ  effaddr
    {X86_64.I_EFF (X86_64.I_INC, X86_64.I64b, $2)}

  | I_SETNB  effaddr
    {X86_64.I_EFF (X86_64.I_SETNB, X86_64.INSb, $2)}

  | I_XCHG  effaddr  COMMA  effaddr
    {X86_64.I_EFF_EFF (X86_64.I_XCHG, X86_64.INSb, $2, $4)}
  | I_XCHGB  effaddr  COMMA  effaddr
    {X86_64.I_EFF_EFF (X86_64.I_XCHG, X86_64.I8b, $2, $4)}
  | I_XCHGW  effaddr  COMMA  effaddr
    {X86_64.I_EFF_EFF (X86_64.I_XCHG, X86_64.I16b, $2, $4)}
  | I_XCHGL  effaddr  COMMA  effaddr
    {X86_64.I_EFF_EFF (X86_64.I_XCHG, X86_64.I32b, $2, $4)}
  | I_XCHGQ  effaddr  COMMA  effaddr
    {X86_64.I_EFF_EFF (X86_64.I_XCHG, X86_64.I64b, $2, $4)}

  | I_UXCH  effaddr  COMMA  effaddr
    {X86_64.I_EFF_EFF (X86_64.I_XCHG_UNLOCKED, X86_64.INSb, $2, $4)}
  | I_UXCHB  effaddr  COMMA  effaddr
    {X86_64.I_EFF_EFF (X86_64.I_XCHG_UNLOCKED, X86_64.I8b, $2, $4)}
  | I_UXCHW  effaddr  COMMA  effaddr
    {X86_64.I_EFF_EFF (X86_64.I_XCHG_UNLOCKED, X86_64.I16b, $2, $4)}
  | I_UXCHL  effaddr  COMMA  effaddr
    {X86_64.I_EFF_EFF (X86_64.I_XCHG_UNLOCKED, X86_64.I32b, $2, $4)}
  | I_UXCHQ  effaddr  COMMA  effaddr
    {X86_64.I_EFF_EFF (X86_64.I_XCHG_UNLOCKED, X86_64.I64b, $2, $4)}

  | I_CMPXCHG  effaddr  COMMA  reg
    {X86_64.I_CMPXCHG (X86_64.INSb, $2, $4)}
  | I_CMPXCHGB  effaddr  COMMA  reg
    {X86_64.I_CMPXCHG (X86_64.I8b, $2, $4)}
  | I_CMPXCHGW  effaddr  COMMA  reg
    {X86_64.I_CMPXCHG (X86_64.I16b, $2, $4)}
  | I_CMPXCHGL  effaddr  COMMA  reg
    {X86_64.I_CMPXCHG (X86_64.I32b, $2, $4)}
  | I_CMPXCHGQ  effaddr  COMMA  reg
    {X86_64.I_CMPXCHG (X86_64.I64b, $2, $4)}

  | I_CMOVC  reg COMMA effaddr
    {X86_64.I_CMOVC (X86_64.INSb, $2, $4)}
  | I_CMOVCB  reg COMMA effaddr
    {X86_64.I_CMOVC (X86_64.I8b, $2, $4)}
  | I_CMOVCW  reg COMMA effaddr
    {X86_64.I_CMOVC (X86_64.I16b, $2, $4)}
  | I_CMOVCL  reg COMMA effaddr
    {X86_64.I_CMOVC (X86_64.I32b, $2, $4)}
  | I_CMOVCQ  reg COMMA effaddr
    {X86_64.I_CMOVC (X86_64.I64b, $2, $4)}

  | I_LOCK semi_opt instr
    {X86_64.I_LOCK $3 }

effaddr:
  | rm64  {X86_64.Effaddr_rm64 $1}

rm64:
  |  reg {X86_64.Rm64_reg $1}
  |  LPAR reg RPAR {X86_64.Rm64_deref ($2, 0)}
  |  LBRK reg RBRK {X86_64.Rm64_deref ($2, 0)}
  |  k LPAR reg RPAR {X86_64.Rm64_deref ($3, Misc.string_as_int $1)}
  |  k LBRK reg RBRK {X86_64.Rm64_deref ($3, Misc.string_as_int $1)}
  |  LBRK NAME RBRK {X86_64.Rm64_abs (Constant.mk_sym $2)}
  |  LPAR NAME RPAR {X86_64.Rm64_abs (Constant.mk_sym $2)}
  |  LBRK NUM RBRK {X86_64.Rm64_abs (Constant.Concrete $2)}

operand:
  | effaddr {X86_64.Operand_effaddr $1}
  | k {X86_64.Operand_immediate (Misc.string_as_int $1) }
  | INTEL_NUM {X86_64.Operand_immediate (Misc.string_as_int $1)} /* enough ? */
