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
  | I_OR  operand  COMMA  effaddr
    {I_EFF_OP (I_OR, INSb , $4,$2)}
  | I_ORB  operand  COMMA  effaddr
    {I_EFF_OP (I_OR, I8b, $4,$2)}
  | I_ORW  operand  COMMA  effaddr
    {I_EFF_OP (I_OR, I16b, $4,$2)}
  | I_ORL  operand  COMMA  effaddr
    {I_EFF_OP (I_OR, I32b, $4,$2)}
  | I_ORQ  operand  COMMA  effaddr
    {I_EFF_OP (I_OR, I64b, $4,$2)}

  | I_XOR  operand  COMMA  effaddr
    {I_EFF_OP (I_XOR, INSb, $4,$2)}
  | I_XORB  operand  COMMA  effaddr
    {I_EFF_OP (I_XOR, I8b, $4,$2)}
  | I_XORW  operand  COMMA  effaddr
    {I_EFF_OP (I_XOR, I16b, $4,$2)}
  | I_XORL  operand  COMMA  effaddr
    {I_EFF_OP (I_XOR, I32b, $4,$2)}
  | I_XORQ  operand  COMMA  effaddr
    {I_EFF_OP (I_XOR, I64b, $4,$2)}

  | I_ADD   operand  COMMA  effaddr
    {I_EFF_OP (I_ADD, INSb, $4,$2)}
  | I_ADDB   operand  COMMA  effaddr
    {I_EFF_OP (I_ADD, I8b, $4,$2)}
  | I_ADDW   operand  COMMA  effaddr
    {I_EFF_OP (I_ADD, I16b, $4,$2)}
  | I_ADDL   operand  COMMA  effaddr
    {I_EFF_OP (I_ADD, I32b, $4,$2)}
  | I_ADDQ   operand  COMMA  effaddr
    {I_EFF_OP (I_ADD, I64b, $4,$2)}

  | I_MOV   operand  COMMA  effaddr
    {I_EFF_OP (I_MOV, INSb, $4,$2)}
  | I_MOVB   operand  COMMA  effaddr
    {I_EFF_OP (I_MOV, I8b, $4,$2)}
  | I_MOVW   operand  COMMA  effaddr
    {I_EFF_OP (I_MOV, I16b, $4,$2)}
  | I_MOVL   operand  COMMA  effaddr
    {I_EFF_OP (I_MOV, I32b, $4,$2)}
  | I_MOVQ   operand  COMMA  effaddr
    {I_EFF_OP (I_MOV, I64b, $4,$2)}

  | I_CMP  operand  COMMA  effaddr
    {I_EFF_OP (I_CMP, INSb, $4,$2)}
  | I_CMPB  operand  COMMA  effaddr
    {I_EFF_OP (I_CMP, I8b, $4,$2)}
  | I_CMPW  operand  COMMA  effaddr
    {I_EFF_OP (I_CMP, I16b, $4,$2)}
  | I_CMPL  operand  COMMA  effaddr
    {I_EFF_OP (I_CMP, I32b, $4,$2)}
  | I_CMPQ  operand  COMMA  effaddr
    {I_EFF_OP (I_CMP, I64b, $4,$2)}

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

  | I_DEC  effaddr
    {I_EFF (I_DEC, INSb, $2)}
  | I_DECB  effaddr
    {I_EFF (I_DEC, I8b, $2)}
  | I_DECW  effaddr
    {I_EFF (I_DEC, I16b, $2)}
  | I_DECL  effaddr
    {I_EFF (I_DEC, I32b, $2)}
  | I_DECQ  effaddr
    {I_EFF (I_DEC, I64b, $2)}

  | I_INC  effaddr
    {I_EFF (I_INC, INSb, $2)}
  | I_INCB  effaddr
    {I_EFF (I_INC, I8b, $2)}
  | I_INCW  effaddr
    {I_EFF (I_INC, I16b, $2)}
  | I_INCL  effaddr
    {I_EFF (I_INC, I32b, $2)}
  | I_INCQ  effaddr
    {I_EFF (I_INC, I64b, $2)}

  | I_SETNB  effaddr
    {I_EFF (I_SETNB, INSb, $2)}

  | I_XCHG  effaddr  COMMA  effaddr
    {I_EFF_EFF (I_XCHG, INSb, $2, $4)}
  | I_XCHGB  effaddr  COMMA  effaddr
    {I_EFF_EFF (I_XCHG, I8b, $2, $4)}
  | I_XCHGW  effaddr  COMMA  effaddr
    {I_EFF_EFF (I_XCHG, I16b, $2, $4)}
  | I_XCHGL  effaddr  COMMA  effaddr
    {I_EFF_EFF (I_XCHG, I32b, $2, $4)}
  | I_XCHGQ  effaddr  COMMA  effaddr
    {I_EFF_EFF (I_XCHG, I64b, $2, $4)}

  | I_UXCH  effaddr  COMMA  effaddr
    {I_EFF_EFF (I_XCHG_UNLOCKED, INSb, $2, $4)}
  | I_UXCHB  effaddr  COMMA  effaddr
    {I_EFF_EFF (I_XCHG_UNLOCKED, I8b, $2, $4)}
  | I_UXCHW  effaddr  COMMA  effaddr
    {I_EFF_EFF (I_XCHG_UNLOCKED, I16b, $2, $4)}
  | I_UXCHL  effaddr  COMMA  effaddr
    {I_EFF_EFF (I_XCHG_UNLOCKED, I32b, $2, $4)}
  | I_UXCHQ  effaddr  COMMA  effaddr
    {I_EFF_EFF (I_XCHG_UNLOCKED, I64b, $2, $4)}

  | I_CMPXCHG  effaddr  COMMA  reg
    {I_CMPXCHG (INSb, $2, $4)}
  | I_CMPXCHGB  effaddr  COMMA  reg
    {I_CMPXCHG (I8b, $2, $4)}
  | I_CMPXCHGW  effaddr  COMMA  reg
    {I_CMPXCHG (I16b, $2, $4)}
  | I_CMPXCHGL  effaddr  COMMA  reg
    {I_CMPXCHG (I32b, $2, $4)}
  | I_CMPXCHGQ  effaddr  COMMA  reg
    {I_CMPXCHG (I64b, $2, $4)}

  | I_CMOVC  reg COMMA effaddr
    {I_CMOVC (INSb, $2, $4)}
  | I_CMOVCB  reg COMMA effaddr
    {I_CMOVC (I8b, $2, $4)}
  | I_CMOVCW  reg COMMA effaddr
    {I_CMOVC (I16b, $2, $4)}
  | I_CMOVCL  reg COMMA effaddr
    {I_CMOVC (I32b, $2, $4)}
  | I_CMOVCQ  reg COMMA effaddr
    {I_CMOVC (I64b, $2, $4)}

  | I_LOCK semi_opt instr
    {I_LOCK $3 }

effaddr:
  | rm64  {Effaddr_rm64 $1}

rm64:
  |  reg {Rm64_reg $1}
  |  LPAR reg RPAR {Rm64_deref ($2, 0)}
  |  LBRK reg RBRK {Rm64_deref ($2, 0)}
  |  k LPAR reg RPAR {Rm64_deref ($3, Misc.string_as_int $1)}
  |  k LBRK reg RBRK {Rm64_deref ($3, Misc.string_as_int $1)}
  |  LBRK NAME RBRK {Rm64_abs (Constant.mk_sym $2)}
  |  LBRK NUM RBRK {Rm64_abs (Constant.Concrete $2)}

operand:
  | effaddr {Operand_effaddr $1}
  | k {Operand_immediate (Misc.string_as_int $1) }
  | INTEL_NUM {Operand_immediate (Misc.string_as_int $1)} /* enough ? */
