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

module A = ARMBase
%}

%token EOF
%token <ARMBase.reg> ARCH_REG
%token <string> SYMB_REG
%token <int> NUM
%token <string> NAME
%token <string> META
%token <string> CODEVAR
%token <int> PROC

%token SEMI COMMA PIPE COLON LBRK RBRK

/* Instruction tokens */

%token I_ADD I_ADDS I_SUB I_SUBS I_AND I_ANDS I_B I_BEQ I_BNE I_CMP I_MOV I_MOVNE I_MOVEQ I_XOR I_XORS I_DMB I_DSB I_ISB I_CBZ I_CBNZ
%token I_LDR I_LDREX I_LDRNE I_LDREQ I_STR I_STRNE I_STREQ I_STREX
%token I_SY I_ST I_ISH I_ISHST I_NSH I_NSHST I_OSH I_OSHST
%type <MiscParser.proc list * (ARMBase.parsedPseudo) list list> main
%start  main

%type <ARMBase.parsedPseudo list> instr_option_seq
%start instr_option_seq

%%

main:
| semi_opt proc_list iol_list EOF { $2,$3 }

semi_opt:
| { () }
| SEMI { () }

proc_list:
| ps=separated_nonempty_list(PIPE,PROC) SEMI
    { List.map (fun p -> p,None) ps }

iol_list :
|  instr_option_list SEMI
    {[$1]}
|  instr_option_list SEMI iol_list {$1::$3}

instr_option_list :
  | instr_option
      {[$1]}
  | instr_option PIPE instr_option_list
      {$1::$3}

instr_option_seq:
| separated_nonempty_list(SEMI,instr_option) EOF { $1 }

instr_option :
|            { A.Nop }
| NAME COLON instr_option { A.Label ($1,$3) }
| CODEVAR { A.Symbolic $1 }
| instr      { A.Instruction $1}

reg:
| SYMB_REG { A.Symbolic_reg $1 }
| ARCH_REG { $1 }

k:
| NUM  { MetaConst.Int $1 }
| META { MetaConst.Meta $1 }

instr:
  | I_ADD reg COMMA reg COMMA k
     { A.I_ADD (A.DontSetFlags,$2,$4,$6) }
  | I_ADDS reg COMMA reg COMMA k
     { A.I_ADD (A.SetFlags,$2,$4,$6) }
  | I_ADD reg COMMA reg COMMA reg
     { A.I_ADD3 (A.DontSetFlags,$2, $4, $6) }
  | I_ADDS reg COMMA reg COMMA reg
     { A.I_ADD3 (A.SetFlags,$2, $4, $6) }
  | I_SUB reg COMMA reg COMMA k
     { A.I_SUB (A.DontSetFlags,$2,$4,$6) }
  | I_SUBS reg COMMA reg COMMA k
     { A.I_SUB (A.SetFlags,$2,$4,$6) }
  | I_SUB reg COMMA reg COMMA reg
     { A.I_SUB3 (A.DontSetFlags,$2, $4, $6) }
  | I_SUBS reg COMMA reg COMMA reg
     { A.I_SUB3 (A.SetFlags,$2, $4, $6) }
  | I_AND reg COMMA reg COMMA k
     { A.I_AND (A.DontSetFlags,$2,$4,$6) }
  | I_ANDS reg COMMA reg COMMA k
     { A.I_AND (A.SetFlags,$2,$4,$6) }
  | I_B NAME
     { A.I_B $2 }
  | I_BNE NAME
     { A.I_BNE $2 }
  | I_BEQ NAME
     { A.I_BEQ $2 }
  | I_CBZ reg COMMA NAME
     { A.I_CB (false,$2,$4) }
  | I_CBNZ reg COMMA NAME
     { A.I_CB (true,$2,$4) }
  | I_CMP reg COMMA k
     { A.I_CMPI ($2,$4) }
  | I_CMP reg COMMA reg
     { A.I_CMP ($2,$4) }
/* Load */
  | I_LDR reg COMMA reg
     { A.I_LDR ($2,$4, A.AL) }
  | I_LDR reg COMMA LBRK reg RBRK
     { A.I_LDR ($2,$5,A.AL) }
  | I_LDR reg COMMA LBRK reg COMMA reg RBRK
     { A.I_LDR3 ($2,$5,$7,A.AL) }
  | I_LDRNE reg COMMA reg
     { A.I_LDR ($2,$4,A.NE) }
  | I_LDRNE reg COMMA LBRK reg RBRK
     { A.I_LDR ($2,$5,A.NE) }
  | I_LDRNE reg COMMA LBRK reg COMMA reg RBRK
     { A.I_LDR3 ($2,$5,$7,A.NE) }
  | I_LDREQ reg COMMA reg
     { A.I_LDR ($2,$4,A.EQ) }
  | I_LDREQ reg COMMA LBRK reg RBRK
     { A.I_LDR ($2,$5,A.EQ) }
  | I_LDREQ reg COMMA LBRK reg COMMA reg RBRK
     { A.I_LDR3 ($2,$5,$7,A.EQ) }
  | I_LDREX reg COMMA reg
     { A.I_LDREX ($2,$4) }
  | I_LDREX reg COMMA LBRK reg RBRK
     { A.I_LDREX ($2,$5) }
/* Store */
  | I_STR reg COMMA reg
     { A.I_STR ($2,$4,A.AL) }
  | I_STR reg COMMA LBRK reg RBRK
     { A.I_STR ($2,$5,A.AL) }
  | I_STR reg COMMA LBRK reg COMMA reg RBRK
     { A.I_STR3 ($2,$5,$7,A.AL) }
  | I_STRNE reg COMMA reg
     { A.I_STR ($2,$4,A.NE) }
  | I_STRNE reg COMMA LBRK reg RBRK
     { A.I_STR ($2,$5,A.NE) }
  | I_STRNE reg COMMA LBRK reg COMMA reg RBRK
     { A.I_STR3 ($2,$5,$7,A.NE) }
  | I_STREQ reg COMMA reg
     { A.I_STR ($2,$4,A.EQ) }
  | I_STREQ reg COMMA LBRK reg RBRK
     { A.I_STR ($2,$5,A.EQ) }
  | I_STREQ reg COMMA LBRK reg COMMA reg RBRK
     { A.I_STR3 ($2,$5,$7,A.EQ) }
  | I_STREX reg COMMA reg COMMA LBRK reg RBRK
     { A.I_STREX ($2,$4,$7,A.AL) }
/* MOVE */
  | I_MOV reg COMMA k
     { A.I_MOVI ($2,$4,A.AL) }
  | I_MOVNE reg COMMA k
     { A.I_MOVI ($2,$4,A.NE) }
  | I_MOVEQ reg COMMA k
     { A.I_MOVI ($2,$4,A.EQ) }
  | I_MOV reg COMMA reg
     { A.I_MOV ($2,$4,A.AL) }
  | I_MOVNE reg COMMA reg
     { A.I_MOV ($2,$4,A.NE) }
  | I_MOVEQ reg COMMA reg
     { A.I_MOV ($2,$4,A.EQ) }
  | I_XOR reg COMMA reg COMMA reg
     { A.I_XOR (A.DontSetFlags,$2,$4,$6) }
  | I_XORS reg COMMA reg COMMA reg
     { A.I_XOR (A.SetFlags,$2,$4,$6) }
/* FENCES */
  | I_DMB { A.I_DMB A.SY }
  | I_DSB opt { A.I_DSB $2 }
  | I_DMB opt { A.I_DMB $2 }
  | I_DSB { A.I_DSB A.SY }
  | I_ISB { A.I_ISB }

opt:
  | I_SY { A.SY }
  | I_ST { A.ST }
  | I_ISH { A.ISH }
  | I_ISHST { A.ISHST }
  | I_NSH { A.NSH }
  | I_NSHST { A.NSHST }
  | I_OSH { A.OSH }
  | I_OSHST { A.OSHST }
