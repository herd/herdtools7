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

%token SEMI COMMA PIPE COLON LBRK RBRK LPAREN RPAREN

/* Instruction tokens */

%token I_ADD I_ADDS I_BX I_SUB I_SUBS I_AND I_ORR I_ANDS I_ANDEQ I_B I_BEQ I_BNE I_CMP I_MOV I_MOVW I_MOVT I_MOVNE I_MOVEQ I_XOR I_XORS I_DMB I_DSB I_ISB I_CBZ I_CBNZ
%token I_LDR I_LDREX I_LDRNE I_LDREQ I_LDRD I_LDM I_LDMIB I_STR I_STRNE I_STREQ I_STREX I_LDA I_STL I_LDAEX I_STLEX I_PUSH I_POP I_MOVWEQ I_MOVTEQ
%token I_SY I_ST I_ISH I_ISHST I_NSH I_NSHST I_OSH I_OSHST
%token S_LSL
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

shift:
  | COMMA S_LSL k { A.S_LSL $3 }

instr:
  | I_ADD reg COMMA reg COMMA k
     { A.I_ADD (A.DontSetFlags,$2,$4,$6) }
  | I_ADDS reg COMMA reg COMMA k
     { A.I_ADD (A.SetFlags,$2,$4,$6) }
  | I_ADD reg COMMA reg COMMA reg
     { A.I_ADD3 (A.DontSetFlags,$2, $4, $6) }
  | I_ADDS reg COMMA reg COMMA reg
     { A.I_ADD3 (A.SetFlags,$2, $4, $6) }
  | I_BX reg
     { A.I_BX $2 }
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
  | I_ORR reg COMMA reg COMMA k
     { A.I_ORR (A.DontSetFlags,$2,$4,$6) }
  | I_ANDS reg COMMA reg COMMA k
     { A.I_AND (A.SetFlags,$2,$4,$6) }
  | I_ANDEQ reg COMMA reg COMMA reg
     { (* This is historically used as a NOP when regs are the same*)
       if $2 = $4 && $4 = $6 then A.I_NOP else A.I_ANDC (A.EQ,$2,$4,$6) }
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
  | I_LDR reg COMMA LBRK reg COMMA reg shift RBRK
     { A.I_LDR3_S ($2,$5,$7,$8,A.AL) }
  | I_LDR reg COMMA LBRK reg COMMA k RBRK
     { A.I_LDRO ($2,$5,$7,A.AL) }
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
  | I_LDAEX reg COMMA LBRK reg RBRK
     { A.I_LDAEX ($2,$5) }
  | I_LDA reg COMMA LBRK reg RBRK
     { A.I_LDA ($2, $5)}
  (* 2-reg and 3-reg variants of LDM for now *)
  | I_LDM reg COMMA LPAREN reg COMMA reg RPAREN
     { A.I_LDM2 ($2, $5, $7, A.NO) }
  | I_LDMIB reg COMMA LPAREN reg COMMA reg RPAREN
     { A.I_LDM2 ($2, $5, $7, A.IB) }
  | I_LDM reg COMMA LPAREN reg COMMA reg COMMA reg RPAREN
     { A.I_LDM3 ($2, $5, $7, $9, A.NO) }
  | I_PUSH LPAREN reg RPAREN
     { A.I_NOP }
  | I_PUSH LPAREN reg COMMA reg RPAREN
     { A.I_NOP }
  | I_POP LPAREN reg COMMA reg RPAREN
     { A.I_NOP }
  | I_POP LPAREN reg RPAREN
     { A.I_NOP }
  (* LDRD syntax comes in two forms - LDRD Rd1, Rd2, [Ra] and *)
  (* LDRD Rd1, [Ra] - in both cases LDRD requires Rd2 is Rd(1+1) *)
  (* so the second register can be and is omitted e.g  by GCC-10*)
  | I_LDRD reg COMMA reg COMMA LBRK reg RBRK
     { A.I_LDRD ($2,$4,$7,None) }
  | I_LDRD reg COMMA LBRK reg RBRK
     { A.I_LDRD ($2,A.next_reg $2,$5,None) }
  | I_LDRD reg COMMA reg COMMA LBRK reg COMMA k RBRK
     { A.I_LDRD ($2,$4,$7,Some $9) }
  | I_LDRD reg COMMA LBRK reg COMMA k RBRK
     { A.I_LDRD ($2,A.next_reg $2,$5,Some $7) }
/* Store */
  | I_STR reg COMMA reg
     { A.I_STR ($2,$4,A.AL) }
  | I_STR reg COMMA LBRK reg RBRK
     { A.I_STR ($2,$5,A.AL) }
  | I_STR reg COMMA LBRK reg COMMA reg RBRK
     { A.I_STR3 ($2,$5,$7,A.AL) }
  | I_STR reg COMMA LBRK reg COMMA reg shift RBRK
     { A.I_STR3_S ($2,$5,$7,$8,A.AL) }
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
  | I_STL reg COMMA LBRK reg RBRK
     { A.I_STL ($2, $5,A.AL) }
  | I_STLEX reg COMMA reg COMMA LBRK reg RBRK
     { A.I_STLEX ($2,$4,$7) }
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
  | I_MOVW reg COMMA k
     { A.I_MOVW ($2,$4,A.AL)}
  | I_MOVWEQ reg COMMA k
     { A.I_MOVW ($2,$4,A.EQ) }
  | I_MOVT reg COMMA k
     { A.I_MOVT ($2,$4,A.AL)}
  | I_MOVTEQ reg COMMA k
     { A.I_MOVT ($2,$4,A.EQ)}
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
