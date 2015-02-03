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
module PPC = PPCBase
open PPC
%}

%token EOF
%token <PPCBase.reg> ARCH_REG
%token <string> SYMB_REG
%token <int> NUM
%token <string> NAME
%token <int> PROC

%token SEMI COMMA PIPE COLON LPAR RPAR

%token <int> CRK

/* Instruction tokens */
%token LI
%token ADD ADDDOT SUB SUBF SUBFDOT SUBDOT XOR XORDOT OR ORDOT  AND ANDDOT
%token MULL MULLDOT DIV DIVDOT
%token ADDI SUBI ORI XORI ANDIDOT MULLI
%token LWZ LWZU LWZX MR STW STWU STWX LWARX STWCX CMPWI CMPW
%token LD STD LDX STDX
%token SYNC EIEIO ISYNC LWSYNC DCBF B BEQ BNE BLT BGT BLE BGE BNL BNG
%token NOR NORDOT NEG NEGDOT SLW SRAWI SRAW BL BLR MTLR MFLR
%token LMW STMW
%token COMMENT
%token <string> STRING

%type <int list * (PPCBase.pseudo) list list * MiscParser.gpu_data option> main
%start  main

%nonassoc SEMI
%%

main:
| semi_opt proc_list iol_list EOF { $2,$3,None }
| semi_opt proc_list EOF { $2,[],None }

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
| NAME LPAR reg_list RPAR
             { Macro ($1,$3) }
| instr      { Instruction $1}

reg_list :
| { [] }
| reg { [$1] }
| reg COMMA reg_list { $1 :: $3 }

instr:
  | ADD reg COMMA reg COMMA reg
    { Padd (DontSetCR0,$2,$4,$6) }
  | ADDDOT reg COMMA reg COMMA reg
    { Padd (SetCR0,$2,$4,$6) }
  | SUB reg COMMA reg COMMA reg
    { Psub (DontSetCR0,$2,$4,$6) }
  | SUBDOT reg COMMA reg COMMA reg
    { Psub (SetCR0,$2,$4,$6) }
  | SUBF reg COMMA reg COMMA reg
    { Psubf (DontSetCR0,$2,$4,$6) }
  | SUBFDOT reg COMMA reg COMMA reg
    { Psubf (SetCR0,$2,$4,$6) }
  | ADDI reg COMMA reg COMMA k
    { Paddi ($2,$4,$6) }
  | SUBI reg COMMA reg COMMA k
    { Paddi ($2,$4, 0 - $6) }
  | CMPWI reg COMMA k
    { Pcmpwi (0,$2,$4) }
  | CMPWI crindex COMMA reg COMMA k
    { Pcmpwi ($2,$4,$6) }
  | CMPW crindex COMMA reg COMMA reg
    { Pcmpw ($2,$4,$6)}
  | CMPW reg COMMA reg
    { Pcmpw (0,$2,$4)}
  | LI reg COMMA k
    { Pli ($2,$4) }
  | XOR reg COMMA reg COMMA reg
    { Pxor (DontSetCR0,$2,$4,$6) }
  | XORDOT reg COMMA reg COMMA reg
    { Pxor (SetCR0,$2,$4,$6) }
  | XORI reg COMMA reg COMMA k
    { Pxori ($2,$4,$6) }
  | AND reg COMMA reg COMMA reg
    { Pand (DontSetCR0,$2,$4,$6) }
  | ANDDOT reg COMMA reg COMMA reg
    { Pand (SetCR0,$2,$4,$6) }
  | ANDIDOT reg COMMA reg COMMA k
    { Pandi ($2,$4,$6) }
  | OR reg COMMA reg COMMA reg
    { Por (DontSetCR0,$2,$4,$6) }
  | ORDOT reg COMMA reg COMMA reg
    { Por (SetCR0,$2,$4,$6) }
  | ORI reg COMMA reg COMMA k
    { Pori ($2,$4,$6) }
  | MULL  reg COMMA reg COMMA reg
    { Pmull (DontSetCR0,$2,$4,$6) }
  | MULLDOT  reg COMMA reg COMMA reg
    { Pmull (SetCR0,$2,$4,$6) }
  | MULLI reg COMMA reg COMMA k
    { Pmulli ($2,$4,$6) }
  | DIV  reg COMMA reg COMMA reg
    { Pdiv (DontSetCR0,$2,$4,$6) }
  | DIVDOT  reg COMMA reg COMMA reg
    { Pdiv (SetCR0,$2,$4,$6) }
  | LWZ reg COMMA idx COMMA reg
    { Plwz ($2,$4,$6)}
  | LWZ reg COMMA idx LPAR reg RPAR
    { Plwz ($2,$4,$6)}
  | LWZU reg COMMA idx COMMA reg
    { Plwzu ($2,$4,$6)}
  | LWZU reg COMMA idx LPAR reg RPAR
    { Plwzu ($2,$4,$6)}
  | LD reg COMMA idx COMMA reg
    { Pld ($2,$4,$6)}
  | LD reg COMMA idx LPAR reg RPAR
    { Pld ($2,$4,$6)}
  | LWZX reg COMMA reg COMMA reg
    { Plwzx ($2,$4,$6)}
  | LDX reg COMMA reg COMMA reg
    { Pldx ($2,$4,$6)}
  | MR reg COMMA reg
    { Pmr ($2,$4) }
  | STW reg COMMA idx COMMA reg
    { Pstw ($2,$4,$6) }
  | STW reg COMMA idx LPAR reg RPAR
    { Pstw ($2,$4,$6) }
  | STWU reg COMMA idx COMMA reg
    { Pstwu ($2,$4,$6) }
  | STWU reg COMMA idx LPAR reg RPAR
    { Pstwu ($2,$4,$6) }
  | STD reg COMMA idx COMMA reg
    { Pstd ($2,$4,$6) }
  | STD reg COMMA idx LPAR reg RPAR
    { Pstd ($2,$4,$6) }
  | STWX reg COMMA reg COMMA reg
    { Pstwx ($2,$4,$6) }
  | STDX reg COMMA reg COMMA reg
    { Pstdx ($2,$4,$6) }
  | LWARX  reg COMMA reg COMMA reg
    { Plwarx ($2,$4,$6)}
  | STWCX reg COMMA reg COMMA reg
    { Pstwcx ($2,$4,$6) }
  | SYNC
    { Psync }
  | EIEIO
    { Peieio }
  | LWSYNC
    { Plwsync }
  | ISYNC
    { Pisync }
  | DCBF reg COMMA reg
    { Pdcbf ($2,$4) }
  | B NAME { Pb $2 }
  | BEQ NAME { Pbcc (Eq,$2) }
  | BNE NAME { Pbcc (Ne,$2) }
  | BLT NAME { Pbcc (Lt,$2) }
  | BGE NAME { Pbcc (Ge,$2) }
  | BNL NAME { Pbcc (Ge,$2) }
  | BGT NAME { Pbcc (Gt,$2) }
  | BLE NAME { Pbcc (Le,$2) }
  | BNG NAME { Pbcc (Le,$2) }
  | NOR reg COMMA reg COMMA reg
    { Pnor (DontSetCR0,$2,$4,$6)}
  | NORDOT reg COMMA reg COMMA reg
    { Pnor (SetCR0,$2,$4,$6)}
  | NEG reg COMMA reg 
    { Pneg (DontSetCR0,$2,$4)}
  | NEGDOT reg COMMA reg 
    { Pneg (SetCR0,$2,$4)}
  | SLW reg COMMA reg COMMA reg
    { Pslw (DontSetCR0,$2,$4,$6)}
  | SRAWI reg COMMA reg COMMA k
    { Psrawi (DontSetCR0,$2,$4,$6)}
  | SRAW reg COMMA reg COMMA reg
    { Psraw (DontSetCR0,$2,$4,$6)}
  | BL NAME { Pbl $2 }
  | BLR { Pblr }
  | MTLR reg { Pmtlr $2}
  | MFLR reg { Pmflr $2}
  | LMW reg  COMMA idx LPAR reg RPAR { Plmw ($2,$4,$6) }
  | STMW reg  COMMA idx LPAR reg RPAR { Pstmw ($2,$4,$6) }
  | COMMENT STRING { Pcomment $2 }
 
k:
| NUM  { $1 }

idx:
| NUM  { $1 }


crindex:
| CRK  { $1 }

reg:
| SYMB_REG { Symbolic_reg $1 }
| ARCH_REG { $1 }
