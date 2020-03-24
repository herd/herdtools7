%{
(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Bell = BellBase
open Bell
%}

%token EOF SEMI COMMA PIPE COLON LPAR RPAR RBRAC LBRAC LBRACE RBRACE SCOPES LEVELS REGIONS MOV AND ADD BRANCH EQ NEQ READ WRITE FENCE RMW XOR PLUS CALL
%token <BellBase.reg> REG
%token <int> NUM
%token <string> CODEVAR
%token <string> NAME
%token <string> META
%token <BellBase.reg> SYMB_REG
%token <int> PROC

%type <MiscParser.proc list * (BellBase.parsedPseudo) list list * MiscParser.extra_data > main 
%type <BellBase.parsedPseudo list> instr_option_seq
%start main instr_option_seq

%type <BellInfo.test> scopes_and_memory_map
%%

main:
| semi_opt proc_list iol_list scopes_and_memory_map EOF
   { $2,$3, MiscParser.BellExtra $4 }

semi_opt:
| { () }
| SEMI { () }

instr_option :
|            { Nop }
| NAME COLON instr_option { Label ($1,$3) }
| instr      { Instruction $1}
| CODEVAR    { Symbolic $1 }

instr_option_list :
  | instr_option
      {[$1]}
  | instr_option PIPE instr_option_list 
      {$1::$3}

instr_option_seq:
  | xs=separated_nonempty_list(SEMI,instr_option) EOF { xs }

iol_list :
|  instr_option_list SEMI
    {[$1]}
|  instr_option_list SEMI iol_list {$1::$3}

name:
| NAME { $1}
/*(* hideous fix for instruction characters *)*/
| READ {"r"}
| WRITE {"w"}
| BRANCH {"b"}
| FENCE {"f"}

name_list_ne:
|  name COMMA name_list_ne
  {$1::$3}
| name
  {[$1]}

name_list:
| name_list_ne {$1}
| {[]}

name_set:
| name_list { Label.Set.of_list $1}


annot_list_option:
| LBRAC name_list RBRAC {$2}
| {[]}

reg:
| REG { $1}
| SYMB_REG { $1 }

reg_or_addr:
| reg  {Rega $1}
| name { Abs $1}

k:
| NUM { MetaConst.Int $1 }
| META { MetaConst.Meta $1 }

reg_or_imm:
| reg {Regi $1}
| k   { Imm $1}

any_value:
| reg_or_addr { IAR_roa $1 }
| k { IAR_imm $1}

addr_op:
| reg_or_addr {BellBase.Addr_op_atom($1)}
| reg_or_addr PLUS reg_or_imm { BellBase.Addr_op_add($1,$3) }

operation:
| any_value
  { RAI($1) }

| LPAR ADD any_value any_value RPAR
 { OP (Add,$3,$4) }

| LPAR XOR any_value any_value RPAR
 { OP (Xor,$3,$4) }

| LPAR AND any_value any_value RPAR
 { OP (And,$3,$4) }

| LPAR EQ any_value any_value RPAR
  { OP (Eq,$3,$4) }

| LPAR NEQ any_value any_value RPAR
  { OP (Neq,$3,$4) }

fence_labels_option:
| { None }
| LBRACE name_set RBRACE LBRACE name_set RBRACE {Some($2,$5)}

instr:

| READ annot_list_option reg addr_op
  { Pld($3,$4,$2) }

| WRITE annot_list_option addr_op reg_or_imm
  { Pst($3,$4,$2) }

| FENCE annot_list_option fence_labels_option
 { Pfence(Fence ($2,$3)) }

| CALL LBRAC name RBRAC
  { Pcall $3 }

| RMW annot_list_option reg operation addr_op  
  { Prmw($3,$4,$5,$2)}

| BRANCH annot_list_option reg NAME
  { Pbranch (Some $3,$4,$2) }

| BRANCH annot_list_option NAME
  { Pbranch (None,$3,$2) }

| MOV reg operation
  { Pmov ($2,$3) }

