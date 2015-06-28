%{

(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


module Bell = BellBase
open Bell
    
%}

%token EOF SEMI COMMA PIPE COLON LPAR RPAR RBRAC LBRAC LBRACE RBRACE SCOPES REGIONS MOV AND ADD BRANCH EQ NEQ READ WRITE FENCE RMW CAS EXCH DOT XOR PLUS
%token <BellBase.reg> REG
%token <int> NUM
%token <string> NAME
%token <string> META
%token <BellBase.reg> SYMB_REG
%token <string> MEM_ANNOT
%token <int> PROC

%type <int list * (BellBase.parsedPseudo) list list * MiscParser.extra_data > main 
%type <BellBase.parsedPseudo list> instr_option_seq
%start main instr_option_seq

%nonassoc SEMI

%token SCOPETREE THREAD COMMA 

%type <BellInfo.test> scopes_and_memory_map
%%

main:
| semi_opt proc_list iol_list scopes_and_memory_map EOF
   { $2,$3, MiscParser.BellExtra $4 }

semi_opt:
| { () }
| SEMI { () }

proc_list:
| PROC SEMI
    {[$1]}

| PROC PIPE proc_list  { $1::$3 }

instr_option :
|            { Nop }
| NAME COLON instr_option { Label ($1,$3) }
| instr      { Instruction $1}

instr_option_list :
  | instr_option
      {[$1]}
  | instr_option PIPE instr_option_list 
      {$1::$3}

instr_option_seq:
  | instr_option
      {[$1]}
  | instr_option SEMI instr_option_seq 
      {$1::$3}

iol_list :
|  instr_option_list SEMI
    {[$1]}
|  instr_option_list SEMI iol_list {$1::$3}

name_list_ne:
|  NAME COMMA name_list_ne
  {$1::$3}
| NAME
  {[$1]}
name_list:
| name_list_ne {$1}
| {[]}

annot_list_option:
| LBRAC name_list RBRAC {$2}
| {[]}

reg:
| REG { $1}
| SYMB_REG { $1 }

reg_or_addr:
| reg  {Rega $1}
| NAME { Abs (Constant.Symbolic $1)}

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

operation:
| any_value
  { RAI($1) }

| LPAR ADD any_value any_value RPAR
 { Add($3,$4) }

| LPAR XOR any_value any_value RPAR
 { Xor($3,$4) }

| LPAR AND any_value any_value RPAR
 { And($3,$4) }

| LPAR EQ any_value any_value RPAR
  { Eq($3,$4) }

| LPAR NEQ any_value any_value RPAR
  { Neq($3,$4) }

fence_labels_option:
| { None }
| LBRACE name_list_ne RBRACE LBRACE name_list_ne RBRACE {Some($2,$5)}

instr:

| READ annot_list_option reg addr_op
  { Pld($3,$4,$2) }

| WRITE annot_list_option addr_op reg_or_imm
  { Pst($3,$4,$2) }

| FENCE annot_list_option fence_labels_option
 { Pfence(Fence ($2,$3)) 
(*jade: not sure why two levels here: could we just have Pfence, like for the others?*)
 }

| RMW annot_list_option reg operation addr_op  
  { Prmw($3,$4,$5,$2)}

| BRANCH annot_list_option reg NAME
  { Pbranch ($3,$4,$2) }

| MOV reg operation
  { Pmov ($2,$3)}

proc:
 | PROC { $1 }
 | NUM { $1 }

proc_list_sc:
| proc proc_list_sc {$1::$2}
| {[]}

scope_tree_list:
| scope_tree {[$1]}
| scope_tree scope_tree_list {$1::$2}
scope_tree:
 | LPAR NAME scope_tree_list RPAR  
   {
   BellInfo.Children($2,$3)
   }
 | LPAR NAME proc_list_sc RPAR 
   {
   BellInfo.Leaf($2,$3)
   }
top_scope_tree:
 | scope_tree_list
    { let ts = $1 in
      match ts with
      | [t] -> t
      | _ -> BellInfo.Children ("",ts) }
scope_option:
| SCOPES COLON top_scope_tree {Some $3}
| {None}

memory_map_option:
| REGIONS COLON memory_map {Some $3}
| {None}
memory_map_atom:
 | NAME COLON NAME
{ ($1,$3) }
memory_map:
 | memory_map_atom COMMA memory_map {$1::$3}
 | memory_map_atom {[$1]}
 | {[]
    (*jade: todo memory map*)
   }

scopes_and_memory_map:
 | scope_option memory_map_option
{ { BellInfo.scopes=$1; BellInfo.regions=$2; }}

