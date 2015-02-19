%{

(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(* Tyler Sorensen, University College London                         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


module Bell = BellBase
open Bell
open Printf
    
%}

%token EOF SEMI COMMA PIPE COLON LPAR RPAR RBRAC LBRAC LBRACE RBRACE SCOPES REGIONS MOV AND ADD BEQ READ WRITE FENCE
%token <BellBase.reg> REG
%token <int> NUM
%token <string> NAME 
%token <string> MEM_ANNOT
%token <string> SCOPE
%token <string> REGION
%token <int> PROC

%type <int list * (BellBase.pseudo) list list * MiscParser.gpu_data option * Bell_info.bell_test_info option > main 
%start  main

%nonassoc SEMI

%token SCOPETREE GLOBAL SHARED DEVICE KERNEL CTA WARP THREAD COMMA PTX_REG_DEC 

%type <Bell_info.bell_test_info> scopes_and_memory_map
%%

main:
| semi_opt proc_list iol_list scopes_and_memory_map EOF { $2,$3, None, Some $4 }

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
| instr      { Instruction $1}

instr:
| READ LPAR read_annot_list RPAR reg COMMA LBRAC roa RBRAC
  { 
  Pld($5, $8, $3) 
  }
| WRITE LPAR write_annot_list RPAR LBRAC roa RBRAC COMMA roi 
   { 
  Pst($6, $9, $3) }

| FENCE LPAR fence_annot_list RPAR
  {
  Pfence(Fence($3))}

/* Easier stuff */
| MOV reg COMMA roi
   { Pmov($2,$4) }

| ADD reg COMMA roi COMMA roi
 { Padd($2,$4,$6) }

| AND reg COMMA roi COMMA roi
 { Pand($2,$4,$6) }

| BEQ roi COMMA roi COMMA NAME
  {Pbeq($2,$4,$6)}


read_annot_list:
|  NAME COMMA read_annot_list
  {
  $1::$3
   }

| NAME
  {
  [$1]
  }
| {[]}

write_annot_list:
|  NAME COMMA write_annot_list
  {
  $1::$3
   }

| NAME 
  {
  [$1]
  }
| {[]}

fence_annot_list:
|  NAME COMMA fence_annot_list
  {
  $1::$3
   }

| NAME 
  {
  [$1]
  }
| {[]}

roa:
| REG {Rega $1}
| NAME { Abs (Constant.Symbolic $1)}

roi:
| REG {Regi $1}
| NUM { Imm $1}

reg:
| REG { $1 }

scopes_and_memory_map:
 | scope_option memory_map_option
{ { Bell_info.scopes=$1; Bell_info.regions=$2; }}

scope_option:
| SCOPES COLON scope_tree {Some $3}
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
 | {[]}

proc_list_sc:
| PROC proc_list_sc {$1::$2}
| PROC {[$1]}
| {[]}

scope_tree_list:
| scope_tree {[$1]}
| scope_tree scope_tree_list {$1::$2}

scope_tree:
 | LPAR NAME scope_tree_list RPAR  
   {
   Bell_info.Children($2,$3)
   }
 | LPAR NAME proc_list_sc RPAR 
   {
   Bell_info.Leaf($2,$3)
   }
