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
    
%}

%token EOF SEMI COMMA PIPE COLON LPAR RPAR RBRAC LBRAC LBRACE RBRACE SCOPES REGIONS MOV AND ADD BEQ READ WRITE FENCE RMW CAS EXCH DOT XOR PLUS
%token <BellBase.reg> REG
%token <int> NUM
%token <string> NAME 
%token <string> MEM_ANNOT
%token <string> SCOPE
%token <string> REGION
%token <int> PROC

%type <int list * (BellBase.pseudo) list list * MiscParser.gpu_data option * BellInfo.test option > main 
%start  main

%nonassoc SEMI

%token SCOPETREE GLOBAL SHARED DEVICE KERNEL CTA WARP THREAD COMMA PTX_REG_DEC 

%type <BellInfo.test> scopes_and_memory_map
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
| READ LBRAC annot_list RBRAC reg addr_op 
  { Pld($5, $6, $3) }

 | WRITE LBRAC annot_list RBRAC addr_op roi 
 { Pst($5, $6, $3) }

| RMW DOT rmw2_op LBRAC annot_list RBRAC reg roa roi
  { Prmw2_op($7,$8,$9,$3,$5)}

| RMW DOT rmw3_op LBRAC annot_list RBRAC reg roa roi roi
  { Prmw3_op($7,$8,$9,$10,$3,$5)}


| FENCE LBRAC annot_list RBRAC
 { Pfence(Fence($3)) }

| MOV reg COMMA iar
 { Pmov($2,$4) }

| ADD reg COMMA iar COMMA iar
 { Padd($2,$4,$6) }

| AND reg COMMA iar COMMA iar
 { Pand($2,$4,$6) }

| XOR reg COMMA iar COMMA iar
 { Pxor($2,$4,$6) }

| BEQ roi COMMA roi COMMA NAME
 { Pbeq($2,$4,$6) }

annot_list:
|  NAME COMMA annot_list
  {$1::$3}
| NAME
  {[$1]}
| {[]}

addr_op:
| roa {BellBase.Addr_op_atom($1)}
| roa PLUS roi {BellBase.Addr_op_add($1,$3)}

rmw2_op:
| ADD  { RMWAdd  }
| EXCH { RMWExch }

rmw3_op:
| CAS  { RMWCAS }



roa:
| REG {Rega $1}
| NAME { Abs (Constant.Symbolic $1)}

roi:
| REG {Regi $1}
| NUM { Imm $1}

iar:
| roa { IAR_roa $1}
| NUM { IAR_imm $1}


reg:
| REG { $1 }

scopes_and_memory_map:
 | scope_option memory_map_option
{ { BellInfo.scopes=$1; BellInfo.regions=$2; }}

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
