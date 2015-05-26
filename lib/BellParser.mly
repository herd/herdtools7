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

%token EOF SEMI COMMA PIPE COLON LPAR RPAR RBRAC LBRAC LBRACE RBRACE SCOPES REGIONS MOV AND ADD BRANCH BEQ BNE BAL READ WRITE FENCE RMW CAS EXCH DOT XOR PLUS
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

/* backward compatibility: some files have '(' ... ')' for annotations */
old_annot_list:
| LBRAC annot_list RBRAC { $2 }
| LPAR annot_list RPAR { $2 }

/* similarily for R and W arguments there can be some [ ] */
old_addr_op:
| addr_op  { $1 }
| LBRAC addr_op RBRAC { $2 }

/* some optional commas... */
old_comma_opt:
| { () }
| COMMA { () }

condition:
| BEQ { Eq }
| BNE { Ne }

rmw2_op_annot:
| rmw2_op { ($1,[]) }
| rmw2_op COMMA annot_list_ne { ($1,$3) }

rmw3_op_annot:
| rmw3_op { ($1,[]) }
| rmw3_op COMMA annot_list_ne { ($1,$3) }

fence_labels_opt:
| { None }
| LPAR LBRACE annot_list_ne RBRACE COMMA LBRACE annot_list_ne RBRACE RPAR {Some($3,$7)}

instr:
| READ old_annot_list reg old_comma_opt old_addr_op 
  { Pld($3, $5, $2) }

 | WRITE old_annot_list old_addr_op old_comma_opt roi 
 { Pst($3, $5, $2) }

| RMW LBRAC rmw2_op_annot RBRAC reg old_comma_opt roa old_comma_opt roi
  { Prmw2_op($5,$7,$9,fst $3,snd $3)}

| RMW LBRAC rmw3_op_annot RBRAC reg old_comma_opt roa old_comma_opt roi old_comma_opt roi
  { Prmw3_op($5,$7,$9,$11,fst $3,snd $3)}

| FENCE old_annot_list fence_labels_opt
 { Pfence(Fence ($2,$3)) }

| MOV reg COMMA iar
 { Pmov($2,$4) }

| ADD reg COMMA iar COMMA iar
 { Pop(Add,$2,$4,$6) }

| AND reg COMMA iar COMMA iar
 { Pop(And,$2,$4,$6) }

| XOR reg COMMA iar COMMA iar
 { Pop(Xor,$2,$4,$6) }

| BRANCH LBRAC condition RBRAC reg COMMA roi COMMA NAME
  { Pbcc ($3,$5,$7,$9) }

| BRANCH LBRAC BAL RBRAC NAME
  { Pbal $5 }

annot_list_ne:
|  NAME COMMA annot_list
  {$1::$3}
| NAME
  {[$1]}
annot_list:
| annot_list_ne {$1}
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
 | {[]}

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
