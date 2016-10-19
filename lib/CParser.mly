%{
(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


open CBase
open MemOrder
open CType
open MemOrderOrAnnot
%}

%token EOF
%token <string> IDENTIFIER
%token <string> BASE_TYPE
%token <string> ATOMIC_TYPE
%token <string> CONSTVAR
%token <string> CODEVAR
%token <int> PROC
%token LPAR RPAR COMMA LBRACE RBRACE STAR 
%token ATOMIC CHAR INT
%token MUTEX
%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CONST VOLATILE 

/* For shallow parsing */
%token <string> BODY
%type <string CAst.t list> shallow_main 
%start shallow_main

/* For deep parsing */
%token <int> CONSTANT
%token NULL
%token SEMI COLON EQ EQ_OP NEQ_OP DOT
%token XOR PIPE
%token LAND
%token ADD SUB
%token MUL DIV
%token WHILE IF ELSE
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%token <MemOrder.t> MEMORDER
%token LD LD_EXPLICIT ST ST_EXPLICIT EXC EXC_EXPLICIT FENCE LOCK UNLOCK SPINLOCK SPINUNLOCK SCAS WCAS
%token LOAD STORE UNDERFENCE XCHG
%token <Op.op> ATOMIC_FETCH
%token <Op.op> ATOMIC_FETCH_EXPLICIT

%left PIPE
%left XOR
%left LAND

%nonassoc EQ_OP NEQ_OP
%left ADD SUB
%left STAR DIV

%type <(CBase.pseudo list) CAst.test list> deep_main
%start deep_main

%type <CBase.pseudo list> pseudo_seq
%start pseudo_seq

%type <CBase.macro list> macros
%start macros

%%

parameter_list:
| { [] }
| parameter_declaration { [ $1 ] }
| parameter_declaration COMMA parameter_list { $1 :: $3 }
 
parameter_declaration:
| toptyp IDENTIFIER { {CAst.param_ty = $1; param_name = $2} }

toptyp:
| typ STAR { Pointer $1 }

typ:
| typ STAR { Pointer $1 } 
| typ VOLATILE { Volatile $1 } 
| ATOMIC base { Atomic $2 }
| VOLATILE base0 { Volatile $2 }
| base { $1 }

base0:
| ATOMIC_TYPE { Atomic (Base $1) }
| BASE_TYPE { (Base $1) }
| ty_attr MUTEX { Base ($1 ^ "mutex") }
| ty_attr CHAR { Base ($1 ^ "char") }
| ty_attr INT { Base ($1 ^ "int") }


base:
| base0 { $1 }
| LPAR typ RPAR { $2 }

ty_attr:
| { "" }

shallow_main:
| EOF { [] }
| BODY shallow_main { CAst.Global $1 :: $2 }
| PROC LPAR parameter_list RPAR BODY shallow_main
    { CAst.Test {CAst.proc = $1; params = $3; body = $5} :: $6 }

declaration:
| typ IDENTIFIER SEMI {}

initialisation:
| typ IDENTIFIER EQ expr { StoreReg ($2,$4) ; }

annot:
| IDENTIFIER { $1 }
| LOCK       { "lock" }
| UNLOCK       { "unlock" }

annot_list:
| annot COMMA annot_list
  {$1::$3}
| annot
  {[$1]}


expr:
| LPAR expr RPAR { $2 }
| CONSTANT { Const(Constant.Concrete $1) }
| CONSTVAR { Const(Constant.Symbolic $1) }
| IDENTIFIER { LoadReg $1 }
| STAR IDENTIFIER { LoadMem (LoadReg $2,AN []) }
| STAR LPAR expr RPAR { LoadMem ($3,AN []) }
| LD LPAR expr RPAR { LoadMem($3,MO SC) }
| LOAD LBRACE annot_list RBRACE LPAR expr RPAR { LoadMem($6,AN $3) }
| LD_EXPLICIT LPAR expr COMMA MEMORDER RPAR { LoadMem($3,MO $5) }
| expr STAR expr { Op(Op.Mul,$1,$3) }
| expr ADD expr { Op(Op.Add,$1,$3) }
| expr SUB expr { Op(Op.Sub,$1,$3) }
| expr DIV expr { Op(Op.Div,$1,$3) }
| expr LAND expr { Op(Op.And,$1,$3) }
| expr PIPE expr { Op(Op.Or,$1,$3) }
| expr XOR expr { Op(Op.Xor,$1,$3) }
| expr EQ_OP expr { Op(Op.Eq,$1,$3) }
| expr NEQ_OP expr { Op(Op.Ne,$1,$3) }
| EXC LPAR expr COMMA expr RPAR
  { Exchange($3, $5, MO SC) }
| EXC_EXPLICIT LPAR expr COMMA expr COMMA MEMORDER RPAR
  { Exchange($3, $5, MO $7) }
| XCHG LBRACE annot_list RBRACE LPAR expr COMMA expr RPAR
  { Exchange($6,$8,AN $3) }
| ATOMIC_FETCH LPAR expr COMMA expr RPAR
  { Fetch ($3, $1, $5, SC) }
| ATOMIC_FETCH_EXPLICIT LPAR expr COMMA expr COMMA MEMORDER RPAR
  { Fetch($3, $1, $5, $7) }
| IDENTIFIER LPAR args RPAR
  { ECall ($1,$3) }

args:
| { [] }
| args_ne { $1 }

args_ne:
| expr { [$1] }
| expr COMMA args_ne { $1 :: $3 }

location:
| IDENTIFIER { LoadReg($1) }
| STAR location { LoadMem($2,AN []) }
| LPAR expr RPAR { $2 }

instruction:
| IF LPAR expr RPAR block_ins %prec LOWER_THAN_ELSE 
  { If($3,$5,None) }
| IF LPAR expr RPAR block_ins ELSE block_ins 
  { If($3,$5,Some $7) }
| initialisation SEMI
  { $1 }
| IDENTIFIER EQ expr SEMI
  { StoreReg($1,$3) }
| STAR location EQ expr SEMI
  { StoreMem($2,$4,AN []) }
| STORE LBRACE annot_list RBRACE LPAR expr COMMA expr RPAR SEMI
  { StoreMem($6,$8,AN $3) }
| ST LPAR expr COMMA expr RPAR SEMI
  { StoreMem($3, $5, MO SC) }
| ST_EXPLICIT LPAR expr COMMA expr COMMA MEMORDER RPAR SEMI
  { StoreMem($3, $5, MO $7) }

| LOCK LPAR expr RPAR SEMI
  { Lock ($3,MutexC11) }
| UNLOCK LPAR expr RPAR SEMI
  { Unlock ($3,MutexC11) }
| SPINLOCK LPAR expr RPAR SEMI
  { Lock ($3,MutexLinux) }
| SPINUNLOCK LPAR expr RPAR SEMI
  { Unlock ($3,MutexLinux) }
| UNDERFENCE LBRACE annot_list RBRACE SEMI
  { Fence(AN $3) }
| FENCE LPAR MEMORDER RPAR SEMI
  { Fence(MO $3) }
| CODEVAR SEMI
  { Symb $1 }
| IDENTIFIER LPAR args RPAR SEMI
  { PCall ($1,$3) }

ins_seq:
| block_ins { [$1] }
| block_ins ins_seq { $1::$2 }
| declaration { [] }
| declaration ins_seq { $2 }

block_ins:
| instruction { $1 }
| LBRACE ins_seq RBRACE { Seq($2) }

pseudo_seq:
| block_ins { [Instruction $1] }
| block_ins pseudo_seq { (Instruction $1)::$2 }
| declaration { [] }
| declaration pseudo_seq { $2 }

function_def:
| PROC LPAR parameter_list RPAR LBRACE pseudo_seq RBRACE
  { { CAst.proc = $1; 
      CAst.params = $3; 
      CAst.body = $6 } }

trans_unit:
| function_def
  { [$1] }
| trans_unit function_def 
  { $1 @ [$2] }

deep_main:
| trans_unit EOF { $1 }

formals_ne:
| IDENTIFIER { [ $1 ] }
| IDENTIFIER COMMA formals_ne { $1 :: $3 }

formals:
| { [] }
| formals_ne { $1 }

body:
| LBRACE ins_seq RBRACE
  { match $2 with
  | [i]  -> i
  | is -> Seq is }

macro:
| IDENTIFIER LPAR formals RPAR expr { EDef ($1,$3,$5) }
| IDENTIFIER LPAR formals RPAR body { PDef ($1,$3,$5) }
macros:
| { [] }
| macro macros { $1 :: $2 }

