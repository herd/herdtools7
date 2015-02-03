/*********************************************************************/
/*                          Litmus                                   */
/*                                                                   */
/*     Jacques-Pascal Deplaix, INRIA Paris-Rocquencourt, France.     */
/*     John Wickerson, Imperial College London, UK.                  */
/*                                                                   */
/*  Copyright 2010 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

%{
open Constant
open OpenCLBase
open CType
%}

%token EOF
%token <string> IDENTIFIER
%token <string> ATOMIC_TYPE
%token <int> PROC
%token LPAR RPAR COMMA LBRACE RBRACE STAR 
%token UNSIGNED SIGNED ATOMIC LONG DOUBLE BOOL INT VOID FLOAT CHAR SHORT
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
%token SEMI COLON EQ EQ_OP NEQ_OP LEQ_OP DOT
%token XOR PIPE
%token LAND
%token ADD SUB
%token MUL DIV
%token WHILE IF ELSE
%nonassoc LOWER_THAN_ELSE /* This fixes the dangling-else problem */
%nonassoc ELSE
%token <OpenCLBase.mem_order> MEMORDER
%token <OpenCLBase.mem_scope> MEMSCOPE
%token <MemSpaceMap.gpu_memory_space> MEMREGION
%token GLOBAL LOCAL
%token SCOPETREE DEVICE KERNEL CTA WARP THREAD
%token LD LD_EXPLICIT ST ST_EXPLICIT EXC EXC_EXPLICIT FENCE LOCK UNLOCK BARRIER
%token SCAS WCAS SCAS_EXPLICIT WCAS_EXPLICIT
%token <Op.op> ATOMIC_FETCH
%token <Op.op> ATOMIC_FETCH_EXPLICIT
%type <(int * OpenCLBase.pseudo list) list * MiscParser.gpu_data option> deep_main 
%start deep_main
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
| GLOBAL base { Global $2 }
| LOCAL base { Local $2 }
| base { $1 }

base0:
| ATOMIC_TYPE { Atomic (Base $1) }
| ty_attr MUTEX { Base ($1 ^ "mutex") }
| ty_attr CHAR { Base ($1 ^ "char") }
| ty_attr INT { Base ($1 ^ "int") }
| ty_attr LONG { Base ($1 ^ "long") }
| ty_attr FLOAT { Base ($1 ^ "float") }
| ty_attr DOUBLE { Base ($1 ^ "double") }
| ty_attr LONG LONG { Base ($1 ^ "long long") }
| ty_attr LONG DOUBLE { Base ($1 ^ "long double") }
| BOOL { Base ("_Bool") }

base:
| base0 { $1 }
| LPAR typ RPAR { $2 }

ty_attr:
| { "" }
| UNSIGNED { "unsigned " }
| SIGNED { "signed " }

shallow_main:
| SCOPETREE scope_tree EOF { [] } /* when doing shallow-parse, ignore scope_tree for now. */
| BODY shallow_main { CAst.Global $1 :: $2 }
| PROC LPAR parameter_list RPAR BODY shallow_main
    { CAst.Test {CAst.proc = $1; params = $3; body = $5} :: $6 }

primary_expression:
| IDENTIFIER
  { Eregister $1 }
| CONSTANT 
  { Econstant (Concrete $1) }
| NULL
  { Econstant (Concrete 0) }
| LPAR expression RPAR 
  { Eparen $2 }

postfix_expression:
| primary_expression 
  { $1 }
| ST LPAR assignment_expression COMMA assignment_expression RPAR
  { Estore ($3, $5, OpenCLBase.SC, OpenCLBase.S_all_svm_devices) }
| ST_EXPLICIT LPAR assignment_expression COMMA assignment_expression COMMA MEMORDER RPAR
  { Estore ($3, $5, $7, OpenCLBase.S_all_svm_devices) }
| ST_EXPLICIT LPAR assignment_expression COMMA assignment_expression COMMA MEMORDER COMMA MEMSCOPE RPAR
  { Estore ($3, $5, $7, $9) }
| EXC LPAR assignment_expression COMMA assignment_expression RPAR
  { Eexchange ($3, $5, OpenCLBase.SC, OpenCLBase.S_all_svm_devices) }
| EXC_EXPLICIT LPAR assignment_expression COMMA assignment_expression COMMA MEMORDER RPAR
  { Eexchange ($3, $5, $7, OpenCLBase.S_all_svm_devices) }
| EXC_EXPLICIT LPAR assignment_expression COMMA assignment_expression COMMA MEMORDER COMMA MEMSCOPE RPAR
  { Eexchange ($3, $5, $7, $9) }
| ATOMIC_FETCH LPAR assignment_expression COMMA assignment_expression RPAR
  { Efetch ($1, $3, $5, OpenCLBase.SC, OpenCLBase.S_all_svm_devices) }
| ATOMIC_FETCH_EXPLICIT LPAR assignment_expression COMMA assignment_expression COMMA MEMORDER RPAR
  { Efetch ($1, $3, $5, $7, OpenCLBase.S_all_svm_devices) }
| ATOMIC_FETCH_EXPLICIT LPAR assignment_expression COMMA assignment_expression COMMA MEMORDER COMMA MEMSCOPE RPAR
  { Efetch ($1, $3, $5, $7, $9) }
| LD LPAR assignment_expression RPAR
  { Eload ($3, OpenCLBase.SC, OpenCLBase.S_all_svm_devices) }
| LD_EXPLICIT LPAR assignment_expression COMMA MEMORDER RPAR
  { Eload ($3, $5, OpenCLBase.S_all_svm_devices) }
| LD_EXPLICIT LPAR assignment_expression COMMA MEMORDER COMMA MEMSCOPE RPAR
  { Eload ($3, $5, $7) }
| FENCE LPAR fence_flags COMMA MEMORDER COMMA MEMSCOPE RPAR
  { Efence ($3,$5,$7) }
| WCAS LPAR assignment_expression COMMA assignment_expression COMMA assignment_expression RPAR
  { Ecas ($3,$5,$7,OpenCLBase.SC,OpenCLBase.SC,OpenCLBase.S_all_svm_devices,false) }
| SCAS LPAR assignment_expression COMMA  assignment_expression COMMA assignment_expression RPAR
  { Ecas ($3,$5,$7,OpenCLBase.SC,OpenCLBase.SC,OpenCLBase.S_all_svm_devices,true) }
| WCAS_EXPLICIT LPAR assignment_expression COMMA assignment_expression COMMA assignment_expression COMMA MEMORDER COMMA MEMORDER RPAR
  { Ecas ($3,$5,$7,$9,$11,OpenCLBase.S_all_svm_devices,false) }
| SCAS_EXPLICIT LPAR assignment_expression COMMA  assignment_expression COMMA assignment_expression COMMA MEMORDER COMMA MEMORDER RPAR
  { Ecas ($3,$5,$7,$9,$11,OpenCLBase.S_all_svm_devices,true) }
| WCAS_EXPLICIT LPAR assignment_expression COMMA assignment_expression COMMA assignment_expression COMMA MEMORDER COMMA MEMORDER COMMA MEMSCOPE RPAR
  { Ecas ($3,$5,$7,$9,$11,$13,false) }
| SCAS_EXPLICIT LPAR assignment_expression COMMA  assignment_expression COMMA assignment_expression COMMA MEMORDER COMMA MEMORDER COMMA MEMSCOPE RPAR
  { Ecas ($3,$5,$7,$9,$11,$13,true) }

unary_expression:
| postfix_expression 
  { $1 }
| STAR unary_expression
  { Eload ($2, OpenCLBase.NA, OpenCLBase.S_workitem) }

cast_expression:
| unary_expression { $1 }

multiplicative_expression:
| cast_expression { $1 }
| multiplicative_expression STAR cast_expression
   { Eop (Op.Mul,$1,$3) }
| multiplicative_expression DIV cast_expression
   { Eop (Op.Div,$1,$3) }

additive_expression:
| multiplicative_expression { $1 }
| additive_expression ADD multiplicative_expression
  { Eop (Op.Add,$1,$3) }
| additive_expression SUB multiplicative_expression
  { Eop (Op.Sub,$1,$3) }

shift_expression:
| additive_expression { $1 }

relational_expression:
| shift_expression { $1 }

equality_expression:
| relational_expression 
  { $1 }
| equality_expression EQ_OP relational_expression 
  { Eop (Op.Eq,$1,$3) }
| equality_expression NEQ_OP relational_expression 
  { Eop (Op.Ne,$1,$3) }
| equality_expression LEQ_OP relational_expression 
  { Eop (Op.Le,$1,$3) }

and_expression:
| equality_expression { $1 }
| and_expression LAND  equality_expression
  { Eop (Op.And,$1,$3) }

exclusive_or_expression:
| and_expression { $1 }
| exclusive_or_expression XOR  and_expression
  { Eop (Op.Xor,$1,$3) }

inclusive_or_expression: 
| exclusive_or_expression { $1 }
| inclusive_or_expression PIPE exclusive_or_expression
  { Eop (Op.Or,$1,$3) }

logical_and_expression: 
| inclusive_or_expression { $1 }

logical_or_expression:
| logical_and_expression { $1 }

conditional_expression:
| logical_or_expression { $1 }

assignment_expression:
| conditional_expression 
  { $1 }
| IDENTIFIER assignment_operator assignment_expression
  { Eassign ($1, $3) }
| STAR IDENTIFIER assignment_operator assignment_expression
  { Estore (Eregister $2, $4, OpenCLBase.NA, OpenCLBase.S_workitem) }

assignment_operator:
| EQ { () }

expression:
| assignment_expression { $1 }
| expression COMMA assignment_expression { Ecomma ($1,$3) }

declaration:
| typ  init_declarator SEMI  { $2; }

init_declarator:
| IDENTIFIER
  { Pblock [] }
| IDENTIFIER EQ initialiser 
  { Pexpr (Eassign ($1, $3)) }

initialiser:
| assignment_expression 
  { $1 }

statement:
| declaration /* (* Added to allow mid-block declarations *) */
  { $1 }
| IDENTIFIER COLON BARRIER LPAR fence_flags RPAR SEMI
  { Pbarrier ($1,$5,OpenCLBase.S_workgroup) }
| IDENTIFIER COLON BARRIER LPAR fence_flags COMMA MEMSCOPE RPAR SEMI
  { Pbarrier ($1,$5,$7) }
| compound_statement
  { Pblock $1 }
| expression_statement
  { $1 }
| selection_statement
  { $1 }
| iteration_statement
  { $1 }

fence_flags:
| MEMREGION { [$1] }
| MEMREGION PIPE fence_flags { $1 :: $3 }

compound_statement:
| LBRACE RBRACE
  { [] }
| LBRACE statement_list RBRACE
  { $2 }

statement_list:
| statement
  { [$1] }
| statement statement_list
  { $1 :: $2 }

expression_statement:
| SEMI
  { Pblock [] }
| expression SEMI
  { Pexpr $1 }

selection_statement:
| IF LPAR expression RPAR statement %prec LOWER_THAN_ELSE
  { Pif ($3, $5, Pblock []) }
| IF LPAR expression RPAR statement ELSE statement
  { Pif ($3, $5, $7) }

iteration_statement:
| WHILE LPAR expression RPAR statement
  { Pwhile($3,$5) }

function_definition:
| PROC LPAR parameter_list RPAR compound_statement
  { { CAst.proc = $1; 
      CAst.params = $3; 
      CAst.body = List.map (fun ins -> Instruction ins) $5 } }

translation_unit:
| function_definition
  { [$1] }
| translation_unit function_definition 
  { $1 @ [$2] }

deep_main:
| translation_unit SCOPETREE scope_tree EOF 
  { let proc_list,param_map = 
      List.fold_right (fun p (proc_list, param_map) -> 
        let proc_list = (p.CAst.proc,p.CAst.body) :: proc_list in
        let param_map = p.CAst.params :: param_map in
        (proc_list, param_map)) $1 ([], [])  
    in
    let additional = 
      { MiscParser.empty_gpu with 
        MiscParser.scope_tree=Some $3; 
        MiscParser.param_map = param_map; } 
    in
    (proc_list, Some additional) }


/* 
   Parsing a simple S expression that needs to have a certain value.
*/

scope_tree :
|  device_list {$1}

device_list :
|  device { [$1] }
|  device device_list { [$1]@$2 }

device:
|  LPAR DEVICE kernel_list RPAR {$3}

kernel_list :
|  kernel { [$1] }
|  kernel kernel_list { [$1]@$2 }
|  cta_list {List.map (fun x -> [x]) $1}

kernel:
|  LPAR KERNEL cta_list RPAR {$3}

cta_list:
|  cta { [$1] }
|  cta cta_list { [$1]@$2 }
|  warp_list {List.map (fun x -> [x]) $1}

cta:
| LPAR CTA warp_list RPAR {$3}

warp_list:
|  warp { [$1] }
|  warp warp_list { [$1]@$2 }
|  thread_list {List.map (fun x -> [x]) $1}

warp:
| LPAR WARP thread_list RPAR { $3 }

thread_list:
|  thread { [$1] }
|  thread thread_list { [$1]@$2 }

thread:
| PROC {$1}
