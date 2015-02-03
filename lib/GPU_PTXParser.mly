/*********************************************************************/
/*                        Memevents                                  */
/*                                                                   */
/* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     */
/* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         */
/* John Wickerson, Imperial College London, UK.                      */
/*                                                                   */
/*  Copyright 2010 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

%{
module GPU_PTX = GPU_PTXBase
open GPU_PTX
%}

%token EOF
%token <GPU_PTXBase.reg> ARCH_REG
%token <GPU_PTXBase.op_type> OP_TYPE
%token <GPU_PTXBase.state_space> STATE_SPACE
%token <GPU_PTXBase.cache_op> CACHE_OP
%token <Op.op> CMP_OP
%token <GPU_PTXBase.bar_scope> BARRIER_SCOPE
%token <int> NUM
%token <string> NAME
%token <int> PROC

%token SEMI COMMA PIPE COLON LPAR RPAR LBRAC RBRAC LBRACE RBRACE AMPERSAT BANG

%token <int> CRK

/* Instruction tokens */
%token ST LD MEMBAR MOV ADD AND CVT VOL SETP BRA ATOM ATOM_EXCH ATOM_ADD ATOM_CAS

%type <int list * (GPU_PTXBase.pseudo) list list * MiscParser.gpu_data option> main 
%start  main

%nonassoc SEMI

%token SCOPETREE GLOBAL SHARED DEVICE KERNEL CTA WARP THREAD COMMA PTX_REG_DEC 

%type <MiscParser.gpu_data> scopes_and_memory_map

%%

main:
| semi_opt proc_list iol_list scopes_and_memory_map EOF { $2,$3,Some $4 }

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
  | AMPERSAT reg instr
    { Pguard ($2,$3) }

  | AMPERSAT BANG reg instr
    { Pguardnot ($3,$4) }

  | BRA NAME
    { Pjmp $2 }

  | SETP CMP_OP OP_TYPE reg COMMA ins_op COMMA ins_op
    { Psetp ($2, $4, $6, $8, $3) }

  | ST prefix cop OP_TYPE LBRAC reg RBRAC COMMA reg
    { Pst ($6, $9, $2, $3, $4) }

  | LD prefix cop OP_TYPE reg COMMA LBRAC reg RBRAC
    { Pld ($5, $8, $2, $3, $4) }

  | ST VOL prefix OP_TYPE LBRAC reg RBRAC COMMA reg
    { Pstvol ($6, $9, $3, $4) }

  | LD VOL prefix OP_TYPE reg COMMA LBRAC reg RBRAC
    { Pldvol ($5, $8, $3, $4) }

  | MOV OP_TYPE reg COMMA ins_op
    { Pmov ($3,$5,$2) }

  | ADD OP_TYPE reg COMMA ins_op COMMA ins_op
    { Padd ($3,$5,$7,$2) }

  | AND OP_TYPE reg COMMA ins_op COMMA ins_op
    { Pand ($3,$5,$7,$2) }

  | CVT OP_TYPE OP_TYPE reg COMMA reg
    { Pcvt ($4,$6,$2,$3) }

  | MEMBAR BARRIER_SCOPE
    { Pmembar ($2) }
      
  | ATOM prefix atom_op OP_TYPE reg COMMA LBRAC reg RBRAC COMMA ins_op
    {Patom2op($5,$8,$11,$2,$3,$4) }

  | ATOM prefix atom_op OP_TYPE reg COMMA LBRAC reg RBRAC COMMA ins_op COMMA ins_op
    {Patom3op($5,$8,$11,$13,$2,$3,$4) }


atom_op:
| ATOM_ADD { GPU_PTXBase.Atom_add}
| ATOM_EXCH { GPU_PTXBase.Atom_exch}
| ATOM_CAS { GPU_PTXBase.Atom_cas}
// Implement the rest of the atomic ops as needed

ins_op:
| reg {Reg $1}
| NUM {Im $1}

prefix:
|             { NOMP }
| STATE_SPACE { $1 }

cop:
|          { NCOP }
| CACHE_OP { $1 }
 
reg:
| ARCH_REG { $1 }

/* 
   Parsing a simple S expression that needs to have a certain value.
*/

scopes_and_memory_map : 
| SCOPETREE scope_tree memory_map 
   { { MiscParser.scope_tree=Some $2; mem_space_map=$3; param_map = [];}}

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

memory_map:
| memory_map_list { $1 }
|                 { [] }

memory_map_list:
| memory_map_atom { [$1] }
| memory_map_atom COMMA memory_map_list { [$1]@$3 }

memory_map_atom:
| NAME COLON GLOBAL { ($1,GPU_PTXBase.GlobalMem) }
| NAME COLON SHARED { ($1,GPU_PTXBase.LocalMem) }
