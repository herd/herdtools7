/*********************************************************************/
/*                        Herd                                       */
/*                                                                   */
/* Luc Maranget, INRIA Paris-Rocquencourt, France.                   */
/* Jade Alglave, University College London, UK.                      */
/* John Wickerson, Imperial College London, UK.                      */
/*                                                                   */
/*  Copyright 2013 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/


%{
open AST

let mk_loc () =
  TxtLoc.make
    (Parsing.symbol_start_pos ())
    (Parsing.symbol_end_pos ())


let as_op op = function
  | Op (_,op0,es) when op0 = op -> es
  | e -> [e]

let do_op op e1 e2 =
  let es1 = as_op op e1
  and es2 = as_op op e2 in
  Op (mk_loc(),op,es1@es2)

let pp () =
  let open Lexing in
  let start = symbol_start_pos ()
  and fin = symbol_end () in
  let pos = start.pos_cnum in
  let len = fin - pos in
  {pos;len}

%}
%token EOF
%token <string> VAR
%token <string> TAG
%token <string> STRING
%token <string> LATEX
%token INCLUDE
%token LPAR RPAR LBRAC RBRAC BEGIN END LACC RACC
%token EMPTY UNDERSCORE
%token WITHCO WITHOUTCO WITHINIT WITHOUTINIT
%token WITHSC WITHOUTSC
/* Access direction */
%token MM  MR  MW WM WW WR RM RW RR INT EXT NOID SAMELOC
/* Plain/Atomic */
%token AA AP PA PP
%token ALT SEMI UNION INTER COMMA DIFF PLUSPLUS
%token STAR PLUS OPT INV COMP NOT HAT TWO
%token LET REC AND ACYCLIC IRREFLEXIVE TESTEMPTY EQUAL SHOW UNSHOW AS FUN IN PROCEDURE CALL FOREACH DO CHECKCALL FORORDER FROM
%token REQUIRES
%token ARROW
%token SEQTEST
%token ENUM DEBUG MATCH WITH
%type <AST.t> main
%start main

/* Precedences */
%left prec_app
%right UNION
%right PLUSPLUS
%right SEMI
%left DIFF
%right INTER
%nonassoc STAR PLUS OPT INV COMP NOT
%nonassoc HAT
%%

main:
| VAR options ins_list EOF { $2,$1,$3 }
| STRING options ins_list EOF { $2,$1,$3 }

options:
| WITHCO options { ModelOption.set_enumco true $2 }
| WITHOUTCO options { ModelOption.set_enumco false $2 }
| WITHINIT options { ModelOption.set_init true $2 }
| WITHOUTINIT options { ModelOption.set_init false $2 }
| WITHSC options { ModelOption.set_enumsc true $2 }
| WITHOUTSC options { ModelOption.set_enumsc false $2 }
|    { ModelOption.default }

ins_list:
| { [] }
| ins ins_list { $1 :: $2 }

ins:
| LET pat_bind_list { Let (mk_loc (),$2) }
| LET REC pat_bind_list { Rec (mk_loc (),$3) }
| deftest { $1 }
| SHOW exp AS VAR { ShowAs (mk_loc(),$2, $4) }
| SHOW var_list { Show (mk_loc(),$2) }
| UNSHOW var_list { UnShow (mk_loc(),$2) }
| LATEX { Latex (mk_loc(),$1) }
| INCLUDE STRING { Include (mk_loc(),$2) }
| PROCEDURE VAR LPAR formals RPAR EQUAL ins_list END
   { Procedure (mk_loc (),$2,$4,$7) }
| PROCEDURE VAR VAR EQUAL ins_list END
   { Procedure (mk_loc (),$2,[$3],$5) }
| CALL VAR LPAR fargs RPAR { Call (mk_loc (),$2,$4) }
| CALL VAR exp { Call (mk_loc (),$2,[$3]) }
| ENUM VAR EQUAL altopt alttags { Enum (mk_loc (),$2,$5) }
| DEBUG exp { Debug (mk_loc (),$2) }
| FOREACH VAR IN exp DO ins_list END
    { Foreach (mk_loc (),$2,$4,$6) }
| FORORDER VAR IN exp FROM exp optional_name
    { ForOrder (mk_loc (),$2,$4,$6,$7) }
altopt:
| ALT  { () }
|      { () }

alttags:
| TAG { [$1] }
| TAG ALT alttags { $1 :: $3 }

deftest:
| test_type test exp optional_name { Test(mk_loc(),pp (),$2,$3,$4,$1) }
| CHECKCALL VAR LPAR fargs RPAR optional_name { ProcedureTest(mk_loc(), $2,$4,$6)  }

test_type:
|          { Provides }
| REQUIRES { Requires }

optional_name:
|        { None }
| AS VAR { Some $2 }

test:
| ACYCLIC { Acyclic }
| IRREFLEXIVE { Irreflexive }
| TESTEMPTY { TestEmpty }

var_list:
| VAR { [$1] }
| VAR comma_opt var_list { $1 :: $3 }

comma_opt:
/* |       { () } */
| COMMA { () }

bind:
| VAR EQUAL exp { ($1,$3) }

pat_bind:
| bind { $1 }
| VAR VAR EQUAL exp
   { ($1,Fun (mk_loc(),[$2],$4,$1,ASTUtils.free_body [$2] $4)) }
| VAR LPAR formals RPAR EQUAL exp
   { ($1,Fun (mk_loc(),$3,$6,$1,ASTUtils.free_body $3 $6)) }

pat_bind_list:
| pat_bind { [$1] }
| pat_bind AND pat_bind_list { $1 :: $3 }


formals:
|          { [] }
| formalsN { $1 }

formalsN:
| VAR                { [$1] }
| VAR COMMA formalsN { $1 :: $3 }

exp:
| LET pat_bind_list IN exp { Bind (mk_loc(),$2,$4) }
| LET REC pat_bind_list IN exp { BindRec (mk_loc(),$3,$5) }
| FUN VAR ARROW exp
    { Fun (mk_loc(),[$2],$4,"*fun*",ASTUtils.free_body [$2] $4) }
| FUN LPAR formals RPAR ARROW exp
    { Fun (mk_loc(),$3,$6,"*fun*",ASTUtils.free_body $3 $6) }
| base { $1 }

simple:
| EMPTY { Konst (mk_loc(),Empty RLN) }
| TAG  { Tag (mk_loc (),$1) }
| LACC args RACC { ExplicitSet (mk_loc (),$2) }
| UNDERSCORE { Var (mk_loc (),"_") }
| LPAR exp RPAR { $2 }
| BEGIN exp END { $2 }

base:
| simple { $1 }
| select LPAR exp RPAR { Op1 (mk_loc(),$1,$3) }
| exp0 { $1 }
| base STAR base {Op (mk_loc(),Cartesian, [$1; $3])}
| LBRAC exp RBRAC {Op1(mk_loc(),Set_to_rln,$2)}
| base STAR { Op1(mk_loc(),Star,$1) }
| base PLUS { Op1(mk_loc(),Plus,$1) }
| base OPT { Op1(mk_loc(),Opt,$1) }
| base HAT INV { Op1(mk_loc(),Inv,$1) }
| base HAT TWO { Op1(mk_loc(),Square,$1) }
| base SEMI base { do_op Seq $1 $3 }
| base UNION base { do_op Union $1 $3 }
| base PLUSPLUS base { Op (mk_loc (), Add, [$1; $3]) }
| base DIFF base { Op (mk_loc (),Diff, [$1; $3;]) }
| base INTER base {  Op (mk_loc (),Inter, [$1; $3;]) }
| COMP base { Op1 (mk_loc(),Comp RLN, $2) }
| NOT base { Op1 (mk_loc(),Comp SET, $2) }
| MATCH exp WITH altopt clause_list END
    {
     let cls,d = $5 in
     Match (mk_loc(),$2,cls,d)
    }
| MATCH exp WITH altopt set_clauses END
    {
     let e,f = $5 in
     MatchSet (mk_loc (),$2,e,f)
   }

empty_clause:
| LACC RACC ARROW exp { $4 }

element_clause:
| VAR PLUSPLUS VAR ARROW exp { $1, $3, $5 }

set_clauses:
| empty_clause ALT element_clause { $1, $3 }
| element_clause ALT empty_clause { $3, $1 }

clause:
| TAG ARROW exp { $1,$3 }

clause_list:
| clause { [$1],None }
| UNDERSCORE ARROW exp { [],Some $3 }
| clause ALT clause_list
    {
     let cls,d = $3 in
     $1 :: cls, d
    }

exp0:
| VAR                 { Var (mk_loc (),$1) }
| exp0  arg %prec prec_app   { App (mk_loc (),$1,[$2]) }
| exp0 LPAR fargs RPAR { App (mk_loc(),$1,$3) }

arg:
| VAR { Var (mk_loc (),$1) }
| simple { $1 }


fargs:
| { [] }
| exp COMMA argsN { $1::$3 }

args:
| { [] }
| argsN { $1 }

argsN:
| exp            { [ $1 ] }
| exp COMMA argsN { $1 :: $3 }

select:
| MM { Select (WriteRead,WriteRead) }
| MW { Select (WriteRead,Write) }
| MR { Select (WriteRead,Read) }
| WM { Select (Write,WriteRead) }
| WW { Select (Write,Write) }
| WR { Select (Write,Read) }
| RM { Select (Read,WriteRead) }
| RW { Select (Read,Write) }
| RR { Select (Read,Read) }
/* Atomic/Plain */
| AA { Select (Atomic,Atomic) }
| AP { Select (Atomic,Plain) }
| PA { Select (Plain,Atomic) }
| PP { Select (Plain,Plain) }
/* additional filters */
| EXT { Ext }
| INT { Int }
| NOID { NoId }
| SAMELOC { SameLoc }
