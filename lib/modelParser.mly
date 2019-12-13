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
  let start = Parsing.symbol_start_pos ()
  and fin = Parsing.symbol_end () in
  let pos = start.Lexing.pos_cnum in
  let len = fin - pos in
  Pos {pos;len}

let tuple_pat = function
  | [p] -> Pvar p
  | ps  -> Ptuple ps
%}

%token EOF
%token <string> VAR
%token <string> TAG
%token <string> STRING
%token INCLUDE
%token LPAR RPAR BEGIN END LACC RACC LBRAC RBRAC
%token EMPTY UNDERSCORE SUBSET
%token ALT SEMI UNION INTER COMMA DIFF PLUSPLUS
%token STAR PLUS OPT INV COMP HAT
%token LET REC AND WHEN ACYCLIC IRREFLEXIVE TESTEMPTY EQUAL
%token SHOW UNSHOW AS FUN IN PROCEDURE CALL FORALL DO FROM
%token TRY INSTRUCTIONS DEFAULT IF THEN ELSE
%token REQUIRES FLAG
%token ARROW
%token ENUM DEBUG MATCH WITH
%type <AST.t> main
%start main

/* Precedences */
%right UNION
%right PLUSPLUS
%right SEMI
%left DIFF
%right INTER
%nonassoc STAR PLUS OPT COMP
%nonassoc HAT
%%

main:
| identity topins_list EOF
 {
  let a,id = $1 in
   ModelOption.set_arch a ModelOption.default, id, $2 }

identity:
| VAR VAR { $1,$2 }
| VAR STRING { $1,$2 }
| VAR { $1,$1 }
| STRING  { $1,$1 }
|   { "None","Unknown" }


topins_list:
| { [] }
| topins topins_list { $1 :: $2 }

ins_list:
| { [] }
| ins ins_list { $1 :: $2 }

in_opt:
|    { () }
| IN { () }

ins_clause:
| TAG ARROW ins_list { $1,$3 }

ins_clause_list:
| ins_clause { [$1],None }
| UNDERSCORE ARROW ins_list { [],Some $3 }
| ins_clause ALT ins_clause_list
    {
     let cls,d = $3 in
     $1 :: cls, d
    }


topins:
| ENUM VAR EQUAL altopt alttags { Enum (mk_loc (),$2,$5) }
| ins { $1 }

pat0:
| UNDERSCORE { None }
| VAR        { Some $1 }

ins:
| LET pat_bind_list in_opt { Let (mk_loc (),$2) }
| LET REC pat_bind_list  in_opt { Rec (mk_loc (),$3,None) }
| LET REC pat_bind_list WHEN app_test in_opt { Rec (mk_loc (),$3,Some $5) }
| MATCH exp WITH altopt ins_clause_list END
    {
     let cls,d = $5 in
     InsMatch (mk_loc(),$2,cls,d)
    }
| deftest { $1 }
| SHOW base AS VAR { ShowAs (mk_loc(),$2, $4) }
| SHOW var_list { Show (mk_loc(),$2) }
| UNSHOW var_list { UnShow (mk_loc(),$2) }
| INCLUDE STRING { Include (mk_loc(),$2) }
| PROCEDURE VAR LPAR formals RPAR EQUAL ins_list END
   { Procedure (mk_loc (),$2,tuple_pat $4,$7,IsNotRec) }
| PROCEDURE VAR pat0 EQUAL ins_list END
   { Procedure (mk_loc (),$2,Pvar $3,$5,IsNotRec) }
| PROCEDURE REC VAR LPAR formals RPAR EQUAL ins_list END
   { Procedure (mk_loc (),$3,tuple_pat $5,$8,IsRec) }
| PROCEDURE REC VAR pat0 EQUAL ins_list END
   { Procedure (mk_loc (),$3,Pvar $4,$6,IsRec) }

| CALL VAR simple optional_name { Call (mk_loc (),$2,$3,$4) }
| DEBUG exp { Debug (mk_loc (),$2) }
| FORALL VAR IN exp DO ins_list END
    { Forall (mk_loc (),$2,$4,$6) }
| WITH VAR FROM exp
    { WithFrom (mk_loc (),$2,$4) }


//Bell file declarations
| INSTRUCTIONS VAR LBRAC args RBRAC  {Events(mk_loc(),$2,$4,false)}
| DEFAULT VAR LBRAC args RBRAC  {Events(mk_loc(),$2,$4,true)}


altopt:
| ALT  { () }
|      { () }

alttags:
| TAG { [$1] }
| TAG ALT alttags { $1 :: $3 }

deftest:
| test_type app_test { Test ($2,$1) }


app_test:
| test exp optional_name { (mk_loc(),pp (),$1,$2,$3) }

test_type:
|          { Check }
| REQUIRES { UndefinedUnless }
| FLAG     { Flagged }

optional_name:
|        { None }
| AS VAR { Some $2 }

do_test:
| ACYCLIC { Acyclic }
| IRREFLEXIVE { Irreflexive }
| TESTEMPTY { TestEmpty }

test:
| do_test { Yes $1 }
| COMP do_test { No $2}

var_list:
| VAR { [$1] }
| VAR COMMA var_list { $1 :: $3 }

bind:
| LPAR formals RPAR EQUAL exp { (mk_loc (),tuple_pat $2,$5) }
| formalsN EQUAL exp { (mk_loc (),tuple_pat $1,$3) }

pat_bind:
| bind { $1 }
| VAR pat0 EQUAL exp
   { (mk_loc (),Pvar (Some $1),Fun (mk_loc(),Pvar $2,$4,$1,ASTUtils.free_body [$2] $4)) }
| VAR LPAR formals RPAR EQUAL exp
   { (mk_loc(),Pvar (Some $1),Fun (mk_loc(),tuple_pat $3,$6,$1,ASTUtils.free_body $3 $6)) }

pat_bind_list:
| pat_bind { [$1] }
| pat_bind AND pat_bind_list { $1 :: $3 }


formals:
|          { [] }
| formalsN { $1 }

formalsN:
| pat0                { [$1] }
| pat0 COMMA formalsN { $1 :: $3 }

exp:
| LET pat_bind_list IN exp { Bind (mk_loc(),$2,$4) }
| LET REC pat_bind_list IN exp { BindRec (mk_loc(),$3,$5) }
| FUN pat0 ARROW exp
    { Fun (mk_loc(),Pvar $2,$4,"*fun*",ASTUtils.free_body [$2] $4) }
| FUN LPAR formals RPAR ARROW exp
    { Fun (mk_loc(),tuple_pat $3,$6,"*fun*",ASTUtils.free_body $3 $6) }
| TRY exp WITH exp
    { Try (mk_loc(),$2,$4) }
| IF cond THEN exp ELSE exp
    { If (mk_loc(),$2,$4,$6) }
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
| baseortuple { $1 }

cond:
| exp EQUAL exp  { Eq ($1,$3) }
| exp SUBSET exp { Subset ($1,$3) }
| exp IN exp     { In ($1,$3) }

simple:
| EMPTY { Konst (mk_loc(),Empty RLN) }
| TAG  { Tag (mk_loc (),$1) }
| LACC args RACC { ExplicitSet (mk_loc (),$2) }
| UNDERSCORE  { Konst (mk_loc(),Universe SET) }
| LPAR RPAR { Op (mk_loc (),Tuple,[]) }
| LPAR exp RPAR { $2 }
| BEGIN exp END { $2 }
| LBRAC exp RBRAC { Op1 (mk_loc(),ToId,$2) }

tupleargs:
| base COMMA tupleend { $1 :: $3 }

tupleend:
| base { [$1] }
| base COMMA tupleend { $1 :: $3 }

base:
| simple { $1 }
| exp0 { $1 }
| base STAR base {Op (mk_loc(),Cartesian, [$1; $3])}
| base STAR { Op1(mk_loc(),Star,$1) }
| base PLUS { Op1(mk_loc(),Plus,$1) }
| base OPT { Op1(mk_loc(),Opt,$1) }
| base HAT INV { Op1(mk_loc(),Inv,$1) }
| base SEMI base { do_op Seq $1 $3 }
| base UNION base { do_op Union $1 $3 }
| base PLUSPLUS base { Op (mk_loc (), Add, [$1; $3]) }
| base DIFF base { Op (mk_loc (),Diff, [$1; $3;]) }
| base INTER base {  Op (mk_loc (),Inter, [$1; $3;]) }
| COMP base { Op1 (mk_loc(),Comp, $2) }

baseortuple:
| base { $1 }
| tupleargs { Op (mk_loc (),Tuple,$1) }

empty_clause:
| LACC RACC ARROW exp { $4 }

element_clause2:
| pat0 PLUSPLUS pat0 ARROW exp { EltRem ($1, $3, $5) }

element_clause3:
| pat0 UNION pat0 PLUSPLUS pat0 ARROW exp { PreEltPost ($1,$3,$5,$7) }

element_clause:
| element_clause2 { $1 }
| element_clause3 { $1 }

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
| VAR          { Var (mk_loc (),$1) }
| exp0  arg    { App (mk_loc (),$1,$2) }

arg:
| VAR { Var (mk_loc (),$1) }
| simple { $1 }


args:
| { [] }
| argsN { $1 }

argsN:
| base            { [ $1 ] }
| base COMMA argsN { $1 :: $3 }
