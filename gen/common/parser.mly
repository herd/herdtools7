%{
open Ast

let mk_seq lhs rhs =
  match lhs,rhs with
  | Seq lhs, Seq rhs -> Seq (lhs @ rhs)
  | lhs, Seq rhs -> Seq (lhs :: rhs)
  | Seq lhs, rhs -> Seq (lhs @ [rhs])
  | lhs, rhs -> Seq [lhs ; rhs]

let mk_choice lhs rhs =
  match lhs,rhs with
  | Choice lhs, Choice rhs -> Choice (lhs @ rhs)
  | lhs, Choice rhs -> Choice (lhs :: rhs)
  | Choice lhs, rhs -> Choice (lhs @ [rhs])
  | lhs, rhs -> Choice [lhs ; rhs]

%}

%token <string> RELAXATION
%token LEFT_SQUIRE RIGHT_SQUIRE
%token COMMA
%token CHOICE_BAR
%right COMMA
%right CHOICE_BAR
%token EOF
%start <t> main

%%

main:
  | r = relax EOF { r }
  | r = relax COMMA EOF { r }

relax:
  | r = RELAXATION { One r }
  | lhs = relax CHOICE_BAR rhs = relax { mk_choice lhs rhs }
  | lhs = relax COMMA rhs = relax { mk_seq lhs rhs }
  | LEFT_SQUIRE r = relax RIGHT_SQUIRE { Multi r }
