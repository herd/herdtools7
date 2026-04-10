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
%token LEFT_SQUARE RIGHT_SQUARE
%token COMMA
%token CHOICE_BAR
%token EOF
%token OPTION
%start <string t> main

%%

main:
  | r = relax EOF { r }

(* Explicit precedence, from highest to lowest:
   - `suffix` handles postfix `?` and atomic terms such as `[..]`,
   - `choice` handles `|`,
   - `relax` handles `,`.
   For example, `A|B,C|[D,E]?` parses as `(A|B),(C|([D,E]?))`. *)
relax:
  | lhs = choice COMMA rhs = relax { mk_seq lhs rhs }
  | r = choice { r }

choice:
  | lhs = suffix CHOICE_BAR rhs = choice { mk_choice lhs rhs }
  | r = suffix { r }

suffix:
  | opt = suffix OPTION { Opt opt }
  | r = RELAXATION { One r }
  | LEFT_SQUARE r = relax RIGHT_SQUARE { Multi r }
