%token <string> PREDICATE
%token LEFT_BRACKET RIGHT_BRACKET
%token <string> RELAXATION
%token LEFT_SQUARE RIGHT_SQUARE
%token COMMA
%token CHOICE_BAR
%token EOF
%token OPTION
%start <(string,string) Ast.t> main
%start <(string,string) Ast.t> diycross7
%start <(string,string) Ast.t> diy7
%start <(string,string) Ast.t> cumul

%%

main:
  | r = relax EOF { r }

  (* In `diycross7`, the top level `,` and whitespace should be treated as `Choice`
     rather than the default `Seq` *)
diycross7:
  | r = choice EOF { r }
  | lhs = choice COMMA rhs = diycross7 { Ast.Choice [lhs ; rhs] }

(* Explicit precedence, from highest to lowest:
   - `suffix` handles postfix `?` and atomic terms such as `[..]`,
   - `choice` handles `|`,
   - `relax` handles `,`.
   For example, `A|B,C|[D,E]?` parses as `(A|B),(C|([D,E]?))`. *)
relax:
  | lhs = choice COMMA rhs = relax { Ast.Seq [lhs ; rhs] }
  | r = choice { r }

choice:
  | lhs = suffix CHOICE_BAR rhs = choice { Ast.Choice [lhs ; rhs] }
  | r = suffix { r }

suffix:
  | opt = suffix OPTION { Ast.Opt opt }
  | r = RELAXATION { Ast.One r }
  | LEFT_SQUARE r = relax RIGHT_SQUARE { r }

(* In `diy7`, top level `,` and whitespace are `Choice`, and `@predicate(...)`
   may decorate a relaxation expression. *)
diy7:
  | r = diy7_choice EOF { r }
  | lhs = diy7_choice COMMA rhs = diy7 { Ast.Choice [lhs ; rhs] }

diy7_relax:
  | lhs = diy7_choice COMMA rhs = diy7_relax { Ast.Seq [lhs ; rhs] }
  | r = diy7_choice { r }

diy7_choice:
  | lhs = diy7_suffix CHOICE_BAR rhs = diy7_choice { Ast.Choice [lhs ; rhs] }
  | r = diy7_suffix { r }

diy7_suffix:
  | opt = diy7_suffix OPTION { Ast.Opt opt }
  | r = RELAXATION { Ast.One r }
  | LEFT_SQUARE r = diy7_relax RIGHT_SQUARE { r }
  | pred = PREDICATE LEFT_BRACKET r = diy7_relax RIGHT_BRACKET { Ast.Predicate (pred,r) }


(* In `diy7 -cumul`,
   ALL `,` and whitespace should be treated as `Choice`
   rather than the default `Seq` *)
cumul:
  | r = cumul_relax EOF { r }

cumul_relax:
  | lhs = cumul_choice COMMA rhs = cumul_relax { Ast.Choice [lhs ; rhs] }
  | r = cumul_choice { r }

cumul_choice:
  | lhs = cumul_suffix CHOICE_BAR rhs = cumul_choice { Ast.Choice [lhs ; rhs] }
  | r = cumul_suffix { r }

cumul_suffix:
  | opt = cumul_suffix OPTION { Ast.Opt opt }
  | r = RELAXATION { Ast.One r }
  | LEFT_SQUARE r = cumul_relax RIGHT_SQUARE { r }
