%token <string> RELAXATION
%token LEFT_SQUARE RIGHT_SQUARE
%token COMMA
%token CHOICE_BAR
%token EOF
%token OPTION
%start <string Ast.t> main

%%

main:
  | r = relax EOF { r }

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
