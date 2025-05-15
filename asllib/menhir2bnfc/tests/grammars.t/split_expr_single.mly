(* Taken from https://gitlab.inria.fr/fpottier/menhir/-/blob/master/demos/calc/parser.mly *)

%token <int> INT
%token IF THEN ELSE ELSEIF
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%nonassoc ELSE
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start <int> main
%type  <int> expr
%type  <int> e_else

%%

main:
| e = expr EOL
    { e }

e_else:
| ELSEIF [@internal:true] e1 = expr THEN e2 = expr e3 = e_else
  { if e1 then e2 else e3 }
| ELSE e1 = expr
  { e1 }

expr:
| i = INT
    { i }
| LPAREN e = expr RPAREN
    { e }
| IF e1 = expr THEN e2 = expr e3 = e_else
    { if e1 then e2 else e3 }
| e1 = expr PLUS e2 = expr
    { e1 + e2 }
| e1 = expr MINUS e2 = expr
    { e1 - e2 }
| e1 = expr TIMES e2 = expr
    { e1 * e2 }
| e1 = expr DIV e2 = expr
    { e1 / e2 }
| MINUS e = expr %prec UMINUS
    { - e }
