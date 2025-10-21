{
open SpecParser (* The type token is defined in SpecParser.mli *)

exception Error of string

let count_new_lines str =
  let count = ref 0 in
  String.iter (fun c -> if c = '\n' then incr count) str;
  !count
}

let identifier = ['a'-'z' 'A'-'Z' '_' ] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let latex_macro = ['\\'] ['a'-'z' 'A'-'Z']+

rule token = parse
    (* Whitespace and Comments *)
    | '\n'                     { Lexing.new_line lexbuf; token lexbuf}
    | "\r\n"                   { Lexing.new_line lexbuf; token lexbuf}
    | [' ''\t''\r']+           { token lexbuf }
    | "//" [^'\n']*            { token lexbuf }
    | "/*"                     { c_comments lexbuf }

    (* Strings *)
    | '"' ([^'"']* as s) '"'   {  (* Count the number of newline characters in the string.
                                     This doesn't account for escape sequences (TODO). *)
                                  let new_lines =  count_new_lines s in
                                  for _i = 0 to (new_lines - 1) do
                                    Lexing.new_line lexbuf
                                  done;
                                  STRING(s)
                               }

    (* Keywords *)
    | "ast"               { AST }
    | "case"              { CASE }
    | "constant"          { CONSTANT }
    | "constants_set"     { CONSTANTS_SET }
    | "function"          { FUNCTION }
    | "INDEX"             { INDEX }
    | "latex"             { LATEX }
    | "list0"             { LIST0 }
    | "list1"             { LIST1 }
    | "math_macro"        { MATH_MACRO }
    | "math_layout"       { MATH_LAYOUT }
    | "option"            { OPTION }
    | "fun"               { FUN }
    | "partial"           { PARTIAL }
    | "powerset"          { POWERSET }
    | "powerset_finite"   { POWERSET_FINITE }
    | "prose_application" { PROSE_APPLICATION }
    | "prose_description" { PROSE_DESCRIPTION }
    | "relation"          { RELATION }
    | "render"            { RENDER }
    | "rule"              { RULE }
    | "semantics"         { SEMANTICS }
    | "typedef"           { TYPEDEF }
    | "typing"            { TYPING }
    | "UNION"             { UNION }
    | "IFF"               { IFF }
    | "LIST"              { LIST }
    | "SIZE"              { SIZE }
    | "SOME"              { SOME }

    (* Punctuation and operators *)
    | '.'            { DOT }
    | ','            { COMMA }
    | ':'            { COLON }
    | ';'            { SEMI }
    | '|'            { VDASH }
    | "=:"           { EQ }
    | '='            { EQ }
    | '('            { LPAR }
    | ')'            { RPAR }
    | '['            { LBRACKET }
    | ']'            { RBRACKET }
    | '{'            { LBRACE }
    | '}'            { RBRACE }
    | '-'            { MINUS }
    | "->"           { ARROW }
    | "--"           { MINUS_MINUS }
    | ":="           { COLON_EQ }

    | identifier as lxm { IDENTIFIER(lxm) }
    | latex_macro as lxm { LATEX_MACRO(lxm) }

    | eof            { EOF }
    | _ {
      let lxm = Lexing.lexeme lexbuf in
      let msg = Format.sprintf "unrecognized token %s" lxm in raise (Error msg) }

(* C-style comments *)
and c_comments = parse
  | "*/"          { token      lexbuf }
  | '*'           { c_comments lexbuf }
  | '\n'          { Lexing.new_line lexbuf; c_comments lexbuf}
  | "\r\n"        { Lexing.new_line lexbuf; c_comments lexbuf}
  | [^ '*' '\n']+ { c_comments lexbuf }
  | ""            { raise (Error "unterminated string") }
