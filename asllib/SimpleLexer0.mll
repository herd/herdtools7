{
open Parser0       (* The type token is defined in parser.mli *)

exception LexerError

let names_to_tokens = [
  ( "AArch32", QUALIFIER "AArch32");
  ( "AArch64", QUALIFIER "AArch64");
  ( "AND", AND );
  ( "CONSTRAINED_UNPREDICTABLE", CONSTRAINED_UNPRED );
  ( "DIV", DIV );
  ( "EOR", EOR );
  ( "IMPLEMENTATION_DEFINED", IMPLEM_DEFINED );
  ( "IN", IN );
  ( "IFF", IFF );
  ( "IMPLIES", IMPLIES );
  ( "MOD", MOD );
  ( "NOT", NOT );
  ( "OR", OR );
  ( "QUOT", QUOT );
  ( "REM", REM );
  ( "SEE", SEE );
  ( "UNDEFINED", UNDEFINED );
  ( "UNKNOWN", UNKNOWN );
  ( "UNPREDICTABLE", UNPREDICTABLE );
  ( "__ExceptionTaken", UU_EXCEPTIONTAKEN );
  ( "__NOP", UU_NOP );
  ( "__UNALLOCATED", UU_UNALLOCATED );
  ( "__UNPREDICTABLE", UU_UNPREDICTABLE );
  ( "__array", UU_ARRAY );
  ( "__builtin", UU_BUILTIN );
  ( "__conditional", UU_CONDITIONAL );
  ( "__config", UU_CONFIG );
  ( "__decode", UU_DECODE );
  ( "__encoding", UU_ENCODING );
  ( "__event", UU_EVENT );
  ( "__execute", UU_EXECUTE );
  ( "__field", UU_FIELD );
  ( "__function", UU_FUNCTION );
  ( "__guard", UU_GUARD );
  ( "__instruction", UU_INSTRUCTION );
  ( "__instruction_set", UU_INSTRUCTION_SET );
  ( "__map", UU_MAP );
  ( "__newmap", UU_NEWMAP );
  ( "__newevent", UU_NEWEVENT );
  ( "__operator1", UU_OPERATOR_ONE );
  ( "__operator2", UU_OPERATOR_TWO );
  ( "__opcode", UU_OPCODE );
  ( "__postdecode", UU_POSTDECODE );
  ( "__readwrite", UU_READWRITE );
  ( "__register", UU_REGISTER );
  ( "__unpredictable_unless", UU_UNPREDICTABLE_UNLESS );
  ( "__write", UU_WRITE );
  ( "array", ARRAY );
  ( "assert", ASSERT );
  ( "bits", BITS );
  ( "case", CASE );
  ( "catch", CATCH );
  ( "constant", CONSTANT );
  ( "do", DO );
  ( "downto", DOWNTO );
  ( "else", ELSE );
  ( "elsif", ELSIF );
  ( "enumeration", ENUMERATION );
  ( "for", FOR );
  ( "if", IF );
  ( "is", IS );
  ( "of", OF );
  ( "otherwise", OTHERWISE );
  ( "record", RECORD );
  ( "repeat", REPEAT );
  ( "return", RETURN );
  ( "then", THEN );
  ( "throw", THROW );
  ( "to", TO );
  ( "try", TRY );
  ( "type", TYPE );
  ( "typeof", TYPEOF );
  ( "until", UNTIL );
  ( "when", WHEN );
  ( "while", WHILE );
]

let tr_name =
  let tr_table = Hashtbl.create ~random:false (List.length names_to_tokens) in
  let () = List.iter (fun (name, tok) -> Hashtbl.add tr_table name tok) names_to_tokens in
  fun name ->
    match Hashtbl.find_opt tr_table name with
    | Some tok -> tok
    | None -> IDENTIFIER name

let string_of_token = function
  | AND -> "AND"
  | CONSTRAINED_UNPRED -> "CONSTRAINED_UNPREDICTABLE"
  | DIV -> "DIV"
  | EOR -> "EOR"
  | IMPLEM_DEFINED -> "IMPLEMENTATION_DEFINED"
  | IN -> "IN"
  | IFF -> "IFF"
  | IMPLIES -> "IMPLIES"
  | MOD -> "MOD"
  | NOT -> "NOT"
  | OR -> "OR"
  | QUOT -> "QUOT"
  | REM -> "REM"
  | SEE -> "SEE"
  | UNDEFINED -> "UNDEFINED"
  | UNKNOWN -> "UNKNOWN"
  | UNPREDICTABLE -> "UNPREDICTABLE"
  | UU_EXCEPTIONTAKEN -> "__ExceptionTaken"
  | UU_NOP -> "__NOP"
  | UU_UNALLOCATED -> "__UNALLOCATED"
  | UU_UNPREDICTABLE -> "__UNPREDICTABLE"
  | UU_ARRAY -> "__array"
  | UU_BUILTIN -> "__builtin"
  | UU_CONDITIONAL -> "__conditional"
  | UU_CONFIG -> "__config"
  | UU_DECODE -> "__decode"
  | UU_ENCODING -> "__encoding"
  | UU_EVENT -> "__event"
  | UU_EXECUTE -> "__execute"
  | UU_FIELD -> "__field"
  | UU_FUNCTION -> "__function"
  | UU_GUARD -> "__guard"
  | UU_INSTRUCTION -> "__instruction"
  | UU_INSTRUCTION_SET -> "__instruction_set"
  | UU_MAP -> "__map"
  | UU_NEWMAP -> "__newmap"
  | UU_NEWEVENT -> "__newevent"
  | UU_OPERATOR_ONE -> "__operator1"
  | UU_OPERATOR_TWO -> "__operator2"
  | UU_OPCODE -> "__opcode"
  | UU_POSTDECODE -> "__postdecode"
  | UU_READWRITE -> "__readwrite"
  | UU_REGISTER -> "__register"
  | UU_UNPREDICTABLE_UNLESS -> "__unpredictable_unless"
  | UU_WRITE -> "__write"
  | ARRAY -> "array"
  | ASSERT -> "assert"
  | BITS -> "bits"
  | CASE -> "case"
  | CATCH -> "catch"
  | CONSTANT -> "constant"
  | DO -> "do"
  | DOWNTO -> "downto"
  | ELSE -> "else"
  | ELSIF -> "elsif"
  | ENUMERATION -> "enumeration"
  | FOR -> "for"
  | IF -> "if"
  | IS -> "is"
  | OF -> "of"
  | OTHERWISE -> "otherwise"
  | RECORD -> "record"
  | REPEAT -> "repeat"
  | RETURN -> "return"
  | THEN -> "then"
  | THROW -> "throw"
  | TO -> "to"
  | TRY -> "try"
  | TYPE -> "type"
  | TYPEOF -> "typeof"
  | UNTIL -> "until"
  | WHEN -> "when"
  | WHILE -> "while"
  (* Operators *)
  | BANG -> "!"
  | BANG_EQ -> "!="
  | AMP_AMP -> "&&"
  | AMP -> "&"
  | LPAREN -> "("
  | RPAREN -> ")"
  | STAR -> "*"
  | PLUS_PLUS -> "++"
  | PLUS -> "+"
  | PLUS_COLON -> "+:"
  | COMMA -> ","
  | MINUS -> "-"
  | DOT -> "."
  | DOT_DOT -> ".."
  | SLASH -> "/"
  | COLON -> ":"
  | SEMICOLON -> ";"
  | LT -> "<"
  | LT_LT -> "<<"
  | LT_EQ -> "<="
  | EQ -> "="
  | EQ_EQ -> "=="
  | EQ_GT -> "=>"
  | GT -> ">"
  | GT_EQ -> ">="
  | GT_GT -> ">>"
  | LBRACK -> "["
  | RBRACK -> "]"
  | CARET -> "^"
  | LBRACE -> "{"
  | LBRACE_LBRACE -> "{{"
  | BAR_BAR -> "||"
  | RBRACE -> "}"
  | RBRACE_RBRACE -> "}}"
    (* Literals *)
  | STRING_LIT (s) -> "\"" ^ s ^ "\""
  | BITS_LIT bv -> Bitvector.to_string bv
  | MASK_LIT s -> "'" ^ s ^ "'"
  | INT_LIT i -> string_of_int i
  | REAL_LIT r -> string_of_float r
  | QUALIFIER s -> s
  (* Special values *)
  | INDENT -> "IDENT"
  | DEDENT -> "DEDENT"
  | EOF -> "EOF"
  | EOL -> "EOL"
  | IDENTIFIER s -> s

}

let hex_lit = '0''x'['0'-'9' 'A' - 'F' 'a'-'f' '_']+
let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
rule token = parse
    (* whitespace and comments *)
    | ['\n']                      { Lexing.new_line lexbuf; EOL }
    | [' ' '\t']                  { token lexbuf }
    | '/' '/' [^'\n']*            { token lexbuf }
    | '#' [^'\n']*                { token lexbuf }
    | '/' '*'                     { comment 1 lexbuf }

    (* numbers, strings and identifiers *)
    | '"' ([^'"']* as s) '"'                { STRING_LIT(s)                     }
    | '\'' ['0' '1' ' ']* '\''       as lxm { BITS_LIT(Bitvector.of_string lxm) }
    | '\'' (['0' '1' 'x' ' ']* as s) '\''   { MASK_LIT(s)                       }
    | hex_lit                        as lxm { INT_LIT(int_of_string lxm)        }
    | ['0'-'9']+ '.' ['0'-'9']+      as lxm { REAL_LIT(float_of_string lxm)     }
    | ['0'-'9']+                     as lxm { INT_LIT(int_of_string lxm)        }
    | identifier                     as lxm { tr_name (lxm)                     }

    (* delimiters *)
    | '!'            { BANG       }
    | '!' '='        { BANG_EQ    }
    | '&' '&'        { AMP_AMP }
    | '&'            { AMP  }
    | '('            { LPAREN     }
    | ')'            { RPAREN     }
    | '*'            { STAR       }
    | '+' '+'        { PLUS_PLUS  }
    | '+'            { PLUS       }
    | '+' ':'        { PLUS_COLON }
    | ','            { COMMA      }
    | '-'            { MINUS      }
    | '.'            { DOT        }
    | '.' '.'        { DOT_DOT    }
    | '/'            { SLASH      }
    | ':'            { COLON      }
    | ';'            { SEMICOLON  }
    | '<'            { LT         }
    | '<' '<'        { LT_LT      }
    | '<' '='        { LT_EQ      }
    | '='            { EQ         }
    | '=' '='        { EQ_EQ      }
    | '=' '>'        { EQ_GT      }
    | '>'            { GT         }
    | '>' '='        { GT_EQ      }
    | '>' '>'        { GT_GT      }
    | '['            { LBRACK     }
    | ']'            { RBRACK     }
    | '^'            { CARET      }
    | '{'            { LBRACE     }
    | '{' '{'        { LBRACE_LBRACE }
    | '|' '|'        { BAR_BAR    }
    | '}'            { RBRACE     }
    | '}' '}'        { RBRACE_RBRACE }
    | eof            { EOF        }
    | _              { raise LexerError }

and comment depth = parse
      '/' '*' { comment (depth+1) lexbuf }
    | '*' '/' { if depth = 1 then token lexbuf else comment (depth-1) lexbuf }
    | '\n'    { Lexing.new_line lexbuf; comment depth lexbuf }
    | _       { comment depth lexbuf }

{
  let token_with_debug lexbuf =
    let tok = token lexbuf in
    let () = Printf.eprintf "Parsed token %s\n" (string_of_token tok) in
    tok
}

