(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

{
open Parser0 (* The type token is defined in parser.mli *)

let tr_name = function
  | "AArch32" -> QUALIFIER "AArch32"
  | "AArch64" -> QUALIFIER "AArch64"
  | "AND" -> AND
  | "CONSTRAINED_UNPREDICTABLE" -> CONSTRAINED_UNPRED
  | "DIV" -> DIV
  | "EOR" -> XOR
  | "IMPLEMENTATION_DEFINED" -> IMPLEM_DEFINED
  | "IN" -> IN
  | "IFF" -> IFF
  | "IMPLIES" -> IMPLIES
  | "MOD" -> MOD
  | "NOT" -> NOT
  | "OR" -> OR
  | "QUOT" -> QUOT
  | "REM" -> REM
  | "SEE" -> SEE
  | "UNDEFINED" -> UNDEFINED
  | "UNKNOWN" -> UNKNOWN
  | "UNPREDICTABLE" -> UNPREDICTABLE
  | "__ExceptionTaken" -> UU_EXCEPTIONTAKEN
  | "__NOP" -> UU_NOP
  | "__UNALLOCATED" -> UU_UNALLOCATED
  | "__UNPREDICTABLE" -> UU_UNPREDICTABLE
  | "__array" -> UU_ARRAY
  | "__builtin" -> UU_BUILTIN
  | "__conditional" -> UU_CONDITIONAL
  | "__config" -> UU_CONFIG
  | "__decode" -> UU_DECODE
  | "__encoding" -> UU_ENCODING
  | "__event" -> UU_EVENT
  | "__execute" -> UU_EXECUTE
  | "__field" -> UU_FIELD
  | "__function" -> UU_FUNCTION
  | "__guard" -> UU_GUARD
  | "__instruction" -> UU_INSTRUCTION
  | "__instruction_set" -> UU_INSTRUCTION_SET
  | "__map" -> UU_MAP
  | "__newmap" -> UU_NEWMAP
  | "__newevent" -> UU_NEWEVENT
  | "__operator1" -> UU_OPERATOR_ONE
  | "__operator2" -> UU_OPERATOR_TWO
  | "__opcode" -> UU_OPCODE
  | "__postdecode" -> UU_POSTDECODE
  | "__readwrite" -> UU_READWRITE
  | "__register" -> UU_REGISTER
  | "__unpredictable_unless" -> UU_UNPREDICTABLE_UNLESS
  | "__write" -> UU_WRITE
  | "array" -> ARRAY
  | "assert" -> ASSERT
  | "bit" -> BIT
  | "bits" -> BITS
  | "boolean" -> BOOLEAN
  | "case" -> CASE
  | "catch" -> CATCH
  | "constant" -> CONSTANT
  | "config" -> CONFIG
  | "DEBUG" -> DEBUG
  | "do" -> DO
  | "downto" -> DOWNTO
  | "else" -> ELSE
  | "elsif" -> ELSIF
  | "enumeration" -> ENUMERATION
  | "FALSE" -> BOOL_LIT false
  | "for" -> FOR
  | "integer" -> INTEGER
  | "if" -> IF
  | "is" -> IS
  | "let" -> LET
  | "limit" -> LIMIT
  | "of" -> OF
  | "otherwise" -> OTHERWISE
  | "real" -> REAL
  | "record" -> RECORD
  | "repeat" -> REPEAT
  | "return" -> RETURN
  | "then" -> THEN
  | "throw" -> THROW
  | "to" -> TO
  | "TRUE" -> BOOL_LIT true
  | "try" -> TRY
  | "type" -> TYPE
  | "typeof" -> TYPEOF
  | "until" -> UNTIL
  | "Unreachable" -> UNREACHABLE
  | "when" -> WHEN
  | "while" -> WHILE
  | name -> IDENTIFIER name

let string_of_token = function
  | AND -> "AND"
  | ARRAY -> "array"
  | ASSERT -> "assert"
  | BIT -> "bit"
  | BITS -> "bits"
  | BOOLEAN -> "boolean"
  | CASE -> "case"
  | CATCH -> "catch"
  | CONFIG -> "config"
  | CONSTANT -> "constant"
  | CONSTRAINED_UNPRED -> "CONSTRAINED_UNPREDICTABLE"
  | DEBUG -> "DEBUG"
  | DIV -> "DIV"
  | DO -> "do"
  | DOWNTO -> "downto"
  | ELSE -> "else"
  | ELSIF -> "elsif"
  | ENUMERATION -> "enumeration"
  | XOR -> "EOR"
  | FOR -> "for"
  | IF -> "if"
  | IFF -> "IFF"
  | IMPLEM_DEFINED -> "IMPLEMENTATION_DEFINED"
  | IMPLIES -> "IMPLIES"
  | IN -> "IN"
  | INTEGER -> "integer"
  | IS -> "is"
  | MOD -> "MOD"
  | NOT -> "NOT"
  | OF -> "of"
  | OR -> "OR"
  | OTHERWISE -> "otherwise"
  | QUOT -> "QUOT"
  | REAL -> "real"
  | RECORD -> "record"
  | REM -> "REM"
  | REPEAT -> "repeat"
  | RETURN -> "return"
  | SEE -> "SEE"
  | THEN -> "then"
  | THROW -> "throw"
  | TO -> "to"
  | TRY -> "try"
  | TYPE -> "type"
  | TYPEOF -> "typeof"
  | UNDEFINED -> "UNDEFINED"
  | UNKNOWN -> "UNKNOWN"
  | UNPREDICTABLE -> "UNPREDICTABLE"
  | UNREACHABLE -> "Unreachable"
  | UNTIL -> "until"
  | UU_ARRAY -> "__array"
  | UU_BUILTIN -> "__builtin"
  | UU_CONDITIONAL -> "__conditional"
  | UU_CONFIG -> "__config"
  | UU_DECODE -> "__decode"
  | UU_ENCODING -> "__encoding"
  | UU_EVENT -> "__event"
  | UU_EXCEPTIONTAKEN -> "__ExceptionTaken"
  | UU_EXECUTE -> "__execute"
  | UU_FIELD -> "__field"
  | UU_FUNCTION -> "__function"
  | UU_GUARD -> "__guard"
  | UU_INSTRUCTION -> "__instruction"
  | UU_INSTRUCTION_SET -> "__instruction_set"
  | UU_MAP -> "__map"
  | UU_NEWEVENT -> "__newevent"
  | UU_NEWMAP -> "__newmap"
  | UU_NOP -> "__NOP"
  | UU_OPCODE -> "__opcode"
  | UU_OPERATOR_ONE -> "__operator1"
  | UU_OPERATOR_TWO -> "__operator2"
  | UU_POSTDECODE -> "__postdecode"
  | UU_READWRITE -> "__readwrite"
  | UU_REGISTER -> "__register"
  | UU_UNALLOCATED -> "__UNALLOCATED"
  | UU_UNPREDICTABLE -> "__UNPREDICTABLE"
  | UU_UNPREDICTABLE_UNLESS -> "__unpredictable_unless"
  | UU_WRITE -> "__write"
  | WHEN -> "when"
  | WHILE -> "while"
  (* Operators *)
  | AMP -> "&"
  | AMP_AMP -> "&&"
  | BANG -> "!"
  | BANG_EQ -> "!="
  | BAR_BAR -> "||"
  | CARET -> "^"
  | COLON -> ":"
  | COMMA -> ","
  | DOT -> "."
  | DOT_DOT -> ".."
  | EQ -> "="
  | EQ_EQ -> "=="
  | EQ_GT -> "=>"
  | GT -> ">"
  | GT_EQ -> ">="
  | GT_GT -> ">>"
  | LBRACE -> "{"
  | LBRACE_LBRACE -> "{{"
  | LBRACK -> "["
  | LIMIT -> "limit"
  | LPAREN -> "("
  | LT -> "<"
  | LT_EQ -> "<="
  | LT_LT -> "<<"
  | MINUS -> "-"
  | PLUS -> "+"
  | PLUS_COLON -> "+:"
  | PLUS_PLUS -> "++"
  | RBRACE -> "}"
  | RBRACE_RBRACE -> "}}"
  | RBRACK -> "]"
  | RPAREN -> ")"
  | SEMICOLON -> ";"
  | SLASH -> "/"
  | STAR -> "*" (* Literals *)
  | BOOL_LIT b -> if b then "TRUE" else "FALSE"
  | STRING_LIT s -> "\"" ^ s ^ "\""
  | BITS_LIT bv -> Bitvector.to_string bv
  | MASK_LIT m -> "'" ^ Bitvector.mask_to_string m ^ "'"
  | INT_LIT i -> Z.to_string i
  | REAL_LIT r -> Q.to_string r
  | QUALIFIER s -> s
  (* Special values *)
  | INDENT -> "IDENT"
  | DEDENT -> "DEDENT"
  | EOF -> "EOF"
  | EOL -> "EOL"
  | LET -> "LET"
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
    | '"' ([^'"']* as s) '"'                { STRING_LIT (s)                        }
    | '\'' ['0' '1' ' ']* '\''       as lxm { BITS_LIT (Bitvector.of_string lxm)    }
    | '\'' (['0' '1' 'x' ' ']* as s) '\''   { MASK_LIT (Bitvector.mask_of_string s) }
    | hex_lit                        as lxm { INT_LIT (Z.of_string lxm)             }
    | ['0'-'9']+ '.' ['0'-'9']+      as lxm { REAL_LIT (Q.of_string lxm)            }
    | ['0'-'9']+                     as lxm { INT_LIT (Z.of_string lxm)             }
    | identifier                     as lxm { tr_name (lxm)                         }

    (* delimiters *)
    | '!'            { BANG       }
    | '!' '='        { BANG_EQ    }
    | '&'            { AMP  }
    | '&' '&'        { AMP_AMP }
    | '('            { LPAREN     }
    | ')'            { RPAREN     }
    | '*'            { STAR       }
    | '+'            { PLUS       }
    | '+' '+'        { PLUS_PLUS  }
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
    | _
      {
        let p1 = Lexing.lexeme_start_p lexbuf and p2 = Lexing.lexeme_end_p lexbuf in
        Error.fatal_here p1 p2 Error.UnknownSymbol
      }

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

