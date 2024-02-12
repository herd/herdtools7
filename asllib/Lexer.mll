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

exception LexerError

open Parser

let bitvector_lit lxm = BITVECTOR_LIT (Bitvector.of_string lxm)
let mask_lit lxm = MASK_LIT (Bitvector.mask_of_string lxm)

let tr_name s = match s with
| "AND"           -> AND
| "array"         -> ARRAY
| "as"            -> AS
| "assert"        -> ASSERT
| "begin"         -> BEGIN
| "bit"           -> BIT
| "bits"          -> BITS
| "boolean"       -> BOOLEAN
| "case"          -> CASE
| "catch"         -> CATCH
| "config"        -> CONFIG
| "constant"      -> CONSTANT
| "debug"|"DEBUG" -> DEBUG
| "DIV"           -> DIV
| "DIVRM"         -> DIVRM
| "do"            -> DO
| "downto"        -> DOWNTO
| "else"          -> ELSE
| "elsif"         -> ELSIF
| "end"           -> END
| "enumeration"   -> ENUMERATION
| "EOR" | "XOR"   -> EOR
| "exception"     -> EXCEPTION
| "FALSE"         -> BOOL_LIT false
| "for"           -> FOR
| "func"          -> FUNC
| "getter"        -> GETTER
| "if"            -> IF
| "IN"            -> IN
| "integer"       -> INTEGER
| "let"           -> LET
| "MOD"           -> MOD
| "NOT"           -> NOT
| "of"            -> OF
| "OR"            -> OR
| "otherwise"     -> OTHERWISE
| "pass"          -> PASS
| "pragma"        -> PRAGMA
| "real"          -> REAL
| "record"        -> RECORD
| "repeat"        -> REPEAT
| "return"        -> RETURN
| "setter"        -> SETTER
| "string"        -> STRING
| "subtypes"      -> SUBTYPES
| "then"          -> THEN
| "throw"         -> THROW
| "to"            -> TO
| "try"           -> TRY
| "TRUE"          -> BOOL_LIT true
| "type"          -> TYPE
| "UNKNOWN"       -> UNKNOWN
| "until"         -> UNTIL
| "var"           -> VAR
| "when"          -> WHEN
| "where"         -> WHERE
| "while"         -> WHILE
| "with"          -> WITH
| x               -> IDENTIFIER x
}

let digit = ['0'-'9']
let int_lit = digit ('_' | digit)*
let hex_alpha = ['a'-'f' 'A'-'F']
let hex_lit = '0' 'x' (digit | hex_alpha) ('_' | digit | hex_alpha)*
let real_lit = digit ('_' | digit)* '.' digit ('_' | digit)*
let alpha = ['a'-'z' 'A'-'Z']
let bits = ['0' '1' 'z' ' ']*
let mask = ['0' '1' 'x' ' ']*
let identifier = (alpha | '_') (alpha|digit|'_')*

(*
   We are not using [Scanf.unescape] because:
     - [Scanf.unescaped] basically follows the lexical conventions of OCaml,
       while we follow the lexical conventions of ASL;
     - if they were to diverge, we would have to re-implement it
     - they do not support the same escape sequences:
       - ASL supports only [\\], [\"], [\n], [\t]
       - From OCaml Manual:
            escape-sequence	::=	\ (\ ∣ " ∣ ' ∣ n ∣ t ∣ b ∣ r ∣ space)
              ∣	 \ (0…9) (0…9) (0…9)
              ∣	 \x (0…9 ∣ A…F ∣ a…f) (0…9 ∣ A…F ∣ a…f)
              ∣	 \o (0…3) (0…7) (0…7)
     - using [unescaped] is not very explicit
     - We would still need lexing character by character because ocamllex
       cannot match negatively on the two string character that escape the end
       of a string literal.
*)
rule escaped_string_chars acc = parse
  | 'n'  { Buffer.add_char acc '\n'; string_lit acc lexbuf }
  | 't'  { Buffer.add_char acc '\t'; string_lit acc lexbuf }
  | '"'  { Buffer.add_char acc '"'; string_lit acc lexbuf }
  | '\\' { Buffer.add_char acc '\\'; string_lit acc lexbuf }
  | [^ 'n' 't' '"' '\\'] { raise LexerError }

and string_lit acc = parse
  | '"'   { STRING_LIT (Buffer.contents acc) }
  | '\\'  { escaped_string_chars acc lexbuf }
  | [^ '"' '\\']+ as lxm { Buffer.add_string acc lxm; string_lit acc lexbuf }

and token = parse
    | '\n'                     { Lexing.new_line lexbuf; token lexbuf }
    | [' ''\t''\r']+           { token lexbuf                     }
    | "//" [^'\n']*            { token lexbuf                     }
    | int_lit as lxm           { INT_LIT(Z.of_string lxm)         }
    | hex_lit as lxm           { INT_LIT(Z.of_string lxm)         }
    | real_lit as lxm          { REAL_LIT(Q.of_string lxm)        }
    | '"'                      { string_lit (Buffer.create 16) lexbuf }
    | '\'' (bits as lxm) '\''  { bitvector_lit lxm                }
    | '\'' (mask as lxm) '\''  { mask_lit lxm                     }
    | '!'                      { BNOT                             }
    | ','                      { COMMA                            }
    | '<'                      { LT                               }
    | ">>"                     { SHR                              }
    | "&&"                     { BAND                             }
    | "-->"                    { IMPL                             }
    | "<<"                     { SHL                              }
    | ']'                      { RBRACKET                         }
    | ')'                      { RPAR                             }
    | ".."                     { SLICING                          }
    | '='                      { EQ                               }
    | '{'                      { LBRACE                           }
    | "!="                     { NEQ                              }
    | '-'                      { MINUS                            }
    | "<->"                    { BEQ                              }
    | '['                      { LBRACKET                         }
    | '('                      { LPAR                             }
    | '.'                      { DOT                              }
    | "<="                     { LEQ                              }
    | '^'                      { POW                              }
    | '*'                      { MUL                              }
    | '/'                      { RDIV                             }
    | "=="                     { EQ_OP                            }
    | "||"                     { BOR                              }
    | '+'                      { PLUS                             }
    | ':'                      { COLON                            }
    | "=>"                     { ARROW                            }
    | '}'                      { RBRACE                           }
    | "++"                     { CONCAT                           }
    | "::"                     { COLON_COLON                      }
    | '>'                      { GT                               }
    | "+:"                     { PLUS_COLON                       }
    | "*:"                     { STAR_COLON                       }
    | ';'                      { SEMI_COLON                       }
    | ">="                     { GEQ                              }
    | identifier as lxm        { tr_name lxm                      }
    | eof                      { EOF                              }
    | ""                       { raise LexerError                 }
