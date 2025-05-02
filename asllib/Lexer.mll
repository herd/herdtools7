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

open Tokens

open Error

module type CONFIG = sig
    (** Allow variables starting with a double underscore (__) *)
    val allow_double_underscore : bool
    val allow_unknown : bool
end

let reserved_keywords = [
    "pure";
    "readonly";
]

let is_reserved_keyword: string -> bool =
  let tbl: (string, unit) Hashtbl.t = Hashtbl.create (List.length reserved_keywords) in
  let () = List.iter (fun s -> Hashtbl.add tbl s ()) reserved_keywords in
  fun s -> Hashtbl.mem tbl s

(* Get a Tokens.token from the string name of the token.
   A useful utility when working with cmly files.

   Note that this set only contains simple enumerative types, not compound types

   This function's exhaustiveness is guaranteed by the test in tests/Lexer. *)
let token_of_string =
 let s t = Some t in
 function
 | "ACCESSOR"           -> s ACCESSOR
 | "AND"                -> s AND
 | "ARRAY"              -> s ARRAY
 | "ARROW"              -> s ARROW
 | "AS"                 -> s AS
 | "ASSERT"             -> s ASSERT
 | "BAND"               -> s BAND
 | "BEGIN"              -> s BEGIN
 | "BEQ"                -> s BEQ
 | "BIARROW"            -> s BIARROW
 | "BIT"                -> s BIT
 | "BITS"               -> s BITS
 | "BNOT"               -> s BNOT
 | "BOOLEAN"            -> s BOOLEAN
 | "BOR"                -> s BOR
 | "CASE"               -> s CASE
 | "CATCH"              -> s CATCH
 | "COLLECTION"         -> s COLLECTION
 | "COLON"              -> s COLON
 | "COLON_COLON"        -> s COLON_COLON
 | "COMMA"              -> s COMMA
 | "CONFIG"             -> s CONFIG
 | "CONSTANT"           -> s CONSTANT
 | "DEBUG"              -> s DEBUG
 | "DIV"                -> s DIV
 | "DIVRM"              -> s DIVRM
 | "DO"                 -> s DO
 | "DOT"                -> s DOT
 | "DOWNTO"             -> s DOWNTO
 | "ELSE"               -> s ELSE
 | "ELSIF"              -> s ELSIF
 | "END"                -> s END
 | "ENUMERATION"        -> s ENUMERATION
 | "EOF"                -> s EOF
 | "XOR"                -> s XOR
 | "EQ"                 -> s EQ
 | "EQ_OP"              -> s EQ_OP
 | "EXCEPTION"          -> s EXCEPTION
 | "FOR"                -> s FOR
 | "FUNC"               -> s FUNC
 | "GEQ"                -> s GEQ
 | "GETTER"             -> s GETTER
 | "GT"                 -> s GT
 | "IF"                 -> s IF
 | "IMPLEMENTATION"     -> s IMPLEMENTATION
 | "IMPDEF"             -> s IMPDEF
 | "IMPL"               -> s IMPL
 | "IN"                 -> s IN
 | "INTEGER"            -> s INTEGER
 | "LBRACE"             -> s LBRACE
 | "LBRACKET"           -> s LBRACKET
 | "LEQ"                -> s LEQ
 | "LET"                -> s LET
 | "LOOPLIMIT"          -> s LOOPLIMIT
 | "LPAR"               -> s LPAR
 | "LT"                 -> s LT
 | "MINUS"              -> s MINUS
 | "MOD"                -> s MOD
 | "MUL"                -> s MUL
 | "NEQ"                -> s NEQ
 | "NOT"                -> s NOT
 | "OF"                 -> s OF
 | "OR"                 -> s OR
 | "OTHERWISE"          -> s OTHERWISE
 | "PASS"               -> s PASS
 | "PLUS"               -> s PLUS
 | "PLUS_COLON"         -> s PLUS_COLON
 | "POW"                -> s POW
 | "PRAGMA"             -> s PRAGMA
 | "PRINTLN"            -> s PRINTLN
 | "PRINT"              -> s PRINT
 | "RBRACE"             -> s RBRACE
 | "RBRACKET"           -> s RBRACKET
 | "RDIV"               -> s RDIV
 | "REAL"               -> s REAL
 | "RECORD"             -> s RECORD
 | "RECURSELIMIT"       -> s RECURSELIMIT
 | "REPEAT"             -> s REPEAT
 | "RETURN"             -> s RETURN
 | "RPAR"               -> s RPAR
 | "STAR_COLON"         -> s STAR_COLON
 | "SEMI_COLON"         -> s SEMI_COLON
 | "SETTER"             -> s SETTER
 | "SHL"                -> s SHL
 | "SHR"                -> s SHR
 | "SLICING"            -> s SLICING
 | "STRING"             -> s STRING
 | "SUBTYPES"           -> s SUBTYPES
 | "THEN"               -> s THEN
 | "THROW"              -> s THROW
 | "TO"                 -> s TO
 | "TRY"                -> s TRY
 | "TYPE"               -> s TYPE
 | "ARBITRARY"          -> s ARBITRARY
 | "UNREACHABLE"        -> s UNREACHABLE
 | "UNTIL"              -> s UNTIL
 | "VAR"                -> s VAR
 | "WHEN"               -> s WHEN
 | "WHERE"              -> s WHERE
 | "WHILE"              -> s WHILE
 | "WITH"               -> s WITH
 | "LLBRACKET"          -> s LLBRACKET
 | "RRBRACKET"          -> s RRBRACKET
 | _ -> None

(** Convert a lexical token to the symbol it lexes *)
let token_to_symbol = function
  | BNOT               -> "!"
  | COMMA              -> ","
  | LT                 -> "<"
  | SHR                -> ">>"
  | BAND               -> "&&"
  | IMPL               -> "-->"
  | SHL                -> "<<"
  | RBRACKET           -> "]"
  | RRBRACKET          -> "]]"
  | RPAR               -> ")"
  | SLICING            -> ".."
  | EQ                 -> "="
  | LBRACE             -> "{"
  | NEQ                -> "!="
  | MINUS              -> "-"
  | BEQ                -> "<->"
  | LBRACKET           -> "["
  | LLBRACKET          -> "[["
  | LPAR               -> "("
  | DOT                -> "."
  | LEQ                -> "<="
  | POW                -> "^"
  | MUL                -> "*"
  | RDIV               -> "/"
  | EQ_OP              -> "=="
  | BOR                -> "||"
  | PLUS               -> "+"
  | COLON              -> ":"
  | ARROW              -> "=>"
  | BIARROW            -> "<=>"
  | RBRACE             -> "}"
  | COLON_COLON        -> "::"
  | GT                 -> ">"
  | PLUS_COLON         -> "+:"
  | STAR_COLON         -> "*:"
  | SEMI_COLON         -> ";"
  | GEQ                -> ">="
  (* Keywords *)
  | ACCESSOR           -> "accessor"
  | AND                -> "AND"
  | ARRAY              -> "array"
  | AS                 -> "as"
  | ASSERT             -> "assert"
  | BEGIN              -> "begin"
  | BIT                -> "bit"
  | BITS               -> "bits"
  | BOOLEAN            -> "boolean"
  | CASE               -> "case"
  | CATCH              -> "catch"
  | COLLECTION         -> "collection"
  | CONFIG             -> "config"
  | CONSTANT           -> "constant"
  | DIV                -> "DIV"
  | DIVRM              -> "DIVRM"
  | DO                 -> "do"
  | DOWNTO             -> "downto"
  | ELSE               -> "else"
  | ELSIF              -> "elsif"
  | END                -> "end"
  | ENUMERATION        -> "enumeration"
  | XOR                -> "XOR"
  | EXCEPTION          -> "exception"
  | FOR                -> "for"
  | FUNC               -> "func"
  | GETTER             -> "getter"
  | IF                 -> "if"
  | IMPLEMENTATION     -> "implementation"
  | IMPDEF             -> "impdef"
  | IN                 -> "IN"
  | INTEGER            -> "integer"
  | LET                -> "let"
  | LOOPLIMIT          -> "looplimit"
  | MOD                -> "MOD"
  | NOT                -> "NOT"
  | OF                 -> "of"
  | OR                 -> "OR"
  | OTHERWISE          -> "otherwise"
  | PASS               -> "pass"
  | PRAGMA             -> "pragma"
  | PRINTLN            -> "println"
  | PRINT              -> "print"
  | REAL               -> "real"
  | RECORD             -> "record"
  | RECURSELIMIT       -> "recurselimit"
  | REPEAT             -> "repeat"
  | RETURN             -> "return"
  | SETTER             -> "setter"
  | STRING             -> "string"
  | SUBTYPES           -> "subtypes"
  | THEN               -> "then"
  | THROW              -> "throw"
  | TO                 -> "to"
  | TRY                -> "try"
  | TYPE               -> "type"
  | ARBITRARY          -> "ARBITRARY"
  | UNREACHABLE        -> "Unreachable"
  | UNTIL              -> "until"
  | VAR                -> "var"
  | WHEN               -> "when"
  | WHERE              -> "where"
  | WHILE              -> "while"
  | WITH               -> "with"
  | BOOL_LIT _
  | INT_LIT _
  | REAL_LIT _
  | STRING_LIT _
  | BITVECTOR_LIT _
  | MASK_LIT _
  | IDENTIFIER _
  | EOF
  | DEBUG -> assert false (* Complex tokens *)


module Make (Config : CONFIG) = struct

exception LexerError

let new_line lexbuf = Lexing.new_line lexbuf; lexbuf
let bitvector_lit lxm = BITVECTOR_LIT (Bitvector.of_string lxm)
let mask_lit lxm = MASK_LIT (Bitvector.mask_of_string lxm)
let reserved_err s = Error.fatal_unknown_pos @@ (Error.ReservedIdentifier s)

let fatal lexbuf desc =
  AST.
    {
      desc;
      version = V1;
      pos_start = Lexing.lexeme_start_p lexbuf;
      pos_end = Lexing.lexeme_end_p lexbuf;
    }
  |> Error.fatal

let tr_name s = match s with
| "accessor"      -> ACCESSOR
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
| "collection"    -> COLLECTION
| "config"        -> CONFIG
| "constant"      -> CONSTANT
| "__debug__"
| "__DEBUG__"     -> DEBUG
| "DIV"           -> DIV
| "DIVRM"         -> DIVRM
| "do"            -> DO
| "downto"        -> DOWNTO
| "else"          -> ELSE
| "elsif"         -> ELSIF
| "end"           -> END
| "enumeration"   -> ENUMERATION
| "XOR"           -> XOR
| "exception"     -> EXCEPTION
| "FALSE"         -> BOOL_LIT false
| "for"           -> FOR
| "func"          -> FUNC
| "getter"        -> GETTER
| "if"            -> IF
| "impdef"        -> IMPDEF
| "implementation" -> IMPLEMENTATION
| "IN"            -> IN
| "integer"       -> INTEGER
| "let"           -> LET
| "looplimit"     -> LOOPLIMIT
| "MOD"           -> MOD
| "NOT"           -> NOT
| "of"            -> OF
| "OR"            -> OR
| "otherwise"     -> OTHERWISE
| "pass"          -> PASS
| "pragma"        -> PRAGMA
| "println"       -> PRINTLN
| "print"         -> PRINT
| "real"          -> REAL
| "record"        -> RECORD
| "recurselimit"  -> RECURSELIMIT
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
| "UNKNOWN"       ->
    if Config.allow_unknown then ARBITRARY
    else fatal_unknown_pos (Error.ObsoleteSyntax s)
| "ARBITRARY"     -> ARBITRARY
| "Unreachable"   -> UNREACHABLE
| "until"         -> UNTIL
| "var"           -> VAR
| "when"          -> WHEN
| "where"         -> WHERE
| "while"         -> WHILE
| "with"          -> WITH
(* Reserved identifiers *)
| x when is_reserved_keyword x -> reserved_err x
| x when not Config.allow_double_underscore
         && ASTUtils.string_starts_with ~prefix:"__" x -> reserved_err x
(* End of reserved identifiers *)
| x               -> IDENTIFIER x
}

let digit = ['0'-'9']
let int_lit = digit ('_' | digit)*
let hex_alpha = ['a'-'f' 'A'-'F']
let hex_lit = '0' 'x' (digit | hex_alpha) ('_' | digit | hex_alpha)*
let real_lit = int_lit '.' int_lit
let alpha = ['a'-'z' 'A'-'Z']
let string_lit = '"' [^ '"']* '"'
let bit = ['0' '1' ' ']
let bits = bit*
let mask = (bit | 'x' | '(' bit+ ')')*
let identifier = (alpha | '_') (alpha|digit|'_')*

(*
   Lexing of string literals
   =========================

   We are not using [Scanf.unescaped] because:
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
  | '\n'  { Buffer.add_char acc '\n'; new_line lexbuf |> string_lit acc }
  | [^ '"' '\\' '\n']+ as lxm { Buffer.add_string acc lxm; string_lit acc lexbuf }
  | ""    { raise LexerError }

(*
   Lexing of c-style comments
   ==========================
*)

and c_comments = parse
  | "*/"          { token      lexbuf }
  | '*'           { c_comments lexbuf }
  | '\n'          { new_line lexbuf |> c_comments }
  | [^ '*' '\n']+ { c_comments lexbuf }
  | ""            { raise LexerError  }

(*
   Lexing of ASL tokens
   ====================
*)

and token = parse
    | '\n'                     { new_line lexbuf |> token         }
    | [' ''\t''\r']+           { token lexbuf                     }
    | "//" [^'\n']*            { token lexbuf                     }
    | "/*"                     { c_comments lexbuf                }
    | int_lit as lxm           { INT_LIT(Z.of_string lxm)         }
    | hex_lit as lxm           { INT_LIT(Z.of_string lxm)         }
    | real_lit as lxm          { REAL_LIT(Q.of_string lxm)        }
    | '"'                      { string_lit (Buffer.create 16) lexbuf }
    | '\'' (bits as lxm) '\''  { bitvector_lit lxm                }
    | '\'' (mask as lxm) '\''  { mask_lit lxm                     }  (* Warning: masks with no unknown 'x' characters will be lexed as bitvectors. *)
    | '!'                      { BNOT                             }
    | ','                      { COMMA                            }
    | '<'                      { LT                               }
    | ">>"                     { SHR                              }
    | "&&"                     { BAND                             }
    | "-->"                    { IMPL                             }
    | "<<"                     { SHL                              }
    | ']'                      { RBRACKET                         }
    | "]]"                     { RRBRACKET                        }
    | ')'                      { RPAR                             }
    | ".."                     { SLICING                          }
    | '='                      { EQ                               }
    | '{'                      { LBRACE                           }
    | "!="                     { NEQ                              }
    | '-'                      { MINUS                            }
    | "<->"                    { BEQ                              }
    | '['                      { LBRACKET                         }
    | "[["                     { LLBRACKET                        }
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
    | "<=>"                    { BIARROW                          }
    | '}'                      { RBRACE                           }
    | "++"                     { fatal lexbuf (ObsoleteSyntax "string concatenation with ++") }
    | "::"                     { COLON_COLON                      }
    | '>'                      { GT                               }
    | "+:"                     { PLUS_COLON                       }
    | "*:"                     { STAR_COLON                       }
    | ';'                      { SEMI_COLON                       }
    | ">="                     { GEQ                              }
    | "@looplimit"             { fatal lexbuf (ObsoleteSyntax "Loop limits with @looplimit") }
    | identifier as lxm        { tr_name lxm                      }
    | eof                      { EOF                              }
    | ""                       { raise LexerError                 }
{
end
}
