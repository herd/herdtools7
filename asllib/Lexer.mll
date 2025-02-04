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

module Make (Config : CONFIG) = struct

exception LexerError

let new_line lexbuf = Lexing.new_line lexbuf; lexbuf
let bitvector_lit lxm = BITVECTOR_LIT (Bitvector.of_string lxm)
let mask_lit lxm = MASK_LIT (Bitvector.mask_of_string lxm)
let reserved_err s = Error.fatal_unknown_pos @@ (Error.ReservedIdentifier s)

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
| "XOR"           -> EOR
| "exception"     -> EXCEPTION
| "FALSE"         -> BOOL_LIT false
| "for"           -> FOR
| "func"          -> FUNC
| "getter"        -> GETTER
| "if"            -> IF
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
    else Error.fatal_unknown_pos @@ (Error.ObsoleteSyntax s)
| "ARBITRARY"     -> ARBITRARY
| "Unreachable"   -> UNREACHABLE
| "until"         -> UNTIL
| "var"           -> VAR
| "when"          -> WHEN
| "where"         -> WHERE
| "while"         -> WHILE
| "with"          -> WITH
(* Reserved identifiers *)
| "SAMPLE" | "UNSTABLE"
| "_" | "any"
| "assume" | "assumes"
| "call" | "cast"
| "class" | "dict"
| "endcase" | "endcatch" | "endclass"
| "endevent" | "endfor" | "endfunc" | "endgetter"
| "endif" | "endmodule" | "endnamespace" | "endpackage"
| "endproperty" | "endrule" | "endsetter" | "endtemplate"
| "endtry" | "endwhile"
| "event" | "export"
| "extends" | "extern" | "feature"
| "gives"
| "iff" | "implies" | "import"
| "intersect" | "intrinsic"
| "invariant" | "list"
| "map" | "module" | "namespace" | "newevent"
| "newmap" | "original"
| "package" | "parallel"
| "port" | "private"
| "profile" | "property" | "protected" | "public"
| "requires" | "rethrow" | "rule"
| "shared" | "signal"
| "template"
| "typeof" | "union"
| "using"
| "ztype" -> reserved_err s
| x when not Config.allow_double_underscore
         && ASTUtils.string_starts_with ~prefix:"__" x -> reserved_err x
(* End of reserved identifiers *)
| x               -> IDENTIFIER x
}

let digit = ['0'-'9']
let int_lit = digit ('_' | digit)*
let hex_alpha = ['a'-'f' 'A'-'F']
let hex_lit = '0' 'x' (digit | hex_alpha) ('_' | digit | hex_alpha)*
let real_lit = digit ('_' | digit)* '.' digit ('_' | digit)*
let alpha = ['a'-'z' 'A'-'Z']
let string_lit = '"' [^ '"']* '"'
let bits = ['0' '1' ' ']*
let mask = ['0' '1' 'x' ' ']*
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
    | '}'                      { RBRACE                           }
    | "++"                     { fatal_unknown_pos (ObsoleteSyntax "string concatenation with ++") }
    | "::"                     { COLON_COLON                      }
    | '>'                      { GT                               }
    | "+:"                     { PLUS_COLON                       }
    | "*:"                     { STAR_COLON                       }
    | ';'                      { SEMI_COLON                       }
    | ">="                     { GEQ                              }
    | "@looplimit"             { ARROBASE_LOOPLIMIT               }
    | identifier as lxm        { tr_name lxm                      }
    | eof                      { EOF                              }
    | ""                       { raise LexerError                 }
{
end
}
