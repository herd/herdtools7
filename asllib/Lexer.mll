(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
(* Authors:                                                                 *)
(* Hadrien Renaud, University College London, UK.                           *)
(****************************************************************************)

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
| "DIV"           -> DIV
| "do"            -> DO
| "downto"        -> DOWNTO
| "else"          -> ELSE
| "elsif"         -> ELSIF
| "end"           -> END
| "enumeration"   -> ENUMERATION
| "EOR"           -> EOR
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
let string_lit = '"' [^ '"']* '"'
let bits = ['0' '1' 'z' ' ']*
let mask = ['0' '1' 'x' ' ']*
let identifier = (alpha | '_') (alpha|digit|'_')*

rule token = parse
    | '\n'                     { Lexing.new_line lexbuf; token lexbuf }
    | [' ''\t''\r']+           { token lexbuf                     }
    | "//" [^'\n']*            { token lexbuf                     }
    | int_lit as lxm           { INT_LIT(int_of_string lxm)       }
    | hex_lit as lxm           { INT_LIT(int_of_string lxm)       }
    | real_lit as lxm          { REAL_LIT(float_of_string lxm)    }
    | '"' ([^ '"']* as lxm) '"' { STRING_LIT(lxm)                  }
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
    | ';'                      { SEMI_COLON                       }
    | ">="                     { GEQ                              }
    | identifier as lxm        { tr_name lxm                      }
    | eof                      { EOF                              }
    | ""                       { raise LexerError                 }
