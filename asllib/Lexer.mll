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

(* From herdtool's LexMisc *)
let incr_lineno lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  let n = pos.pos_lnum + 1 in
  lexbuf.lex_curr_p <- { pos with pos_lnum = n; pos_bol = pos.pos_cnum }

open Parser

let tr_name s = match s with
| "AND" -> AND
| "UNKNOWN" -> UNKNOWN
| "array" -> ARRAY
| "assumes" -> ASSUMES
| "call" -> CALL
| "class" -> CLASS
| "do" -> DO
| "end" -> END
| "endevent" -> ENDEVENT
| "endif" -> ENDIF
| "endproperty" -> ENDPROPERTY
| "endtry" -> ENDTRY
| "exception" -> EXCEPTION
| "feature" -> FEATURE
| "gives" -> GIVES
| "import" -> IMPORT
| "invariant" -> INVARIANT
| "map" -> MAP
| "newmap" -> NEWMAP
| "parallel" -> PARALLEL
| "private" -> PRIVATE
| "public" -> PUBLIC
| "requires" -> REQUIRES
| "set" -> SET
| "string" -> STRING
| "throw" -> THROW
| "typeof" -> TYPEOF
| "var" -> VAR
| "with" -> WITH
| "DIV" -> DIV
| "NOT" -> NOT
| "UNSTABLE" -> UNSTABLE
| "as" -> AS
| "bit" -> BIT
| "case" -> CASE
| "config" -> CONFIG
| "downto" -> DOWNTO
| "endcase" -> ENDCASE
| "endfor" -> ENDFOR
| "endmodule" -> ENDMODULE
| "endrule" -> ENDRULE
| "endwhile" -> ENDWHILE
| "export" -> EXPORT
| "for" -> FOR
| "if" -> IF
| "integer" -> INTEGER
| "is" -> IS
| "module" -> MODULE
| "of" -> OF
| "pass" -> PASS
| "profile" -> PROFILE
| "real" -> REAL
| "rethrow" -> RETHROW
| "setter" -> SETTER
| "subtypes" -> SUBTYPES
| "to" -> TO
| "union" -> UNION
| "when" -> WHEN
| "ztype" -> ZTYPE
| "EOR" -> EOR
| "IN" -> IN
| "OR" -> OR
| "SAMPLE" -> SAMPLE
| "_" -> ANY
| "any" -> ANY
| "assert" -> ASSERT
| "assume" -> ASSUME
| "bits" -> BITS
| "boolean" -> BOOLEAN
| "cast" -> CAST
| "catch" -> CATCH
| "constant" -> CONSTANT
| "dict" -> DICT
| "else" -> ELSE
| "elsif" -> ELSIF
| "endcatch" -> ENDCATCH
| "endclass" -> ENDCLASS
| "endfunc" -> ENDFUNC
| "endgetter" -> ENDGETTER
| "endnamespace" -> ENDNAMESPACE
| "endpackage" -> ENDPACKAGE
| "endsetter" -> ENDSETTER
| "endtemplate" -> ENDTEMPLATE
| "enumeration" -> ENUMERATION
| "event" -> EVENT
| "extends" -> EXTENDS
| "extern" -> EXTERN
| "func" -> FUNC
| "getter" -> GETTER
| "iff" -> IFF
| "implies" -> IMPLIES
| "intersect" -> INTERSECT
| "intrinsic" -> INTRINSIC
| "let" -> LET
| "list" -> LIST
| "Mem" -> MEM
| "namespace" -> NAMESPACE
| "newevent" -> NEWEVENT
| "otherwise" -> OTHERWISE
| "package" -> PACKAGE
| "port" -> PORT
| "pragma" -> PRAGMA
| "PSTATE" -> PSTATE
| "property" -> PROPERTY
| "protected" -> PROTECTED
| "record" -> RECORD
| "repeat" -> REPEAT
| "return" -> RETURN
| "rule" -> RULE
| "shared" -> SHARED
| "signal" -> SIGNAL
| "template" -> TEMPLATE
| "then" -> THEN
| "try" -> TRY
| "type" -> TYPE
| "until" -> UNTIL
| "using" -> USING
| "where" -> WHERE
| "while" -> WHILE
| "X" -> X
| x -> IDENTIFIER x

}

let digit = ['0'-'9']
let int_lit = digit ('_' | digit)*
let hex_alpha = ['a'-'f' 'A'-'F']
let hex_lit = '0' 'x' (digit | hex_alpha) ('_' | digit | hex_alpha)*
let real_lit = digit ('_' | digit)* '.' digit ('_' | digit)*
let alpha = ['a'-'z' 'A'-'Z']
let string_lit = '"' [^ '"']* '"'
let bitvector_lit = '\'' ['0' '1' 'z' ' ']* '\''
let mask_lit = '\'' ['0' '1' 'x' ' ']* '\''
let identifier = (alpha | '_') (alpha|digit|'_')*

rule token = parse
    | '\n'              { incr_lineno lexbuf; token lexbuf }
    | [' ''\t''\r']+    { token lexbuf }
    | "//" [^'\n']*     { token lexbuf }
    | int_lit as lxm    { INT_LIT(lxm) }
    | hex_lit as lxm    { INT_LIT(lxm) }
    | real_lit as lxm   { REAL_LIT(lxm) }
    | bitvector_lit as lxm    { BITVECTOR_LIT(lxm) }
    | "TRUE"            { BOOL_LIT(true) }
    | "FALSE"           { BOOL_LIT(false) }
    | '!'    { BNOT }
    | ','    { COMMA }
    | '<'    { LT }
    | ">>"    { SHR }
    | "&&"    { BAND }
    | "-->"    { IMPL }
    | "<<"    { SHL }
    | ']'    { RBRACKET }
    | ')'    { RPAR }
    | ".."    { SLICING }
    | '='    { EQ }
    | '{'    { LBRACE }
    | "!="    { NEQ }
    | '-'    { MINUS }
    | "<->"    { BEQ }
    | '['    { LBRACKET }
    | '('    { LPAR }
    | '.'    { DOT }
    | "<="    { LEQ }
    | '^'    { POW }
    | '*'    { MUL }
    | '/'    { RDIV }
    | "=="    { EQ_OP }
    | "||"    { BOR }
    | '+'    { PLUS }
    | ':'    { COLON }
    | "=>"    { ARROW }
    | '}'    { RBRACE }
    | "++"    { CONCAT }
    | "::"    { COLON_COLON }
    | '>'    { GT }
    | "+:"    { PLUS_COLON }
    | ';'    { SEMI_COLON }
    | ">="    { GEQ }
    | '%'     { MOD }
    | identifier as lxm { tr_name lxm }
    | eof               { EOF }
    | ""     { raise LexerError }
