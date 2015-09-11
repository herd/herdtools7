(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


{
module Make(O:LexUtils.Config) = struct
open Lexing
open LexMisc
open ModelParser
module LU = LexUtils.Make(O)

(* Efficient from ocaml 4.02 *)

  let check_keyword = function
    | "let" -> LET
    | "rec" -> REC
    | "and" -> AND
    | "when" -> WHEN
    | "acyclic" -> ACYCLIC
    | "irreflexive" -> IRREFLEXIVE
    | "show" -> SHOW
    | "unshow" -> UNSHOW
    | "empty" -> TESTEMPTY
    | "subset" -> SUBSET (*jade: a virer*)
    | "as" -> AS
    | "fun" ->  FUN
    | "in" -> IN
    | "undefined_unless" -> REQUIRES (* jade: deprecated?, indeed but still here ! *)
    | "flag" -> FLAG
    | "withco" -> WITHCO (*jade: a virer*) 
    | "withoutco" ->  WITHOUTCO (*jade: a virer*)
    | "withinit" -> WITHINIT  (*jade: a virer*) 
    | "withoutinit" ->  WITHOUTINIT (*jade: a virer*)    
    | "withsc" -> WITHSC  (*jade: a virer*)    
    | "withoutsc" -> WITHOUTSC (*jade: a virer*)
    | "include" -> INCLUDE
    | "begin" -> BEGIN
    | "end" -> END
    | "procedure" -> PROCEDURE
    | "call" -> CALL
    | "enum" -> ENUM
    | "debug" -> DEBUG
    | "match" -> MATCH
    | "with" -> WITH
    | "forall" -> FORALL
    | "from" -> FROM
    | "do" -> DO
    | "try" -> TRY
    | "if" -> IF
    | "then" -> THEN
    | "else" -> ELSE
    | "yield" -> YIELD
    (* for bell files *)
    | "instructions" -> INSTRUCTIONS
    | "default" -> DEFAULT
    | x -> VAR x

}

let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = '_' ? alpha (alpha|digit|'_' | '.' | '-')* '\''?

rule token = parse
| [' ''\t'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "//" [^'\n']* { token lexbuf }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf }
| '#' [^'\n']* '\n' { incr_lineno lexbuf ; token lexbuf }
| '('   { LPAR }
| ')'   { RPAR }
| '{'   { LACC }
| '}'   { RACC }
| '['   { LBRAC }
| ']'   { RBRAC }
| '_'   { UNDERSCORE }
| '0'   { EMPTY }
| '|'   { UNION }
| "||"  { ALT }
| '&'   { INTER }
| '*'   { STAR }
| '~'   { COMP }
| '+'   { PLUS }
| "++"  { PLUSPLUS }
| '^'   { HAT }
| "-1"  { INV }
| '\\'  { DIFF }
| '?'   { OPT }
| '='   { EQUAL }
| ';'   { SEMI }
| ','   { COMMA }
| "->"  { ARROW }
| '{'   { let buf = Buffer.create 4096 in
          get_body 0 buf lexbuf;
          LATEX (Buffer.contents buf)
        }
| '"' ([^'"']* as s) '"' { STRING s } (* '"' *)
| '\'' (name as x) { TAG x }
| name as x { check_keyword x }
| eof { EOF }
| ""  { error "Model lexer" lexbuf }

and get_body i buf = parse
| '\n' as lxm
    { Lexing.new_line lexbuf ;
      Buffer.add_char buf lxm ;
      get_body i buf lexbuf ; }
| '{' as lxm
    { Buffer.add_char buf lxm;
      get_body (succ i) buf lexbuf
    }
| '}' as lxm
    { if i > 0 then begin
       Buffer.add_char buf lxm;
       get_body (pred i) buf lexbuf
     end
    }
| eof { LexMisc.error "missing a closing brace" lexbuf }
| _ as lxm { Buffer.add_char buf lxm; get_body i buf lexbuf }

{
let token lexbuf =
   let tok = token lexbuf in
   if O.debug then begin
     Printf.eprintf
       "%a: Lexed '%s'\n"
       Pos.pp_pos2
       (lexeme_start_p lexbuf,lexeme_end_p lexbuf)
       (lexeme lexbuf)
   end ;
   tok
end
}
