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

  let do_check_keyword = function
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
    | "assert" -> ASSERT
    | "include" -> INCLUDE
    | "variant" -> VARIANT
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
    (* Model option *)
    | "catdep" -> CATDEP
    (* for bell files *)
    | "instructions" -> INSTRUCTIONS
    | "default" -> DEFAULT
    | "not" -> LNOT
    | x -> VAR x


  open LexItem

  let check_keyword f lxm =
    let r = do_check_keyword lxm in
    begin match r with
    | VAR _ -> f Ord lxm
    | _ -> f Keyword lxm
    end ;
    r

  let wrap_comment f =
    let buff = Buffer.create 16 in
    let add_c c = Buffer.add_char buff c
    and over () = f Comment (Buffer.contents buff) in
    add_c '(' ; add_c '*' ;
    add_c,over
}

let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = '_' ? alpha (alpha|digit|'_' | '.' | '-')* '\''?

rule token f = parse
| [' ''\t''\r']+ as lxm { f Blank lxm ; token f lexbuf }
| '\n'      { incr_lineno lexbuf; f Blank "\n" ; token f lexbuf }
| ("//"|"#") [^'\n']* as lxm { f Comment lxm ; token f lexbuf }
| "(*"
    { let add,over = wrap_comment f in
      LU.skip_comment_fun add lexbuf ;
      over () ;
      token f lexbuf }
| '('   { f Delim "(" ; LPAR }
| ')'   { f Delim ")" ; RPAR }
| '{'   { f Delim "{" ; LACC }
| '}'   { f Delim "}" ; RACC }
| '['   { f Delim "[" ; LBRAC }
| ']'   { f Delim "]" ; RBRAC }
| '_'   { f Keyword "_" ; UNDERSCORE }
| '0'   { f Keyword "0" ; EMPTY }
| '|'   { f Operator "|" ; UNION }
| "||"  { f Operator "||" ; ALT }
| '&'   { f Operator "&" ; INTER }
| "&&"  { f Operator "&&" ; LAND }
| '*'   { f Operator "*" ; STAR }
| '~'   { f Operator "~" ; COMP }
| '+'   { f Operator "+" ; PLUS }
| "++"  { f Operator "++" ; PLUSPLUS }
| '^'   { f Operator "^" ; HAT }
| "-1"  { f Operator "-1" ; INV }
| '\\'  { f Operator "\\" ; DIFF }
| '?'   { f Operator "?" ; OPT }
| '='   { f Operator "=" ; EQUAL }
| ';'   { f Operator ";" ; SEMI }
| ','   { f Operator "," ; COMMA }
| "->"  { f Operator "->" ; ARROW }
| '"' ([^'"']* as s) '"' as lxm { f String lxm ; STRING s } (* '"' *)
| '\'' (name as x) as lxm { f Ord lxm ; TAG x }
| name as x { check_keyword f x }
| eof { f Eof "" ; EOF }
| ""  { error "Model lexer" lexbuf }

{
let do_token f lexbuf =
   let tok = token f lexbuf in
   if O.debug then begin
     Printf.eprintf
       "%a: Lexed '%s'\n"
       Pos.pp_pos2
       (lexeme_start_p lexbuf,lexeme_end_p lexbuf)
       (lexeme lexbuf)
   end ;
   tok

let token lexbuf = do_token Misc.ing2 lexbuf

let token_fun f lexbuf = ignore (do_token f lexbuf)

end
}
