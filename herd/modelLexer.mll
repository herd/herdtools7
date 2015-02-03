(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


{
module Make(O:LexUtils.Config) = struct
open Lexing
open LexMisc
open ModelParser
module LU = LexUtils.Make(O)

(* Efficient from ocaml 4.02 *)

  let check_keyword = function
    | "MM" -> MM
    | "MR" -> MR
    | "MW" -> MW
    | "WM" -> WM
    | "WW" -> WW
    | "WR" -> WR
    | "RM" -> RM
    | "RW" -> RW
    | "RR" -> RR
    | "AA" -> AA
    | "AP" -> AP
    | "PA" -> PA
    | "PP" -> PP
    | "let" -> LET
    | "rec" -> REC
(*
    | "set" -> SET
    | "rln" -> RLN
*)
    | "and" -> AND
    | "acyclic" -> ACYCLIC
    | "irreflexive" -> IRREFLEXIVE
    | "show" -> SHOW
    | "unshow" -> UNSHOW
    | "empty" -> TESTEMPTY
    | "as" -> AS
    | "fun" ->  FUN
    | "in" -> IN
    | "undefined_unless" -> REQUIRES
    | "ext" -> EXT
    | "int" -> INT
    | "sameloc" -> SAMELOC
    | "noid" -> NOID
    | "withco" -> WITHCO
    | "withoutco" ->  WITHOUTCO
    | "withinit" -> WITHINIT
    | "withoutinit" ->  WITHOUTINIT
    | "withsc" -> WITHSC
    | "withoutsc" -> WITHOUTSC
    | "include" -> INCLUDE
    | "begin" -> BEGIN
    | "end" -> END
    | "procedure" -> PROCEDURE
    | "checkcall" -> CHECKCALL
    | "call" -> CALL
    | "enum" -> ENUM
    | "debug" -> DEBUG
    | "match" -> MATCH
    | "with" -> WITH
    | "foreach" -> FOREACH
    | "fororder" -> FORORDER
    | "from" -> FROM
    | "do" -> DO
    | x -> VAR x


}

let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = alpha (alpha|digit|'_' | '.' | '-')* '\''?

rule token = parse
| [' ''\t'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf }
| '#' [^'\n']* '\n' { incr_lineno lexbuf ; token lexbuf }
| '('   { LPAR }
| ')'   { RPAR }
| '['   { LBRAC }
| ']'   { RBRAC }
| '{'   { LACC }
| '}'   { RACC }
| '_'   { UNDERSCORE }
| '0'   { EMPTY }
| '|'   { UNION }
| "||"  { ALT }
| '&'   { INTER }
| '*'   { STAR }
| '~'   { COMP }
| '!'   { NOT }
| '+'   { PLUS }
| "++"  { PLUSPLUS }
| '^'   { HAT }
| '2'   { TWO }
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
