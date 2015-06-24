(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

{

exception Error of string
let error msg = raise (Error msg)

type t =
  | One of string
  | Seq of string list

}
let blank = [','' ''\t''\n''\r']
let not_blank = [^','' ''\t''\n''\r' '[' ']']

rule main = parse
| eof { [] }
| '['
{
 let seq = pseq lexbuf in
 Seq seq::main lexbuf
}
| blank+ { main lexbuf }
| not_blank+ as lxm { One lxm :: main lexbuf }
| "" { error "main" }

and pseq = parse
| eof { failwith "] missing" }
| ']' { [] }
| blank+ { pseq lexbuf }
| not_blank+ as lxm { lxm :: pseq lexbuf }
| "" { error "pseq" }

and just_split = parse
| eof { [] }
| blank+ { just_split lexbuf }
| not_blank+ as lxm { lxm :: just_split lexbuf }
| "" { error "just_split" }
{


let split s = main (Lexing.from_string s)
let just_split s = just_split (Lexing.from_string s)

}
