(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

{
type t = Flush | Touch | TouchStore | Ignore

let as_int s = try int_of_string s with _ -> assert false

}

let alpha = ['a'-'z''A'-'Z''_']
let digit = ['0'-'9']
let name = alpha (alpha|digit)*
let num = digit+
rule main = parse
| [' ''\t''\n'',']+ { main lexbuf }
| (num as proc) ' '* ':' ' '*
  (name as id)  ' '* '=' ' '*
  ('T'|'I'|'F'|'W' as i)
{
 let i = match i with
 | 'I' -> Ignore
 | 'T' -> Touch
 | 'F' -> Flush
 | 'W' -> TouchStore
 | _ -> assert false in
 (as_int proc,id,i)::main lexbuf
}
| eof { [] }
| "" { assert false }

{
let parse s = main (Lexing.from_string s)
}

