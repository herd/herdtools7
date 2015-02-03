(*********************************************************************)
(*                          Litmus/DIY                               *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

{
exception Error
}

let blank = [' ''\n']
let digit = ['0'-'9']

rule main = parse
| ',' | blank+  { main lexbuf }
| digit+ as lxm { int_of_string lxm :: main lexbuf }
| eof { [] }
| "" { raise Error }

{

let ints s = main (Lexing.from_string s)

let pp_ints xs = String.concat "," (List.map string_of_int xs)

}
