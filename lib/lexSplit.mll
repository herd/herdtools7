(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

{
exception Error
}

let blank = [' ''\n''\r''\t']
let digit = ['0'-'9']
let printable = ['0'-'9''a'-'z''A'-'Z'':']
rule main = parse
| ',' | blank+  { main lexbuf }
| digit+ as lxm { int_of_string lxm :: main lexbuf }
| eof { [] }
| "" { raise Error }

and strings = parse
| ',' { strings lexbuf }
| [^',']+ as lxm { lxm :: strings lexbuf }
| eof { [] }
| "" { raise Error }

and strings_spaces = parse
| (','|blank)+ { strings_spaces lexbuf }
| printable+ as lxm { lxm :: strings_spaces lexbuf }
| eof { [] }
| "" { raise Error }

{

let ints s = main (Lexing.from_string s)
let strings s = strings (Lexing.from_string s)
let strings_spaces s = strings_spaces (Lexing.from_string s)

let pp_ints xs = String.concat "," (List.map string_of_int xs)

}
