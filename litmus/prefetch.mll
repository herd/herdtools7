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
