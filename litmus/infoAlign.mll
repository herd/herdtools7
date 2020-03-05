(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

{
 let as_int s = try int_of_string s with _ -> assert false
}

let alpha = ['a'-'z''A'-'Z''_']
let digit = ['0'-'9']
let name = alpha (alpha|digit)*
let num = digit+
rule main = parse
| [' ''\t''\n'',']+ { main lexbuf }
| (name as id) ' '* ':' ' '* (num as i)
    { (id,as_int i) :: main lexbuf } 
| eof { [] }
| "" { assert false }

{
let parse s = main (Lexing.from_string s)
}

