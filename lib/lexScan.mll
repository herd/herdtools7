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

(** Miscellaneous lexers *)

{

}

let digit = ['0'-'9']
let num = ['1'-'9']digit*
let hexa_digit = (digit|['a'-'f''A'-'F'])
let hexa_num = ("0x"|"0X")hexa_digit+
let alpha = [ 'a'-'z' 'A'-'Z']
let blank = [' ' '\t' '\r']
let not_blank = [^' ''\t''\r']
let name  = alpha (alpha|digit|'_' | '/' | '.' | '-')*

rule num_rule = parse
| blank* (num|hexa_num) blank* eof { true }
| ""  { false }

and info_rule = parse
| (name as key) blank* '=' blank* (_* as value) blank* eof
  { let p = key,value in Some p }
| "" { None }
{
let is_num s = num_rule (Lexing.from_string s)
let info s = info_rule (Lexing.from_string s)
}
