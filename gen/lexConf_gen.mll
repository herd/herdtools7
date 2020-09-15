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
}
let blank = [' ''\t''\r']
let not_blank = [^' ''\t''\n''\r']
let alpha = ['A'-'Z' 'a'-'z']
let opt = '-' (('_'|alpha)+)

rule main = parse
| eof { [] }
| (opt as opt) blank* '\n' {opt :: main lexbuf}
| (opt as opt) blank+
  ((not_blank [^'\n']* not_blank | not_blank) as arg)
   blank*
 {opt :: arg :: main lexbuf}
| blank* '\n' {main lexbuf}
| '#' [^'\n']* '\n' {main lexbuf}
| "" {Warn.fatal "Bad conf file"}

{

let conf chan = main (Lexing.from_channel chan)

}
