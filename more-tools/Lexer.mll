(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

{
open Tokens
}

let alpha =['a'-'z''A'-'Z']
let digit = ['0'-'9']

rule token = parse
| [' ''\t'','';']+ { token lexbuf }
| '\n' { LexMisc.incr_lineno lexbuf ; token lexbuf }
| 'o' { ROUND }
| '.' { DOT }
| ':' { COLON }
| "-"|"--" { DASH }
| "++" { PLUS }
| alpha (alpha|digit|'-'|'/')* as lxm { WORD lxm }
| eof { EOF }
| "" { LexMisc.error "uoiam lexer" lexbuf }

{

}
