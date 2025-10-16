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

exception Error of string
let error msg = raise (Error msg)

type t =
  | One of string
  | Seq of string list

let pp = function
| One s -> Printf.sprintf "One(%s)" s
| Seq ss ->
  Printf.sprintf "Seq(%s)" (String.concat "," ss)
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
