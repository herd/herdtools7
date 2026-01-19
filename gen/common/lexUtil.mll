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

let pp = Ast.pp
}

let blank = [','' ''\t''\n''\r']
let not_blank = [^',' ' ' '\t' '\n' '\r' '[' ']']
let relexation = ['A'-'Z' 'a'-'z' '0'-'9' '-' '.' '*']

rule just_split = parse
| eof { [] }
| blank+ { just_split lexbuf }
| not_blank+ as lxm { lxm :: just_split lexbuf }
| "" { error "just_split" }
{

let split s =
  (* TODO *)
  let parsed = Lexing.from_string s |> Parser.main Lexer.token in
  let open Ast in
  let result = match parsed with
  | One _ -> [ parsed ]
  | Seq seq -> seq
  | Choice _ -> assert false
  | Multi _ -> assert false
  in
  result
let just_split s = just_split (Lexing.from_string s)

}
