(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Scope tags *)

type t =
  | No
  | Default
  | One of BellInfo.scopes
  | Gen of (string * int * int) list
  | All
let tags = ["none";"default";"all";"(<scope>:<int>:<int>)+"]

let parse_gen tag =
  let i = String.index_from tag 0 ':' in
  let j = String.index_from tag (i+1) ':' in
  let sc = String.sub tag 0 i in
  let min = int_of_string (String.sub tag (i+1) (j-i-1)) in
  let max = int_of_string (String.sub tag (j+1) (String.length tag-(j+1))) in
  sc,min,max

let some_colon s =
  try ignore (String.index s ':') ; true
  with Not_found -> false

let parse tag = match tag with
| "none"|"no" -> Some No
| "default"|"def" -> Some Default
| "all" -> Some All
| _ ->
    if some_colon tag then
      begin try
        let tags = LexSplit.strings tag in
        let t =
          Gen
            (List.map
               (fun tag -> parse_gen tag)
               tags) in
        Some t
      with _ -> None
      end
    else
      let module Lexer = ScopeLexer.Make(LexUtils.Default) in
      let lexbuf = Lexing.from_string tag in
      let st =
        GenParser.call_parser "_none_" lexbuf
          Lexer.token ScopeParser.top_scope_tree in
      Some (One st)

