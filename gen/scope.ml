(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*       Luc Maranget INRIA Paris-Rocquencourt, France.              *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

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

