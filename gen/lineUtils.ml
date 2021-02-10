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

(** Parse a line of edges + optional scope tree *)

module type E = sig
  type edge
  val parse_edges : string -> edge list
end

module Make(E:E) :sig
  val parse : string ->  string * E.edge list * BellInfo.scopes option
end = struct

  let parse s =
    try
      let r = String.index s ':' in
      let name  = String.sub s 0 r
      and es = String.sub s (r+1) (String.length s - (r+1)) in
      let es,st =
        try
          let r = String.index es '(' in
          let es = String.sub es 0 r
          and st = String.sub es r (String.length es - r) in
          es,Some st
        with Not_found -> es,None in
      let es = E.parse_edges es in
      let st = match st with
      | None -> None
      | Some st ->
          let module Lexer = ScopeLexer.Make(LexUtils.Default) in
          let lexbuf = Lexing.from_string st in
          let st =
            GenParserUtils.call_parser "_none_" lexbuf
              Lexer.token ScopeParser.main in
          Some st in
      name,es,st
          with
          | Not_found | Invalid_argument _ ->
              Warn.fatal "bad line: %s" s

end
