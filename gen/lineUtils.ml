(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*                 Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

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
            GenParser.call_parser "_none_" lexbuf
              Lexer.token ScopeParser.top_scope_tree in
          Some st in
      name,es,st
          with
          | Not_found | Invalid_argument _ ->
              Warn.fatal "bad line: %s" s

end
