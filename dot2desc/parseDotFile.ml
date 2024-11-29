(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2024-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type Config = sig
  val debuglexer : bool
end

module Make(C: Config) : sig
  val parse_file : string -> ParsedDotGraph.t list
end = struct
  let do_parse_file channel =
    let module DotLexer = DotLexer.Make(struct
      let debug = C.debuglexer
    end) in
    let lexbuf = Lexing.from_channel channel in
    try
      DotParser.main DotLexer.token lexbuf
    with
    | DotParser.Error ->
      Printf.eprintf "Syntax error at position %d\n" (Lexing.lexeme_start lexbuf);
      exit 1
    | e -> raise e

  let parse_file filename =
    Misc.input_protect do_parse_file filename
end
