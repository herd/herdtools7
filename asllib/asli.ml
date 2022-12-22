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
(* Authors:                                                                 *)
(* Hadrien Renaud, University College London, UK.                           *)
(****************************************************************************)

open Asllib

(* Taken from herdtools7/lib/Pos.ml *)
let pp_pos chan pos =
  let open Lexing in
  Printf.fprintf chan "File \"%s\", line %i, character %i" pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)

let build_ast_from_file f =
  let lexbuf = Lexing.from_channel (open_in f) in
  let () =
    lexbuf.Lexing.lex_curr_p <-
      { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = f }
  in
  try Parser.ast Lexer.token lexbuf with
  | Parser.Error ->
      Printf.eprintf "%a: Cannot parse." pp_pos lexbuf.Lexing.lex_curr_p;
      exit 1
  | Lexer.LexerError ->
      Printf.eprintf "%a: unknown token." pp_pos lexbuf.Lexing.lex_curr_p;
      exit 1

let typing ast =
  let open Typing in
  try build_tenv ast with
  | TypingError (NotYetImplemented s) ->
      Printf.eprintf "Typing error: not yet implemented - %s" s;
      exit 1
  | TypingError (UndefinedIdentifier x) ->
      Printf.eprintf "Undefined identifier %s" x;
      exit 1
  | TypingError (TypeError s) ->
      Printf.eprintf "Type error: %s" s;
      exit 1
  | TypingError (Internal_InvalidScope s) ->
      Printf.eprintf "Typing internal error: bad scope '%s'" s;
      exit 1

let exec ast =
  let open Native in
  match NativeInterpreter.run ast [] [] () with
  | Ok _li -> Printf.printf "Ran ok.\n"
  | Error err -> Printf.printf "%a\n" pp_err err

let () =
  let f = Sys.argv.(1) in
  let () = Printf.printf "\r                                           \r" in
  let () = Printf.printf "Parsing %s...\n" f in
  let ast = build_ast_from_file f in
  let () = Printf.printf "Found the following AST:\n" in
  let () = Format.printf "%a\n\n@?" PP.pp_t ast in
  let () = Printf.printf "Typing %s...\n" f in
  let _ = typing ast in
  let () = Printf.printf "Running %s...\n" f in
  exec ast
