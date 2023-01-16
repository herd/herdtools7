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

let exec ast =
  let open Native in
  try
    let _ = NativeInterpreter.run ast [] [] () in
    ()
  with NativeInterpreterExn e -> Printf.printf "%a\n" pp_err e

type args = {
  exec : bool;
  file : string;
  print_ast : bool;
  print_serialized : bool;
}

let parse_args : unit -> args =
  let target_file = ref "" in
  let exec = ref true in
  let print_ast = ref false in
  let print_serialized = ref false in
  let speclist =
    [
      ("--only-parse", Arg.Clear exec, "Do not execute the asl program.");
      ("--parse-only", Arg.Clear exec, "Do not execute the asl program.");
      ("--exec", Arg.Set exec, "Execute the asl program.");
      ( "--print",
        Arg.Set print_ast,
        "Print the parsed AST to stdout before executing it." );
      ( "--serialize",
        Arg.Set print_serialized,
        "Print the parsed AST to stdout in the serialized format." );
    ]
  in
  let anon_fun = ( := ) target_file in
  let usage_msg =
    "ASL parser and interpreter.\n\nUSAGE:\n\tasli [OPTIONS] [FILE]\n\n"
  in
  fun () ->
    let () = Arg.parse speclist anon_fun usage_msg in
    {
      exec = !exec;
      file = !target_file;
      print_ast = !print_ast;
      print_serialized = !print_serialized;
    }

let () =
  let args = parse_args () in
  let ast = build_ast_from_file args.file in

  let () = if args.print_ast then Format.printf "%a@." PP.pp_t ast in

  let () =
    if args.print_serialized then print_string (Serialize.t_to_string ast)
  in

  let () = if args.exec then exec ast in

  ()
