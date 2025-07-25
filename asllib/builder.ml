(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

open Lexing

let _debug = false

type token = Tokens.token
type ast_type = [ `Opn | `Ast ]
type version = [ `ASLv0 | `ASLv1 ]

type parser_config = {
  allow_no_end_semicolon : bool;
  allow_expression_elsif : bool;
  allow_double_underscore : bool;
  allow_unknown : bool;
  allow_storage_discards : bool;
  allow_hyphenated_pending_constraint : bool;
  allow_local_constants : bool;
  allow_single_arrows : bool;
  allow_empty_structured_type_declarations : bool;
  allow_function_like_statements : bool;
}

type version_selector = [ `ASLv0 | `ASLv1 | `Any ]

let default_parser_config =
  {
    allow_no_end_semicolon = false;
    allow_expression_elsif = false;
    allow_double_underscore = false;
    allow_unknown = false;
    allow_storage_discards = false;
    allow_hyphenated_pending_constraint = false;
    allow_local_constants = false;
    allow_single_arrows = false;
    allow_empty_structured_type_declarations = false;
    allow_function_like_statements = false;
  }

let select_type ~opn ~ast = function
  | Some `Opn -> opn
  | Some `Ast -> ast
  | None -> ast

let _ast_type_to_string = select_type ~opn:"Opn" ~ast:"Ast"

let lexbuf_set_filename lexbuf pos_fname =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname };
  lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname }

let from_lexbuf ast_type parser_config version (lexbuf : lexbuf) =
  let open Error in
  let () =
    if _debug then
      Format.eprintf "Parsing %s from file %s@."
        (_ast_type_to_string ast_type)
        lexbuf.lex_curr_p.pos_fname
  in
  let cannot_parse lexbuf =
    fatal_here lexbuf.lex_start_p lexbuf.lex_curr_p CannotParse
  in
  let unknown_symbol lexbuf =
    fatal_here lexbuf.lex_start_p lexbuf.lex_curr_p UnknownSymbol
  in
  match version with
  | `ASLv1 -> (
      let module Parser = Parser.Make (struct
        let allow_no_end_semicolon = parser_config.allow_no_end_semicolon
        let allow_expression_elsif = parser_config.allow_expression_elsif
        let allow_storage_discards = parser_config.allow_storage_discards

        let allow_function_like_statements =
          parser_config.allow_function_like_statements

        let allow_hyphenated_pending_constraint =
          parser_config.allow_hyphenated_pending_constraint

        let allow_local_constants = parser_config.allow_local_constants

        let allow_empty_structured_type_declarations =
          parser_config.allow_empty_structured_type_declarations
      end) in
      let module Lexer = Lexer.Make (struct
        let allow_double_underscore = parser_config.allow_double_underscore
        let allow_unknown = parser_config.allow_unknown
        let allow_single_arrows = parser_config.allow_single_arrows

        let allow_function_like_statements =
          parser_config.allow_function_like_statements
      end) in
      let parse = select_type ~opn:Parser.opn ~ast:Parser.spec ast_type in
      try parse Lexer.token lexbuf with
      | Parser.Error -> cannot_parse lexbuf
      | Lexer.LexerError -> unknown_symbol lexbuf)
  | `ASLv0 -> (
      let parse = select_type ~opn:Gparser0.opn ~ast:Gparser0.ast ast_type
      and lexer0 = Lexer0.token () in
      try parse lexer0 lexbuf with Parser0.Error -> cannot_parse lexbuf)

let from_lexbuf' ast_type parser_config version lexbuf () =
  from_lexbuf ast_type parser_config version lexbuf

let close_after chan f =
  try
    let res = f () in
    close_in chan;
    res
  with e ->
    close_in_noerr chan;
    raise e

let open_file filename =
  let chan = open_in filename in
  let lexbuf = from_channel chan in
  lexbuf_set_filename lexbuf filename;
  (lexbuf, chan)

let from_file ?ast_type ?(parser_config = default_parser_config) version f =
  let lexbuf, chan = open_file f in
  close_after chan @@ from_lexbuf' ast_type parser_config version lexbuf

let from_file_result ?ast_type ?(parser_config = default_parser_config) version
    f =
  let lexbuf, chan = open_file f in
  close_after chan @@ Error.intercept
  @@ from_lexbuf' ast_type parser_config version lexbuf

let from_lexer_lexbuf ?ast_type ?(parser_config = default_parser_config) version
    _lexer lexbuf =
  Error.intercept (from_lexbuf' ast_type parser_config version lexbuf) ()

let from_file_multi_version ?ast_type ?parser_config = function
  | `Any -> (
      fun fname ->
        match from_file_result ?ast_type ?parser_config `ASLv0 fname with
        | Error e ->
            let () =
              Format.eprintf
                "@[Ignoring error on parser v0: %a.@ Trying with parser v1 \
                 ...@]@."
                Error.pp_error e
            in
            from_file_result ?ast_type ?parser_config `ASLv1 fname
        | Ok ast -> Ok ast)
  | (`ASLv0 | `ASLv1) as version ->
      from_file_result ?ast_type ?parser_config version

let from_string ?ast_type ?(parser_config = default_parser_config) ~filename
    ~ast_string version =
  let lexbuf = Lexing.from_string ~with_positions:true ast_string in
  lexbuf_set_filename lexbuf filename;
  from_lexbuf ast_type parser_config version lexbuf

let obfuscate prefix = ASTUtils.rename_locals (( ^ ) prefix)

let make_builtin d =
  let open AST in
  match d.desc with
  | D_Func f -> D_Func { f with builtin = true } |> ASTUtils.add_pos_from d
  | D_TypeDecl _ ->
      prerr_string "Type declaration cannot be builtin";
      exit 1
  | D_GlobalStorage _ ->
      prerr_string "Storage declaration cannot be builtin";
      exit 1
  | D_Pragma _ ->
      prerr_string "Pragma declaration cannot be builtin";
      exit 1

let stdlib =
  let filename = "ASL Standard Library" and ast_string = Asl_stdlib.stdlib in
  lazy
    (from_string ~filename ~ast_string ~ast_type:`Ast `ASLv1
    |> obfuscate "__stdlib_local_"
    |> List.map make_builtin)

let stdlib0 =
  let filename = "ASL Standard Library (V0 compatibility)"
  and ast_string = Asl_stdlib.stdlib0 in
  lazy
    (from_string ~filename ~ast_string ~ast_type:`Ast `ASLv0
    |> obfuscate "__stdlib_local_"
    |> List.map make_builtin)

let with_stdlib ast =
  ast
  |> List.rev_append (Lazy.force stdlib)
  |> List.rev_append (Lazy.force stdlib0)

let extract_name k d =
  let open AST in
  match d.desc with
  | D_Func { name; _ } -> name :: k
  | D_TypeDecl _ ->
      prerr_string "Type declaration in stdlib.asl";
      exit 1
  | D_GlobalStorage _ ->
      prerr_string "Storage declaration in stdlib.asl";
      exit 1
  | D_Pragma _ ->
      prerr_string "Pragma declaration in stdlib.asl";
      exit 1

let is_stdlib_name =
  let open ASTUtils in
  let set =
    lazy
      (let extract_names ds =
         List.fold_left extract_name [] ds |> ISet.of_list
       in
       Lazy.force stdlib |> extract_names)
  in
  fun name -> ISet.mem name (Lazy.force set)

let with_primitives ?(loc = ASTUtils.dummy_annotated) primitives ast =
  let primitive_decls =
    List.map
      AST.(
        fun (f, _) ->
          D_Func { f with builtin = true } |> ASTUtils.add_pos_from loc)
      primitives
    |> obfuscate "__primitive_local_"
  in
  ASTUtils.patch ~src:ast ~patches:primitive_decls
