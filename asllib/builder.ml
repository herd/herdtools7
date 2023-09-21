(******************************************************************************)
(*                           the diy toolsuite                                *)
(*                                                                            *)
(* Jade Alglave, University College London, UK.                               *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
(*                                                                            *)
(* Copyright 2015-present Institut National de Recherche en Informatique et   *)
(* en Automatique and the authors. All rights reserved.                       *)
(*                                                                            *)
(* This software is governed by the CeCILL-B license under French law and     *)
(* abiding by the rules of distribution of free software. You can use,        *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B   *)
(* license as circulated by CEA, CNRS and INRIA at the following URL          *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.              *)
(******************************************************************************)
(* Authors:                                                                   *)
(* Hadrien Renaud, University College London, UK.                             *)
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

type token = Parser.token
type ast_type = [ `Opn | `Ast ]
type version = [ `ASLv0 | `ASLv1 ]
type version_selector = [ `ASLv0 | `ASLv1 | `Any ]

let select_type ~opn ~ast = function
  | Some `Opn -> opn
  | Some `Ast -> ast
  | None -> ast

let _ast_type_to_string = select_type ~opn:"Opn" ~ast:"Ast"

let from_lexbuf ast_type version (lexbuf : lexbuf) =
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
      let parse = select_type ~opn:Parser.opn ~ast:Parser.ast ast_type in
      try parse Lexer.token lexbuf |> ASTUtils.no_primitive with
      | Parser.Error -> cannot_parse lexbuf
      | Lexer.LexerError -> unknown_symbol lexbuf)
  | `ASLv0 -> (
      let parse = select_type ~opn:Gparser0.opn ~ast:Gparser0.ast ast_type
      and lexer0 = Lexer0.token () in
      try parse lexer0 lexbuf |> ASTUtils.no_primitive
      with Parser0.Error -> cannot_parse lexbuf)

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
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  (lexbuf, chan)

let from_file ?ast_type version f =
  let lexbuf, chan = open_file f in
  close_after chan @@ fun () -> from_lexbuf ast_type version lexbuf

let from_file_result ?ast_type version f =
  let lexbuf, chan = open_file f in
  let res =
    Error.intercept (fun () -> from_lexbuf ast_type version lexbuf) ()
  in
  close_in chan;
  res

let from_lexer_lexbuf ?ast_type version _lexer lexbuf =
  Error.intercept (fun () -> from_lexbuf ast_type version lexbuf) ()

let from_file_multi_version ?ast_type = function
  | `Any -> (
      fun fname ->
        match from_file_result ?ast_type `ASLv0 fname with
        | Error e ->
            let () =
              Format.eprintf
                "@[Ignoring error on parser v0: %a.@ Trying with parser v1 \
                 ...@]@."
                Error.pp_error e
            in
            from_file_result ?ast_type `ASLv1 fname
        | Ok ast -> Ok ast)
  | (`ASLv0 | `ASLv1) as version -> from_file_result ?ast_type version

let rec list_first_opt f = function
  | [] -> None
  | h :: t -> ( match f h with Some x -> Some x | None -> list_first_opt f t)

let asl_libdir = Filename.concat Version.libdir "asllib"

let stdlib_not_found_message =
  {|Asllib cannot find stdlib.asl. If you have installed herdtools7, it should
be at: |}
  ^ asl_libdir
  ^ {|
herdtools7 builds are dependent on the installation path, please rebuild the
whole herdtools7 toolsuite if you want to move the executables or their
libraries. For example, if you want to move the installed files from
$OLD_PREFIX to $NEW_PREFIX, you should run:
  make uninstall PREFIX=$OLD_PREFIX
  make build install PREFIX=$NEW_PREFIX
|}

let stdlib =
  (* Share code with: lib/myLib ? *)
  let ( / ) = Filename.concat in
  let to_try =
    [ asl_libdir; "asllib" / "libdir"; "/jherd" (* For web interface *) ]
  in
  let to_try =
    match Sys.getenv_opt "ASL_LIBDIR" with
    | None ->
        let () = if _debug then Printf.eprintf "Cannot find ASL_LIBDIR.\n" in
        to_try
    | Some p -> p :: to_try
  in
  let to_try =
    match Sys.getenv_opt "DUNE_SOURCEROOT" with
    | None ->
        let () =
          if _debug then Printf.eprintf "Cannot find DUNE_SOURCEROOT\n"
        in
        to_try
    | Some p -> (p / "asllib" / "libdir") :: to_try
  in
  let try_one dir_path =
    let file_path = dir_path / "stdlib.asl" in
    if Sys.file_exists file_path then
      match from_file_result ~ast_type:`Ast `ASLv1 file_path with
      | Ok ast ->
          let () =
            if _debug then Printf.eprintf "Selecting stdlib from %s.\n" dir_path
          in
          Some ast
      | Error _ ->
          let () =
            if _debug then Printf.eprintf "Cannot parse %s.\n" file_path
          in
          None
    else None
  in
  lazy
    (match list_first_opt try_one to_try with
    | Some ast -> ast
    | None ->
        let () = prerr_string stdlib_not_found_message in
        exit 1)

let with_stdlib ast =
  List.rev_append (Lazy.force stdlib |> ASTUtils.no_primitive) ast
