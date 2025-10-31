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

(** Builds an {!AST.t} from some files. *)

type token = Tokens.token
type ast_type = [ `Opn | `Ast ]
type version = [ `ASLv0 | `ASLv1 ]
type version_selector = [ `ASLv0 | `ASLv1 | `Any ]
type parser_config = { v0_use_split_chunks : bool }

val default_parser_config : parser_config
(** The default parser configuration. It sets [v0_use_split_chunks] to [false].
*)

val from_file_result :
  ?ast_type:ast_type ->
  ?parser_config:parser_config ->
  version ->
  string ->
  AST.t Error.result

val from_file :
  ?ast_type:ast_type ->
  ?parser_config:parser_config ->
  version ->
  string ->
  AST.t

val from_lexer_lexbuf :
  ?ast_type:ast_type ->
  ?parser_config:parser_config ->
  version ->
  'a ->
  lexbuf ->
  AST.t Error.result

val from_file_multi_version :
  ?ast_type:ast_type ->
  ?parser_config:parser_config ->
  version_selector ->
  string ->
  AST.t Error.result

val from_string :
  ?ast_type:ast_type ->
  ?parser_config:parser_config ->
  filename:string ->
  ast_string:string ->
  version ->
  AST.t

val stdlib : AST.t Lazy.t
val with_stdlib : ?no_stdlib0:bool -> AST.t -> AST.t
val is_stdlib_name : AST.identifier -> bool

val with_primitives :
  ?loc:unit AST.annotated -> (AST.func * 'a) list -> AST.t -> AST.t
