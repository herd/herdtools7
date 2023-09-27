(******************************************************************************)
(*                                ASLRef                                      *)
(*                                                                            *)
(* Copyright (c) 2022-present, Arm Limited or its affiliates.                 *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(* SPDX-License-Identifier: Apache-2.0                                        *)
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

(** Builds an {!AST.t} from some files. *)

type token = Parser.token
type ast_type = [ `Opn | `Ast ]
type version = [ `ASLv0 | `ASLv1 ]
type version_selector = [ `ASLv0 | `ASLv1 | `Any ]

val from_file_result :
  ?ast_type:ast_type -> version -> string -> 'p AST.t Error.result

val from_file : ?ast_type:ast_type -> version -> string -> 'p AST.t

val from_lexer_lexbuf :
  ?ast_type:ast_type -> version -> 'a -> lexbuf -> 'p AST.t Error.result

val from_file_multi_version :
  ?ast_type:ast_type -> version_selector -> string -> 'p AST.t Error.result

val stdlib : unit AST.t Lazy.t
val with_stdlib : 'a AST.t -> 'a AST.t
