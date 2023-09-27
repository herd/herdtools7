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
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
(* Jade Alglave, Arm Ltd and UCL, UK.                                         *)
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

(** The Typing module is yet a single-entry-point module. It only exports the 
    function [annotate_ast] which fills type-annotations holes in the AST.
    It should provide enough information to disambiguate any type-dependent
    behaviour. *)

val infer_value : AST.literal -> AST.type_desc

type strictness = [ `Silence | `Warn | `TypeCheck ]
(** Possible strictness of type-checking. *)

val type_check_ast :
  strictness -> 'p AST.t -> StaticEnv.env -> 'p AST.t * StaticEnv.env
(** Typechecks the AST, and returns an AST with type inference holes filled.

    @raise Error.ASLException if the AST does not type-checks.
*)
