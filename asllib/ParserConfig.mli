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

(** The CONFIG module signature for the ASL1 Parser *)
module type CONFIG = sig
  val allow_no_end_semicolon : bool
  (** Allow no semicolon after [end]. *)

  val allow_expression_elsif : bool
  (** Allow [elsif] at the expression level. *)

  val allow_storage_discards : bool
  (** Allow storage declarations to discard their right-hand sides. *)

  val allow_hyphenated_pending_constraint : bool
  (** Allow pending constrained integer types to be denoted by a hyphen. *)

  val allow_local_constants : bool
  (** Allow declarations of local constant storage. *)

  val allow_empty_structured_type_declarations : bool
  (** Allow declarations of structured types with implicitly empty fields. *)
end
