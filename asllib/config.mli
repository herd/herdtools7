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

val max_exploding_interval_exp : int ref
(** [max_exploding_interval_exp] denote the 2-exponent of the maximum size of
    intervals that will be exploded before any multiplication. *)

val allow_double_colon : bool ref
(** Allow [x :: ty] syntax to declare types of identifiers. *)

val allow_no_begin : bool ref
(** Allow removing the [begin] keyword at the begining of a function. *)

type typing_strictness = Silence | Warn | TypeCheck

val typing_strictness : typing_strictness ref
val default_loop_unrolling : int ref
val command_line_args : (string * Arg.spec * string) list
