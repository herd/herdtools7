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

type typing_strictness = Silence | Warn | TypeCheck
type output_format = HumanReadable | CSV

let max_exploding_interval_exp : int ref = ref 13
let allow_double_colon : bool ref = ref true
let allow_no_begin : bool ref = ref false
let typing_strictness : typing_strictness ref = ref TypeCheck
let default_loop_unrolling : int ref = ref 0
let output_format : output_format ref = ref HumanReadable

let command_line_args =
  let set_strictness s () = typing_strictness := s in
  [
    ( "--no-type-check",
      Arg.Unit (set_strictness Silence),
      " Do not type-check, only perform minimal type-inference. Default for v0."
    );
    ( "--type-check-warn",
      Arg.Unit (set_strictness Warn),
      " Do not type-check, only perform minimal type-inference. Log typing \
       errors on stderr." );
    ( "--type-check-strict",
      Arg.Unit (set_strictness TypeCheck),
      " Perform type-checking, Fatal on any type-checking error. Default for \
       v1." );
    ( "--allow-double-colon",
      Arg.Set allow_double_colon,
      " Allow double colon '::' to specify types." );
    ( "--disallow-double-colon",
      Arg.Clear allow_double_colon,
      " Disallow double colon '::' to specify types." );
    ( "--allow-no-begin",
      Arg.Set allow_no_begin,
      " Allow declaration of subprograms without any 'begin' keyword." );
    ( "--disallow-no-begin",
      Arg.Clear allow_no_begin,
      " Disallow declaration of subprograms without any 'begin' keyword." );
    ( "--default-loop-unrolling",
      Arg.Set_int default_loop_unrolling,
      " Set the default value for loop unrolling maximums for concurrent cases."
    );
    ( "--max-exploding-interval",
      Arg.Set_int max_exploding_interval_exp,
      " Set the log2 of the maximum width of an interval exploded before a \
       multiplication" );
    ( "--print-csv",
      Arg.Unit (fun () -> output_format := CSV),
      " Set the output format to CSV." );
  ]
