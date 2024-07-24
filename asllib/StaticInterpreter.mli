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

(** Static Interpretation of Expressions. *)

exception StaticEvaluationUnknown

val static_eval : StaticEnv.env -> AST.expr -> AST.literal
(** [static_eval env e] statically evaluates [e] in [env] into a literal.
    @raise ASLException if the a type error is detected or the expression is
        not one of the following: [E_Literal], [E_Var], [E_Binop], [E_Unop],
        [E_Slice], or [E_Cond].
    @raise UnsupportedExpr if the given expression cannot evaluate to a literal.
*)

val slices_to_positions : StaticEnv.env -> AST.slice list -> int list
(** [slices_to_positions slices] statically evaluates [slices] and, unless a type error
    is detected, returns a list of indices represented by them.
*)
