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

open AST
open ASTUtils
module SEnv = StaticEnv

let ( |: ) = Instrumentation.TypingNoInstr.use_with

module InterpConf = struct
  module Instr = Instrumentation.SemanticsNoInstr

  let unroll = 0
  let error_handling_time = Error.Static
  let log_nondet_choice = false (* Not relevant here *)
end

module SB = Native.StaticBackend
module SI = Interpreter.Make (Native.StaticBackend) (InterpConf)

let eval_from ~loc env e =
  try SI.eval_expr env e
  with
  | Error.(ASLException { pos_start; pos_end; desc; _ })
  when pos_start == dummy_pos && pos_end == dummy_pos
  ->
    Error.fatal_from loc desc

(* Begin StaticEval *)
let static_eval (senv : SEnv.env) (e : expr) : literal =
  let env =
    let open SI.IEnv in
    let global =
      global_from_static senv.global
        ~storage:(Storage.map SB.v_of_literal senv.SEnv.global.constant_values)
    and local =
      local_empty_scoped
        (SB.Scope.global ~init:true)
        ~storage:(Storage.map SB.v_of_literal senv.SEnv.local.constant_values)
    in
    { global; local }
  in
  match eval_from ~loc:e env e with
  | SI.Normal (Native.NV_Literal l, _env) ->
      l |: Instrumentation.TypingRule.StaticEval
  | SI.Normal _ | SI.Throwing _ ->
      Error.fatal_from e (UnsupportedExpr (Static, e))
(* End *)

let static_eval_to_int env e =
  match static_eval env e with L_Int z -> Z.to_int z | _ -> assert false
