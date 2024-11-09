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
end

module SB = Native.StaticBackend
module SI = Interpreter.Make (Native.StaticBackend) (InterpConf)

let static_eval (senv : SEnv.env) (e : expr) : literal =
  let funcs =
    IMap.map
      (fun (func : AST.func) -> (ref 0, func))
      senv.SEnv.global.subprograms
  and global_storage =
    IMap.map SB.v_of_literal senv.SEnv.global.constant_values
    |> Storage.of_v_map
  and local_v_map = IMap.map SB.v_of_literal senv.SEnv.local.constant_values in
  let env =
    let open SI.IEnv in
    {
      global = { static = senv.global; funcs; storage = global_storage };
      local = local_of_v_map local_v_map;
    }
  in
  let res =
    try SI.eval_expr env e with
    | Error.(ASLException { desc = UndefinedIdentifier x; _ })
      when not (SEnv.is_undefined x senv) ->
        raise SB.StaticEvaluationUnknown
    | Error.(ASLException { pos_start; pos_end; desc; _ })
      when pos_start == dummy_pos && pos_end == dummy_pos ->
        Error.fatal_from e desc
  in
  match res with
  | SI.Normal (Native.NV_Literal l, _env) ->
      l |: Instrumentation.TypingRule.StaticEval
  | SI.Normal _ | SI.Throwing _ ->
      Error.fatal_from e (UnsupportedExpr (Static, e))
