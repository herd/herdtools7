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
module TypingRule = Instrumentation.TypingRule

let ( |: ) = Instrumentation.TypingNoInstr.use_with

let ( |: ) = Instrumentation.TypingNoInstr.use_with
let fatal_from = Error.fatal_from
let unsupported_expr e = fatal_from e Error.(UnsupportedExpr (Static, e))

exception StaticEvaluationUnknown

let int_max x y = if x >= y then x else y

let rec static_eval (env : SEnv.env) : expr -> literal =
  let rec expr_ e =
    match e.desc with
    | E_Literal v -> v
    | E_Var x -> (
        try SEnv.lookup_constants env x
        with Not_found ->
          let () =
            if false then
              Format.eprintf "Failed to lookup %S in env: %a@." x
                StaticEnv.pp_env env
          in
          if SEnv.is_undefined x env then
            Error.fatal_from e (Error.UndefinedIdentifier x)
          else raise StaticEvaluationUnknown)
    | E_Binop (op, e1, e2) ->
        let v1 = expr_ e1 and v2 = expr_ e2 in
        Operations.binop_values e Error.Static op v1 v2
    | E_Unop (op, e) ->
        let v = expr_ e in
        Operations.unop_values e Error.Static op v
    | E_Slice (e', slices) ->
        let positions = slices_to_positions env slices in
        let pos_max = List.fold_left int_max 0 positions in
        let bv =
          match expr_ e' with
          | L_Int i -> Bitvector.of_z (pos_max + 1) i
          | L_BitVector bv when Bitvector.length bv > pos_max -> bv
          | v ->
              fatal_from e
              @@ Error.MismatchType
                   (PP.literal_to_string v, [ integer'; default_t_bits ])
        in
        L_BitVector (Bitvector.extract_slice bv positions)
    | E_Cond (e_cond, e1, e2) ->
        let v_cond = expr_ e_cond in
        let b =
          match v_cond with
          | L_Bool b -> b
          | _ ->
              fatal_from e
              @@ Error.MismatchType (PP.literal_to_string v_cond, [ T_Bool ])
        in
        if b then expr_ e1 else expr_ e2
    | _ -> unsupported_expr e
  in
  expr_ |: TypingRule.StaticEval
  expr_ |: TypingRule.StaticEval

and slices_to_positions env =
  let check_positive e x =
    if x >= 0 then x
    else fatal_from e @@ Error.MismatchType (string_of_int x, [ integer' ])
  in
  let eval_to_int e =
    static_eval env e |> Operations.value_as_int e |> check_positive e
  in
  let slice_to_positions =
    let interval top len = List.init len (( - ) top) in
    function
    | Slice_Single e -> [ eval_to_int e ]
    | Slice_Range (etop, ebot) ->
        let pbot = eval_to_int ebot and ptop = eval_to_int etop in
        interval ptop (ptop - pbot + 1)
    | Slice_Length (ebot, elength) ->
        let pbot = eval_to_int ebot and plength = eval_to_int elength in
        let ptop = pbot + plength - 1 in
        interval ptop plength
    | Slice_Star (efactor, elength) ->
        let pfactor = eval_to_int efactor and plength = eval_to_int elength in
        let ptop = (pfactor * plength) + plength - 1 in
        interval ptop plength
  in
  fun slices -> slices |> List.map slice_to_positions |> List.concat
