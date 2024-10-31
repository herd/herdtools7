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

let desugar_setter call fields rhs =
  let loc = to_pos call and { desc = name, params, args } = call in
  let () = assert (loc.version = V1) in
  let here desc = add_pos_from loc desc in
  match fields with
  | [] ->
      (* Setter(rhs, ...); *)
      S_Call { name; args = rhs :: args; params; call_type = ST_Setter }
  | _ ->
      let temp = fresh_var "__setter_v1_temporary" in
      (* temp = Getter(...); *)
      let read =
        let getter_call =
          E_Call { name; args; params; call_type = ST_Getter } |> here
        in
        S_Decl (LDK_Var, LDI_Var temp, Some getter_call) |> here
      in
      (* temp.field = rhs OR temp.[field1, field2, ...] = rhs; *)
      let modify =
        let temp_le = LE_Var temp |> here in
        let lhs =
          match fields with
          | [ field ] -> LE_SetField (temp_le, field)
          | _ -> LE_SetFields (temp_le, fields, [])
        in
        S_Assign (lhs |> here, rhs) |> here
      in
      (* Setter(rhs, ...); *)
      let write =
        let temp_e = E_Var temp |> here in
        S_Call { name; args = temp_e :: args; params; call_type = ST_Setter }
        |> here
      in
      S_Seq (s_then read modify, write)
