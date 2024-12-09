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
  let loc = to_pos call and { desc = { name; params; args } } = call in
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
        S_Decl (LDK_Var, LDI_Var temp, None, Some getter_call) |> here
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

let desugar_elided_parameter ldk lhs ty (call : call annotated) =
  let bits_e =
    match ty.desc with
    | T_Bits (bits_e, []) -> bits_e
    | _ ->
        (* For example, let x = foo{,M}(args); cannot be desugared as there is
           no bits(_) annotation on the left-hand side *)
        Error.fatal_from (to_pos call) CannotParse
  in
  let params = bits_e :: call.desc.params in
  let rhs = E_Call { call.desc with params } |> add_pos_from call in
  S_Decl (ldk, lhs, None, Some rhs)

(* -------------------------------------------------------------------------
    Left-hand sides
   ------------------------------------------------------------------------- *)

type lhs_field = identifier annotated

type lhs_access = {
  base : identifier annotated;
  access : access;
  slices : slice list annotated;
}

and access =
  | Access_None
  | Access_Fields of lhs_field list
  | Access_Array of expr
  | Access_ArrayFields of expr * lhs_field list

let desugar_lhs_access { base; access; slices } =
  let var = LE_Var base.desc |> add_pos_from base in
  let nested_fields base fields =
    List.fold_left
      (fun acc field -> LE_SetField (acc, field.desc) |> add_pos_from field)
      base fields
  in
  let index_array base idx = LE_SetArray (base, idx) |> add_pos_from idx in
  let var_access =
    match access with
    | Access_None -> var
    | Access_Fields flds -> nested_fields var flds
    | Access_Array idx -> index_array var idx
    | Access_ArrayFields (idx, flds) -> nested_fields (index_array var idx) flds
  in
  match slices.desc with
  | [] -> var_access
  | _ -> LE_Slice (var_access, slices.desc) |> add_pos_from slices

let desugar_lhs_tuple laccess_opts =
  let bases =
    List.filter_map (Option.map (fun { base } -> base.desc)) laccess_opts.desc
  in
  match get_first_duplicate bases with
  | Some dup -> Error.fatal_from (to_pos laccess_opts) (MultipleWrites dup)
  | None ->
      let desugar_one = function
        | None -> LE_Discard |> add_pos_from laccess_opts
        | Some laccess -> desugar_lhs_access laccess
      in
      LE_Destructuring (List.map desugar_one laccess_opts.desc)
      |> add_pos_from laccess_opts
