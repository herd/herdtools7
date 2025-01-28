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

(*
  This file describes the bnfc AST and defines a few utility functions to print and update the contained data
 *)

(* Define BNFC data types *)
type term =
  | Literal of string
  (* Literal which is referenced via a token name. Can be replaced by a literal in the future *)
  | LitReference of string
  | Reference of string

type decl = Decl of { ast_name : string; name : string; terms : term list }
type t = { entrypoints : string list; decls : decl list }

(*
   BNFC data structure utilities
 *)

(** Group decl lists into a list of lists where each sub-list has a common name *)
let rec group_by_name decl_list =
  match decl_list with
  | [] -> []
  | Decl { name } :: _ ->
      let common, rest =
        List.partition
          (fun (Decl { name = name2 }) -> String.equal name name2)
          decl_list
      in
      common :: group_by_name rest

(** Conevert bnfc declaration terms to string *)
let string_of_term term =
  match term with
  | Literal str -> "\"" ^ str ^ "\""
  | LitReference id | Reference id -> id

let string_of_entrypoints eps =
  Printf.sprintf "entrypoints %s;" (String.concat ", " eps)

(** Conert the bnfc type to string *)
let string_of_bnfc bnfc =
  let print_decl_list decl_list =
    let longest_name (Decl { ast_name }) acc =
      max acc (String.length ast_name)
    in
    let max_name = List.fold_right longest_name decl_list 0 in
    let print_decl (Decl { ast_name; name; terms }) =
      let terms_str = String.concat " " @@ List.map string_of_term terms in
      let space = String.make (max_name - String.length ast_name) ' ' in
      Printf.sprintf "%s. %s%s ::= %s;" ast_name space name terms_str
    in
    let decl_strs = List.map print_decl decl_list in
    String.concat "\n" decl_strs
  in
  let grouped_decls = group_by_name bnfc.decls in
  let eps = string_of_entrypoints bnfc.entrypoints in
  let decl_strs = List.map print_decl_list grouped_decls in
  String.concat "\n\n" (eps :: decl_strs)

(** Given a sorting order of the generated BNFC names. Order the bnfc ast
    using the order of the names specified *)
let sort_bnfc bnfc order =
  let sort_fn el1 el2 =
    let get_idx (Decl { name }) =
      let idx_opt = Utils.list_find_index (String.equal name) order in
      Option.value ~default:Int.max_int idx_opt
    in
    let l = get_idx el1 in
    let r = get_idx el2 in
    Int.compare l r
  in
  (* Sanity check that all order names actually exsit *)
  let () =
    let name_not_found =
      List.filter
        (fun n ->
          not
          @@ List.exists (fun (Decl { name }) -> String.equal name n) bnfc.decls)
        order
    in
    if List.length name_not_found > 0 then (
      Printf.printf
        "The following names specified in the order list don't exist [%s]\n"
      @@ String.concat ", " name_not_found;
      raise @@ Failure "Order file error.")
  in
  { bnfc with decls = List.sort sort_fn bnfc.decls }

(** Convert the bnfc ast into a simpler format which exludes AST information *)
let simplified_bnfc bnfc =
  let snake_case_id name =
    let cvt_char idx c =
      let is_upper c = match c with 'A' .. 'Z' -> true | _ -> false in
      if not @@ is_upper c then String.make 1 c
      else
        let lower = String.make 1 @@ Char.lowercase_ascii c in
        if Int.equal idx 0 then lower else "_" ^ lower
    in
    List.init (String.length name) (String.get name)
    |> List.mapi cvt_char |> String.concat ""
  in
  let print_terms (Decl { terms }) =
    let print_term term =
      match term with
      | Reference id -> snake_case_id id
      | _ -> string_of_term term
    in
    let str_terms = List.map print_term terms in
    String.concat " " str_terms
  in
  let print_decl decl_list =
    let name = match List.hd decl_list with Decl { name } -> name in
    let id = snake_case_id name in
    let padding_len = String.length id + 3 in
    (* + " ::" *)
    let sep = Printf.sprintf "\n%s| " @@ String.make padding_len ' ' in
    Printf.sprintf "%s ::= %s" id
      (String.concat sep @@ List.map print_terms decl_list)
  in
  let grouped_decls = group_by_name bnfc.decls in
  let eps = Printf.sprintf "// %s" (string_of_entrypoints bnfc.entrypoints) in
  let decls = List.map print_decl grouped_decls |> String.concat "\n\n" in
  String.concat "\n\n" [ eps; decls ]
