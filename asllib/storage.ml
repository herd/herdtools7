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

open ASTUtils

let _runtime_assertions = true

type pointer = int

module PMap = Map.Make (Int)
module PSet = Set.Make (Int)

type 'v t = { env : pointer IMap.t; mem : 'v PMap.t }

let alloc =
  let next = ref 0 in
  fun () ->
    let r = !next in
    next := r + 1;
    r

let empty = { env = IMap.empty; mem = PMap.empty }
let mem x t = IMap.mem x t.env

let assign x v t =
  let p = IMap.find x t.env in
  { t with mem = PMap.add p v t.mem }

let declare x v t =
  let () =
    if _runtime_assertions && mem x t then
      let () =
        Printf.eprintf "Storage element %s already declared in env.\n%!" x
      in
      assert false
  in
  let p = alloc () in
  { env = IMap.add x p t.env; mem = PMap.add p v t.mem }

let add x v t = try assign x v t with Not_found -> declare x v t

let find x t =
  let p = IMap.find x t.env in
  PMap.find p t.mem

let find_opt x t = try find x t with Not_found -> None

let remove x t =
  try
    let p = IMap.find x t.env in
    { mem = PMap.remove p t.mem; env = IMap.remove x t.env }
  with Not_found -> t

let patch_mem ~t_env ~t_mem to_avoid =
  let env = t_env.env
  and mem =
    try
      List.fold_left
        (fun mem x ->
          let p = IMap.find x t_mem.env in
          PMap.remove p mem)
        t_mem.mem to_avoid
    with Not_found ->
      let () =
        Printf.eprintf "Bug in unsetting one of ";
        List.iter (fun s -> Printf.eprintf "%s, " s) to_avoid;
        Printf.eprintf "\n%!"
      in
      assert false
  in
  { env; mem }
