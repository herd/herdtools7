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

type 'v t = 'v IMap.t

let empty = IMap.empty
let mem x t = IMap.mem x t

let assign x v t =
  IMap.update x (function None -> raise Not_found | Some _ -> Some v) t

let declare x v t =
  if _runtime_assertions then
    IMap.update x
      (function
        | None -> Some v
        | Some _ ->
            let () =
              Printf.eprintf "Storage element %s already declared in env.\n%!" x
            in
            assert false)
      t
  else IMap.add x v t

let of_v_map map = map
let add x v t = IMap.add x v t
let find x t = IMap.find x t
let find_opt x t = IMap.find_opt x t
let remove x t = IMap.remove x t

let patch_mem ~t_env ~t_mem to_avoid =
  List.fold_left (fun t x -> IMap.remove x t) t_env to_avoid
  |> IMap.mapi (fun x _ -> IMap.find x t_mem)

let to_seq t = IMap.to_seq t

let pp_print pp_elt =
  let open Format in
  let pp_sep f () = fprintf f ",@ " in
  let pp_one f (key, elt) = fprintf f "@[<h 2>%s |-> @[%a@]@]" key pp_elt elt in
  fun f t ->
    fprintf f "@[<hv 2>{@ %a}@]" (PP.pp_print_seq ~pp_sep pp_one) (to_seq t)

let map f t = IMap.map f t
