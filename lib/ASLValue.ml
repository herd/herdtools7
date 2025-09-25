(******************************************************************************)
(*                           the diy toolsuite                                *)
(*                                                                            *)
(* Jade Alglave, University College London, UK.                               *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
(*                                                                            *)
(* Copyright 2015-present Institut National de Recherche en Informatique et   *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.              *)
(*                                                                            *)
(* This software is governed by the CeCILL-B license under French law and     *)
(* abiding by the rules of distribution of free software. You can use,        *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B   *)
(* license as circulated by CEA, CNRS and INRIA at the following URL          *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.              *)
(******************************************************************************)
(* Authors:                                                                   *)
(* Hadrien Renaud, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
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

module Instr =
  AArch64Instr.Make
    (struct
      let is_morello = false
    end)
    (struct
      type exec = ASLBase.instruction
      type data = AArch64Base.instruction

      let from_exec _ = Warn.fatal "ASLValue.from_exec"
      and to_exec _ = Warn.fatal "ASLValue.to_exec"
    end)

module ASLConstant =
  SymbConstant.Make (ASLScalar) (AArch64PteVal) (AArch64AddrReg) (Instr)

module V = SymbValue.Make (ASLConstant) (ASLSymData) (ASLOp)
module BVData = ASLSymData.BVData

let debug_value c =
  match c with
  | V.Var (_, ASLSymData.NoData) -> V.pp_v c
  | V.Var (s, sdata) -> Printf.sprintf "S%d %s" s (ASLSymData.pp sdata)
  | _ -> V.pp_v c

let get_sdata = function
  | V.Var (_, sdata) -> Some sdata
  | V.Val (Constant.Concrete (ASLScalar.S_BitVector bv)) ->
      Some (ASLSymData.Bitvector (BVData.of_bitvector bv))
  | _ -> None

let get_bv_data = function
  | V.Var (_, ASLSymData.Bitvector sdata) -> Some sdata
  | V.Val (Constant.Concrete (ASLScalar.S_BitVector bv)) ->
      Some (BVData.of_bitvector bv)
  | _ -> None

let to_fully_determined_opt = function
  | ASLSymData.NoData -> None
  | ASLSymData.Bitvector bv_data -> (
      match BVData.to_fully_determined_opt bv_data with
      | Some bv -> Some (V.Val (Constant.Concrete (ASLScalar.S_BitVector bv)))
      | None -> None)

let set_sdata ?(override = false) sdata = function
  | V.Val _ as v -> v
  | V.Var (s, _) when override -> V.Var (s, sdata)
  | V.Var (s, sdata') as v -> (
      match to_fully_determined_opt sdata with
      | Some v -> v
      | None ->
          let () =
            match ASLSymData.(get_length sdata, get_length sdata') with
            | Some l1, Some l2 when not (Int.equal l1 l2) ->
                Warn.fatal "Trying to set sdata %s on value %s"
                  (ASLSymData.pp sdata) (debug_value v)
            | _ -> ()
          in
          V.Var (s, ASLSymData.merge sdata sdata'))

let get_length = function
  | V.Var (_, sdata) -> ASLSymData.get_length sdata
  | V.Val (Constant.Concrete (ASLScalar.S_BitVector bv)) ->
      Some (Asllib.Bitvector.length bv)
  | _ -> None

let set_length length = set_sdata (ASLSymData.full_unspecified length)

let find_sub_symbolic bv_data positions =
  match BVData.find_sub_symbolic positions bv_data with
  | Some (_, (s, bv_data')) -> Some (V.Var (s, ASLSymData.Bitvector bv_data'))
  | None -> None

let write_slice positions ~v_src ~v_dst =
  let src_symb = match v_src with V.Var (s, _) -> Some s | _ -> None in
  match (get_bv_data v_src, get_bv_data v_dst) with
  | Some bv_data_src, Some bv_data_dst ->
      let fresh_var () =
        match V.fresh_var () with V.Var (c, _) -> c | _ -> assert false
      in
      let bv_data_res, eqs =
        BVData.write_slice fresh_var ~src:bv_data_src ?src_symb ~dst:bv_data_dst
          positions
      in
      let eqs =
        let var s = V.Var (s, ASLSymData.NoData) in
        List.map
          (fun (new_s, positions, old_s) -> (var new_s, positions, var old_s))
          eqs
      in
      Some (ASLSymData.Bitvector bv_data_res, eqs)
  | _ -> None
