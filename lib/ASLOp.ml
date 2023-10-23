(******************************************************************************)
(*                           the diy toolsuite                                *)
(*                                                                            *)
(* Jade Alglave, University College London, UK.                               *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
(*                                                                            *)
(* Copyright 2023-present Institut National de Recherche en Informatique et   *)
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

type op =
  | Divrm
  | SetIndex of int
  | SetField of string
  | Concat
  | BVSliceSet of int list

(* No extra operation *)
type extra_op1

type 'a constr_op1 =
  | GetIndex of int
  | GetField of string
  | BVSlice of int list
  | ToIntU
  | ToIntS
  | ToBool
  | ToBV
  | BoolNot
  | BVLength

type op1 = extra_op1 constr_op1
type scalar = ASLScalar.t
type pteval = PteVal.ASL.t
type instr = ASLBase.Instr.t
type cst = (scalar, pteval, instr) Constant.t

let pp_op = function
  | Divrm -> "DIVRM"
  | SetIndex i -> Printf.sprintf "Set[%d]" i
  | SetField x -> Printf.sprintf "Set[%S]" x
  | Concat -> "Concat"
  | BVSliceSet positions ->
      Printf.sprintf "SliceSet[%s]"
      @@ String.concat ", "
      @@ List.map string_of_int positions

let pp_op1 _hexa = function
  | GetIndex i -> Printf.sprintf "Get[%d]" i
  | GetField x -> Printf.sprintf "Get[%S]" x
  | BVSlice positions ->
      Printf.sprintf "Slice[%s]" @@ String.concat ", "
      @@ List.map string_of_int positions
  | ToIntU -> "ToIntU"
  | ToIntS -> "ToIntS"
  | ToBool -> "ToBool"
  | ToBV -> "ToBV"
  | BoolNot -> "BoolNot"
  | BVLength -> "BVLength"

let ( let* ) = Option.bind
let return_concrete s = Some (Constant.Concrete s)
let as_concrete = function Constant.Concrete v -> Some v | _ -> None

let as_concrete_vector = function
  | Constant.ConcreteVector v -> Some v
  | _ -> None

let as_concrete_record = function
  | Constant.ConcreteRecord v -> Some v
  | _ -> None

let all_64_bits_positions = List.init 64 (( - ) 63)

let list_set n =
  let rec list_set acc n elt = function
    | [] -> None
    | _ :: t when n == 0 -> Some (List.rev acc @ (elt :: t))
    | h :: t -> list_set (h :: acc) (n - 1) elt t
  in
  list_set [] n

let do_op op c1 c2 =
  match op with
  | Divrm ->
      let* s1 = as_concrete c1 in
      let* s2 = as_concrete c2 in
      let* s = ASLScalar.try_divrm s1 s2 in
      return_concrete s
  | SetIndex i ->
      let* vec = as_concrete_vector c1 in
      let* vec' = list_set i c2 vec in
      Some (Constant.ConcreteVector vec')
  | SetField x ->
      let* record = as_concrete_record c1 in
      if StringMap.mem x record then
        let record' = StringMap.add x c2 record in
        Some (Constant.ConcreteRecord record')
      else None
  | Concat ->
      let* s1 = as_concrete c1 in
      let* s2 = as_concrete c2 in
      let* s = ASLScalar.try_concat s1 s2 in
      return_concrete s
  | BVSliceSet positions ->
      let* s1 = as_concrete c1 in
      let* s2 = as_concrete c2 in
      let* s = ASLScalar.try_write_slice positions s1 s2 in
      return_concrete s

let do_op1 op cst =
  match op with
  | GetIndex i ->
      let* vec = as_concrete_vector cst in
      List.nth_opt vec i
  | GetField x ->
      let* record = as_concrete_record cst in
      StringMap.find_opt x record
  | ToIntS -> (
      match cst with
      | Constant.Concrete s ->
          ASLScalar.convert_to_int_signed s |> return_concrete
      | Constant.Symbolic _ -> Some cst
      | _ -> None)
  | ToIntU -> (
      match cst with
      | Constant.Concrete s ->
          ASLScalar.convert_to_int_unsigned s |> return_concrete
      | Constant.Symbolic _ -> Some cst
      | _ -> None)
  | ToBV -> (
      match cst with
      | Constant.Concrete s -> ASLScalar.convert_to_bv s |> return_concrete
      | Constant.Symbolic _ -> Some cst
      | _ -> None)
  | ToBool ->
      let* s = as_concrete cst in
      return_concrete (ASLScalar.convert_to_bool s)
  | BVSlice positions -> (
      match cst with
      | Constant.Concrete s ->
          let* s' = ASLScalar.try_extract_slice s positions in
          return_concrete s'
      | Constant.Symbolic x ->
          if Misc.list_eq ( = ) positions all_64_bits_positions then
            Some (Constant.Symbolic x)
          else if positions = [63] then
            return_concrete (ASLScalar.zeros 1)
          else None
      | _ -> None)
  | BoolNot -> (
      let open Constant in
      let open ASLScalar in
      match cst with
      | Concrete (S_Bool b) -> return_concrete (S_Bool (not b))
      | _ -> None)
  | BVLength -> (
      let open Constant in
      let open ASLScalar in
      match cst with
      | Concrete (S_BitVector bv) ->
          return_concrete (ASLScalar.of_int (BV.length bv))
      | _ -> None)

let shift_address_right _ _ = None
let orop _ _ = None
let andnot2 _ _ = None
let andop _ _ = None
let mask _ _ = None
