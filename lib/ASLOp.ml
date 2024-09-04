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
  | ToBV of int
  | BoolNot
  | BVLength
  | OA
  | ToAArch64
  | FromAArch64

type op1 = extra_op1 constr_op1
type scalar = ASLScalar.t
type pteval = AArch64PteVal.t
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
  | ToBV sz -> Printf.sprintf "ToBV%d" sz
  | BoolNot -> "BoolNot"
  | BVLength -> "BVLength"
  | OA -> "GetOA"
  | ToAArch64 -> "ToAArch64"
  | FromAArch64 -> "FromAArch64"


let ( let* ) = Option.bind
let return c = Some c
let return_concrete s = Constant.Concrete s |> return
let as_concrete = function Constant.Concrete v -> Some v | _ -> None

let as_concrete_vector = function
  | Constant.ConcreteVector v -> Some v
  | _ -> None

let as_concrete_record = function
  | Constant.ConcreteRecord v -> Some v
  | _ -> None

(*******************)
(* Check intervals *)
(*******************)

let do_is_interval_from_to hi lo =
  let rec check prev = function
    | [] -> prev=lo
    | x::xs -> prev-1=x && check x xs in
  check hi

let is_interval_from_to hi lo = function
  | x::xs when x=hi -> do_is_interval_from_to x lo xs
  | _ -> false

let do_is_interval_from hi =  do_is_interval_from_to hi 0

let is_interval_from hi = function
  | x::xs when x=hi -> do_is_interval_from hi xs
  | _ -> false

(* Lower bits of address *)
let is_address_mask = function
  | [] -> false
  | x::xs -> x >= 41 && do_is_interval_from x xs

(* Complete mask *)
let is_mask_64 = function
  | 63::xs -> do_is_interval_from 63 xs
  | _ -> false

let list_set n =
  let rec list_set acc n elt = function
    | [] -> None
    | _ :: t when n == 0 -> Some (List.rev acc @ (elt :: t))
    | h :: t -> list_set (h :: acc) (n - 1) elt t
  in
  list_set [] n

let set_slice positions c1 c2 =
  let* s1 = as_concrete c1 in
  let* s2 = as_concrete c2 in
  let* s = ASLScalar.try_write_slice positions s1 s2 in
  return_concrete s

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
      let is_empty s = s = ASLScalar.S_BitVector Asllib.Bitvector.empty in
      (match (as_concrete c1, as_concrete c2) with
      | (Some s, _) when is_empty s ->
          Some c2
      | (_, Some s) when is_empty s ->
          Some c1
      | (Some s1, Some s2) ->
        let* s = ASLScalar.try_concat s1 s2 in
        return_concrete s
      | _ -> None)
  | BVSliceSet positions ->
      if is_mask_64 positions then
        match c2 with
        | Constant.PteVal _ -> return c2
        | _ ->  set_slice positions c1 c2
      else if is_interval_from_to 127 64 positions then begin
        match c1 with
        | Constant.PteVal _ -> return c1
        | _ ->  set_slice positions c1 c2
      end else
        set_slice positions c1 c2

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
  | ToAArch64 -> (
      match cst with
      | Constant.Concrete s ->
          ASLScalar.convert_to_int_signed s |> return_concrete
      | Constant.Symbolic _|Constant.PteVal _ -> Some cst
      | _ -> None)
  | FromAArch64 -> (
      match cst with
      | Constant.Concrete s ->
          ASLScalar.as_bv s |> return_concrete
      | Constant.Symbolic _|Constant.PteVal _ -> Some cst
      | _ -> None)
  | ToIntU -> (
      match cst with
      | Constant.Concrete s ->
          ASLScalar.convert_to_int_unsigned s |> return_concrete
      | Constant.Symbolic _ -> Some cst
      | _ -> None)
  | ToBV sz -> (
      match cst with
      | Constant.Concrete s -> ASLScalar.convert_to_bv sz s |> return_concrete
      | Constant.(Symbolic _|PteVal _) -> Some cst
      | _ -> None)
  | ToBool ->
      let* s = as_concrete cst in
      return_concrete (ASLScalar.convert_to_bool s)
  | BVSlice positions -> (
      match cst with
      | Constant.Concrete s ->
          let* s' = ASLScalar.try_extract_slice s positions in
          return_concrete s'
      | Constant.PteVal pte ->
          begin
            match positions with
            | [(54|53|50);] -> (* XPN/UXPN/GP, all disabled *)
                Some (Constant.Concrete ASLScalar.zeros_size_one)
            | [11;] -> (* Res0 ? *)
                 Some (Constant.Concrete ASLScalar.zeros_size_one)
            | [10;] -> (* AF *)
                let af = pte.AArch64PteVal.af in
                Some (Constant.Concrete (ASLScalar.bv_of_bit af))
            | [9;8;] ->
              (* Sharability domain -> inner sharable *)
              Some (Constant.Concrete (ASLScalar.bv_of_string "10"))
            | [7;6;] -> (* AP *)
                let db =  1-pte.AArch64PteVal.db in
                Some (Constant.Concrete (ASLScalar.bv_of_bits [db;0;]))
            | [5;] ->
              (* EL0 *)
                Some (Constant.Concrete ASLScalar.zeros_size_one)
            | [4;3;2;] ->
              (* memattr *)
                Some (Constant.Concrete (ASLScalar.zeros 3))
            | [0;]  ->
              (* Valid *)
                let valid = pte.AArch64PteVal.valid in
                Some (Constant.Concrete (ASLScalar.bv_of_bit valid))
            | _ when  is_address_mask positions -> Some cst
            | _ -> None
          end
      | Constant.Symbolic x ->
          if is_address_mask positions then
            Some (Constant.Symbolic x)
          else begin
          (* MSB of virtual address is assumed null.
           * This hypothesis is reasonable for user programs,
           * less so for kernel code. *)
            match positions with
            | [63] -> Some (Constant.Concrete ASLScalar.zeros_size_one)
            | [55] -> Some (Constant.Concrete ASLScalar.zeros_size_one)
            | [63; 62; 61; 60; 59; 58; 57; 56;
               55; 54; 53; 52; 51; 50; 49; 48;]
              -> Some (Constant.Concrete (ASLScalar.zeros 16))
            | _ -> None
          end
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
  | OA -> None (* Delay always *)

let shift_address_right _ _ = None
let orop _ _ = None
let andnot2 _ _ = None
let andop _ _ = None
let mask _ _ = None

let fromExtra pteval = pteval
and toExtra pteval = pteval
