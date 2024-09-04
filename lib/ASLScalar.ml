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

let _dbg = false

module BV = Asllib.Bitvector
module AST = Asllib.AST

type t = S_Int of Z.t | S_Bool of bool | S_BitVector of BV.t

(* Irrelevant here? *)
let machsize = MachSize.Quad
let zero = S_Int Z.zero

let unique_zero = false

let one = S_Int Z.one
let zeros sz = S_BitVector (BV.zeros sz)
let bv_of_string s = S_BitVector (BV.of_string s)

(*
 * Integer dump is made assuming a 64bits basis.
 * For instance '-1' in hexadecimal will be printed as
 * 0xffffffffffffffff and not as 0x-1
 *)

let z63 = Z.shift_left Z.one (MachSize.nbits machsize-1)
let z64 = Z.shift_left Z.one (MachSize.nbits machsize)

let norm_unsigned z = if Z.sign z < 0 then Z.add z z64 else z

let norm_signed z =
  let z = Z.erem z z64 in
  if Z.geq z z63 then Z.sub z z64 else z

let z_format_hexa z = "0x" ^ Z.format "%x" z

let pp_bv bv =
  let pp =  BV.to_string bv in
  if _dbg then
    if String.length pp > 6 then
      "'..."
      ^ String.sub pp (String.length pp - 4) 3 ^ "'"
    else pp
  else pp

let pp hexa = function
  | S_Int i -> if hexa then z_format_hexa (norm_unsigned i) else Z.format "%d" i
  | S_Bool true -> "TRUE"
  | S_Bool false -> "FALSE"
  | S_BitVector bv ->
      if hexa then z_format_hexa (BV.to_z_unsigned bv) else  pp_bv bv

let pp_unsigned hexa = function
  | S_Int i ->
      let i = norm_unsigned i in
      if hexa then z_format_hexa i else Z.format "%d" i
  | S_Bool true -> "TRUE"
  | S_Bool false -> "FALSE"
  | S_BitVector bv ->
      if hexa then z_format_hexa (BV.to_z_unsigned bv) else pp_bv bv

let of_string s =
  try S_Int (Z.of_string s)
  with Invalid_argument _ -> (
    match s with
    | "TRUE" -> S_Bool true
    | "FALSE" -> S_Bool false
    | _ -> S_BitVector (BV.of_string s))

let of_int i = S_Int (Z.of_int i)

let to_int = function
  | S_Int i ->  Z.to_int (norm_signed i)
  | S_Bool true -> 1
  | S_Bool false -> 0
  | S_BitVector bv ->
     BV.to_int_signed bv

let of_int64 i = S_Int (Z.of_int64 i)

let to_int64 = function
  | S_Int i -> Z.to_int64 i
  | S_Bool true -> Int64.one
  | S_Bool false -> Int64.zero
  | S_BitVector bv -> BV.to_int64_signed bv

let to_native_value = function
  | S_Int i -> AST.L_Int i
  | S_Bool b -> AST.L_Bool b
  | S_BitVector bv -> AST.L_BitVector bv

let compare s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> Z.compare i1 i2
  | S_Bool b1, S_Bool b2 -> Bool.compare b1 b2
  | S_BitVector bv1, S_BitVector bv2 -> BV.compare bv1 bv2
  | S_Int _, (S_Bool _ | S_BitVector _) | S_Bool _, S_BitVector _ -> -1
  | (S_Bool _ | S_BitVector _), S_Int _ | S_BitVector _, S_Bool _ -> 1

let unsigned_compare s1 s2 =
  let i1 = to_int64 s1 and i2 = to_int64 s2 in
  Int64.unsigned_compare i1 i2

let equal s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> Z.equal i1 i2
  | S_Bool b1, S_Bool b2 -> b1 = b2
  | S_BitVector bv1, S_BitVector bv2 -> BV.equal bv1 bv2
  | _ -> false

let is_zero = function
  | S_Int i -> Z.equal Z.zero i
  | S_BitVector bv -> BV.is_zeros bv
  | S_Bool _ -> false

let zop_expressive pp_op op s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (op i1 i2)
  | S_BitVector bv1, S_Int i2 ->
      let sz = BV.length bv1 in
      S_BitVector (bv1 |> BV.to_z_unsigned |> op i2 |> BV.of_z sz)
  | S_BitVector bv1, S_BitVector bv2
       when BV.length bv1=BV.length bv2
    ->
     let z1 = BV.to_z_unsigned bv1
     and z2 = BV.to_z_unsigned bv2 in
     let z = op z1 z2 in
     S_BitVector (BV.of_z (BV.length bv1) z)
  | _ ->
     Warn.fatal "ASLScalar invalid op: %s %s %s"
       (pp false s1) pp_op (pp false s2)

let add = zop_expressive "add" Z.add
and sub = zop_expressive "sub" Z.sub

let zop pp_op op s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (op i1 i2)
  | _ ->
      Warn.fatal "ASLScalar invalid op: %s %s %s"
        pp_op
        (pp false s1)
        (pp false s2)

let asl_rem z1 z2 = Z.sub z1 (Z.mul z2 (Z.fdiv z1 z2))
let mul = zop "mul" Z.mul
and div = zop "div" Z.divexact
and rem = zop "rem" asl_rem

let logor s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Z.logor i1 i2)
  | S_Bool b1, S_Bool b2 -> S_Bool (b1 || b2)
  | S_BitVector bv1, S_BitVector bv2 -> S_BitVector (BV.logor bv1 bv2)
  | _ -> Warn.fatal "ASLScalar invalid op: %s OR %s" (pp false s1) (pp false s2)

let logand s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Z.logand i1 i2)
  | S_Bool b1, S_Bool b2 -> S_Bool (b1 && b2)
  | S_BitVector bv1, S_BitVector bv2 -> S_BitVector (BV.logand bv1 bv2)
  | _ ->
      Warn.fatal "ASLScalar invalid op: %s logand %s" (pp false s1)
        (pp false s2)

let logxor s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Z.logxor i1 i2)
  | S_Bool b1, S_Bool b2 -> S_Bool (b1 != b2)
  | S_BitVector bv1, S_BitVector bv2 -> S_BitVector (BV.logxor bv1 bv2)
  | _ ->
      Warn.fatal "ASLScalar invalid op: %s logxor %s" (pp false s1)
        (pp false s2)

let lognot = function
  | S_Int i -> S_Int (Z.lognot i)
  | S_Bool b -> S_Bool (not b)
  | S_BitVector bv -> S_BitVector (BV.lognot bv)

let abs = function
  | S_Int i -> S_Int (Z.abs i)
  | s -> Warn.fatal "ASLScalar invalid op: %s abs" (pp false s)

let shift_left = function
  | S_Int i -> fun k -> S_Int (Z.shift_left i k)
  | s1 -> Warn.fatal "ASLScalar invalid op: %s shift_left" (pp false s1)

let shift_right_logical s1 _k =
  Warn.fatal "ASLScalar invalid op: %s shift_right_logical" (pp false s1)

let shift_right_arithmetic = function
  | S_Int i -> fun k -> S_Int (Z.shift_right i k)
  | s1 ->
      Warn.fatal "ASLScalar invalid op: %s shift_right_arithmetic" (pp false s1)

let addk = function
  | S_Int i -> fun k -> S_Int (Z.add i (Z.of_int k))
  | S_BitVector bv ->
     fun k ->
     S_BitVector
       (BV.to_z_unsigned bv
        |> Z.add (Z.of_int k)
        |> BV.of_z (BV.length bv))
  | s1 -> Warn.fatal "ASLScalar invalid op: %s addk" (pp false s1)

let bit_at i1 = function
  | S_Int i2 ->
      let r = if Z.testbit i2 i1 then Z.one else Z.zero in
      S_Int r
  | S_BitVector bv -> S_BitVector (BV.extract_slice bv [ i1 ])
  | s1 -> Warn.fatal "ASLScalar invalid op: %s bit_at" (pp false s1)

let lt s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> Z.lt i1 i2
  | _ -> Warn.fatal "ASLScalar invalid op: %s lt %s" (pp false s1) (pp false s2)

let le s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> Z.leq i1 i2
  | _ -> Warn.fatal "ASLScalar invalid op: %s le %s" (pp false s1) (pp false s2)

let do_mask sz z =
  let nsz = MachSize.nbits sz in
  let msk = Z.sub (Z.shift_left Z.one nsz) Z.one in
  Z.logand z msk

let mask sz = function
  | S_Int i -> S_Int (do_mask sz i)
  | S_BitVector bv ->
      let n' = MachSize.nbits sz in
      let n = BV.length bv in
      if n = n' then S_BitVector bv
      else if n' > n then S_BitVector (BV.concat [ BV.zeros (n' - n); bv ])
      else S_BitVector (BV.extract_slice bv (List.init n' (( - ) (n' - 1))))
  | s -> Warn.fatal "ASLScalar invalid op: _ mask %s" (pp false s)

let do_sxt sz z =
  let v = do_mask sz z in
  let m = Z.shift_left Z.one (MachSize.nbits sz - 1) in
  Z.sub (Z.logxor v m) m

let sxt sz = function S_Int i -> S_Int (do_sxt sz i) | s -> s
let get_tag _t = assert false
let set_tag _b _t = assert false

let as_int = function
  | S_Int z -> Z.to_int z |> Misc.some
  | S_Bool false -> Some 0
  | S_Bool true -> Some 1
  | S_BitVector _ -> None

let convert_to_int_signed = function
  | S_Int _ as s -> s
  | S_Bool false -> S_Int Z.zero
  | S_Bool true -> S_Int Z.one
  | S_BitVector bv -> S_Int (BV.to_z_signed bv)

let convert_to_int_unsigned = function
  | S_Int _ as s -> s
  | S_Bool false -> S_Int Z.zero
  | S_Bool true -> S_Int Z.one
  | S_BitVector bv -> S_Int (BV.to_z_unsigned bv)

let to_caml_bool = function
  | S_Bool b -> b
  | S_Int _|S_BitVector _   as sc -> not (is_zero sc)

let as_bool sc = Some (to_caml_bool sc)
let convert_to_bool sc = S_Bool (to_caml_bool sc)


(** Convert to bitvector of known size *)
let convert_to_bv sz = function
  | S_BitVector bv as v ->
      let len = BV.length bv in
      if sz < len then S_BitVector (BV.prefix bv sz)
      else if sz > len then
        let zs = BV.zeros (sz-len) in
        S_BitVector (BV.concat [ zs; bv; ])
      else v
  | S_Int i -> S_BitVector (BV.of_z sz i)
  | S_Bool false -> S_BitVector (BV.zeros sz)
  | S_Bool true -> S_BitVector (BV.of_z sz Z.one)

(** Transform to bitvector, size fixed to 64 if transformation occurs. *)
let as_bv = function
  | S_BitVector _ as bv -> bv
  | v -> convert_to_bv 64 v

let try_extract_slice s positions =
  match s with
  | S_BitVector bv ->
      if List.exists (( <= ) (BV.length bv)) positions then None
      else Some (S_BitVector (BV.extract_slice bv positions))
  | S_Int i ->
      if Z.equal Z.zero i then
        Some (S_BitVector (BV.zeros (List.length positions) ))
      else if Z.equal Z.minus_one i then
        Some (S_BitVector (BV.ones (List.length positions)))
      else if List.exists (( <= ) 64) positions then None
      else Some (S_BitVector (BV.extract_slice (BV.of_z 64 i) positions))
  | _ -> None

let try_concat s1 s2 =
  match (s1, s2) with
  | S_BitVector bv1, S_BitVector bv2 ->
      Some (S_BitVector (BV.concat [ bv1; bv2 ]))
  | _ -> None

let try_divrm s1 s2 =
  match (s1, s2) with
  | S_Int s1, S_Int s2 when Z.sign s2 = 1 -> Some (S_Int (Z.fdiv s1 s2))
  | _ -> None

let try_write_slice positions dst src =
  match (dst, src) with
  | S_BitVector dst, S_BitVector src ->
      if List.exists (( <= ) (BV.length dst)) positions then None
      else Some (S_BitVector (BV.write_slice dst src positions))
  | _ -> None

let empty = S_BitVector BV.empty
let zeros_size_one = S_BitVector (BV.zeros 1)

let bv_of_bool b =  S_BitVector (if b then BV.one else BV.zero)

let bv_of_int x = S_BitVector (BV.of_int x)

let do_bv_of_bit = function
  | 0 -> BV.zero
  | 1 -> BV.one
  | _ -> assert false

let bv_of_bit b =  S_BitVector (do_bv_of_bit b)

let bv_of_bits bs =
  let bv = List.map do_bv_of_bit bs |> BV.concat in
  S_BitVector bv

let printable_z z = norm_signed z

include NoPromote
