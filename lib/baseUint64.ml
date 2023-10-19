(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type t = Int64.t

let num_bits = 64

let zero = 0L
let one = 1L
let max_int = 0xFFFFFFFFFFFFFFFFL

let logand = Int64.logand
let logor = Int64.logor
let logxor = Int64.logxor
let lognot = Int64.lognot

  (* For signed integers, the top bit is the sign.
   * Thus, we will occasionally need to check if that bit is set,
   * or clear the bit for interaction with Int64.t. *)
  let top_bit = 0x8000000000000000L
  let has_top_bit_set a = logand a top_bit <> 0L
  let clear_top_bit a = logand a (lognot top_bit)

  let shift_left = Int64.shift_left
  let shift_right = Int64.shift_right
  let shift_right_logical = Int64.shift_right_logical

  let leading_zeros a =
    let rec leading_zeros' i a =
      match i with
      | 64 -> 64
      | _ ->
          if has_top_bit_set a then
            i
          else
            leading_zeros' (i+1) (shift_left a 1)
    in
    leading_zeros' 0 a

  let compare = Int64.unsigned_compare

  let equal = Int64.equal

  let add = Int64.add
  let sub = Int64.sub
  let mul = Int64.mul

  let div = Int64.unsigned_div
  let rem = Int64.unsigned_rem

  let pred = sub one
  let succ = add one

  let of_uint8 u = u
  let of_uint16 u = u
  let of_uint32 u = u

  let to_int = Int64.to_int
  let of_int = Int64.of_int

  let of_int64 i = i

  let to_string a = Printf.sprintf "%Lu" a
  let to_string_hex a = Printf.sprintf "0x%Lx" a

  let of_string a =
    if String.length a >= 2 then
      match a.[0], a.[1] with
      | '0', ('x'|'o'|'b'|'u')
        -> Int64.of_string a
      | _
        -> Int64.of_string ("0u" ^ a)
    else
      Int64.of_string ("0u" ^ a)
