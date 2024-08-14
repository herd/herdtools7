(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

include Int128

let unique_zero = true

let printable c = c

let shift_right_arithmetic = Int128.shift_right

let addk x k = match k with
  | 0 -> x
  | 1 -> Int128.succ x
  | _ -> Int128.add x (Int128.of_int k)

let machsize = MachSize.S128
let pp hexa v =
  Printf.sprintf "%s" (if hexa then (Int128.to_string_hex v) else (Int128.to_string v))
let pp_unsigned = pp (* Hum *)

let lt v1 v2 = compare v1 v2 < 0
let le v1 v2 = compare v1 v2 <= 0
let bit_at k v = Int128.logand v (Int128.shift_left Int128.one k)

let mask sz =
  let open MachSize in
  match sz with
  | Byte -> fun v -> Int128.logand v (Int128.of_int 0xff)
  | Short -> fun v -> Int128.logand v (Int128.of_int 0xffff)
  | Word -> fun v ->  Int128.logand v (Int128.of_int64 0xffffffffL)
  | Quad -> fun (_,b) -> Int64.zero,b
  | S128 -> fun v -> v

let sxt sz v = match sz with
  | MachSize.S128 -> v
  | _ ->
     let v = mask sz v in
     let nb = MachSize.nbits sz in
     let m = Int128.shift_left Int128.one (nb-1) in
     Int128.sub (Int128.logxor v m) m

let shift_right_logical = Int128.shift_right_logical
let shift_left = Int128.shift_left
let lognot = Int128.lognot
let abs = Int128.abs
let logxor = Int128.logxor
let logand = Int128.logand
let logor = Int128.logor
let div = Int128.div
let rem = Int128.rem
let mul = Int128.mul
let sub = Int128.sub
let add = Int128.add
let equal = Int128.equal
let compare = Int128.compare
let is_zero =  Int128.equal Int128.zero
let to_int64 = Int128.to_int64
let of_int64 = Int128.of_int64
let to_int = Int128.to_int
let of_int = Int128.of_int
let of_string = Int128.of_string
let one = Int128.one
let zero = Int128.zero

type t = Int128.t
let unsigned_compare = Int128.unsigned_compare


let get_tag _ = assert false
let set_tag _ = assert false

include NoPromote
