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


include Int64

let unique_zero = true
let is_zero = equal 0L

let printable c = c

let shift_right_arithmetic = Int64.shift_right

let addk x k = match k with
  | 0 -> x
  | 1 -> succ x
  | _ -> add x (of_int k)

let machsize = MachSize.Quad

let pp hexa v =
  Printf.sprintf (if hexa then "0x%Lx" else "%Li") v

let pp_unsigned hexa v =
  Printf.sprintf (if hexa then "0x%Lx" else "%Lu") v

let lt v1 v2 = compare v1 v2 < 0
let le v1 v2 = compare v1 v2 <= 0
let bit_at k v = Int64.logand v (Int64.shift_left Int64.one k)
let mask sz =
  let open MachSize in
  match sz with
  | Byte -> fun v -> logand v 0xffL
  | Short -> fun v -> logand v 0xffffL
  | Word -> fun v ->  logand v 0xffffffffL
  | Quad -> fun v -> v
  | S128 -> fun v -> Warn.fatal "mask 64 bit value %s with s128 mask" (pp_unsigned true v)

let sxt sz v =
  let open MachSize in
  match sz with
  | Quad -> v
  | _ ->
     let v = mask sz v in
     let nb = nbits sz in
     let m = shift_left one (nb-1) in
     sub (logxor v m) m

let of_int64 = Misc.identity
let to_int64 = Misc.identity

let get_tag _ = assert false
let set_tag _ = assert false

include NoPromote
