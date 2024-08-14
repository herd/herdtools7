(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2024-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module BV = struct
  type t = Z.t

  let of_string = Z.of_string

  let pp x = "0x" ^ Z.format "%x" x

  let of_int x = Printf.sprintf "%u" x |> Z.of_string
  and to_int x = try Z.to_int x with Z.Overflow -> Warn.fatal "SVE.to_int"

  let of_int64 = Z.of_int64_unsigned
  and to_int64 x = try Z.to_int64_unsigned x with Z.Overflow -> Warn.fatal "SVE.to_int64"

  let compare = Z.compare
  let equal = Z.equal
  let is_zero = Z.equal Z.zero

  let logor = Z.logor
  and logand = Z.logand
  and logxor = Z.logxor
  and lognot = Z.lognot

  let shift_left = Z.shift_left
  and shift_right = Z.shift_right

  let bit_at k x = Z.logand x (Z.shift_left Z.one k)
  let lt = Z.lt
  let le = Z.leq

  let mask8 = Z.of_int 0xff
  let mask16 = Z.of_int 0xffff
  let mask32 = Z.of_int64_unsigned 0xffffffffL
  let mask64 = Z.of_int64_unsigned 0xffffffffffffffffL

  let mask sz =
    let open MachSize in
    match sz with
    | Byte -> fun v -> Z.logand v mask8
    | Short -> fun v -> Z.logand v mask16
    | Word -> fun v ->  Z.logand v mask32
    | Quad -> fun v -> logand v mask64
    | S128 -> Misc.identity
end

module Translate = struct

  let promote = Z.of_int64_unsigned
  and demote x = Z.logand x BV.mask64 |> Z.to_int64_unsigned

end

include (ExtendScalar.Make(Int64Scalar)(BV)(Translate) : Scalar.S)
