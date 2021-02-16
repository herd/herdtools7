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

module CapabilityScalar = struct
  open Uint
  type t = bool * Uint128.t

  let zero = false, Uint128.zero
  let one = false, Uint128.one
  (* "NUM COLON NUM" as "Value:Tag" *)
  let of_string x =
    let idx_opt = String.index_opt x ':' in
    let idx = if Misc.is_some idx_opt then Misc.as_some idx_opt else 0 in
    let y = if Misc.is_some idx_opt then String.sub x 0 idx else x in
    let tag =
      if Misc.is_some idx_opt then
        if int_of_string (String.sub x (idx + 1)
            (String.length x - idx - 1)) <> 0 then true
        else false
      else false in
    tag, Uint128.of_string y
  let of_int x = false, Uint128.of_int x
  let to_int (_,x) = Uint128.to_int x
  let compare (t1,x1) (t2,x2) =
    match Uint128.compare x1 x2 with
    | 0 -> compare t1 t2
    | r -> r

  let add (_,x1) (_,x2) = false, Uint128.add x1 x2
  let sub (_,x1) (_,x2) = false, Uint128.sub x1 x2
  let mul (_,x1) (_,x2) = false, Uint128.mul x1 x2
  let div (_,x1) (_,x2) = false, Uint128.div x1 x2
  let logor (_,x1) (_,x2) = false, Uint128.logor x1 x2
  let logand (_,x1) (_,x2) = false, Uint128.logand x1 x2
  let logxor (_,x1) (_,x2) = false, Uint128.logxor x1 x2
  let lognot (_,x) = false, Uint128.lognot x
  let shift_left (_,x) k = false, Uint128.shift_left x k
  let shift_right_logical (_,x) k = false, Uint128.shift_right_logical x k
  let shift_right_arithmetic (_,x) k = false, Uint128.shift_right x k
  let bit_at k (_,x) = false, Uint128.logand x (Uint128.shift_left Uint128.one k)

  let addk (t,x) k = match k with
  | 0 -> t, x
  | 1 -> false, Uint128.succ x
  | _ -> false, Uint128.add x (Uint128.of_int k)

  let machsize = MachSize.S128
  let pp hexa (t,v) =
    Printf.sprintf "%s%s"
      (if hexa then (Uint128.to_string_hex v) else (Uint128.to_string v))
      (if t then ":1" else "")
  let lt v1 v2 = compare v1 v2 < 0
  let le v1 v2 = compare v1 v2 <= 0
  let mask sz =
    let open MachSize in
    match sz with
    | Byte -> fun (_,v) -> false, Uint128.logand v (Uint128.of_uint8 Uint8.max_int)
    | Short -> fun (_,v) -> false, Uint128.logand v (Uint128.of_uint16 Uint16.max_int)
    | Word -> fun (_,v) -> false, Uint128.logand v (Uint128.of_uint32 Uint32.max_int)
    | Quad -> fun (_,v) -> false, Uint128.logand v (Uint128.of_uint64 Uint64.max_int)
    | S128 -> fun (t,v) -> t, v

  let get_tag (t,_) = t
  let set_tag t (_,x) = t, x

  let sxt sz v = match sz with
    | MachSize.S128 -> v
    | _ ->
      let t,v = mask sz v in
      let nb = MachSize.nbits sz in
      let m = Uint128.shift_left Uint128.one (nb-1) in
      t,Uint128.sub (Uint128.logxor v m) m
end

include SymbConstant.Make(CapabilityScalar)

