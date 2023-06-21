module BV = Asllib.Bitvector
module AST = Asllib.AST

type t = S_Int of Z.t | S_Bool of bool | S_BitVector of BV.t

(* Irrelevant here? *)
let machsize = MachSize.Quad
let zero = S_Int Z.zero
let one = S_Int Z.one

let of_string s =
  try S_Int (Z.of_string s)
  with Invalid_argument _ -> (
    match s with
    | "TRUE" -> S_Bool true
    | "FALSE" -> S_Bool false
    | _ -> S_BitVector (BV.of_string s))

let pp hexa = function
  | S_Int i -> if hexa then Z.format "0x%x" i else Z.format "%d" i
  | S_Bool true -> "TRUE"
  | S_Bool false -> "FALSE"
  | S_BitVector bv -> BV.to_string bv

let pp_unsigned hexa = function
  | S_Int i -> if hexa then Z.format "0x%x" i else Z.format "%u" i
  | S_Bool true -> "TRUE"
  | S_Bool false -> "FALSE"
  | S_BitVector bv -> BV.to_string bv

let of_int i = S_Int (Z.of_int i)

let to_int = function
  | S_Int i -> Z.to_int i
  | S_Bool true -> 1
  | S_Bool false -> 0
  | S_BitVector bv -> BV.to_int bv

let of_int64 i = S_Int (Z.of_int64 i)

let to_int64 = function
  | S_Int i -> Z.to_int64 i
  | S_Bool true -> Int64.one
  | S_Bool false -> Int64.zero
  | S_BitVector bv -> BV.to_int64_signed bv

let to_native_value = function
  | S_Int i -> AST.V_Int (Z.to_int i)
  | S_Bool b -> AST.V_Bool b
  | S_BitVector bv -> AST.V_BitVector bv

let compare s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> Z.compare i1 i2
  | S_Bool b1, S_Bool b2 -> Bool.compare b1 b2
  | S_BitVector bv1, S_BitVector bv2 -> BV.compare bv1 bv2
  | S_Int _, (S_Bool _ | S_BitVector _) | S_Bool _, S_BitVector _ -> -1
  | (S_Bool _ | S_BitVector _), S_Int _ | S_BitVector _, S_Bool _ -> 1

let equal s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> Z.equal i1 i2
  | S_Bool b1, S_Bool b2 -> b1 = b2
  | S_BitVector bv1, S_BitVector bv2 -> BV.equal bv1 bv2
  | _ -> false

let add s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Z.add i1 i2)
  | S_BitVector bv1, S_Int i2 ->
      let sz = BV.length bv1 in
      S_BitVector (bv1 |> BV.to_z_unsigned |> Z.add i2 |> BV.of_z sz)
  | _ ->
      Warn.fatal "ASLScalar invalid op: %s add %s" (pp false s1) (pp false s2)

let sub s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Z.sub i1 i2)
  | _ ->
      Warn.fatal "ASLScalar invalid op: %s sub %s" (pp false s1) (pp false s2)

let mul s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Z.mul i1 i2)
  | _ ->
      Warn.fatal "ASLScalar invalid op: %s mul %s" (pp false s1) (pp false s2)

let div s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Z.div i1 i2)
  | _ ->
      Warn.fatal "ASLScalar invalid op: %s div %s" (pp false s1) (pp false s2)

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

let convert_to_bool = function
  | S_Int i -> S_Bool (not (Z.equal i Z.zero))
  | S_Bool b -> S_Bool b
  | S_BitVector _ as s ->
      Warn.fatal "ASLScalar invalid op: to_bool %s" (pp false s)

let convert_to_bv = function
  | S_BitVector _ as bv -> bv
  | S_Int i -> S_BitVector (BV.of_z 64 i)
  | S_Bool false -> S_BitVector (BV.zeros 64)
  | S_Bool true -> S_BitVector (BV.of_z 64 Z.one)

let try_extract_slice s positions =
  match s with
  | S_BitVector bv ->
      if List.exists (( <= ) (BV.length bv)) positions then None
      else Some (S_BitVector (BV.extract_slice bv positions))
  | S_Int i ->
      if Z.equal Z.zero i then
        Some (S_BitVector (BV.zeros (List.length positions)))
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

let try_write_slice positions dst src =
  match (dst, src) with
  | S_BitVector dst, S_BitVector src ->
      if List.exists (( <= ) (BV.length dst)) positions then None
      else Some (S_BitVector (BV.write_slice dst src positions))
  | _ -> None

let empty = S_BitVector BV.empty
