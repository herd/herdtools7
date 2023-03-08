module BV = Asllib.Bitvector
module AST = Asllib.AST

type t = S_Int of Int64.t | S_Bool of bool | S_BitVector of BV.t

(* Irrelevant here? *)
let machsize = MachSize.Quad
let zero = S_Int Int64.zero
let one = S_Int Int64.one

let of_string s =
  match Int64.of_string_opt s with
  | Some i -> S_Int i
  | None -> (
      match s with
      | "TRUE" -> S_Bool true
      | "FALSE" -> S_Bool false
      | _ -> S_BitVector (BV.of_string s))

let pp hexa = function
  | S_Int i -> Int64Scalar.pp hexa i
  | S_Bool true -> "TRUE"
  | S_Bool false -> "FALSE"
  | S_BitVector bv -> BV.to_string bv

let pp_unsigned hexa = function
  | S_Int i -> Int64Scalar.pp_unsigned hexa i
  | S_Bool true -> "TRUE"
  | S_Bool false -> "FALSE"
  | S_BitVector bv -> BV.to_string bv

let of_int i = S_Int (Int64Scalar.of_int i)

let to_int = function
  | S_Int i -> Int64.to_int i
  | S_Bool true -> 1
  | S_Bool false -> 0
  | S_BitVector bv -> BV.to_int bv

let of_int64 i = S_Int i

let to_int64 = function
  | S_Int i -> i
  | S_Bool true -> Int64.one
  | S_Bool false -> Int64.zero
  | S_BitVector bv -> BV.to_int64 bv

let to_native_value = function
  | S_Int i -> AST.V_Int (Int64.to_int i)
  | S_Bool b -> AST.V_Bool b
  | S_BitVector bv -> AST.V_BitVector bv

let compare s1 s2 =
  match (s1, s2) with
  | S_Bool false, S_Int 0L | S_Int 0L, S_Bool false -> 0
  | S_Int i1, S_Int i2 -> Int64.compare i1 i2
  | S_Int _, _ | _, S_Int _ -> 1
  | S_BitVector bv1, S_BitVector bv2 -> BV.compare bv1 bv2
  | S_BitVector _, _ | _, S_BitVector _ -> 1
  | S_Bool b1, S_Bool b2 -> Bool.compare b1 b2

let equal s1 s2 =
  match (s1, s2) with
  | S_Bool false, S_Int 0L | S_Int 0L, S_Bool false -> true
  | S_Int i1, S_Int i2 -> Int64.equal i1 i2
  | S_Bool b1, S_Bool b2 -> b1 = b2
  | S_BitVector bv1, S_BitVector bv2 -> BV.equal bv1 bv2
  | _ -> false

let add s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Int64.add i1 i2)
  | _ ->
      Warn.fatal "ASLScalar invalid op: %s add %s" (pp false s1) (pp false s2)

let sub s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Int64.sub i1 i2)
  | _ ->
      Warn.fatal "ASLScalar invalid op: %s sub %s" (pp false s1) (pp false s2)

let mul s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Int64.mul i1 i2)
  | _ ->
      Warn.fatal "ASLScalar invalid op: %s mul %s" (pp false s1) (pp false s2)

let div s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Int64.div i1 i2)
  | _ ->
      Warn.fatal "ASLScalar invalid op: %s div %s" (pp false s1) (pp false s2)

let logor s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Int64.logor i1 i2)
  | S_Bool b1, S_Bool b2 -> S_Bool (b1 || b2)
  | S_BitVector bv1, S_BitVector bv2 -> S_BitVector (BV.logor bv1 bv2)
  | _ -> Warn.fatal "ASLScalar invalid op: %s OR %s" (pp false s1) (pp false s2)

let logand s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Int64.logand i1 i2)
  | S_Bool b1, S_Bool b2 -> S_Bool (b1 && b2)
  | S_BitVector bv1, S_BitVector bv2 -> S_BitVector (BV.logand bv1 bv2)
  | _ ->
      Warn.fatal "ASLScalar invalid op: %s logand %s" (pp false s1)
        (pp false s2)

let logxor s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Int64.logxor i1 i2)
  | S_Bool b1, S_Bool b2 -> S_Bool (b1 != b2)
  | S_BitVector bv1, S_BitVector bv2 -> S_BitVector (BV.logxor bv1 bv2)
  | _ ->
      Warn.fatal "ASLScalar invalid op: %s logxor %s" (pp false s1)
        (pp false s2)

let lognot = function
  | S_Int i -> S_Int (Int64.lognot i)
  | S_Bool b -> S_Bool (not b)
  | S_BitVector bv -> S_BitVector (BV.lognot bv)

let shift_left = function
  | S_Int i -> fun k -> S_Int (Int64Scalar.shift_left i k)
  | s1 -> Warn.fatal "ASLScalar invalid op: %s shift_left" (pp false s1)

let shift_right_logical = function
  | S_Int i -> fun k -> S_Int (Int64Scalar.shift_right_logical i k)
  | s1 ->
      Warn.fatal "ASLScalar invalid op: %s shift_right_logical" (pp false s1)

let shift_right_arithmetic = function
  | S_Int i -> fun k -> S_Int (Int64Scalar.shift_right_arithmetic i k)
  | s1 ->
      Warn.fatal "ASLScalar invalid op: %s shift_right_arithmetic" (pp false s1)

let addk = function
  | S_Int i -> fun k -> S_Int (Int64Scalar.addk i k)
  | s1 -> Warn.fatal "ASLScalar invalid op: %s addk" (pp false s1)

let bit_at i1 = function
  | S_Int i2 -> S_Int (Int64Scalar.bit_at i1 i2)
  | S_BitVector bv -> S_BitVector (BV.extract_slice bv [ i1 ])
  | s1 -> Warn.fatal "ASLScalar invalid op: %s bit_at" (pp false s1)

let lt s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> Int64Scalar.lt i1 i2
  | _ -> Warn.fatal "ASLScalar invalid op: %s lt %s" (pp false s1) (pp false s2)

let le s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> Int64Scalar.le i1 i2
  | _ -> Warn.fatal "ASLScalar invalid op: %s le %s" (pp false s1) (pp false s2)

let mask sz = function
  | S_Int i -> S_Int (Int64Scalar.mask sz i)
  | S_BitVector bv ->
      let n' = MachSize.nbits sz in
      let n = BV.length bv in
      if n = n' then S_BitVector bv
      else if n' > n then
        S_BitVector (BV.concat [ BV.zeros (n' - n); bv ])
      else
        S_BitVector (BV.extract_slice bv (List.init n' (( - ) (n' - 1))))
  | s -> Warn.fatal "ASLScalar invalid op: _ mask %s" (pp false s)

let sxt sz = function S_Int i -> S_Int (Int64Scalar.sxt sz i) | s -> s
let get_tag _t = assert false
let set_tag _b _t = assert false

let convert_to_int = function
  | S_Int _ as s -> s
  | S_Bool false -> S_Int Int64.zero
  | S_Bool true -> S_Int Int64.one
  | S_BitVector bv -> S_Int (BV.to_int64 bv)

let convert_to_bool = function
  | S_Int i -> S_Bool (not (Int64.equal i 0L))
  | S_Bool b -> S_Bool b
  | S_BitVector _ as s ->
      Warn.fatal "ASLScalar invalid op: to_bool %s" (pp false s)

let try_extract_slice s positions =
  match s with
  | S_BitVector bv ->
      if List.exists (( <= ) (BV.length bv)) positions then None
      else Some (S_BitVector (BV.extract_slice bv positions))
  | S_Int i ->
      if List.exists (( <= ) 64) positions then None
      else Some (S_BitVector (BV.extract_slice (BV.of_int64 i) positions))
  | _ -> None
