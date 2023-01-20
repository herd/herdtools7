open Asllib
module BV = Bitvector

type t = S_Int of Int64.t | S_Bool of bool | S_BitVector of Bitvector.t

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

let compare s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> Int64.compare i1 i2
  | S_Int _, _ | _, S_Int _ -> 1
  | S_BitVector bv1, S_BitVector bv2 -> BV.compare bv1 bv2
  | S_BitVector _, _ | _, S_BitVector _ -> 1
  | S_Bool b1, S_Bool b2 -> Bool.compare b1 b2

let equal s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> Int64.equal i1 i2
  | S_Bool b1, S_Bool b2 -> b1 = b2
  | S_BitVector bv1, S_BitVector bv2 -> BV.equal bv1 bv2
  | _ -> false

let add s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Int64.add i1 i2)
  | _ -> raise (Invalid_argument "ASLScalar.add")

let sub s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Int64.sub i1 i2)
  | _ -> raise (Invalid_argument "ASLScalar.sub")

let mul s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Int64.mul i1 i2)
  | _ -> raise (Invalid_argument "ASLScalar.mul")

let div s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Int64.div i1 i2)
  | _ -> raise (Invalid_argument "ASLScalar.div")

let logor s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Int64.logor i1 i2)
  | S_Bool b1, S_Bool b2 -> S_Bool (b1 || b2)
  | S_BitVector bv1, S_BitVector bv2 -> S_BitVector (BV.logor bv1 bv2)
  | _ -> raise (Invalid_argument "ASLScalar.logor")

let logand s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Int64.logand i1 i2)
  | S_Bool b1, S_Bool b2 -> S_Bool (b1 && b2)
  | S_BitVector bv1, S_BitVector bv2 -> S_BitVector (BV.logand bv1 bv2)
  | _ -> raise (Invalid_argument "ASLScalar.logand")

let logxor s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> S_Int (Int64.logxor i1 i2)
  | S_Bool b1, S_Bool b2 -> S_Bool (b1 != b2)
  | S_BitVector bv1, S_BitVector bv2 -> S_BitVector (BV.logxor bv1 bv2)
  | _ -> raise (Invalid_argument "ASLScalar.logxor")

let lognot = function
  | S_Int i -> S_Int (Int64.lognot i)
  | S_Bool b -> S_Bool (not b)
  | S_BitVector bv -> S_BitVector (BV.lognot bv)

let shift_left = function
  | S_Int i -> fun k -> S_Int (Int64Scalar.shift_left i k)
  | _ -> raise (Invalid_argument "ASLScalar.shift_left")

let shift_right_logical = function
  | S_Int i -> fun k -> S_Int (Int64Scalar.shift_right_logical i k)
  | _ -> raise (Invalid_argument "ASLScalar.shift_right_logical")

let shift_right_arithmetic = function
  | S_Int i -> fun k -> S_Int (Int64Scalar.shift_right_arithmetic i k)
  | _ -> raise (Invalid_argument "ASLScalar.shift_right_arithmetic")

let addk = function
  | S_Int i -> fun k -> S_Int (Int64Scalar.addk i k)
  | _ -> raise (Invalid_argument "ASLScalar.addk")

let bit_at i1 = function
  | S_Int i2 -> S_Int (Int64Scalar.bit_at i1 i2)
  | S_BitVector bv -> S_BitVector (BV.extract_slice bv [ i1 ])
  | S_Bool _ -> raise (Invalid_argument "ASLScalar.bit_at")

let lt s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> Int64Scalar.lt i1 i2
  | _ -> raise (Invalid_argument "ASLScalar.lt")

let le s1 s2 =
  match (s1, s2) with
  | S_Int i1, S_Int i2 -> Int64Scalar.le i1 i2
  | _ -> raise (Invalid_argument "ASLScalar.le")

let mask sz = function
  | S_Int i -> S_Int (Int64Scalar.mask sz i)
  | _ -> raise (Invalid_argument "ASLScalar.mask")

let sxt sz = function
  | S_Int i -> S_Int (Int64Scalar.sxt sz i)
  | _ -> raise (Invalid_argument "ASLScalar.sxt")

let get_tag _t = assert false
let set_tag _b _t = assert false
