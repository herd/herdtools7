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

(** Unsigned integer types in pure OCaml. *)

let int_sub (a : int) (b : int) = a - b

module type S = sig
  type t

  val num_bits : int

  val zero : t
  val one : t
  val max_int : t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t

  val pred : t -> t
  val succ : t -> t

  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t

  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t

  val leading_zeros : t -> int

  val compare : t -> t -> int
  val equal : t -> t -> bool

  val to_int : t -> int
  val of_int : int -> t

  val to_string : t -> string
  val to_string_hex : t -> string
  val of_string : string -> t
end

module Uint8 = struct
  type t = Int64.t
  let max_int = 0xFFL
end

module Uint16 = struct
  type t = Int64.t
  let max_int = 0xFFFFL
end

module Uint32 = struct
  type t = Int64.t
  let max_int = 0xFFFFFFFFL
end

module Uint64 = struct
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

  let compare a b =
    match has_top_bit_set a, has_top_bit_set b with
    | false, false -> Int64.compare a b
    | true, false -> 1
    | false, true -> -1
    | true, true -> Int64.compare a b

  let equal = Int64.equal

  let add = Int64.add
  let sub = Int64.sub
  let mul = Int64.mul

  (* TODO: If we move our minimum OCaml version to 4.08, replace this with
   * Int64.unsigned_div and Int64.unsigned_rem. *)
  let div_and_rem a b =
    match a >= 0L, b >= 0L with
    | true, true -> Int64.div a b, Int64.rem a b
    | true, false -> 0L, a
    | false, true ->
        let int_bits = clear_top_bit a in
        let bottom_d, bottom_r = Int64.div int_bits b, Int64.rem int_bits b in
        let top_d, top_r = Int64.div (lognot top_bit) b, Int64.rem (lognot top_bit) b in
        let combined_d, combined_r =
          if compare (add top_r bottom_r) b = 0 then
            add (add bottom_d top_d) one, 0L
          else
            add bottom_d top_d, add bottom_r top_r
        in
        if compare (add combined_r one) b = 0 then
          add combined_d one, zero
        else
          combined_d, add combined_r one
    | false, false ->
        let compared = compare a b in
        if compared = 0 then
          1L, 0L
        else if compared < 0 then
          0L, a
        else
          1L, sub a b

  let div a b =
    let d, _ = div_and_rem a b in d

  let rem a b =
    let _, r = div_and_rem a b in r

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
    let of_string_dec a = Scanf.sscanf a "%Lu" (fun u -> u) in
    if String.length a >= 2 then
      match a.[0], a.[1] with
      | '0', 'x'
      | '0', 'o'
      | '0', 'b' -> Int64.of_string a
      | _ -> of_string_dec a
    else
      of_string_dec a
end

module Uint128 = struct
  type t = Uint64.t * Uint64.t

  let num_bits = 128

  let of_uint64 u = Uint64.zero, u
  let of_uint32 u = of_uint64 (Uint64.of_uint32 u)
  let of_uint16 u = of_uint64 (Uint64.of_uint16 u)
  let of_uint8 u = of_uint64 (Uint64.of_uint8 u)

  let zero = Uint64.zero, Uint64.zero
  let one = Uint64.zero, Uint64.one
  let max_int = Uint64.max_int, Uint64.max_int

  let logand a b = Uint64.logand (fst a) (fst b), Uint64.logand (snd a) (snd b)
  let logor a b = Uint64.logor (fst a) (fst b), Uint64.logor (snd a) (snd b)
  let logxor a b = Uint64.logxor (fst a) (fst b), Uint64.logxor (snd a) (snd b)
  let lognot a = Uint64.lognot (fst a), Uint64.lognot (snd a)

  let shift_left a by =
    if by < 0 then
      invalid_arg (Printf.sprintf "shift_left by negative: %i" by)
    else if by > num_bits then
      invalid_arg (Printf.sprintf "shift_left by %i" by)
    else if by = 0 then
      a
    else if by < 64 then
      let upper =
        Uint64.logor
          (Uint64.shift_left (fst a) by)
          (Uint64.shift_right_logical (snd a) (64-by))
      in
      upper, Uint64.shift_left (snd a) by
    else
      Uint64.shift_left (snd a) (by-64), Uint64.zero

  let shift_right_logical a by =
    if by < 0 then
      invalid_arg (Printf.sprintf "shift_right by negative: %i" by)
    else if by > num_bits then
      invalid_arg (Printf.sprintf "shift_right_logical %i" by)
    else if by = 0 then
      a
    else if by < 64 then
      let lower =
        Uint64.logor
          (Uint64.shift_left (fst a) (64-by))
          (Uint64.shift_right_logical (snd a) by)
      in
      Uint64.shift_right_logical (fst a) by, lower
    else
      Uint64.zero, Uint64.shift_right_logical (fst a) (by-64)

  let shift_right a by =
    let upper_bit = shift_left one (num_bits - 1) in
    let has_msb = logand a upper_bit = upper_bit in
    let rest = shift_right_logical a by in
    if has_msb then
      let rec upper_bits acc n =
        match n with
        | 0 -> acc
        | n -> upper_bits (logor upper_bit (shift_right_logical acc 1)) (n-1)
      in
      logor rest (upper_bits upper_bit by)
    else
      rest

  let leading_zeros a =
    match a with
    | (0L, a2) -> Uint64.num_bits + (Uint64.leading_zeros a2)
    | (a1, _) -> Uint64.leading_zeros a1

  let compare a b =
    match Uint64.compare (fst a) (fst b) with
    | 0 -> Uint64.compare (snd a) (snd b)
    | n -> n

  let equal a b =
    Uint64.equal (snd a) (snd b) && Uint64.equal (fst a) (fst b)

  let add a b =
    let carry =
      if Uint64.compare (Uint64.add (snd a) (snd b)) (snd a) < 0 then
        1L
      else
        0L
    in
    Uint64.add (Uint64.add (fst a) (fst b)) carry, Uint64.add (snd a) (snd b)

  let sub a b =
    let carry =
      if Uint64.compare (Uint64.sub (snd a) (snd b)) (snd a) > 0 then
        1L
      else
        0L
    in
    Uint64.sub (fst a) (Uint64.add (fst b) carry), Uint64.sub (snd a) (snd b)

  let mul (a1, a2) (b1, b2) =
    (* Multiplication by parts.
     * For example,
     *   25 * 31 =
     *     20*30 + 5*30 + 20*1 + 5*1 =
     *       2*3*100 + 5*3*10 + * 2*1*10 + 5*1. *)
    let upper_bits u =
      Uint64.shift_right_logical u 32
    in
    let lower_bits u =
      let bottom_32_bits = Uint64.of_uint32 Uint32.max_int in
      Uint64.logand u bottom_32_bits
    in
    let a2_1 = upper_bits a2 in
    let a2_2 = lower_bits a2 in

    let b2_1 = upper_bits b2 in
    let b2_2 = lower_bits b2 in

    let t1 = Uint64.mul a2_2 b2_2 in
    let t2 = Uint64.add (upper_bits t1) (Uint64.mul a2_1 b2_2) in
    let t3 = Uint64.add (lower_bits t2) (Uint64.mul a2_2 b2_1) in

    let lower = Uint64.add (lower_bits t1) (Uint64.shift_left t3 32) in
    let upper = Uint64.add (upper_bits t2) (upper_bits t3) in

    let upper = Uint64.add upper (Uint64.mul a2_1 b2_1) in
    let upper = Uint64.add upper (Uint64.mul a1 b2) in
    let upper = Uint64.add upper (Uint64.mul a2 b1) in
    upper, lower

  let div_and_rem a b =
    if compare b zero = 0 then
      raise Division_by_zero
    else if compare a b = 0 then
      one, zero
    else if compare a b < 0 then
      zero, a
    else
      (* Binary long division by shift and subtract. *)
      let rec div_and_rem' (i : int) (q : t) (r : t) (b : t) =
        let q, r =
          if compare r b >= 0 then
            logor q one, sub r b
          else
            q, r
        in
        if i = 0 then
          q, r
        else
          div_and_rem' (int_sub i 1) (shift_left q 1) r (shift_right_logical b 1)
      in
      let shift = (leading_zeros b) - (leading_zeros a) in
      div_and_rem' shift zero a (shift_left b shift)

  let div a b =
    let d, _ = div_and_rem a b in d

  let rem a b =
    let _, r = div_and_rem a b in r

  let pred = sub one
  let succ = add one

  let to_int a = Uint64.to_int (snd a)

  let of_int i =
    if i < 0 then
      Uint64.max_int, Uint64.of_int i
    else
      Uint64.zero, Uint64.of_int i

  let to_int64 (_,lo) = lo

  let of_int64 i =
    let hi =
      if Int64.compare i 0L < 0 then Uint64.max_int else Uint64.zero in
    hi,i


  let to_string a =
    let ten = of_int 10 in
    let string_of_digit q =
      assert (compare q ten < 0) ;
      Uint64.to_string (snd q)
    in
    let rec to_string' total a =
      if compare a ten < 0 then
        (string_of_digit a) :: total
      else
        let q, r = div_and_rem a ten in
        to_string' ((string_of_digit r) :: total) q
    in
    to_string' [] a |> String.concat ""

  let to_string_hex a =
    match a with
    | 0L, a2 -> Uint64.to_string_hex a2
    | a1, a2 -> Printf.sprintf "0x%Lx%016Lx" a1 a2

  let of_string raw =
    let of_string_hex raw =
      let len = String.length raw in
      if len <= 16 then
        Uint64.zero, Uint64.of_string ("0x" ^ raw)
      else if len <= 32 then
        Uint64.of_string ("0x" ^ (String.sub raw 0 (len-16))), Uint64.of_string ("0x" ^ (String.sub raw (len-16) 16))
      else
        failwith "too many hex digits"
    in
    let of_string_oct _a = failwith "not implemented: of_string_oct" in
    let of_string_bin raw =
      let len = String.length raw in
      if len <= 64 then
        Uint64.zero, Uint64.of_string ("0b" ^ raw)
      else if len <= 128 then
        Uint64.of_string ("0b" ^ (String.sub raw 0 (len-64))), Uint64.of_string ("0b" ^ (String.sub raw (len-64) 64))
      else
        failwith "too many bits"
    in
    let of_string_dec a =
      let of_char c =
        match c with
        | '0' -> zero
        | '1' -> one
        | '2' -> of_int 2
        | '3' -> of_int 3
        | '4' -> of_int 4
        | '5' -> of_int 5
        | '6' -> of_int 6
        | '7' -> of_int 7
        | '8' -> of_int 8
        | '9' -> of_int 9
        | _ -> failwith (Printf.sprintf "invalid character: %c" c)
      in
      let ten = of_int 10 in
      let acc = ref zero in
      String.iter (fun c -> acc := add (mul !acc ten) (of_char c)) a ;
      !acc
    in
    let len = String.length raw in
    if len >= 2 then
      match raw.[0], raw.[1] with
      | '0', 'x' -> of_string_hex (String.sub raw 2 (len-2))
      | '0', 'o' -> of_string_oct (String.sub raw 2 (len-2))
      | '0', 'b' -> of_string_bin (String.sub raw 2 (len-2))
      | _ -> of_string_dec raw
    else
      of_string_dec raw
end
