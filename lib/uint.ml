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

  let to_int = Int64.to_int
  let of_int = Int64.of_int

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
