(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

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

  (** [shift_left u by] does a logical shift of [u] by [by] bits.
   *  Behavior is undefined, including potentially raising [Failure],
   *  if [by < 0] or [by >= num_bits]. *)
  val shift_left : t -> int -> t

  (** [shift_right u by] does an arithmetic shift of [u] by [by] bits,
   *  extending the sign bit into the vacated bits.
   *  Behavior is undefined, including potentially raising [Failure],
   *  if [by < 0] or [by >= num_bits]. *)
  val shift_right : t -> int -> t

  (** [shift_right u] does a logical shift of [u] by [by] bits, which does not
   *  consider the sign bit special.
   *  Behavior is undefined, including potentially raising [Failure],
   *  if [by < 0] or [by >= num_bits]. *)
  val shift_right_logical : t -> int -> t

  val leading_zeros : t -> int

  val compare : t -> t -> int
  val equal : t -> t -> bool

  val to_int : t -> int
  val of_int : int -> t

  (** [to_string u] converts [u] to a decimal representation. *)
  val to_string : t -> string

  (** [to_string_hex u] converts [u] to a hexadecimal representation, prefixed
   *  by 0x. For example, [to_string_hex 12345] returns ["0x3039"]. *)
  val to_string_hex : t -> string

  (** [of_string s] converts [s] to an integer, and respects 0x, 0o, and 0b
   *  prefixes for hexadecimal, octal, and binary numbers. It raises [Failure]
   *  if the input is invalid. *)
  val of_string : string -> t
end

module Uint8 : sig
  type t
  val max_int : t
end

module Uint16 : sig
  type t
  val max_int : t
end

module Uint32 : sig
  type t
  val max_int : t
end

module Uint64 : sig
  type t

  include S with type t := t

  val of_int64 : int64 -> t
end

module Uint128 : sig
  type t

  include S with type t := t

  val to_int64 : t -> Int64.t
  val of_int64 : Int64.t -> t
  val of_uint64 : Uint64.t -> t
  val of_uint32 : Uint32.t -> t
  val of_uint16 : Uint16.t -> t
  val of_uint8 : Uint8.t -> t
end
