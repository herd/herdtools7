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

module Uint64 = BaseUint64

module Uint128 = BaseUint128
