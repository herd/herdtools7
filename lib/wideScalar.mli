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

(** Subpart of the Scalar.S interface *)

(*
 * We keep what is useful for wide (vector) registers.
 * Namely, we keep bitwise logical operations and
 * shift operations.
 *   When considered as integers, those scalars are
 * unsigned. That is, functions as `of_int` etc. consider
 * that `int` is interpreted as unsigned.
 * Accordingly, all printing are in hexadecimal.
 *)


module type S = sig

  type  t

  val of_string : string -> t

  val pp : t -> string

  val of_int : int -> t
  val to_int : t -> int (* Hum *)

  val of_int64 : int64 -> t
  val to_int64 : t -> int64 (* Hum *)

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val is_zero : t -> bool

(* Operations *)
  val logor : t -> t -> t
  val logand : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t

  val shift_left : t -> int -> t
  val shift_right : t -> int -> t

  val bit_at : int -> t -> t
  val lt : t -> t -> bool
  val le : t -> t -> bool
  val mask : MachSize.sz -> t -> t

end
