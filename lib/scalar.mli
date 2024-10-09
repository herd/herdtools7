(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


module type S = sig

(* Business as usual *)
  type  t
  val machsize : MachSize.sz

  val zero : t
  val one : t

  (** zero is unique and here it is *)
  val unique_zero : bool

  (** Predicate that considers alternative "zero" constants *)     
  val is_zero : t -> bool

  val of_string : string -> t
  val pp : bool -> t -> string
  val pp_unsigned : bool -> t -> string

  val of_int : int -> t
  val to_int : t -> int (* Hum *)

  val of_int64 : int64 -> t
  val to_int64 : t -> int64 (* Hum *)

  val printable : t -> t
  val compare : t -> t -> int
  val unsigned_compare : t -> t -> int
  val equal : t -> t -> bool

(* Operations *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val logor : t -> t -> t
  val logand : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t
  val abs : t -> t
  val shift_left : t -> int -> t
  val shift_right_logical : t -> int -> t
  val shift_right_arithmetic : t -> int -> t
  val bit_at : int -> t -> t
  val addk : t -> int -> t
  val lt : t -> t -> bool
  val le : t -> t -> bool
  val mask : MachSize.sz -> t -> t
(* Sign extension to size of t *)
  val sxt : MachSize.sz -> t -> t
  val get_tag : t -> bool
  val set_tag : bool -> t -> t
  val promote : t -> t
  val demote : t -> t
end
