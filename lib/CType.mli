(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* The subset of C types that we use *)

type base = string

type t =
  | Base of base
  | Volatile of t
  | Atomic of t
  | Pointer of t
(** limited arrays *)
  | Array of base * int

val voidstar : t
val word : t
val quad : t
val pte : t
val dump : t -> string
val debug : t -> string

type fmt = Direct of string | Macro of string

val get_fmt : bool (* hexa *) -> base -> fmt option

val is_ptr : t -> bool
val is_pte : t -> bool
val is_array : t -> bool
val is_atomic : t -> bool
val strip_atomic : t -> t
val strip_volatile : t -> t
val strip_attributes : t -> t

val is_ptr_to_atomic : t -> bool
(* Identical base types, is signed vs. unsigned *)
val same_base : t -> t -> bool

val type_for_align : int -> t
