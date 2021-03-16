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

type sz = Byte | Short | Word | Quad | S128

val pp : sz -> string
val pp_short : sz -> string
val debug : sz -> string

val nbytes : sz -> int
val nbits : sz -> int
val is_imm16 : int -> bool

val tr_endian : sz -> int -> int

(* All valid offsets for sz2 in sz1 *)
val get_off : sz -> sz -> int list

(* All valid offsets for sz2 in sz1, reduced list *)
val get_off_reduced : sz -> sz -> int list

val compare : sz -> sz -> int

(* Smaller of two *)
val less_than_or_equal : sz -> sz -> bool

module Set : MySet.S with type elt = sz

val min : sz -> sz -> sz

val pred : sz -> sz

module Tag : sig
  type t = Auto | Size of sz
  val tags : string list
  val parse : string -> t option
  val pp : t -> string
end
