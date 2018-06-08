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

type sz = Byte | Short | Word | Quad
type t = sz

val tags : string list
val parse : string -> t option
val pp : t -> string

val pp_short : sz -> string
val debug : sz -> string

val nbytes : sz -> int
val nbits : sz -> int

val tr_endian : sz -> int -> int

(* All valid offsets for sz2 in sz1 *)
val get_off : sz -> sz -> int list
