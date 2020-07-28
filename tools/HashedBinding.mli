(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

include (Hashcons.S with type key = HashedString.t * HashedString.t)

open Hashcons

val as_hashed  : string -> string -> key hash_consed

val as_t :  key hash_consed -> string * string

val as_hash :  key hash_consed -> int

val get_loc :  key hash_consed -> string
val get_v :  key hash_consed -> string

val compare_loc :  key hash_consed ->  key hash_consed -> int
val compare_v :  key hash_consed ->  key hash_consed -> int
val compare :  key hash_consed ->  key hash_consed -> int
