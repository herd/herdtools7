(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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
