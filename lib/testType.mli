(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Type as specified in tests *)

type t =
  | TyDef | TyDefPointer
  | Ty of string | Pointer of string
  | TyArray of string * int
  | Atomic of string

(* Default base type (i.e. int) *)
val default : string

val pp : t -> string

val is_array : t -> bool
val get_array_primitive_ty :t -> string

val size_of : MachSize.sz -> string -> MachSize.sz

val is_signed : string -> bool

val tr_nbits : int -> string
