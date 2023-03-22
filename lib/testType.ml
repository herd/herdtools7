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

open Printf

type t =
  | TyDef | TyDefPointer
  | Ty of string | Pointer of string
  | TyArray of string * int
  | Atomic of string

let default = "int"

let pp = function
  | TyDef -> "TyDef"
  | TyDefPointer -> "TyDefPointer"
  | Ty s -> sprintf "Ty<%s>" s
  | Atomic s -> sprintf "Atomic<%s>" s
  | Pointer s -> sprintf "Pointer<%s>" s
  | TyArray (s,sz) -> sprintf "TyArray<%s,%i>" s sz

let is_array = function
  | TyArray _ -> true
  | _       -> false

let get_array_primitive_ty = function
  | TyArray (ty,_)
  | Pointer ty
    ->  ty
  | TyDef -> "int"
  | t ->
     Warn.user_error "Array of pointer type expected, found %s" (pp t)

(* Simplified typing, size only, integer types only *)
let size_of maximal = function
| "atomic_t"
| "int"|"long"
| "int32_t"
| "uint32_t" ->  MachSize.Word
| "char"|"int8_t" |"uint8_t" -> MachSize.Byte
| "short" | "int16_t" | "uint16_t" -> MachSize.Short
| "int64_t" | "uint64_t" -> MachSize.Quad
| "__int128_t" | "__uint128_t"
| "__int128" | "__uint128"
| "int128_t" | "uint128_t" -> MachSize.S128
| "intptr_t" | "uintptr_t" | "pteval_t"
  -> maximal (* Maximal size = ptr size *)
| t -> Warn.fatal "Cannot find the size of type %s" t

let is_signed = function
| "int"|"long"
| "int32_t"
| "char"|"int8_t"
| "short"|"int16_t"
| "int64_t"
| "int128_t"
| "__int128"
| "intptr_t" -> true
| _ -> false

let tr_nbits = function
| 8 -> "uint8_t"
| 16 -> "uint16_t"
| 32 -> "int"
| 64 -> "uint64_t"
| n -> Warn.fatal "No type for %d bits" n
