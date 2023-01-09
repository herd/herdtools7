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

open Printf

type base = string

type t =
  | Base of base
  | Volatile of t
  | Const of t
  | Atomic of t
  | Pointer of t
(** limited arrays *)
  | Array of base * int

let void = Base "void"
let voidstar = Pointer void
let word = Base "int"
let quad = Base "int64_t"
let int128 = Array ("int",4) (* Why not Base "int128_t"? *)
let pteval_t = Base "pteval_t"
let pte = Pointer pteval_t
let ins_t = Base "ins_t"

let rec  dump = function
  | Base s -> s
  | Volatile (Base s) -> "volatile " ^ s
  | Atomic (Base s) -> "_Atomic " ^ s
  | Volatile t -> sprintf "%s volatile" (dump t)
  | Const t -> sprintf "%s const" (dump t)
  | Atomic t -> sprintf "_Atomic (%s)" (dump t)
  | Pointer t -> dump t  ^ "*"
  | Array (t,sz) -> sprintf "%s_%i_t" t sz

let rec  debug = function
  | Base s -> sprintf "<%s>" s
  | Volatile (Base s) -> "volatile <" ^ s ^ ">"
  | Atomic (Base s) -> "_Atomic " ^ s
  | Volatile t -> sprintf "%s volatile" (debug t)
  | Const t -> sprintf "%s const" (debug t)
  | Atomic t -> sprintf "_Atomic (%s)" (debug t)
  | Pointer t -> debug t  ^ "*"
  | Array (t,sz) -> sprintf "%s[%i]" t sz

type fmt = Direct of string | Macro of string

let fmt10 = function
  | "atomic_t"
  | "int"|"char" ->  Some (Direct "d")
  | "ins_t" -> Some (Direct "s")
  | "long" -> Some (Direct "ld")
  | "int8_t" -> Some (Macro  "PRIi8")
  | "uint8_t" -> Some (Macro  "PRIu8")
  | "int16_t" -> Some (Macro  "PRIi16")
  | "uint16_t" -> Some (Macro  "PRIu16")
  | "int32_t" -> Some (Macro  "PRIi32")
  | "uint32_t" -> Some (Macro  "PRIu32")
  | "int64_t" -> Some (Macro  "PRIi64")
  | "uint64_t" -> Some (Macro  "PRIu64")
  | "intprt_t" -> Some (Macro "PRIiPTR")
  | "uintprt_t" -> Some (Macro "PRIuPTR")
  | _ -> None

let fmt16 = function
  | "atomic_t"
  | "int"|"char" ->  Some (Direct "x")
  | "long" ->  Some (Direct "lx")
  | "int8_t" -> Some (Macro  "PRIx8")
  | "uint8_t" -> Some (Macro  "PRIx8")
  | "int16_t" -> Some (Macro  "PRIx16")
  | "uint16_t" -> Some (Macro  "PRIx16")
  | "int32_t" -> Some (Macro  "PRIx32")
  | "uint32_t" -> Some (Macro  "PRIx32")
  | "int64_t" -> Some (Macro  "PRIx64")
  | "uint64_t" -> Some (Macro  "PRIx64")
  | "intprt_t" -> Some (Macro "PRIxPTR")
  | "uintprt_t" -> Some (Macro "PRIxPTR")
  | "ins_t" -> Some (Direct "s")
  | _ -> None

let get_fmt hexa = if hexa then fmt16 else fmt10

let rec is_ptr =  function
  | Pointer _ -> true
  | Atomic t | Volatile t | Const t -> is_ptr t
  | Array _ | Base _ -> false

let rec is_pte t = match t with
| Base "pteval_t" -> true
| Atomic t|Volatile t -> is_pte t
| _ -> false

let rec is_array = function
  | Array _ -> true
  | Atomic t | Volatile t | Const t -> is_array t
  | Base _ | Pointer _ -> false

let rec is_atomic = function
  | Volatile t | Const t -> is_atomic t
  | Atomic _ -> true
  | Array _ | Base _ | Pointer _ -> false

let rec is_ins_t = function
  | Base "ins_t" -> true
  | Atomic t | Volatile t | Const t -> is_ins_t t
  | Array _|Base _|Pointer _ -> false

let rec strip_atomic = function
  | Volatile t -> Volatile (strip_atomic t)
  | Atomic t -> strip_atomic t (* We handle the case where we have malformed types *)
  | Const t -> Const (strip_atomic t)
  | Pointer t -> Pointer (strip_atomic t)
  | Base s -> Base s
  | Array (s,t) -> Array (s,t)

let rec strip_volatile = function
  | Atomic t -> Atomic (strip_volatile t)
  | Pointer t -> Pointer (strip_volatile t)
  | Const t -> Const (strip_volatile t)
  | Volatile t -> strip_volatile t (* We handle the case where we have malformed types*)
  | Base s -> Base s
  | Array (s,t) -> Array (s,t)

let rec strip_const = function
  | Atomic t -> Atomic (strip_const t)
  | Pointer t -> Pointer (strip_const t)
  | Volatile t -> Volatile (strip_const t)
  | Const t -> strip_const t (* handles the case of malformed types *)
  | Array (t,s) -> Array (t,s)
  | Base s -> Base s

let strip_attributes t = strip_const (strip_atomic (strip_volatile t))

let rec is_ptr_to_atomic = function
  | Volatile t -> is_ptr_to_atomic t
  | Const t -> is_ptr_to_atomic t
  | Pointer t -> is_atomic t
  | Array _ | Base _ | Atomic _ -> false

let same_base t0 t1 = match t0,t1 with
| Base s0,Base s1 ->
    begin match s0,s1 with
    | ("int8_t","uint8_t")|("uint8_t","int8_t")
    | ("int16_t","uint16_t")|("uint16_t","int16_t")
    | ("int32_t","uint32_t")|("uint32_t","int32_t")
    | ("int64_t","uint64_t")|("uint64_t","int64_t")
    | ("int128_t", "uint128_t")|("uint128_t","int128_t")
      -> true
    | _,_ -> false
    end
| _,_ -> false

let type_for_align i = Array ("uint8_t",i)

let element_type = function
  | Array (b,_) -> Base b
  | t -> Warn.fatal "Array type expected, found %s" (dump t)

let rec signed = function
  | Atomic t|Volatile t| Const t -> signed t
  | Pointer _|Array _ -> false
  | Base
      ("atomic_t"|"int"|"char"|"long"|
      "int8_t"|"int16_t"|"int32_t"|"int64_t"|"int128_t") -> true
 | Base _ -> false

(* Best effort *)
let do_base_size =
  let open MachSize in
  function
  | "char"|"int8_t"|"uint8_t" -> Some Byte
  | "short"|"int16_t"|"uint16_t" -> Some Short
  | "int"|"int32_t"|"uint32_t" -> Some Word
  | "int64_t"|"uint64_t" -> Some Quad
  | "int128_t"|"uint128_t" -> Some S128
  | _ -> None

let rec base_size t = match t with
| Atomic t|Volatile t| Const t -> base_size t
| Pointer _|Array _ -> None
| Base b -> do_base_size b
