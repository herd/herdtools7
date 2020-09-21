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
  | Atomic of t
  | Pointer of t
(** limited arrays *)
  | Array of base * int

let voidstar = Pointer (Base "void")
let word = Base "int"
let quad = Base "int64_t"
let pte = Pointer (Base "pteval_t")

let rec  dump = function
  | Base s -> s
  | Volatile (Base s) -> "volatile " ^ s
  | Atomic (Base s) -> "_Atomic " ^ s
  | Volatile t -> sprintf "%s volatile" (dump t)
  | Atomic t -> sprintf "_Atomic (%s)" (dump t)
  | Pointer t -> dump t  ^ "*"
  | Array (t,sz) -> sprintf "%s[%i]" t sz

let rec  debug = function
  | Base s -> sprintf "<%s>" s
  | Volatile (Base s) -> "volatile <" ^ s ^ ">"
  | Atomic (Base s) -> "_Atomic " ^ s
  | Volatile t -> sprintf "%s volatile" (debug t)
  | Atomic t -> sprintf "_Atomic (%s)" (debug t)
  | Pointer t -> debug t  ^ "*"
  | Array (t,sz) -> sprintf "%s[%i]" t sz

type fmt = Direct of string | Macro of string

let fmt10 = function
  | "atomic_t"
  | "int"|"char" ->  Some (Direct "d")
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
  | _ -> None

let get_fmt hexa = if hexa then fmt16 else fmt10

let rec is_ptr =  function
  | Pointer _ -> true
  | Atomic t|Volatile t -> is_ptr t
  | _ -> false

let rec is_pte t = match t with
| Base "pteval_t" -> true
| Atomic t|Volatile t -> is_pte t
| _ -> false

let rec is_array = function
  | Array _ -> true
  | Atomic t|Volatile t -> is_array t
  | _ -> false

let rec is_atomic = function
  | Volatile t -> is_atomic t
  | Atomic _ -> true
  | _ -> false

let strip_atomic0 = function
  | Atomic t -> t
  | t -> t

let rec strip_atomic = function
  | Volatile t -> Volatile (strip_atomic t)
  | t ->  strip_atomic0 t

let strip_volatile0 = function
  | Volatile t -> t
  | t -> t

let rec strip_volatile = function
  | Atomic t -> Atomic (strip_volatile t)
  | Pointer t -> Pointer (strip_volatile t)
  | t ->  strip_volatile0 t

let strip_attributes t = strip_atomic (strip_volatile t)

let rec is_ptr_to_atomic = function
  | Volatile t -> is_ptr_to_atomic t
  | Pointer t -> is_atomic t
  | _ -> false

let same_base t0 t1 = match t0,t1 with
| Base s0,Base s1 ->
    begin match s0,s1 with
    | ("int8_t","uint8_t")|("uint8_t","int8_t")
    | ("int16_t","uint16_t")|("uint16_t","int16_t")
    | ("int32_t","uint32_t")|("uint32_t","int32_t")
    | ("int64_t","uint64_t")|("uint64_t","int64_t")
      -> true
    | _,_ -> false
    end
| _,_ -> false

let type_for_align i = Array ("uint8_t",i)
