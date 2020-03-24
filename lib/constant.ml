(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2019-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

(** Constants in code *)

type syskind = PTE|TAG|TLB

type symbol =
  | Virtual of (string * string option) * int (* (symbol, optional tag), index *)
  | Physical of string * int                  (* symbol, index *)
  | System of (syskind * string)              (* System memory *)

let pp_index base o = match o with
| 0 -> base
| i -> sprintf "%s+%i" base i

let pp_location (s,t) = match t with
| None -> s
| Some t -> sprintf "%s:%s" s t

let pp_symbol = function
  | Virtual (s,o) -> pp_index (pp_location s) o
  | Physical (s,o) -> pp_index (Misc.add_physical s) o
  | System (TLB,s) -> Misc.add_tlb s
  | System (PTE,s) -> Misc.add_pte s
  | System (TAG,s) -> Misc.add_atag s

let as_address = function
  | Virtual ((s,None),0) -> s
  | sym -> Warn.fatal "symbol '%s' is not an address" (pp_symbol sym)

type 'scalar t =
  | Concrete of 'scalar
(* Memory cell, with optional tag and offet *)
  | Symbolic  of symbol
  | Label of Proc.t * string     (* In code *)
  | Tag of string

let do_mk_sym sym = match Misc.tr_pte sym with
| Some s -> System (PTE,s)
| None -> match Misc.tr_physical sym with
  | Some _ -> Warn.user_error "explicit physical address: %s" sym
  | None -> Virtual ((sym,None),0)

let mk_sym s = Symbolic (do_mk_sym s)

and get_sym = function
  | Symbolic (Virtual ((s,_),_)|Physical (s,_)|System (_,s)) -> s
  | Concrete _|Label _| Tag _ -> assert false

let is_non_mixed_symbol = function
  | Symbolic (_,idx) -> idx=0
  | Concrete _|Label _| Tag _ -> true

let default_tag = Tag "green"

let check_sym v =  match v with
| Concrete _ ->  assert false
| Symbolic _|Label _|Tag _ as sym -> sym

module type S =  sig

  module Scalar : Scalar.S

  type v = Scalar.t t
  val intToV  : int -> v
  val nameToV  : string -> v
  val zero : v
  val one : v
  val pp : bool -> v -> string (* true -> hexa *)
  val pp_v  : v -> string
  val compare : v -> v -> int
  val eq : v -> v -> bool
end
