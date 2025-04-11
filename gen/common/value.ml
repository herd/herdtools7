(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type PteType = sig
  type atom
  type pte
  val pp_pte : pte -> string
  val default_pte : string -> pte
  val pte_compare : pte -> pte -> int
  val set_pteval : atom -> pte -> (unit -> string) -> pte
  val can_fault : pte -> bool
  val refers_virtual : pte -> string option
end

module type S = sig
  include PteType
  type v = NoValue | Plain of int | PteValue of pte
  type env = (string * v) list
  val pp_v : ?hexa:bool -> v -> string
  val no_value : v
  val to_int : v -> int
  val from_int : int -> v
  val to_pte : v -> pte
  val from_pte : pte -> v
  val value_compare : v -> v -> int
end

(* Default implement for many functions in signature `S`
   use it by `include Make(...)` *)
module Make(P:PteType) = struct

  include P
  type v = NoValue | Plain of int | PteValue of pte
  type env = (string * v) list

  let no_value = NoValue
  let to_int = function
    | NoValue -> -1
    | Plain v -> v
    | _ -> Warn.user_error "Cannot convert to int"
  let from_int v = Plain v

  (* NOTE to ensure this module satisfy the Value.S requirement,
     implement the following separately.
      let from_pte pte = (*...*)
      let to_pte v = (*...*)
  *)

  let value_compare lhs rhs =
    match lhs, rhs with
    | NoValue, NoValue -> 0
    | NoValue, _ -> -1
    | Plain lhs, Plain rhs -> Misc.int_compare lhs rhs
    | Plain _, NoValue -> 1
    | Plain _, _ -> -1
    | PteValue lhs, PteValue rhs -> pte_compare lhs rhs
    | PteValue _, _ -> 1

  let pp_v ?(hexa=false) = function
    | NoValue -> "**"
    | Plain v -> Printf.sprintf (if hexa then "0x%x" else "%d") v
    | PteValue p -> pp_pte p
end

module NoPte(A:sig type arch_atom end) = struct
  include Make(struct
    type atom = A.arch_atom
    type pte = string
    let pp_pte _ = "[nopte]"
    let default_pte s = s
    let pte_compare _ _ = 0
    let set_pteval _ p _ = p
    let can_fault _t = false
    let refers_virtual _ = None
  end)

  let from_pte _ = Warn.user_error "Cannot convert from pte"
  let to_pte _ = Warn.user_error "Cannot convert to pte"
end
