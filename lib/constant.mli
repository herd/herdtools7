(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Inl_formatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Constants, both symbolic (ie addresses) and concrete (eg integers)  *)

type tag = string option
type cap = Int64.t
type offset = int

(* Symbolic location metadata*)
(* Memory cell, with optional tag, capability<128:95>,optional vector metadata, and offset *)
type symbolic_data =
  {
   name : string ;
   tag : tag ;
   cap : cap ;
   offset : offset ;
  }

val default_symbolic_data : symbolic_data
(*
   Symbols defined below are the union of all possible sort of symbols
   used by all tools. Abstract later?
*)

(* Various kinds of system memory *)

type syskind =
  | PTE  (* Page table entry *)
  | PTE2 (* Page table entry of page table entry (non-writable) *)
  | TLB  (* TLB key *)
  | TAG  (* Tag for MTE *)

type symbol =
  | Virtual of symbolic_data
  | Physical of string * int       (* symbol, index *)
  | System of (syskind * string)   (* System memory *)

val pp_symbol : symbol -> string
val compare_symbol : symbol -> symbol -> int
val symbol_eq : symbol -> symbol -> bool
val as_address : symbol -> string

(* 'phy' is the physical address (initially) matching virual adress 'virt' *)
val virt_match_phy : symbol (* virt *) -> symbol (* phy *)-> bool

module SymbolSet : MySet.S with type elt = symbol
module SymbolMap : MyMap.S with type key = symbol

(* Add scalars *)
type 'scalar t =
  | Concrete of 'scalar
  | ConcreteVector of int * 'scalar t list
  | Symbolic  of symbol
  | Label of Proc.t * string     (* In code *)
  | Tag of string
  | PteVal of PTEVal.t

(* Do nothing on non-scalar *)
val map_scalar : ('scalar -> 'scalar) -> 'scalar t -> 'scalar t

val mk_sym_virtual : string -> 'scalar t
val mk_sym : string -> 'scalar t
val mk_vec : int -> 'scalar t list -> 'scalar t
val mk_replicate : int -> 'scalar t -> 'scalar t

val is_symbol : 'scalar t -> bool
val is_non_mixed_symbol : symbol -> bool

val default_tag : 'scalar t

(* Check  non-concrete constant (and change type!) *)
val check_sym : 'a t -> 'b t

val is_virtual : 'a t -> bool
val as_virtual : 'a t -> string option
val as_symbol : 'a t -> symbol option
val as_symbolic_data : 'a t -> symbolic_data option
val of_symbolic_data : symbolic_data -> 'a t

val as_pte : 'a t -> 'a t option
val is_pt : 'a t -> bool

(* Those two are properties of ptevals.
   At the moment pteval are arch-independant. *)
val same_oa : 'a t -> 'a t -> bool
val writable : bool -> bool -> 'a t -> bool

module type S =  sig

  module Scalar : Scalar.S

  type v = Scalar.t t
  val intToV  : int -> v
  val stringToV  : string -> v
  val nameToV  : string -> v
  val zero : v
  val one : v
  val bit_at : int -> Scalar.t -> Scalar.t
  val pp : bool -> v -> string (* true -> hexa *)
  val pp_unsigned : bool -> v -> string (* true -> hexa *)
  val pp_v  : v -> string
  val compare : v -> v -> int
  val eq : v -> v -> bool
  val vToName : v -> string

(* Arch dependent result *)
  exception Result of Archs.t * v * string
end
