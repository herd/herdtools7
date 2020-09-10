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

(** Constants in code *)

(*
   Symbols defined below are the union of all possible sort of symbols
   used by all tools. Abstract later?
*)

type syskind = PTE|TAG|TLB (* Various kinds of system memory *)

type symbol =
  | Virtual of (string * string option) * int (* (symbol, optional tag), index *)
  | Physical of string * int                  (* symbol, index *)
  | System of (syskind * string)                 (* System memory *)

val pp_symbol : symbol -> string
val as_address : symbol -> string
val symbol_compare : symbol -> symbol -> int
(* 'phy' is the physical address (initially) matching virual adress 'virt' *)
val virt_match_phy : symbol (* virt *) -> symbol (* phy *)-> bool
val is_non_mixed_symbol : symbol -> bool

module SymbolSet : MySet.S with type elt = symbol
module SymbolMap : MyMap.S with type key = symbol

(* Add scalars *)
type 'scalar t =
  | Concrete of 'scalar
(* Memory cell, with optional tag and offet *)
  | Symbolic  of symbol
  | Label of Proc.t * string     (* In code *)
  | Tag of string
  | PteVal of PTEVal.t

val mk_sym : string -> 'scalar t
val get_sym : 'scalar t -> string
val default_tag : 'scalar t

(* Check  non-concrete constant (and change type!) *)
val check_sym : 'a t -> 'b t

val is_virtual : 'a t -> bool
val as_virtual : 'a t -> string option

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

(* Arch dependent result *)
  exception Result of Archs.t * v * string
end
