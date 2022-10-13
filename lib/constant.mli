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

val get_index : symbol -> int option
val pp_symbol_old : symbol -> string
val pp_symbol : symbol -> string
val compare_symbol : symbol -> symbol -> int
val symbol_eq : symbol -> symbol -> bool
val as_address : symbol -> string

val oa2symbol : OutputAddress.t -> symbol

(* 'phy' is the physical address (initially) matching virual adress 'virt' *)
val virt_match_phy : symbol (* virt *) -> symbol (* phy *)-> bool

module SymbolSet : MySet.S with type elt = symbol
module SymbolMap : MyMap.S with type key = symbol

(* Add scalars *)
type ('scalar,'pte) t =
  | Concrete of 'scalar
  | ConcreteVector of ('scalar,'pte) t list
  | Symbolic  of symbol
  | Label of Proc.t * string     (* In code *)
  | Tag of string
  | PteVal of 'pte
  | Instruction of InstrLit.t

val compare :
  ('scalar -> 'scalar -> int) ->
    ('pte -> 'pte -> int) ->
      ('scalar,'pte) t -> ('scalar,'pte) t -> int
val eq :
  ('scalar -> 'scalar -> bool) ->
    ('pte -> 'pte -> bool) ->
      ('scalar,'pte) t -> ('scalar,'pte) t -> bool

(* New style: PTE(s), PHY(s), etc. *)
val pp :
  ('scalar -> string) ->  ('pte -> string) ->  ('scalar,'pte) t  -> string
(* Old style: pte_s, phy_s, etc. *)
val pp_old :
  ('scalar -> string) ->  ('pte -> string) ->  ('scalar,'pte) t  -> string

(* Do nothing on non-scalar *)
val map_scalar : ('a -> 'b) -> ('a,'pte) t -> ('b,'pte) t
val map_label : (Label.t -> Label.t) -> ('s,'pte) t -> ('s,'pte) t
val map : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) t -> ('b,'d) t

val mk_sym_virtual : string -> ('scalar,'pte) t
val mk_sym : string -> ('scalar,'pte) t
val mk_sym_pte : string -> ('scalar,'pte) t
val mk_sym_pte2 : string -> ('scalar,'pte) t
val mk_sym_pa : string -> ('scalar,'pte) t
val old2new : string -> string

val mk_vec : int -> ('scalar,'pte) t list -> ('scalar,'pte) t
val mk_replicate : int -> ('scalar,'pte) t -> ('scalar,'pte) t

val is_symbol : ('scalar,'pte) t -> bool
val is_label : ('scalar,'pte) t -> bool

val is_non_mixed_symbol : symbol -> bool

val default_tag : ('scalar,'pte) t

(* Check  non-concrete constant (and change type!) *)
val check_sym : ('a,'pte) t -> ('b,'pte) t

val is_virtual : ('scalar,'pte) t -> bool
val as_virtual : ('scalar,'pte) t -> string option
val as_symbol : ('scalar,'pte) t -> symbol option
val as_symbolic_data : ('scalar,'pte) t -> symbolic_data option
val of_symbolic_data : symbolic_data -> ('scalar,'pte) t

val as_pte : ('scalar,'pte) t -> ('scalar,'pte) t option
val is_pt : ('scalar,'pte) t -> bool

module type S =  sig

  module Scalar : Scalar.S
  module PteVal : PteVal.S

  type v = (Scalar.t,PteVal.t) t
  val tr : (string,ParsedPteVal.t) t -> v
  val intToV  : int -> v
  val stringToV  : string -> v
  val nameToV  : string -> v
  val zero : v
  val one : v
  val bit_at : int -> Scalar.t -> Scalar.t
  val pp : bool -> v -> string (* true -> hexa *)
  val pp_unsigned : bool -> v -> string (* true -> hexa *)
  val pp_v  : v -> string
  val pp_v_old  : v -> string
  val compare : v -> v -> int
  val eq : v -> v -> bool
  val vToName : v -> string

end
