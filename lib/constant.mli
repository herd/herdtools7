(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Inl_formatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
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
   pac : PAC.t ;
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
  | INTID (* Interrupt location *)

(*
 * Tag location are based upon physical or virtual addresses.
 * In effect the two kinds of tag locations cannot co-exists,
 * as teh formet is for VMSA mode and the latter for
 * ordinary model. However it is more convenient to carry
 * the physsical or virtual status in the location itself.
 *)

type tagkind =
  | PHY
  | VIR

type symbol =
  | Virtual of symbolic_data
  | Physical of string * int       (* symbol, index *)
  | TagAddr of tagkind * string * int
  | System of syskind * string     (* System memory *)

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

(** [(s, p, intr, inst) t] is the type of constants with [s] the type of scalars, [p]
    the type of page table entries, [intr] as the type of interrupt ids
    and [inst] the type of instructions. *)
type ('scalar, 'pte, 'intid, 'instr) t =
  | Concrete of 'scalar  (** A scalar, e.g. 3. *)
  | ConcreteVector of ('scalar, 'pte, 'intid, 'instr) t list
      (** A vector of constants, e.g. [[3, x, NOP]]. *)
  | ConcreteRecord of ('scalar, 'pte, 'intid, 'instr) t StringMap.t
      (** A record of constants, e.g. [{ addr: x; instr: NOP; index: 3 }] *)
  | Symbolic of symbol  (** A symbolic constant, e.g. [x] *)
  | Label of Proc.t * string  (** A label in code. *)
  | Tag of string
  | PteVal of 'pte  (** A page table entry. *)
  | IntidVal of 'intid (** An interrupt id location. *)
  | IntidUpdateVal of IntidUpdateVal.t
  | Instruction of 'instr  (** An instruction. *)
  | Frozen of int (** Frozen symbolic value. *)

val as_scalar : ('scalar, 'pte, 'intid, 'instr) t -> 'scalar option

val compare :
  ('scalar -> 'scalar -> int) ->
    ('pte -> 'pte -> int) ->
      ('intid -> 'intid -> int) ->
        ('instr -> 'instr -> int) ->
          ('scalar, 'pte, 'intid, 'instr) t -> ('scalar, 'pte, 'intid, 'instr) t -> int
val eq :
  ('scalar -> 'scalar -> bool) ->
    ('pte -> 'pte -> bool) ->
      ('intid -> 'intid -> bool) ->
        ('instr -> 'instr -> bool) ->
          ('scalar, 'pte, 'intid, 'instr) t -> ('scalar, 'pte, 'intid, 'instr) t -> bool

(* Return if the collision of two PAC fields can imply equality of the two
   syntactically different constants *)
val collision :
  ('scalar, 'pte, 'intid, 'instr) t ->
    ('scalar, 'pte, 'intid, 'instr) t ->
      (PAC.t * PAC.t) option

(* New style: PTE(s), PHY(s), etc. *)
val pp :
  ('scalar -> string) -> ('pte -> string) -> ('intid -> string) -> ('instr -> string) ->
    ('scalar, 'pte, 'intid, 'instr) t  -> string
(* Old style: pte_s, phy_s, etc. *)
val pp_old :
  ('scalar -> string) ->  ('pte -> string) -> ('intid -> string) -> ('instr -> string) ->
    ('scalar, 'pte, 'intid, 'instr) t  -> string

(* Do nothing on non-scalar *)
val map_scalar : ('a -> 'b) -> ('a,'pte,'intid,'instr) t -> ('b,'pte,'intid,'instr) t
val map_label : (Label.t -> Label.t) -> ('s,'pte,'intid,'instr) t -> ('s,'pte,'intid,'instr) t
val map :
  ('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('g -> 'h) -> ('a,'c,'e,'g) t -> ('b,'d,'f,'h) t

val mk_sym_virtual : string -> ('scalar, 'pte, 'intid, 'instr) t
val mk_sym : string -> ('scalar, 'pte, 'intid, 'instr) t
val mk_sym_with_index : string -> int -> ('scalar, 'pte, 'intid, 'instr) t
val mk_sym_pte : string -> ('scalar, 'pte, 'intid, 'instr) t
val mk_sym_pte2 : string -> ('scalar, 'pte, 'intid, 'instr) t
val mk_sym_pa : string -> ('scalar, 'pte, 'intid, 'instr) t
val mk_sym_intid : string -> ('scalar, 'pte, 'intid, 'instr) t
val old2new : string -> string

val mk_vec : int -> ('scalar,'pte,'intid,'instr) t list -> ('scalar,'pte,'intid,'instr) t
val mk_replicate : int -> ('scalar,'pte,'intid,'instr) t -> ('scalar,'pte,'intid,'instr) t

val is_symbol : ('scalar,'pte,'intid,'instr) t -> bool
val is_label : ('scalar,'pte,'intid,'instr) t -> bool
(* Extract label, if any *)
val as_label :  ('scalar,'pte,'intid,'instr)  t -> Label.Full.full option

val is_non_mixed_symbol : symbol -> bool

val default_tag : ('scalar,'pte,'intid,'instr) t

(* Check  non-concrete constant (and change type!) *)
val check_sym : ('a,'pte,'intid,'instr) t -> ('b,'pte,'intid,'instr) t

val is_virtual : ('scalar,'pte,'intid,'instr) t -> bool
val as_virtual : ('scalar,'pte,'intid,'instr) t -> string option
val as_symbol : ('scalar,'pte,'intid,'instr) t -> symbol option
val as_fault_base :  ('scalar,'pte,'intid,'instr) t -> string option
val as_symbolic_data : ('scalar,'pte,'intid,'instr) t -> symbolic_data option
val of_symbolic_data : symbolic_data -> ('scalar,'pte,'intid,'instr) t


val as_pte : ('scalar, 'pte, 'intid, 'instr) t -> ('scalar, 'pte, 'intid, 'instr) t option
val is_pt : ('scalar, 'pte, 'intid, 'instr)  t -> bool

(* Remove the Pac field of a virtual address *)
val make_canonical : ('scalar,'pte, 'intid, 'instr) t -> ('scalar,'pte,'intid,'instr) t

val is_intid : ('scalar, 'pte, 'intid, 'instr)  t -> bool
val as_intid : ('scalar, 'pte, 'intid, 'instr)  t -> ('scalar, 'pte, 'intid, 'instr) t option

module type S =  sig

  module Scalar : Scalar.S
  module PteVal : PteVal.S
  module IntidVal : IntidVal.S
  module Instr : Instr.S

  type v = (Scalar.t,PteVal.t,IntidVal.t,Instr.t) t

  val tr : (string,ParsedPteVal.t,ParsedIntidVal.t,InstrLit.t) t -> v
  val intToV  : int -> v
  val stringToV  : string -> v
  val nameToV  : string -> v

  (** numeric zero *)
  val zero : v

  (** numeric one *)
  val one : v

  (** Boolean values **)
  val cst_true : v
  val cst_false : v

  val as_int : v -> int option
  val as_bool : v -> bool option
  val bit_at : int -> Scalar.t -> Scalar.t
  val pp : bool -> v -> string (* true -> hexa *)
  val pp_unsigned : bool -> v -> string (* true -> hexa *)
  val pp_v  : v -> string
  val pp_v_old  : v -> string
  val compare : v -> v -> int
  val eq : v -> v -> bool
  val vToName : v -> string
  val is_nop : v -> bool

end
