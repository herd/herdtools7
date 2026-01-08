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

module type SIMD = sig
    (* Atom particular for SIMD *)
    type atom
    val nregs : atom -> int
    val pp : atom -> string

    val initial : int -> int array
    val step : atom -> int -> int array -> int array
    val read : atom -> int array -> int list list
    val reduce: int list list -> int
end

module type RMW = sig
  (* The `rmw` edge *)
  type rmw
  (* Types `atom` and `value` should be passed from outside *)
  type atom

  val pp_rmw : bool (* backward compatibility *) -> rmw -> string
  val is_one_instruction : rmw -> bool
  (* The first boolean indicates whether wildcard syntax is included in the fold *)
  val fold_rmw : bool -> (rmw -> 'a -> 'a) -> 'a -> 'a
  (* Second round of fold, for rmw with back compatible name *)
  val fold_rmw_compat : (rmw -> 'a -> 'a) -> 'a -> 'a
  val applies_atom_rmw : rmw -> atom option -> atom option -> bool
  val show_rmw_reg : rmw -> bool
  val compute_rmw : rmw -> old:int -> operand:int -> int
  val expand_rmw : rmw -> rmw list
  val valid_rmw : rmw list -> bool
end

module type AtomType = sig
  (* The type for all annotations *)
  type atom
  (* The module and type `Value.v` for value. *)
  module Value : Value_gen.S with type atom = atom
  (* SIMD writes and reads *)
  module SIMD : SIMD
  (* RMW operation *)
  module RMW : RMW with type atom = atom
end

module type S = sig
  val bellatom : bool (* true if bell style atoms *)

  include AtomType

  val default_atom : atom
  val instr_atom : atom option
  val applies_atom : atom -> Code.dir -> bool
  val is_ifetch : atom option -> bool
  val compare_atom : atom -> atom -> int
  val get_access_atom : atom option -> MachMixed.t option
  val set_access_atom : atom option -> MachMixed.t -> atom option
  val pp_plain : string
  val pp_as_a : atom option
  val pp_atom : atom -> string
  val fold_non_mixed : (atom -> 'a -> 'a) -> 'a -> 'a
  val fold_mixed : (atom -> 'a -> 'a) -> 'a -> 'a
  val fold_atom : (atom -> 'a -> 'a) -> 'a -> 'a
  val worth_final : atom -> bool
  val varatom_dir : Code.dir -> (atom option -> 'a -> 'a) -> 'a -> 'a
  val merge_atoms : atom -> atom -> atom option
  val overlap_atoms : atom -> atom -> bool
(* Memory bank *)
  val atom_to_bank : atom -> SIMD.atom Code.bank
(* Value computation, for mixed size *)
  val tr_value : atom option -> Value.v -> Value.v
  val overwrite_value : Value.v -> atom option -> Value.v -> Value.v
  val extract_value : Value.v -> atom option -> Value.v
(* Typing of wide accesses as arrays of integers *)
  val as_integers : atom option -> int option
(* Typing of pair accesses is different, so check them *)
  val is_pair : atom option -> bool
  val get_machine_feature : atom option -> StringSet.t
end
