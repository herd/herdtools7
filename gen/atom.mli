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
    type atom
    val nregs : atom -> int
    val pp : atom -> string

    val initial : int -> int array
    val step : atom -> int -> int array -> int array
    val read : atom -> int array -> int list list
end

module type S = sig
  val bellatom : bool (* true if bell style atoms *)

(* SIMD writes and reads *)
  module SIMD : SIMD

  type atom
  val default_atom : atom
  val applies_atom : atom -> Code.dir -> bool
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
  val tr_value : atom option -> Code.v -> Code.v
  val overwrite_value : Code. v -> atom option -> Code.v -> Code.v
  val extract_value : Code. v -> atom option -> Code.v
(* Typing of wide accesses as arrays of integers *)
  val as_integers : atom option -> int option
(* Typing of pair accesses is different, so check them *)
  val is_pair : atom option -> bool
end
