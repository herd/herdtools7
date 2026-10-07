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

module type AtomType = sig
  type atom
  module Value : Value_gen.S with type atom = atom
  module SIMD : Simd.S
  module RMW : Rmw.S with type atom = atom
end

module type S = sig
  val bellatom : bool

  include AtomType

  val default_atom : atom
  val instr_atom : atom option
  val applies_atom : atom -> Code.dir -> bool
  val is_ifetch : atom option -> bool
  val compare_atom : atom -> atom -> int
  val get_access_atom : atom option -> Mixed.t option
  val set_access_atom : atom option -> Mixed.t -> atom option
  val pp_plain : string
  val pp_atom : atom -> string
  val pp_atom_separate : atom -> string list
  val fold_atom : (atom -> 'a -> 'a) -> 'a -> 'a
  val worth_final : atom -> bool
  val varatom_dir : Code.dir -> (atom option -> 'a -> 'a) -> 'a -> 'a
  val merge_atoms : atom -> atom -> atom option
  val overlap_atoms : atom -> atom -> bool
  val atom_to_bank : atom -> SIMD.atom Code.bank
  val tr_value : atom option -> Value.v -> Value.v
  val overwrite_value : Value.v -> atom option -> Value.v -> Value.v
  val extract_value : Value.v -> atom option -> Value.v
  val as_integers : atom option -> int option
  val is_pair : atom option -> bool
  val get_machine_feature : atom option -> StringSet.t
end

module NoWide = struct
  let as_integers _ = None
  let is_pair _ = false
end
