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

(** External view of faults, which are part of final state *)


module type I = sig
  type arch_global
  type prop
  type frstate
  val pp_global : arch_global -> string
  val global_compare : arch_global -> arch_global -> int
(* Identifiers in faults are considered identical *)
  val same_id_fault : arch_global -> arch_global -> bool
  val frstate_compare : frstate -> frstate -> int
end

type ('loc,'prop) atom =  (Proc.t * Label.t option) * 'loc * 'prop option

val pp_fatom : ('loc -> string) -> ('v -> string) -> ('loc,'v) atom -> string
val pp_fatom_noprop : ('loc -> string) -> ('loc,'v) atom -> string

val atom_compare : ('loc -> 'loc -> int) -> ('loc,'prop) atom -> ('loc,'prop) atom -> int

val map_value : ('v -> 'w) -> ('v,'prop) atom -> ('w,'prop) atom

module type S = sig

  type loc_global
  type prop
  type rstate

  type fault = (Proc.t * Label.Set.t) * loc_global * rstate * string option
  val pp_fault : (rstate -> string) -> fault -> string

  module FaultSet : MySet.S with type elt = fault

  type fatom = (loc_global,prop) atom
  val check_one_fatom : (prop option -> rstate -> bool) -> fault -> fatom -> bool
  val check_fatom : (prop option -> rstate -> bool) -> FaultSet.t -> fatom -> bool

  module FaultAtomSet : MySet.S with type elt = fatom

end

module Make : functor (A:I) -> S with type loc_global := A.arch_global and type prop := A.prop and type rstate := A.frstate
