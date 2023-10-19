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
  val pp_global : arch_global -> string
  val global_compare : arch_global -> arch_global -> int
(* Identifiers in faults are considered identical *)
  val same_id_fault : arch_global -> arch_global -> bool

  type fault_type
  val pp_fault_type : fault_type -> string
  val fault_type_compare : fault_type -> fault_type -> int
end

type ('loc, 'ftype) atom =
  (Proc.t * Label.t option) * 'loc option * 'ftype option

val pp_fatom : ('loc -> string) -> ('ftype -> string) -> ('loc,'ftype) atom -> string

val atom_compare : ('loc -> 'loc -> int) -> ('ftype -> 'ftype -> int) ->
                   ('loc,'ftype) atom -> ('loc,'ftype) atom -> int

val map_value : ('v -> 'w) -> ('v,'ftype) atom -> ('w,'ftype) atom

module type S = sig

  type loc_global
  type fault_type

  type fault =
    (Proc.t * Label.Set.t) * loc_global option
    * fault_type option * string option

  val pp_fault : fault -> string

  module FaultSet : MySet.S with type elt = fault

  type fatom = (loc_global,fault_type) atom

  val pp_fatom : fatom -> string
  val check_one_fatom : fault -> fatom -> bool
  val check_fatom : FaultSet.t -> fatom -> bool

  module FaultAtomSet : MySet.S with type elt = fatom

end

module Make : functor (A:I) -> S with type loc_global := A.arch_global and type fault_type := A.fault_type
