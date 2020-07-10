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
  val same_base : arch_global -> arch_global -> bool
end

type 'loc atom =  (Proc.t * Label.t option) * 'loc

val pp_fatom : ('loc -> string) -> 'loc atom -> string

val atom_compare : ('loc -> 'loc -> int) -> 'loc atom -> 'loc atom -> int


module type S = sig

  type loc_global

  type fault = (Proc.t * Label.Set.t) * loc_global
  val pp_fault : fault -> string

  module FaultSet : MySet.S with type elt = fault

  type fatom = loc_global atom
  val check_one_fatom : fault -> fatom -> bool
  val check_fatom : FaultSet.t -> fatom -> bool

end

module Make : functor (A:I) -> S with type loc_global := A.arch_global
