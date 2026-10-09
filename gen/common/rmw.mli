(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type S = sig
  type rmw
  type atom

  val pp_rmw : bool -> rmw -> string
  val equal_rmw : rmw -> rmw -> bool
  val is_one_instruction : rmw -> bool
  val fold_rmw : bool -> (rmw -> 'a -> 'a) -> 'a -> 'a
  val fold_rmw_compat : (rmw -> 'a -> 'a) -> 'a -> 'a
  val applies_atom_rmw : rmw -> atom option -> atom option -> bool
  val show_rmw_reg : rmw -> bool
  val compute_rmw : rmw -> old:int -> operand:int -> int
  val expand_rmw : rmw -> rmw list
  val is_valid_rmw : rmw list -> bool
end

(** No rmw instruction *)
module No(A:sig type atom end) : S with type rmw = unit and type atom = A.atom

(** The only RMW is exchange *)
(* Implemented as load reserve store conditional *)
module LxSx(A:sig type atom end) : S with type rmw = unit and type atom = A.atom

(* Implemented as exchange instruction *)
module Exch(A:sig type atom end) : S with type rmw = unit and type atom = A.atom
