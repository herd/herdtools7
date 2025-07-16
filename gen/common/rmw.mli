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

(** Signature of Rmw helper modules *)

module type S = sig
  type rmw
  type rmw_atom

  val pp_rmw : bool (* backward compatibility *) -> rmw -> string
  val is_one_instruction : rmw -> bool
  (* The first boolean indicates whether wildcard syntax is included in the fold *)
  val fold_rmw : bool -> (rmw -> 'a -> 'a) -> 'a -> 'a
  (* Second round of fold, for rmw with back compatible name *)
  val fold_rmw_compat : (rmw -> 'a -> 'a) -> 'a -> 'a
  val applies_atom_rmw : rmw -> rmw_atom option -> rmw_atom option -> bool
  val show_rmw_reg : rmw -> bool
  val compute_rmw : rmw  -> int (* old *) -> int (* operand *) -> int
  val expand_rmw : rmw -> rmw list
  (* NOTE To ensure unwanted value collision,
    the inital value of a `rmw` operation, if it appears in a cycle,
    returned by `init_rmw`, MUST work together with
    to_rmw_operand`, which returns the next value/operand for the `rmw`
    The `counter` indicate how many writes to a location,
    including directly write or any `rmw` operation. *)
  val init_rmw : rmw -> int
  val to_rmw_operand : rmw -> int (*init*) -> int (*counter*) -> int
end
