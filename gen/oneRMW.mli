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

(** Use when one kind of RMW is avaiable *)

type rmw = unit

val pp_rmw : rmw -> string
val fold_rmw : (rmw -> 'a -> 'a) -> 'a -> 'a
val applies_atom_rmw : rmw -> 'a option -> 'a option -> bool
val show_rmw_reg : rmw -> bool
val compute_rmw : rmw  -> int -> int -> int
