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

(** Use one one kind of RMW is avaiable *)
type rmw = unit

let pp_rmw () = ""

let fold_rmw f r = f () r

let applies_atom_rmw () ar aw = match ar,aw with
| None,None -> true
| _,_ -> false

let show_rmw_reg () = false

let compute_rmw rmw old co_cell  = match rmw with
| _ -> old+co_cell
