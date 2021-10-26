(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
module type Edge = sig
  type arch_edge

  val pp_arch_edge : arch_edge -> string
  val dir_tgt : arch_edge -> Code.dir
  val dir_src : arch_edge -> Code.dir
  val loc_sd : arch_edge -> Code.sd
  val get_ie : arch_edge -> Code.ie
  val fold_edge : (arch_edge -> 'a -> 'a) -> 'a -> 'a
end

module type S = sig
(* Atoms *)
  include Atom.S

(* Page table entry *)
  module PteVal : PteVal_gen.S with type pte_atom = atom

(* Fences *)
  type fence

  val is_isync : fence -> bool

  val compare_fence : fence -> fence -> int

  val default : fence
  val strong : fence

  val pp_fence : fence -> string

  val fold_cumul_fences : (fence -> 'a -> 'a) -> 'a -> 'a
  val fold_all_fences : (fence -> 'a -> 'a) -> 'a -> 'a
  val fold_some_fences : (fence -> 'a -> 'a) -> 'a -> 'a

  open Code

  val orders : fence -> dir -> dir -> bool
  val var_fence : (fence -> 'a -> 'a) -> 'a -> 'a

(* Dependencies *)
  type dp
  val pp_dp : dp -> string
  val fold_dpr : (dp -> 'a -> 'a) -> 'a -> 'a
  val fold_dpw : (dp -> 'a -> 'a) -> 'a -> 'a

(* Defaults for backward compatibility *)
  val ddr_default : dp option
  val ddw_default : dp option
  val ctrlr_default : dp option
  val ctrlw_default : dp option

(* Predicate for control on reads *)
  val is_ctrlr : dp -> bool
  val is_addr : dp -> bool

(* Sequence dependencies *)
  val fst_dp : dp -> dp list
  val sequence_dp : dp -> dp -> dp list

(* Read-Modify-Write *)
  type rmw
  val pp_rmw : rmw -> string
  val fold_rmw : (rmw -> 'a -> 'a) -> 'a -> 'a
  val applies_atom_rmw : rmw -> atom option -> atom option -> bool
  val show_rmw_reg : rmw -> bool
  val compute_rmw : rmw -> int -> int -> int

end
