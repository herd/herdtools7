(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

module type S = sig
(* Atoms *)
  include Atom.S
(* Fences *)
  type fence

  val is_isync : fence -> bool

  val compare_fence : fence -> fence -> int

  val strong : fence

  val pp_fence : fence -> string

  val sig_of_fence : fence -> char

  val fold_cumul_fences : (fence -> 'a -> 'a) -> 'a -> 'a
  val fold_all_fences : (fence -> 'a -> 'a) -> 'a -> 'a
  val fold_some_fences : (fence -> 'a -> 'a) -> 'a -> 'a

  open Code

  val orders : fence -> dir -> dir -> bool

(* Dependencies *)
  type dp
  val pp_dp : dp -> string
  val sig_of_dp : dp -> char
  val fold_dpr : (dp -> 'a -> 'a) -> 'a -> 'a
  val fold_dpw : (dp -> 'a -> 'a) -> 'a -> 'a

(* Defaults for backward compatibility *)
  val ddr_default : dp option
  val ddw_default : dp option
  val ctrlr_default : dp option
  val ctrlw_default : dp option

(* Predicate for control on reads *)
  val is_ctrlr : dp -> bool

(* Sequence dependencies *)
  val fst_dp : dp -> dp list
  val sequence_dp : dp -> dp -> dp list
end
