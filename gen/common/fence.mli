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
module type S = sig
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
  include Dep.S with type dp := dp
end
