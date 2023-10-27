(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2011-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* C dependencies *)
type dp = ADDR | DATA | CTRL

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

(* Dependencies compositin by sequence *)
val fst_dp : dp -> dp list
val sequence_dp : dp -> dp -> dp list
