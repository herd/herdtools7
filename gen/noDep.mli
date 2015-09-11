(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** No dependency *)

type dp
val fold_dpr : 'a -> 'b -> 'b
val fold_dpw : 'a -> 'b -> 'b
val ddr_default : dp option
val ddw_default : dp option
val ctrlr_default : dp option
val ctrlw_default : dp option
val is_ctrlr : 'a -> 'b
val fst_dp : 'a -> 'b
val sequence_dp : 'a -> 'b -> 'c
val pp_dp : 'a -> 'b
