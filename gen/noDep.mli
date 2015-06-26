(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*          Luc Maranget INRIA Paris-Rocquencourt, France.           *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

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
