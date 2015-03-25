(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*        Luc Maranget INRIA Paris-Rocquencourt, France.             *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(** Standard dependencies *)
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

(* Dependencies compositin by sequence *)
val fst_dp : dp -> dp list
val sequence_dp : dp -> dp -> dp list


(* pp *)
val pp_dp : dp -> string

