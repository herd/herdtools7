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

let fold_dpr _f r = r
let fold_dpw _f r = r

let ddr_default = None
let ddw_default = None
let ctrlr_default = None
let ctrlw_default = None

let is_ctrlr _ = assert false

let fst_dp _ = assert false
let sequence_dp _ _ = assert false

let pp_dp _ = assert false
