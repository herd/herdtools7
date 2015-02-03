(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

type t =
  | Acq
  | Rel
  | Acq_Rel
  | SC
  | Rlx
  | Con

let compare = Pervasives.compare

let pp_mem_order = function
  | Acq -> "memory_order_acquire"
  | Rel -> "memory_order_release"
  | Acq_Rel -> "memory_order_acq_rel"
  | SC -> "memory_order_seq_cst"
  | Rlx -> "memory_order_relaxed"
  | Con -> "memory_order_consume"

let pp_mem_order_short = function
  | Acq -> "Acq"
  | Rel -> "Rel"
  | Acq_Rel -> "AR"
  | SC -> "Sc"
  | Rlx -> "Rlx"
  | Con -> "Con"
