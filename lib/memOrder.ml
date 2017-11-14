(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

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

let extract_read mo = match mo with
| Acq|SC|Rlx|Con -> mo
| Acq_Rel -> Acq
| Rel -> Rlx

let extract_write mo = match mo with
| Rel|SC|Rlx -> mo
| Acq_Rel -> Rel
| Acq|Con -> Rlx
