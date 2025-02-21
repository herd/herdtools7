(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2024-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* location -> description *)
val memloc: string -> string

(* location -> address register -> description *)
val memloc_addr_by: string -> string -> string
val tagloc_of: string -> string -> string
val pte_of: string -> string -> string
val pa_of: string -> string -> string
val tlb_of: string -> string -> string

(* register -> description *)
val reg: string -> string

(* register1 -> register2 -> description *)
val reg_pair: string -> string -> string

(* location -> address register -> is_explicit -> description *)
val mem_read: string -> string -> bool -> string
val mem_write: string -> string -> bool ->  string
val tag_read: string -> string -> bool -> string
val tag_write: string -> string -> bool -> string
val pte_read: string -> string -> bool -> string
val pte_write: string -> string -> bool -> string
val pa_read: string -> string -> bool -> string
val pa_write: string -> string -> bool -> string

(* label -> instruction -> description *)
val ifetch: string -> string -> string

(* type -> location -> description *)
val tlbi: string -> string -> string

(* type -> label -> description *)
val dc: string -> string -> string
val ic: string -> string -> string

(* type -> description *)
val generic_tlbi: string -> string
val generic_dc: string -> string
val generic_ic: string -> string

(* register -> description *)
val reg_read: string -> string
val reg_write: string -> string

(* location -> register -> description *)
val mte_cond: string -> string -> string

(* location -> register -> logical predicate -> description *)
val pte_cond: string -> string -> string -> string

(* condition -> description *)
val instr_cond: string -> string

(* register -> description *)
val any_active: string -> string

(* register -> index -> description *)
val active_elem: string -> string -> string

(* lhs -> rhs -> description *)
val eq_contents: string -> string -> string
val neq_contents: string -> string -> string

(* condition -> description *)
val branching: string -> string

(* description *)
val bcc_branching: string
val exc_return: string

(* name -> description *)
val fault: string -> string
val exc_entry: string -> string

(* description *)
val empty: string

(* Map from edge name to description *)
val edges: (string -> string -> string) StringMap.t
