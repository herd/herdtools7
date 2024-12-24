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

(* register -> description *)
val reg: string -> string

(* location -> address register -> description *)
val mem_read: string -> string -> string
val mem_write: string -> string -> string
val tag_read: string -> string -> string
val tag_write: string -> string -> string

(* register -> description *)
val reg_read: string -> string
val reg_write: string -> string

(* location -> register -> description *)
val mte_cond: string -> string -> string

(* condition -> description *)
val instr_cond: string -> string

(* lhs -> rhs -> description *)
val eq_contents: string -> string -> string
val neq_contents: string -> string -> string

(* condition -> description *)
val branching: string -> string

(* name -> description *)
val fault: string -> string
val exc_entry: string -> string

(* Map from edge name to description *)
val edges: (string -> string -> string) StringMap.t
