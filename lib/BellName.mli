(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Bell name handling *)


(* Those names are special *) 
val scopes : string
val regions : string
val levels : string
val nextlevel : string
val wider : string
val narrower : string

(* Change tag names into event set and relation name *)
val tag2instrs_var : string -> string
val tag2rel_var : string -> string

(* Valid names of various categories *)
val r : string
val w : string
val j : string
val f : string
val rmw : string
val b : string
val call : string

(* val all_mem_sets : StringSet.t *)
val all_sets : StringSet.t
val all_rels : StringSet.t
val all_orders : StringSet.t

val tr_compat : string -> string
