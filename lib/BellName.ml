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

let scopes = "scopes"
and regions = "regions"
and wider = "wider"
and narrower = "narrower"

let tag2instrs_var s =
  let len = String.length s in
  assert (len > 0) ;
  let c = s.[0] in
  String.make 1 (Misc.char_uppercase c) ^ String.sub s 1 (len-1)

let tag2rel_var s = s

let r = "R"
and w = "W"
and f = "F"
and rmw = "RMW"
and b = "B"
and call = "CALL"
and srcu = "SRCU"

let all_sets = StringSet.of_list [r; w; f; rmw; b; call; srcu;]
(*jade: why both all_mem_sets and all_sets? is all_mem_sets supposed to be sets
of memory events, in which case f and b shouldn't be in there?*)
let all_rels = StringSet.of_list [scopes;]
let all_orders = StringSet.of_list [scopes;]


let tr_compat = function
  | "CALL" -> f
  | n -> n
