(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Bell name handling *)

let scopes = "scopes"
and regions = "regions"
and wider = "wider"
and narrower = "narrower"

let tag2events_var s =
  let len = String.length s in
  assert (len > 0) ;
  let c = s.[0] in
  String.make 1 (Char.uppercase c) ^ String.sub s 1 (len-1)

let tag2rel_var s = s

let r = "R"
and w = "W"
and f = "F"
and rmw = "RMW"

let all_mem_sets = StringSet.of_list [r; w; f; rmw;]
let all_sets = StringSet.of_list [r; w; f; rmw;]
let all_rels = StringSet.of_list [scopes;]
let all_orders = StringSet.of_list [scopes;]

