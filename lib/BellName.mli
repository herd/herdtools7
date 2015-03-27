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


(* Those names are special *) 
val scopes : string
val regions : string
val wider : string
val narrower : string

(* Change tag names into event set and relation name *)
val tag2events_var : string -> string
val tag2rel_var : string -> string

(* Valid names of various categories *)
val r : string
val w : string
val f : string
val rmw : string

val all_mem_sets : StringSet.t
val all_sets : StringSet.t
val all_rels : StringSet.t
val all_orders : StringSet.t
