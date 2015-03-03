(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(* Tyler Sorensen, University College London                         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Bell information *)

(************)
(* In tests *)
(************)

type mem_space_map = (string * string) list
val pp_mem_map : mem_space_map -> string

type scopes = Leaf of string * int list | Children of string * scopes list
val pp_scopes : scopes -> string

type test = {
  regions : mem_space_map option;
  scopes : scopes option;
}

(**************)
(* For models *)
(**************)

(*
type event_type = string
type annotation = string
type annot_set = annotation list
type annot_group = annot_set list
type event_dec = event_type * annot_group list

type all_event_decs = event_dec StringMap.t
val pp_all_event_decs : all_event_decs -> string


type relation_type = string
type relation_annot = string
type relation_annot_set = relation_annot list
type relation_dec = string * relation_annot_set
type all_relation_decs = relation_dec StringMap.t
val pp_all_rel_decs : all_relation_decs -> string

type order = StringRel.t
type all_order_decs = order StringMap.t
val pp_all_order_decs : all_order_decs -> string

type model = {
  all_events : annot_set;
  events : all_event_decs;
  relations : all_relation_decs;
  orders : all_order_decs;
}


(*
val known_sets : string list
val known_relations : string list
val known_orders : string list
*)

val build_bell_info :
  all_event_decs -> all_relation_decs -> all_order_decs -> model

val check_annots : event_type -> annotation list -> model -> bool
val check_regions : ('a * annotation) list -> model -> bool
val check_scopes : scopes -> model -> bool

val get_mem_annots : model -> string list
val get_region_sets : model -> string list
val get_scope_rels : model -> string list
*)
