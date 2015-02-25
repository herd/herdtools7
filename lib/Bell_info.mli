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

type event_type = string
(* val pp_event_type : string -> string *)

type annotation = string
(* val pp_annotation : string -> string *)

type annot_set = annotation list
(* val pp_annot_set : string list -> string *)

type annot_group = annot_set list
type event_dec = event_type * annot_group list

type all_event_decs = event_dec list
val pp_all_event_decs : all_event_decs -> string


type relation_type = string
type relation_annot = string
type relation_annot_set = relation_annot list
type relation_dec = string * relation_annot_set
type all_relation_decs = relation_dec list
val pp_all_rel_decs : all_relation_decs -> string

type order = string * string
type order_dec = string * order list
type all_order_decs = order_dec list

val pp_all_order_decs : all_order_decs -> string

type bell_model_info = {
  all_events : annot_set;
  events : all_event_decs;
  relations : all_relation_decs;
  orders : all_order_decs;
}

type mem_space_map = (string * string) list
val pp_mem_map : (string * string) list -> string

type scopes = Leaf of string * int list | Children of string * scopes list
val pp_scopes : scopes -> string

type bell_test_info = {
  regions : mem_space_map option;
  scopes : scopes option;
}

(*
val known_sets : string list
val known_relations : string list
val known_orders : string list
*)

val build_bell_info :
  all_event_decs -> all_relation_decs -> all_order_decs -> bell_model_info

val check_annots : event_type -> annotation list -> bell_model_info -> bool
val check_regions : ('a * annotation) list -> bell_model_info -> bool
val check_scopes : scopes -> bell_model_info -> bool

val get_mem_annots : bell_model_info -> string list
val get_region_sets : bell_model_info -> string list
val get_scope_rels : bell_model_info -> string list
