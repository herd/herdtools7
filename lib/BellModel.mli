(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* Tyler Sorensen, University College London, UK.                    *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Bell information ready for usage *)

type annot_set = StringSet.t
type annot_group = annot_set list
val pp_annot_group : annot_group -> string

type event_dec = annot_group list
type event_decs = event_dec StringMap.t
val event_decs_empty : event_decs
val pp_event_dec : event_dec -> string
val pp_event_decs : event_decs -> string

type relation_dec = string list
type relation_decs = relation_dec StringMap.t
val pp_rel_decs : relation_decs -> string

type order_dec = StringRel.t
val pp_order_dec : order_dec -> string
type order_decs = order_dec StringMap.t
val pp_order_decs : order_decs -> string

type default_dec = string list
val pp_default_dec : default_dec -> string
type default_decs = default_dec StringMap.t
val pp_default_decs : default_decs -> string

(* The type of information extracted from bell files *)
type info

val pp_info : info -> string
val empty_info : info

(* Get, do not fail *)
val get_regions : info -> StringSet.t option
val get_events : string -> info -> event_dec
val check_event : string -> string list -> info -> bool
val get_mem_annots : info -> StringSet.t
val get_region_sets : info -> StringSet.t
val get_scope_rels : info -> string list
(* Get, may fail (raises Not_found) *)
val get_relation : string -> info -> string list
val get_order : string -> info -> order_dec
val get_default : string -> info -> default_dec

(*******)
(* Add *)
(*******)

exception Defined

(* The first three functions raise Defined, if information is already here *)
val add_rel : string -> relation_dec -> info -> info
val add_regions : string list -> info -> info
val add_order : string -> order_dec -> info -> info
val add_default : string -> default_dec -> info -> info

(* Cumulate information, ie does not raise Defined *)

(* On some event_decs structire *)
val add_event_dec : string -> annot_group -> event_decs -> event_decs
(* On complet info structure *)
val add_events : string -> annot_group -> info -> info
