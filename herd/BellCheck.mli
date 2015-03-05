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

(** Check code w.r.t. bell definitions *)

type annot_set = StringSet.t
type annot_group = annot_set list
type event_dec = annot_group list
type event_decs = event_dec StringMap.t
val pp_event_decs : event_decs -> string

type relation_dec = string list
type relation_decs = relation_dec StringMap.t
val pp_rel_decs : relation_decs -> string

type order_dec = StringRel.t
val pp_order_dec : order_dec -> string
type order_decs = order_dec StringMap.t
val pp_order_decs : order_decs -> string


type info = {
  all_events : annot_set;
  events : event_decs;
  relations : relation_decs;
  orders : order_decs;
  regions : StringSet.t option;
}

val empty_info : info

(* Get, do not fail *)
val get_mem_annots : info -> StringSet.t
val get_region_sets : info -> StringSet.t
val get_scope_rels : info -> string list
(* Get, may fail (raises Not_found) *)
val get_order : string -> info -> order_dec

(*******)
(* Add *)
(*******)

exception Defined

(* The first three functions raise Defined, if information is already here *)
val add_rel : string -> relation_dec -> info -> info
val add_regions : string list -> info -> info
val add_order : string -> order_dec -> info -> info

(* Cumulate information, ie does not raise Defined *)
val add_events : string -> annot_group -> info -> info

module Make :
    functor (A:Arch.S) ->
      functor
        (C:sig
          val info : info option
          val get_id_and_list : A.instruction -> string * string list
        end) ->
  sig
    val check : A.pseudo MiscParser.t -> unit
  end
