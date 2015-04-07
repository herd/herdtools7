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

open Printf

type annot_set = StringSet.t
type annot_group = annot_set list
type event_dec = annot_group list
type event_decs = event_dec StringMap.t

let pp_string_set set = StringSet.pp_str "," Misc.identity set

let pp_annot_set set = sprintf "{%s}" (pp_string_set set)

let pp_annot_group ag = 
  let mapped = List.map pp_annot_set ag in
  "["^(String.concat ", " mapped)^"]"

let pp_event_dec  l  =
  String.concat "," (List.map (fun x -> pp_annot_group x) l)

let pp_event_dec_bd t l  = sprintf "%s: %s" t (pp_event_dec l)

let pp_event_decs decs = StringMap.pp_str_delim "\n" pp_event_dec_bd decs


type relation_dec = string list
type relation_decs = relation_dec StringMap.t

let pp_rel_annot_set set =
  sprintf "{%s}" (String.concat "," set)

let pp_rel_dec t dec = sprintf "%s: %s" t (pp_rel_annot_set dec)

let pp_rel_decs decs = StringMap.pp_str_delim "\n" pp_rel_dec decs


type order_dec = StringRel.t
type order_decs = order_dec StringMap.t

let pp_order_dec ol =
  StringRel.pp_str " "
    (fun (f,s) -> sprintf "(%s,%s)" f s)
    ol
  
let pp_order_bd t ol =  sprintf "%s: %s" t (pp_order_dec ol)

let pp_order_decs decs = StringMap.pp_str_delim "\n" pp_order_bd decs

type default_dec = string list
type default_decs = default_dec StringMap.t

let pp_default_dec xs = sprintf "[%s]" (String.concat "," xs)
let pp_default_bd k d = sprintf "%s%s" k (pp_default_dec d)
let pp_default_decs decs  = StringMap.pp_str_delim " " pp_default_bd decs

type info = {
  all_events : annot_set; (* This field records all annotations *)
  events : event_decs;
  relations : relation_decs;
  orders : order_decs ;
  defaults : default_decs ;
  regions : StringSet.t option ;
}

let pp_info  i = 
  sprintf "All events: %s\n" (pp_string_set i.all_events) ^
  "Events:\n" ^ pp_event_decs i.events ^ "\n" ^
  "Relations:\n" ^ pp_rel_decs i.relations ^ "\n" ^
  "Orders:\n" ^ pp_order_decs i.orders ^ "\n" ^
  "Defaults: " ^ pp_default_decs i.defaults ^ "\n" ^
  (match i.regions with
  | None -> ""
  | Some r -> sprintf "Regions: %s\n" (pp_string_set r))
 
let empty_info = {
  all_events = StringSet.empty  ;
  events = StringMap.empty ;
  relations = StringMap.empty ;
  orders = StringMap.empty ;
  defaults = StringMap.empty ;
  regions = None ;
}


(* Get *)

let get_regions i = i.regions
(* By default, no annotation allowed *)
let get_events tag {events;_} =  StringMap.safe_find [[]] tag events 


let rec same_length xs ys = match xs,ys with
| [],[] -> true
| _::xs,_::ys -> same_length xs ys
| ([],_::_) | (_::_,[]) -> false


let check_event id al bi =
  let events_group = get_events id bi in
  List.exists
    (fun ag -> same_length ag al && List.for_all2 StringSet.mem al ag)
    events_group

let get_mem_annots i = i.all_events

let get_region_sets i = match i.regions with
| None -> StringSet.empty
| Some r -> r

let get_scope_rels i = StringMap.safe_find [] BellName.scopes i.relations
let get_relation k i =  StringMap.find k i.relations
let get_order k i = StringMap.find k i.orders
let get_default k i = StringMap.find k i.defaults
  
(* Add *)

exception Defined

let add_rel k dec i =
  begin try
    ignore (StringMap.find k i.relations) ; raise Defined
  with Not_found -> () end ;
  let relations = StringMap.add k dec i.relations in
  { i with relations;}

let add_regions dec i =
  match i.regions with
  | None -> { i with regions = Some (StringSet.of_list dec); }
  | Some _ -> raise Defined

let add_events k dec i =
  let old =
    try StringMap.find k i.events
    with Not_found -> [] in
  let events = StringMap.add k (dec::old) i.events
  and all_events =
    if StringSet.mem k BellName.all_mem_sets then
      StringSet.union (StringSet.unions dec) i.all_events
    else i.all_events in
  { i with events; all_events; }

let add_default k dec i =
  try
    ignore (StringMap.find k i.defaults) ; raise Defined
  with Not_found ->
    let defaults = StringMap.add k dec i.defaults in
    { i with defaults; }

let add_order k dec i =
  begin try
    ignore (StringMap.find k i.orders) ; raise Defined
  with Not_found -> () end ;
  let orders = StringMap.add k dec i.orders in
  { i with orders;}
