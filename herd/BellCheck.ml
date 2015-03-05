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

(** Bell model utilities *)

open Printf

type annot_set = StringSet.t
type annot_group = annot_set list
type event_dec = annot_group list
type event_decs = event_dec StringMap.t

let pp_annot_set set =
  sprintf "{%s}" (StringSet.pp_str "," Misc.identity set)

let pp_annot_group ag = 
  let mapped = List.map pp_annot_set ag in
  "["^(String.concat ", " mapped)^"]"

let pp_event_dec t l  = 
  let mapped = List.map (fun x -> pp_annot_group x) l in
  t^": "^ String.concat "," mapped

let pp_event_decs decs = StringMap.pp_str_delim "\n" pp_event_dec decs


type relation_dec = string list
type relation_decs = relation_dec StringMap.t

let pp_rel_annot_set set =
  sprintf "{%s}" (String.concat "," set)

let pp_rel_dec t dec = sprintf "%s %s" t (pp_rel_annot_set dec)

let pp_rel_decs decs = StringMap.pp_str_delim "\n" pp_rel_dec decs


type order_dec = StringRel.t
type order_decs = order_dec StringMap.t

let pp_order_dec ol =
  StringRel.pp_str " "
    (fun (f,s) -> sprintf "(%s,%s)" f s)
    ol
  
let pp_order_bd t ol =  sprintf "%s %s" t (pp_order_dec ol)

let pp_order_decs decs = 
  StringMap.pp_str_delim "\n" pp_order_bd decs

type info = {
  all_events : annot_set; (* This field records all annotations *)
  events : event_decs;
  relations : relation_decs;
  orders : order_decs ;
  regions : StringSet.t option ;
}

let empty_info = {
  all_events = StringSet.empty  ;
  events = StringMap.empty ;
  relations = StringMap.empty ;
  orders = StringMap.empty ;
  regions = None ;
}


(* Get *)
let get_events tag {events;_} =
  StringMap.safe_find [[StringSet.empty]] tag events 

let get_mem_annots i = i.all_events

let get_region_sets i = match i.regions with
| None -> StringSet.empty
| Some r -> r

let get_scope_rels i = StringMap.safe_find [] BellName.scopes i.relations

let get_order k i = StringMap.find k i.orders
  
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

let add_order k dec i =
  begin try
    ignore (StringMap.find k i.orders) ; raise Defined
  with Not_found -> () end ;
  let orders = StringMap.add k dec i.orders in
  { i with orders;}


module Make
    (A:Arch.S)
    (C:sig
      val info : info option
      val get_id_and_list : A.instruction -> string * string list
    end) =
  struct

(******************)
(* Checks of test *)
(******************)

    exception Error of string
    let error fmt =
      ksprintf (fun msg -> raise (Error msg)) fmt

(* Check regions *)

    let check_regions defs (i:info) =
      try
        let regions = match i.regions with
        | None ->
            error "no definition of %s in bell file"  BellName.regions
        | Some r -> r in  
        List.iter
          (fun (_,r) ->
            if not (StringSet.mem r regions) then
              error
                "region %s from test is not part of bell file regions {%s}"
                r (StringSet.pp_str "," Misc.identity regions))
          defs
      with Error msg ->
        Warn.user_error "region error, %s" msg
          
(* Check scope tree *)
    open BellInfo

    let scope_of = function
      | Leaf (sc,_) | Children (sc,_) -> sc

    let check_scopes st i = 
      try
        let scopes =
          try StringMap.find BellName.scopes i.relations
          with Not_found ->
            error "no definition of %s in bell file" BellName.scopes in
        let order = 
          try StringMap.find BellName.scopes i.orders
          with Not_found ->
            error "no definition of scope order in bell file" in

        let rec check_rec a st =
          let sc = scope_of st in
          if not (Misc.Simple.mem sc scopes) then
            error
              "scope tree tag %s is not part of bell file %s declaration"
              sc BellName.scopes
          else if not (StringRel.mem (sc,a) order) then
            error
              "scope tree tag %s does not have %s as a parent"
              sc a
          else match st with
          | Leaf _ -> ()
          | Children (_,ts) -> check_recs sc ts

        and check_recs a ts = List.iter (check_rec a) ts in

        let sc0 = scope_of st in
        if not (StringSet.is_empty (StringRel.succs order sc0)) then
          error
            "top scope tree tag %s is not maximal" sc0
        else match st with
        | Leaf _ -> ()
        | Children (_,ts) -> check_recs sc0 ts
      with Error msg ->
        Warn.user_error "scope error, %s" msg

    let rec same_length xs ys = match xs,ys with
    | [],[] -> true
    | _::xs,_::ys -> same_length xs ys
    | ([],_::_) | (_::_,[]) -> false

    let check_instruction bi i =
      try
        let id,al =
          try C.get_id_and_list i
          with Not_found -> raise Exit in (* If no annotation, no trouble *)
        assert (StringSet.mem id BellName.all_mem_sets) ;
        let events_group = get_events id bi in
        let ok =
          List.exists
            (fun ag -> same_length ag al && List.for_all2 StringSet.mem al ag)
            events_group in
        if not ok then
          error "instruction '%s' does not match bell declarations"
            (A.dump_instruction i)
      with
      | Exit -> () 
      | Error msg -> Warn.user_error "annotation error, %s" msg

    let do_check parsed bi = 
      (* checking instructions *)
      List.iter
        (fun (_,code) ->
          List.iter
            (A.pseudo_iter (check_instruction bi))
            code)
        parsed.MiscParser.prog; 
      
      let test_bi = parsed.MiscParser.bell_info in            
      let test_bi = match test_bi with
      | Some b -> b
      | None -> Warn.fatal "Error getting bell information from test" in
      begin match test_bi.BellInfo.regions with 
      | Some r -> check_regions r bi
      | _ -> ()
      end ;
      begin match test_bi.BellInfo.scopes with 
      | Some s -> check_scopes s bi	  
      | _ -> ()
      end ;
      ()

    let check = match C.info with
    | None -> fun _ -> ()
    | Some bi -> fun parsed -> do_check parsed bi
  end
