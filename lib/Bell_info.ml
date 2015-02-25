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

open Printf

(* For the bell model and test info *)

(* first start off with the event annotations, keep them
   as strings to be generic  *)
type event_type = string

type annotation = string

type annot_set = annotation list
let pp_annot_set ans = "{"^(String.concat "," ans)^"}"

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

type annot_group = annot_set list
let pp_annot_group ag = 
  let mapped = List.map (fun x -> pp_annot_set x) ag in
  "["^(String.concat ", " mapped)^"]"

let flatten_annot_group ag = 
  List.flatten ag

type event_dec = event_type * (annot_group list)

let pp_event_dec ed = 
  let t,l = ed in
  let mapped = List.map (fun x -> pp_annot_group x) l in
  t^": "^(String.concat ("\n"^t^": ") mapped)

let flatten_event_dec ed =
  let _,l = ed in
  let tmp = List.map (fun x -> flatten_annot_group x) l in
  List.flatten tmp

type all_event_decs = event_dec list
let pp_all_event_decs aed =
  let mapped = List.map (fun x -> pp_event_dec x) aed in
  String.concat "\n" mapped

let flatten_all_event_decs aed = 
  let mapped = List.map (fun x -> flatten_event_dec x) aed in
  List.flatten mapped

(* keep relation declarations simple for now until we
   understand them better and what we want to do with them *)
type relation_type = string

type relation_annot = string

type relation_annot_set = relation_annot list
let pp_rel_annot_set ans = "{"^(String.concat "," ans)^"}"

type relation_dec = string * relation_annot_set
let pp_rel_dec rd = 
  let t,l = rd in
  sprintf "%s %s" t (pp_rel_annot_set l)

let flatten_rel_dec rd = 
  let _,l = rd in
  l

type all_relation_decs = relation_dec list
let pp_all_rel_decs ard = 
  let tmp = List.map (fun x -> pp_rel_dec x) ard in
  String.concat "\n" tmp  

(* orders, again, keeping it simple. Our only use case is
   scopes for now. *)
type order = (string * string)
let pp_order o = 
  let f,s = o in
  sprintf "(%s, %s)" f s

type order_dec = (string * order list)
let pp_order_dec od = 
  let t,ol = od in
  let tmp = List.map (fun x -> pp_order x) ol in
  sprintf "%s %s" t (String.concat " " tmp)

type all_order_decs = order_dec list
let pp_all_order_decs aod = 
  let tmp = List.map (fun x -> pp_order_dec x) aod in
  String.concat "\n" tmp  

type model = {
  all_events: annot_set;
  events: all_event_decs;
  relations: all_relation_decs;
  orders: all_order_decs;
}

(* For the bell test info *)

type mem_space_map = (string * string) list

let pp_mem_map mm = 
  let tmp = List.map (fun (n,r) -> sprintf "%s: %s" n r) mm in
  String.concat ", " tmp

type scopes = 
 | Leaf of string* int list
 | Children of string * scopes list

let pp_int_list il = 
  let tmp = List.map (fun x -> sprintf "%d" x) il in
  String.concat " " tmp
  

let rec pp_scopes s =
  match s with 
  | Leaf(s,i) -> sprintf "(%s %s)" s (pp_int_list i)
  | Children(s,sc) -> 
    let tmp = List.map (fun x -> pp_scopes x) sc in
    let concat = String.concat " " tmp in
    sprintf "(%s %s)" s concat

type test = {
  regions : mem_space_map option;
  scopes : scopes option;
}

let known_sets = ["R"; "W"; "F"; "regions"; "RMW";]
let known_relations = ["scopes";]
let known_orders = ["scope_order";]

let check_decs check known msg = 
  List.iter (fun (k,_) ->
    if not (List.mem k known) then 
      Warn.user_error msg k) check 

let list_to_set s = StringSet.of_list s


(* builds bell info and does some sanity checks *)
(* could do some more error checking here, but I'll save it
   for later. For example, we probably want to make sets and 
   relations are disjoint *)

let dbg = false

let build_bell_info sets rels orders = 
  if dbg then begin
    eprintf "EVENTS:\n%s\n" (pp_all_event_decs sets) ;
    eprintf "REL: %s\n" (pp_all_rel_decs rels) ;
    eprintf "ORDER: %s\n" (pp_all_order_decs orders) ;
  end ;
  check_decs sets known_sets "I do not know what to do with set %s";
  check_decs rels known_relations "I do not know what to do with relation %s";
  check_decs orders known_orders "I do not know what to do with order %s";
  let all_events = StringSet.elements 
    (list_to_set 
       (flatten_all_event_decs sets)) in  
  {
    all_events = all_events;
    events = sets;
    relations = rels;    
    orders = orders;    
  }

(* Zip two lists (possibly unequal lengths) into a tuple *)
let rec zip lst1 lst2 = match lst1,lst2 with
  | [],_ -> []
  | _, []-> []
  | (x::xs),(y::ys) -> (x,y) :: (zip xs ys);;

let check_annots t al bi = 
  let check_annot_group ag al =
    let zipped = zip ag al in
    let matched = List.map (fun (l,a) ->
      List.mem a l) zipped in
    List.fold_left (fun acc x -> acc && x) true matched
  in
      
  let known_event = List.mem_assoc t bi.events in
  if (not known_event) && ((List.length al) = 0) then
    true
  else if (not known_event) && ((List.length al) != 0) then
    false
  else
    let event_groups = List.assoc t bi.events in
    let candidates = List.filter (fun v -> 
      if (List.length v) = (List.length al) then true
      else false) event_groups in
    let match_groups = List.map (fun ag -> check_annot_group ag al)
      candidates in
    List.fold_left (fun acc x -> acc || x) false match_groups

let check_regions r bi =                
  let known_regions = List.mem_assoc "regions" bi.events in
  if not known_regions then
    false
  else
    let regions = List.assoc "regions" bi.events in
    let flattened = flatten_event_dec ("",regions) in    
    let checked = List.map (fun (_,reg) -> 
      List.mem reg flattened) r in
    List.fold_left (fun acc x -> acc && x) true checked 

let check_scopes s bi = 

  let above_pred n scopes above scope_order = 
    let f = List.mem n scopes in
    let s = match above with 
      | Some a -> List.mem (n,a) scope_order 
      | None -> true
    in
    f && s
  in
  let rec check_scopes_rec s scopes above scope_order = 
    match s with
    | Leaf (n,_) -> above_pred n scopes above scope_order
    | Children (n,sl) -> 
      let q = above_pred n scopes above scope_order in
      let checked = List.map (fun x -> 
	check_scopes_rec x scopes (Some n) scope_order) sl in
      List.fold_left (fun acc n -> acc && n) q checked
  in 
  let known_scopes = List.mem_assoc "scopes" bi.relations in
  let known_scope_order = List.mem_assoc "scope_order" bi.orders in
  if (not known_scopes) || (not known_scope_order) then
    begin
      Printf.printf "HERE2\n";
      false
    end 
  else 
    begin
      let scopes = List.assoc "scopes" bi.relations in
      let scope_order = List.assoc "scope_order" bi.orders in
      check_scopes_rec s scopes None scope_order
    end

let get_assoc_event a l = 
  let contains = List.mem_assoc a l in
  if contains then
    ("",List.assoc a l)
  else 
    ("",[[[]]])

let get_assoc_rel a l = 
  let contains = List.mem_assoc a l in
  if contains then
    ("",List.assoc a l)
  else 
    ("",[])  

      
let get_mem_annots bi = 
  let reads = flatten_event_dec (get_assoc_event "R" bi.events) in
  let writes = flatten_event_dec (get_assoc_event "W" bi.events) in
  let fences = flatten_event_dec (get_assoc_event "F" bi.events) in
  let unique = StringSet.elements (list_to_set (reads@writes@fences))
  in unique
    
let get_region_sets bi = 
  StringSet.elements 
    (list_to_set 
       (flatten_event_dec (get_assoc_event "regions" bi.events)))
  
let get_scope_rels  bi = 
  StringSet.elements
    (list_to_set
       (flatten_rel_dec (get_assoc_rel "scopes" bi.relations))) 

