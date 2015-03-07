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

let _dbg = false

(* To disable checks that I do not understand yet -> false *)
let _docheck = true


(*************)
(* For tests *)
(*************)

type mem_space_map = (string * string) list

let pp_mem_map m = 
  String.concat ","
    (List.map (fun (n,r) -> sprintf "%s: %s" n r) m)

type scopes = 
 | Leaf of string* int list
 | Children of string * scopes list

let pp_int_list il =
  String.concat " " (List.map (sprintf "%i") il)
  

let rec pp_scopes s =
  match s with 
  | Leaf(s,i) -> sprintf "(%s %s)" s (pp_int_list i)
  | Children(s,sc) -> 
    let pp = List.map (fun x -> pp_scopes x) sc in
    sprintf "(%s %s)" s (String.concat " " pp)

type test = {
  regions : mem_space_map option;
  scopes : scopes option;
}

(*
(**************)
(* Model info *)
(**************)

type event_type = string
type annotation = string
type annot_set = annotation list

let pp_annot_set ans = "{"^(String.concat "," ans)^"}"

type annot_group = annot_set list

let pp_annot_group ag = 
  let mapped = List.map (fun x -> pp_annot_set x) ag in
  "["^(String.concat ", " mapped)^"]"

let pp_annot_groups ags =
  let pp = List.map pp_annot_group ags in
  String.concat ";" pp

let flatten_annot_group ag =  List.flatten ag

type event_dec = event_type * (annot_group list)


let pp_event_dec ed = 
  let t,l = ed in
  let mapped = List.map (fun x -> pp_annot_group x) l in
  t^": "^ String.concat "," mapped

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

type order = StringRel.t

let pp_order_dec od = 
  let t,ol = od in
  sprintf "%s %s" t
    (StringRel.pp_str " "
       (fun (f,s) -> sprintf "(%s,%s)" f s)
       ol)

type all_order_decs = StringRel.t StringMap.t

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

let known_sets = StringSet.of_list ["R"; "W"; "F"; "regions"; "RMW";]
let known_relations = StringSet.of_list ["scopes";]
let known_orders = StringSet.of_list ["scopes";]

let check_decs check known msg = 
  StringMap.iter
    (fun k _ ->
    if not (Simple.mem k known) then 
      Warn.user_error msg k) check 

let list_to_set s = StringSet.of_list s


(* builds bell info and does some sanity checks *)
(* could do some more error checking here, but I'll save it
   for later. For example, we probably want to make sets and 
   relations are disjoint *)

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

let rec same_length xs ys = match xs,ys with
| [],[] -> true
| _::xs,_::ys -> same_length xs ys
| ([],_::_) | (_::_,[]) -> false

let check_annots t al bi =
  if dbg then eprintf "Check: %s{%s}\n" t (String.concat "," al) ;
  try 
    let event_groups = Simple.assoc t bi.events in
    if dbg then
      Printf.eprintf "EG: {%s}, LAL=%i\n"
        (pp_annot_groups event_groups) (List.length al) ;
    List.exists
      (fun ag ->
        same_length ag al &&
        List.for_all2 Simple.mem al ag)
      event_groups
  with Not_found -> not (Misc.consp al)

let check_regions r bi =                
  let known_regions = Simple.mem_assoc "regions" bi.events in
  if not known_regions then
    false
  else
    let regions = Simple.assoc "regions" bi.events in
    let flattened = flatten_event_dec ("",regions) in    
    let checked = List.map (fun (_,reg) -> 
      Simple.mem reg flattened) r in
    List.fold_left (fun acc x -> acc && x) true checked 

let check_scopes s bi = 
  if not docheck then true
  else
    
    let above_pred n scopes above scope_order = 
      let f = Simple.mem n scopes in
      let s = match above with 
      | Some a -> StringRel.mem (a,n) scope_order 
      | None -> true
      in
      if dbg then
        eprintf "Check scope n=%s, a=%s\n" n
          (match above with Some a -> a| None -> "-") ;
      f && s
    in
    let rec check_scopes_rec s scopes above scope_order = 
      match s with
      | Leaf (n,_) -> above_pred n scopes above scope_order
      | Children (n,sl) -> 
          above_pred n scopes above scope_order &&
          List.for_all
            (fun s -> 
              check_scopes_rec s scopes (Some n) scope_order)
            sl
    in 
    try
      let scope_order =
        try Simple.assoc "scopes" bi.orders
        with Not_found ->
          Warn.warn_always "No definition of scope_order in bell file" ;
          raise Exit in
      let scopes =
        try Simple.assoc "scopes" bi.relations
        with Not_found ->
          Warn.warn_always "No definition of scopes in bell file" ;
          raise Exit in
      check_scopes_rec s scopes None scope_order
    with Exit -> true (* bypass check *)

let get_assoc_event a l = 
  let contains = Simple.mem_assoc a l in
  if contains then
    ("",Simple.assoc a l)
  else 
    ("",[[[]]])

let get_assoc_rel a l = 
  let contains = Simple.mem_assoc a l in
  if contains then
    ("",Simple.assoc a l)
  else 
    ("",[])  

let evts2set tag bi =
  StringSet.of_list (flatten_event_dec (get_assoc_event tag bi.events))
      
let get_mem_annots bi = 
  let reads = evts2set "R" bi
  and writes = evts2set "W" bi
  and fences = evts2set "F" bi
  and rmw = evts2set "RMW" bi in
  let all = StringSet.unions [reads;writes;fences;rmw;] in
  StringSet.elements all
    
let get_region_sets bi =  StringSet.elements (evts2set "regions" bi)
  
let get_scope_rels  bi = 
  StringSet.elements
    (list_to_set
       (flatten_rel_dec (get_assoc_rel "scopes" bi.relations))) 

*)
