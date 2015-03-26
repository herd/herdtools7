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

let dbg = true

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

type info = {
  all_events : annot_set; (* This field records all annotations *)
  events : event_decs;
  relations : relation_decs;
  orders : order_decs ;
  regions : StringSet.t option ;
}

let pp_info  i = 
  sprintf "All events: %s\n" (pp_string_set i.all_events) ^
  "Events:\n" ^ pp_event_decs i.events ^ "\n" ^
  "Relations:\n" ^ pp_rel_decs i.relations ^ "\n" ^
  "Orders:\n" ^ pp_order_decs i.orders ^ "\n" ^
  (match i.regions with
  | None -> ""
  | Some r -> sprintf "Regions: %s\n" (pp_string_set r))
 
let empty_info = {
  all_events = StringSet.empty  ;
  events = StringMap.empty ;
  relations = StringMap.empty ;
  orders = StringMap.empty ;
  regions = None ;
}


(* Get *)
(* By default, no annotation allowed *)
let get_events tag {events;_} =  StringMap.safe_find [[]] tag events 

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

    let add sc1 sc2 order =
      let p = StringRel.path sc1 sc2 order in
      if dbg then eprintf "PATH %s %s = [%s]\n"
        sc1 sc2 (String.concat "; " p) ;
      match p with
    | [] ->
        error
          "scope tree tag %s does not have %s as an ancestor"
          sc2 sc1
    | [e] ->
        error "scope tree tag %s appears as it own successor" e
    | _::rem ->
        let rec add_rec = function
          | [] -> assert false
          | [e] ->
              assert (String.compare e sc2 = 0) ;
              fun st -> st
          | e::rem ->
              fun st -> Children (e,[add_rec rem st]) in
        add_rec rem

    let check_tag sc scopes =
      if not (Misc.Simple.mem sc scopes) then
        error
          "scope tree tag %s is not part of bell file %s declaration"
          sc BellName.scopes

    let expand_scope scopes order =
      if dbg then eprintf "ORDER: %s\n" (pp_order_dec order) ;
      let rec expand_rec top st =
        let sc = scope_of st in
        if dbg then eprintf "EXPAND_REC top=%s, sc=%s\n" top sc;
        check_tag sc scopes ;
        let st = match st with
        | Leaf (sc,ps) ->
            expand_leaf sc ps order
        | Children (sc,sts) ->
            Children (sc,List.map (expand_rec sc) sts) in
        add top sc order st

      and expand_leaf sc ps order =
        let leaves = StringRel.leaves sc order in
        begin match StringSet.as_singleton leaves with
        | None ->
            error
              "ambiguity in scope tree: %s does not leads to one leaf"
              sc
        | Some leaf ->            
            if String.compare sc leaf = 0 then (Leaf (leaf,ps))
            else
              let add = add sc leaf order in
              let leaves = List.map (fun p -> Leaf (leaf,[p])) ps in
              Children (sc,List.map add leaves)
        end in

      fun st ->
        let sc = scope_of st in
        if dbg then eprintf "EXPAND: sc='%s'\n" sc ;
        match StringSet.as_singleton (StringRel.roots order) with
        | None ->
            error "ambiguity in scope tree: no unique root"
        | Some root ->
            if dbg then eprintf "EXPAND: root=%s\n" root ;
            if String.compare sc root = 0 then
              match st with
              | Leaf (_,ps) ->
                  expand_leaf sc ps order
              | Children (_,sts) ->
                  Children (sc,List.map (expand_rec sc) sts)
            else match sc with
            | "" ->
                begin match st with
                | Leaf (_,ps) ->
                    expand_leaf root ps order
                | Children (_,sts) ->
                    Children (root,List.map (expand_rec root) sts)
                end
            | _ ->
                check_tag sc scopes ;
                Children (root,[expand_rec root st])

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
        let nst =
          expand_scope scopes (StringRel.inverse order) st in
        eprintf
          "Scope tree:\n%s\n==>\n%s\n%!"
          (BellInfo.pp_scopes st)
          (BellInfo.pp_scopes nst) ;
        nst
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
        if not ok then begin
          if dbg then eprintf "%s: %s\n" id (pp_event_dec events_group) ;
          error "instruction '%s' does not match bell declarations"
            (A.dump_instruction i)
        end
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
      let st =
        match test_bi.BellInfo.scopes with 
        | Some s -> Some (check_scopes s bi)
        | _ -> None in        
      let test_bi = { test_bi with BellInfo.scopes=st;} in
      { parsed with MiscParser.bell_info = Some test_bi; }

    let check = match C.info with
    | None -> Misc.identity
    | Some bi -> fun parsed -> do_check parsed bi
  end
