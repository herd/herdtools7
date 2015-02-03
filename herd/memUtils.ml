(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

module Make(S : SemExtra.S) = struct

  module S = S
  module E = S.E
  module A = S.A
  module V = A.V
  module C = S.C
  module PC = S.O.PC

(*************)	    
(* Utilities *)
(*************)	    

  let iico es =
    E.EventRel.union
      es.E.intra_causality_data
      es.E.intra_causality_control

  let po_strict es =
    E.EventRel.of_pred
      es.E.events es.E.events E.po_strict

  let po_iico_data es =
    E.EventRel.union
      es.E.intra_causality_data
      (po_strict es)

  let po_iico es =  E.EventRel.union (iico es) (po_strict es)

(* slight extension of prog order *)
  let is_before_strict es e1 e2 =
    E.EventRel.mem (e1,e2) es.E.intra_causality_data  ||
    E.EventRel.mem (e1,e2) es.E.intra_causality_control ||
    E.po_strict e1 e2

(* Fence *)
  let po_fence_po po pred =
    let r1 =
      E.EventRel.restrict_domains E.is_mem pred po
    and r2 =
      E.EventRel.restrict_domains pred E.is_mem po in
    E.EventRel.sequence r1 r2

  let get_loc e =  match E.location_of e with
  | Some loc -> loc
  | None -> assert false

(* Lift dependance relation to memory *)
  let restrict p = E.EventRel.filter (fun (e1,e2) -> p e1 && p e2)

  let trans_close_mem r = restrict E.is_mem (S.tr r)
  let trans_close_mems r_p = List.map trans_close_mem r_p

(*Scope operations*)

  let get_proc_loc_tuple scope_tree num = 
    let result = ref (-1,-1,-1,-1,-1) in
    List.iteri (fun dev -> 
      List.iteri (fun ker -> 
        List.iteri (fun cta -> 
          List.iteri (fun wrp -> 
            List.iter (fun thd ->
              if thd = num then
                result := (dev,ker,cta,wrp,thd)
            )
          )
        )
      )
    ) scope_tree; 
    !result

  (*TODO fix scopes and init writes!*)
  let inside_scope e1 e2 s scope_tree =
    let e1_int = match (E.proc_of e1) with
      | Some x -> x
      | _ -> -1
    in
    let e2_int = match (E.proc_of e2) with
      | Some x -> x
      | _ -> -1
    in
    if (e1_int = -1 && e2_int = -1) (* both are init writes *) then 
      true
    else if (e1_int = -1 || e2_int = -1) (* one is an init write *) then 
      false 
    else
      begin
	let d1,k1,wg1,sg1,t1 = get_proc_loc_tuple scope_tree e1_int
	in 
	let d2,k2,wg2,sg2,t2 = get_proc_loc_tuple scope_tree e2_int
	in
	match s with
	| AST.Device     -> d1 = d2
	| AST.Kernel     -> d1 = d2 && k1 = k2
	| AST.Work_Group -> d1 = d2 && k1 = k2 && wg1 = wg2
	| AST.Sub_Group  -> d1 = d2 && k1 = k2 && wg1 = wg2 && sg1 = sg2
	| AST.Work_Item  -> d1 = d2 && k1 = k2 && wg1 = wg2 && sg1 = sg2 && t1 = t2
      end
	    
  let ext_scope scope r scope_tree = 
    E.EventRel.filter 
      (fun (e1,e2) -> not (inside_scope e1 e2 scope scope_tree)) r 

  let int_scope scope r scope_tree = 
    E.EventRel.filter 
      (fun (e1,e2) -> inside_scope e1 e2 scope scope_tree) r


(******************)
(* View of a proc *)
(******************)


  let proc_view_event p e =
    (match E.proc_of e with Some q -> q = p | None -> false) ||
    E.is_mem_store e

  let proc_view_event2 p (e1,e2) =
    proc_view_event p e1 && proc_view_event p e2

  let proc_view p vb = E.EventRel.filter (proc_view_event2 p) vb

(* Perform difference, columnwise, ie difference of projected relations *)
  let diff_p = List.map2 E.EventRel.diff

(* Perform union, columnwise, ie union of projected relations *)
  let union_p = List.map2 E.EventRel.union

  let unions_p rows =
    let cols =
      try Misc.transpose rows
      with Misc.TransposeFailure -> assert false in
    List.map E.EventRel.unions cols

  let transitive_closure_p = List.map E.EventRel.transitive_closure

(* Convert the ordered list representation of a total order to a relation *)
  let rec order_to_pairs k evts = match evts with
  | [] -> k
  | e1 :: tl ->      
      let k = List.fold_left (fun k e2 -> (e1,e2)::k) k tl in
      order_to_pairs k tl
	
  let order_to_rel evts = E.EventRel.of_list (order_to_pairs []  evts)

  let cycle_to_rel cy =
    let rec do_rec seen = function
      | [] -> assert false
      | [e] ->
          if E.EventSet.is_empty seen then E.EventRel.singleton (e,e)
          else  begin
            assert (E.EventSet.mem e seen) ;
            E.EventRel.empty
          end
      | e1::(e2::_ as rem) ->
          if E.EventSet.mem e1 seen then E.EventRel.empty
          else
            E.EventRel.add (e1,e2) (do_rec (E.EventSet.add e1 seen) rem) in
    do_rec E.EventSet.empty cy

(* The same, for successor relation,
   which is enough for feeding topological orders generators *)

  let rec order_to_succ_rel evts = match evts with
  | []|[_] -> E.EventRel.empty
  | e1::(e2::_ as evts) ->
      E.EventRel.add (e1,e2) (order_to_succ_rel evts)


(********)
(* Misc *)
(********)

  let find_source rfmap r =
    try S.RFMap.find  (S.Load r) rfmap
    with Not_found -> assert false

(*******************)
(* RF/FR relations *)
(*******************)

  let make_rf_from_rfmap rfmap =
    S.RFMap.fold
      (fun wt rf k -> match wt,rf with
      | S.Load er,S.Store ew when E.is_mem er ->
          E.EventRel.add (ew,er) k
      | _ -> k)
      rfmap 
      E.EventRel.empty

  let make_rf conc = make_rf_from_rfmap conc.S.rfmap 


  let find_rf er rfm =
    try S.RFMap.find (S.Load er) rfm
    with Not_found -> assert false

  let make_fr conc ws =
    let loads = E.mem_loads_of conc.S.str.E.events
    and stores = E.mem_stores_of conc.S.str.E.events in
    E.EventSet.fold
      (fun er k ->
        let erf = find_rf er conc.S.rfmap in
        E.EventSet.fold
          (fun ew k ->
            if 
              not (E.event_equal er ew) (* RMW *) &&
              E.same_location ew er
            then match erf with
            | S.Init ->
                E.EventRel.add (er,ew) k
            | S.Store erf ->
                if E.EventRel.mem (erf,ew) ws then
                  E.EventRel.add (er,ew) k
                else k
            else k)
          stores k)
      loads E.EventRel.empty

  let make_rf_regs conc =
    S.RFMap.fold
      (fun wt rf k -> match wt,rf with
      | S.Load er,S.Store ew when E.is_reg_any er ->
          E.EventRel.add (ew,er) k
      | _ -> k)
      conc.S.rfmap 
      E.EventRel.empty

  let rext conc e =
    E.is_mem_load e &&
    (match find_rf e conc.S.rfmap with
    | S.Init -> true
    | S.Store ew -> E.proc_of ew <> E.proc_of e)


  let same_source conc e1 e2 = 
    match find_rf e1 conc.S.rfmap,find_rf e2 conc.S.rfmap with
    | S.Init,S.Init -> true
    | S.Store w1,S.Store w2 -> E.event_compare w1 w2 = 0
    | S.Init,S.Store _
    | S.Store _,S.Init -> false

  let ext r = E.EventRel.filter (fun (e1,e2) -> not (E.same_proc e1 e2)) r
  let internal r = E.EventRel.filter (fun (e1,e2) -> E.same_proc e1 e2) r
      

(* po-separation *)
  let sep is_sep is_applicable evts =
    let is_applicable e1 e2 = is_applicable (e1,e2) in
    let rels =
      E.EventSet.fold
        (fun e k ->
          if is_sep e then
            let before =
              E.EventSet.filter
                (fun ea -> E.po_strict ea e)
                evts
            and after =
              E.EventSet.filter
                (fun eb ->  E.po_strict e eb)
                evts in
	    E.EventRel.of_pred before after is_applicable::k
          else k)
        evts [] in
    E.EventRel.unions rels

(* Extract external sub-relation *)

  let extract_external r =
    E.EventRel.filter (fun (e1,e2) -> E.proc_of e1 <> E.proc_of e2) r

(**************************************)
(* Place loads in write_serialization *)
(**************************************)
(* Use rfmap to order loads and stores as much as possible *)

(* ws is write serialization *)
  let find_rf er rfm =
    try S.RFMap.find (S.Load er) rfm
    with Not_found -> assert false

  let first_ws ws ew = E.EventSet.is_empty (E.EventRel.preds ew ws)

  let make_load_stores conc ws =
    let loads = E.mem_loads_of conc.S.str.E.events
    and stores = E.mem_stores_of conc.S.str.E.events in
    E.EventSet.fold
      (fun er k ->
        let erf = find_rf er conc.S.rfmap in
        E.EventSet.fold
          (fun ew k ->
            if E.same_location ew er then match erf with
            | S.Init ->
                if first_ws  ew ws then
                  E.EventRel.add (er,ew) k
                else k                
            | S.Store erf ->
                if E.EventRel.mem (erf,ew) ws then
                  E.EventRel.add (er,ew) k
(*              else if E.EventRel.mem (ew,erf) ws then
                E.EventRel.add (ew,er) k
 *)
                else k
            else k)
          stores k)
      loads E.EventRel.empty


(******************************)
(* Sets and Maps on locations *)
(******************************)

  module LocSet =
    MySet.Make
      (struct
	type t = A.global_loc
	let compare = A.V.compare
      end)


  module LocEnv =
    Map.Make
      (struct
	type t = A.location
	let compare = A.location_compare
      end)

(* Collect various events by their location *)

  let map_loc_find loc m =
    try LocEnv.find loc m
    with Not_found -> []

  let collect_by_loc es pred =
    E.EventSet.fold
      (fun e k ->
        if pred e then
          let loc = get_loc e in
          let evts = map_loc_find loc k in
          LocEnv.add loc (e::evts) k
        else k)
      es.E.events LocEnv.empty

  let collect_reg_loads es = collect_by_loc es E.is_reg_load_any
  and collect_reg_stores es = collect_by_loc es E.is_reg_store_any
  and collect_mem_loads es = collect_by_loc es E.is_mem_load
  and collect_mem_stores es = collect_by_loc es E.is_mem_store
  and collect_loads es = collect_by_loc es E.is_load
  and collect_stores es = collect_by_loc es E.is_store
  and collect_atomics es = collect_by_loc es E.is_atomic
  and collect_mutex_actions es = collect_by_loc es E.is_mutex_action
(*
  and collect_sc_actions es = 
    (* horrible hack ahead -- we're collecting sc actions per location, 
       but associating them all with a sentinel location that's
       then ignored. should really construct a single list, rather
       than one list per location. *)
    let random_loc = S.A.Location_global (V.intToV 0) in
    LocEnv.add random_loc (List.filter E.is_sc_action (E.EventSet.fold (fun e k -> e :: k) es.E.events [])) LocEnv.empty
*)

(* fr to init stores only *)
  let make_fr_partial conc =
    let ws_by_loc = collect_mem_stores conc.S.str in
    let rs_by_loc = collect_mem_loads conc.S.str in
    let rfm = conc.S.rfmap in
    let k =
      LocEnv.fold
        (fun loc rs k ->
          List.fold_left
            (fun k r ->
              match find_rf r rfm with
              | S.Init ->
                  let ws =
                    try LocEnv.find loc ws_by_loc
                    with Not_found -> [] in
                  List.fold_left (fun k w -> (r,w)::k) k ws
              | S.Store _ -> k)
            k rs)
        rs_by_loc [] in
    E.EventRel.of_list k

(*************)
(* Atomicity *)
(*************)


(* Event atomicity class are canonized as
   a mapping from one representant to the class *)
  module Canon = Map.Make(E.OrderedEvent)

  let find_class e map =
    try Canon.find e map with Not_found -> assert false

  let canonical_locked_events_of es =
    E.Atomicity.fold
      (fun evts k ->
	let canon =
	  (* An atomicity class is not empty *)
	  try E.EventSet.choose evts with Not_found -> assert false in
	Canon.add canon evts k)
      es.E.atomicity Canon.empty
      

  let get_canonical_locked_events_of es =
    let lockeds = canonical_locked_events_of es in
    Canon.fold
      (fun e _ k -> E.EventSet.add e k)
      lockeds E.EventSet.empty 

  let get_atomicity_candidates vb es =
(* Canonize *)
    let lockeds = canonical_locked_events_of es in
    let reprs =
      Canon.fold
        (fun e _ k -> E.EventSet.add e k)
        lockeds E.EventSet.empty in
    let filtered_vb =
      E.EventRel.filter
        (fun (e1,e2) -> E.EventSet.mem e1 reprs && E.EventSet.mem e2 reprs)
        vb in
(* Generate orderings, over representants *)
    let perms = E.EventRel.all_topos (PC.verbose > 0) reprs filtered_vb in
    let cands =
(* For every candidate ordering *)
      List.rev_map (* why not be tail-recursive ? *)
        (fun perm ->
	  (* Change into relation *)
	  let canon_cand = order_to_rel perm in
	  (* And lift relation to atomicity classes,
             maybe not the most efficient way... *)
	  let cand =
	    E.EventRel.fold
	      (fun (c1,c2) k ->
	        let atom1 = find_class c1 lockeds
	        and atom2 = find_class c2 lockeds in
	        E.EventRel.union (E.EventRel.cartesian atom1 atom2) k)
	      canon_cand E.EventRel.empty in
	  canon_cand,cand)
        perms in
    reprs,cands

(********************************************)
(* Write serialization candidate generator. *)
(********************************************)


  let restrict_to_mem_stores rel =
    E.EventRel.filter
      (fun (e1,e2) -> E.is_mem_store e1 && E.is_mem_store e2)
      rel

  let fold_write_serialization_candidates conc vb kont res =
    let vb =
      E.EventRel.union vb
        (restrict_to_mem_stores conc.S.last_store_vbf) in
(* Because final state is fixed *)
    let stores_by_loc = collect_mem_stores conc.S.str in
    let orders =
      LocEnv.fold
	(fun _loc stores k ->
          let orders =
	    E.EventRel.all_topos (PC.verbose > 0)
              (E.EventSet.of_list stores) vb in
          List.map order_to_succ_rel orders::k)
        stores_by_loc [] in
    Misc.fold_cross_gen E.EventRel.union E.EventRel.empty
      orders kont res

(* With check *)
  let apply_process_co test conc process_co res =
    try
      fold_write_serialization_candidates
        conc conc.S.pco process_co res
    with E.EventRel.Cyclic ->
      if S.O.debug.Debug.barrier && S.O.PC.verbose > 2 then begin
        let module PP = Pretty.Make(S) in
        let legend =
          sprintf "%s cyclic co precursor"
            test.Test.name.Name.name in
        let pos = conc.S.pos in
        prerr_endline legend ;
        PP.show_legend test  legend conc
          [ ("pos",S.rt pos); ("pco",S.rt conc.S.pco)]
      end ;
      res

(********************************************)
(* Mutex serialization candidate generator. *)
(********************************************)

  let fold_mutex_serialization_candidates conc (* vb *) _ kont res =
    let mutex_actions_by_loc = collect_mutex_actions conc.S.str in
    let lo_orders : E.EventRel.t list list =
      LocEnv.fold
	(fun _loc mutex_actions k ->
          let orders =
	    E.EventRel.all_topos (PC.verbose > 0)
              (E.EventSet.of_list mutex_actions) E.EventRel.empty in
          List.map order_to_succ_rel orders::k)
        mutex_actions_by_loc [] in
    Misc.fold_cross_gen E.EventRel.union E.EventRel.empty lo_orders kont res

(* With check *)
  let apply_process_lo test conc process_lo res =
     try
       fold_mutex_serialization_candidates
         conc conc.S.pco process_lo res
     with E.EventRel.Cyclic ->
       if S.O.debug.Debug.barrier && S.O.PC.verbose > 2 then begin
         let module PP = Pretty.Make(S) in
           let legend =
             sprintf "%s cyclic co or lo precursor"
               test.Test.name.Name.name in
           let pos = conc.S.pos in
           prerr_endline legend ;
           PP.show_legend test  legend conc
             [ ("pos",S.rt pos); ("pco",S.rt conc.S.pco)]
        end ;
        res

(*****************************************)
(* SC serialization candidate generator. *)
(*****************************************)

(*
  let fold_sc_serialization_candidates conc  kont res =
    let sc_actions = collect_sc_actions conc.S.str in
    let sc_orders : E.EventRel.t list list =
      LocEnv.fold
	(fun _loc sc_actions k ->
          let orders =
	    E.EventRel.all_topos (PC.verbose > 0)
              (E.EventSet.of_list sc_actions) E.EventRel.empty in
          List.map order_to_succ_rel orders::k)
        sc_actions [] in
    Misc.fold_cross_gen E.EventRel.union E.EventRel.empty sc_orders kont res
*)


(* Call kont on all orders of the events of xs *)
  let all_elements xs kont =
    let rec do_rec k xs =
      if E.EventSet.is_empty xs then kont k
      else
        E.EventSet.fold (fun x -> do_rec (x::k) (E.EventSet.remove x xs)) xs in
    do_rec [] xs

  let apply_process_sc _test conc kont res =
    let sc_actions =
      E.EventSet.filter E.is_sc_action conc.S.str.E.events in
    all_elements sc_actions (fun xs -> kont (order_to_succ_rel xs)) res

(* Generic toplogical order generator *)
 let apply_orders es vb kfail kont res =
   try
     E.EventRel.all_topos_kont es vb
       (fun k res -> kont (order_to_rel k) res) res
   with E.EventRel.Cyclic -> kfail vb

(*******************************************************)
(* Saturate all memory accesses wrt atomicity classes  *)
(*******************************************************)
  exception FailFast

  let fold_saturated_mem_order es mem_order kont res =

    (* Inputs : es : E.event_structure
       mem_order : possibly unsaturated memory order
       Action: fold over all possibilities of saturating edges 
     *)
    let accesses = E.mem_of es.E.events in
    let is_before e atom = 
      E.EventSet.exists 
	(fun elock -> E.EventRel.mem (e, elock) mem_order)
	atom in
    let is_after e atom = 
      E.EventSet.exists 
	(fun elock -> E.EventRel.mem (elock, e) mem_order)
	atom in
    let add_before e atom =
      E.EventSet.fold
	(fun elock k -> if E.is_mem elock then E.EventRel.add (e, elock) k else k)
	atom E.EventRel.empty in
    let add_after e atom =
      E.EventSet.fold
	(fun elock k -> if E.is_mem elock then E.EventRel.add (elock, e) k else k)
	atom E.EventRel.empty in
    try 
      let (new_order,unresolved) = 
	E.EventSet.fold
	  (fun e (new_ord,unresolved) ->
	    E.Atomicity.fold
	      (fun atom (new_ord,unresolved) ->
		if E.EventSet.mem e atom 
		then (new_ord,unresolved)
		else 
		  let should_add_before = is_before e atom in
		  let should_add_after = is_after e atom in
		  match (should_add_before, should_add_after) with
		  | true,true -> raise FailFast
		  | true, false -> (E.EventRel.union new_ord (add_before e atom),
				    unresolved)
		  | false, true -> (E.EventRel.union new_ord (add_after e atom),
				    unresolved)
		  | false, false -> (new_ord,
				     (e,atom)::unresolved)
	      )
	      es.E.atomicity (new_ord,unresolved)
	  )
	  accesses (E.EventRel.empty, [])
      in
      let resolved : E.EventRel.t list list = 
	List.map 
	  (fun (e,atom) -> 
	    [
	     add_before e atom;
	     add_after e atom]
	  )
	  unresolved in
      Misc.fold_cross_gen E.EventRel.union new_order resolved kont res
    with
      FailFast -> res
          

(*****************************************)
(* Compute write serialization precursor *)
(*****************************************)

          (* We asssume unicity of init write event to x,
             as a defensive measure, works when no init write exists *)
  let rec find_init = function
    | []  -> raise Not_found
    | e::es ->
        if E.is_mem_store_init e then e
        else find_init es

(* Init store to loc is co-before stores to x *)
  let compute_pco_init es =
    let stores = collect_mem_stores es in
    let xs =
      LocEnv.fold
        (fun _loc ews k ->
          try
            let ei = find_init ews in
            List.fold_left
              (fun k ew ->
                if E.event_equal ei ew then k
                else (ei,ew)::k)
              k ews
          with Not_found -> k)
        stores [] in
    E.EventRel.of_list xs

  let compute_pco rfmap ppoloc =
    try
      let pco = 
        E.EventRel.fold
          (fun (e1,e2 as p) k -> match E.get_mem_dir e1, E.get_mem_dir e2 with
          | Dir.W,Dir.W -> E.EventRel.add p k
          | Dir.R,Dir.R ->
              begin match
                find_source rfmap e1,
                find_source rfmap e2
              with
              | S.Store w1,S.Store w2 ->
                  if E.event_equal w1 w2 then k
                  else E.EventRel.add (w1,w2) k
              | S.Init,_ -> k
              | _,S.Init -> raise Exit
              end
          | Dir.R,Dir.W ->
              begin match
                find_source rfmap e1
              with
              | S.Store w1 -> E.EventRel.add (w1,e2) k
              | S.Init -> k
              end
          | Dir.W,Dir.R ->
              begin match
                find_source rfmap e2
              with
              | S.Store w2 ->
                  if E.event_equal e1 w2 then k
                  else E.EventRel.add (e1,w2) k
              | S.Init -> raise Exit
              end)
          ppoloc
          E.EventRel.empty in
      Some pco
    with Exit -> None


(*************************************)
(* Final condition invalidation mode *)
(*************************************)

(*
  A little optimisation: we check whether the existence/non-existence
  of some vo would help in validation/invalidating the constraint
  of the test.

  If no, not need to go on
 *)

  module T = Test.Make(S.A)
      
  let final_is_relevant test fsc =
    let open ConstrGen in
    let cnstr = T.find_our_constraint test in
    match cnstr with
      (* Looking for 'Allow' witness *)
    | ExistsState p ->  C.check_prop p fsc
          (* Looking for witness that invalidates 'Require' *)
    | ForallStates p -> not (C.check_prop p fsc)
          (* Looking for witness that invalidates 'Forbid' *)
    | NotExistsState p -> C.check_prop p fsc


end
