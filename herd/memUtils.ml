(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

module Make(S : SemExtra.S) = struct

  module S = S
  module E = S.E
  module A = S.A
  module C = S.Cons
  module PC = S.O.PC

(*************)
(* Utilities *)
(*************)

  let iico es =
    E.EventRel.transitive_closure
    (E.EventRel.union es.E.intra_causality_data  es.E.intra_causality_control)

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

(***************************)
(* Got all scope relations *)
(***************************)

(* Classify acording to proc *)
module IntMap =
  MyMap.Make
    (struct
      type t = int
      let compare = Misc.int_compare
    end)

let by_proc evts =
  let m = IntMap.empty in
  let m =
    E.EventSet.fold
      (fun e m -> match E.proc_of e with
      | Some p ->
          let old = IntMap.safe_find [] p m in
          IntMap.add p (e::old) m
      | None -> m)
      evts m in
  IntMap.map E.EventSet.of_list m

(*******************)

let get_scope_classes evts =
  let open BellInfo in
  let m = by_proc evts in
  let rec do_rec = function
    | Leaf (s,ps) ->
        let es =
          E.EventSet.unions
            (List.map
               (fun p -> IntMap.safe_find E.EventSet.empty p m)
               ps) in
        es,StringMap.add s [es] StringMap.empty
    | Children (s,ts) ->
        let ess,clss =
          List.fold_left
            (fun (es,clss) t ->
              let es_t,cls_t = do_rec t in
              es_t::es,cls_t::clss)
            ([],[]) ts in
        let es = E.EventSet.unions ess in
        let cls = StringMap.unions (@) clss in
        es,StringMap.add s [es] cls in
  fun sc ->
    let _,cls = do_rec sc in
    cls

let get_scope_rels evts sc =
  let cls = get_scope_classes evts sc in
  StringMap.fold
    (fun s cls k ->
      let r =
        E.EventRel.unions
          (List.map
             (fun evts -> E.EventRel.cartesian evts evts)
             cls) in
      (s,r)::k)
    cls []


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

  let make_write_mem_finals conc =
    let ws = S.RFMap.fold
      (fun wt rf k -> match wt,rf with
      | S.Final _,S.Store e when E.is_mem_store e -> e::k
      | _,_ -> k)
      conc.S.rfmap [] in
    E.EventSet.of_list ws

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
  and collect_mem es = collect_by_loc es E.is_mem
  and collect_loads es = collect_by_loc es E.is_load
  and collect_stores es = collect_by_loc es E.is_store
  and collect_atomics es = collect_by_loc es E.is_atomic

  let partition_events es =
    let env =
      E.EventSet.fold
        (fun e k -> match E.location_of e with
        | Some loc ->
            let evts = map_loc_find loc k in
            LocEnv.add loc (e::evts) k
        | None -> k) es LocEnv.empty in

    LocEnv.fold (fun _ evts k -> E.EventSet.of_list evts::k) env []


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
          List.map E.EventRel.order_to_succ orders::k)
        stores_by_loc [] in
    Misc.fold_cross_gen E.EventRel.union E.EventRel.empty
      orders kont res

(* With check *)
  let apply_process_co test conc process_co res =
    try
      fold_write_serialization_candidates
        conc conc.S.pco process_co res
    with E.EventRel.Cyclic ->
      if S.O.debug.Debug_herd.barrier && S.O.PC.verbose > 2 then begin
        let module PP = Pretty.Make(S) in
        let legend =
          sprintf "%s cyclic co precursor"
            test.Test_herd.name.Name.name in
        let pos = conc.S.pos in
        prerr_endline legend ;
        PP.show_legend test  legend conc
          [ ("pos",S.rt pos); ("pco",S.rt conc.S.pco)]
      end ;
      res

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


end
