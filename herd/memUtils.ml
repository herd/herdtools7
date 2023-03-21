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
  module PC = S.O.PC

(*************)
(* Utilities *)
(*************)

  let iico es = E.EventRel.transitive_closure (E.iico es)

  let po_strict =
    if S.do_deps then
      fun es -> let _,e = es.E.po in e
    else
      fun es ->
        E.EventRel.of_pred
          es.E.events es.E.events E.po_strict

  let po_iico_data es =
    E.EventRel.union
      es.E.intra_causality_data
      (po_strict es)

  let po_iico es =  E.EventRel.union (iico es) (po_strict es)

  let do_po_strict es e1 e2 =
    if S.do_deps then E.EventRel.mem (e1,e2) (po_strict es)
    else E.po_strict e1 e2

  (* Slight extension of prog order *)

  let is_before_strict es =
    let iico = E.iico es in
    fun e1 e2  ->
      (do_po_strict es e1 e2) ||           (* e1 is po-before e2 *)
      (if do_po_strict es e2 e1 then false (* e2 is po-before e1 *)
       else (* e1 and e2 are from the same instruction *)
       E.EventRel.exists_path (e1,e2) iico)

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

let ps2evts m ps =
  E.EventSet.unions
    (List.map
       (fun p -> IntMap.safe_find E.EventSet.empty p m)
       ps)

let get_scope_classes m =
  let open BellInfo in
  let rec do_rec = function
    | Tree (s,ps,ts) ->
        let es = ps2evts m ps in
        let cls = StringMap.add s [es] StringMap.empty in
        let ess,clss =
          List.fold_left
            (fun (es,clss) t ->
              let es_t,cls_t = do_rec t in
              es_t::es,cls_t::clss)
            ([es],[cls]) ts in
        let es = E.EventSet.unions ess in
        let cls = StringMap.unions (@) clss in
        es,StringMap.add s [es] cls in
  fun sc ->
    let _,cls = do_rec sc in
    cls

let get_level_classes m =
  let open BellInfo in
  let rec do_rec = function
    | Tree (s,ps,ts) ->
        let es = ps2evts m ps in
        let cls = StringMap.add s [es] StringMap.empty in
        let clss =
          List.fold_left
            (fun clss t ->
              let cls_t = do_rec t in
              cls_t::clss)
            [cls] ts in
        let cls = StringMap.unions (@) clss in
        cls in
  do_rec

  let tree2succ m =
    let open BellInfo in
    let rec do_rec = function
      | Tree (_,ps,ts) ->
          let es = ps2evts m ps in
          let rs =
            List.fold_left
              (fun rs t ->
                let es_t,r = do_rec t in
                E.EventRel.cartesian es es_t::r::rs)
              [] ts in
          es,E.EventRel.unions rs in
    fun t -> let _,r = do_rec t in r

  let classes2rels cls =
    StringMap.fold
      (fun s cls k ->
        let r =
          E.EventRel.unions
            (List.map
               (fun evts -> E.EventRel.cartesian evts evts)
               cls) in
        (s,r)::k)
      cls []

let get_scope_rels evts sc = classes2rels (get_scope_classes (by_proc evts) sc)

let get_level_rels evts sc =
  let m = by_proc evts in
  let cls = get_level_classes m sc in
  let rs = classes2rels cls
  and s = tree2succ m sc in
  s,rs

let lift_proc_info i evts =
  let m = by_proc evts in
  List.map
    (fun (tag,ps) ->
      let evts =
        lazy begin
          E.EventSet.unions
            (List.map
               (fun p -> IntMap.safe_find E.EventSet.empty p m)
               ps)
        end in
      tag,evts)
    i

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
    MyMap.Make
      (struct
        type t = A.location
        let compare = A.location_compare
      end)

(* Collect various events by their location *)

  let collect_by_loc es pred =
    E.EventSet.fold
      (fun e k ->
        if pred e then
          let loc = get_loc e in
          LocEnv.accumulate loc e k
        else k)
      es.E.events LocEnv.empty

  let not_speculated es e = not (E.EventSet.mem e es.E.speculated)
  let collect_reg_loads es = collect_by_loc es E.is_reg_load_any
  and collect_reg_stores es = collect_by_loc es E.is_reg_store_any
  and collect_mem_loads es = collect_by_loc es E.is_mem_load
  and collect_mem_stores es = collect_by_loc es E.is_mem_store
  and collect_mem es = collect_by_loc es E.is_mem
  and collect_mem_non_init es =
    collect_by_loc es (fun e -> E.is_mem e && Misc.is_some (E.proc_of e))
  and collect_loads es = collect_by_loc es E.is_load
  and collect_stores es = collect_by_loc es E.is_store
  and collect_loads_non_spec es = collect_by_loc es (fun e -> E.is_load e && not_speculated es e)
  and collect_stores_non_spec es = collect_by_loc es (fun e -> E.is_store e && not_speculated es e)
  and collect_atomics es = collect_by_loc es E.is_atomic

  let partition_events es =
    let env =
      E.EventSet.fold
        (fun e k -> match E.location_of e with
        | Some loc ->
            LocEnv.accumulate loc e k
        | None -> k) es LocEnv.empty in

    LocEnv.fold (fun _ evts k -> E.EventSet.of_list evts::k) env []

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

  let is_rwm e = E.is_store e && E.is_load e

  let compute_pco rfmap ppoloc =
    let open Dir in
    let add e1 e2 d1 d2 k =
      match d1, d2 with
      | Dir.W,Dir.W -> E.EventRel.add (e1,e2) k
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
          end in
    let add1 e1 e2 d1 k =
      if is_rwm e2 then add e1 e2 d1 R (add e1 e2 d1 W k)
      else add e1 e2 d1 (E.get_mem_dir e2) k in
    try
      let pco =
        E.EventRel.fold
          (fun (e1,e2) k ->
            if is_rwm e1 then add1 e1 e2 R (add1 e1 e2 W k)
            else add1 e1 e2 (E.get_mem_dir e1) k)
          ppoloc
          E.EventRel.empty in
      Some pco
    with Exit -> None

(*to handle speculation in final state*)

  let remove_spec_from_map es m =
    let spec = es.E.speculated in
    LocEnv.fold
      (fun loc es k ->
        let es =
          List.filter
            (fun e -> not (E.EventSet.mem e spec))
            es in
        match es with
        | [] ->  k
        | _  -> LocEnv.add loc es k)
      m LocEnv.empty

(* Alignment check *)
  let is_aligned tenv senv e =
    let loc = Misc.as_some (E.location_of e) in
    let si = Misc.as_some (S.A.symbolic_data loc) in
    let loc0 = S.A.of_symbolic_data {si with Constant.offset=0;} in
    let t = S.A.look_type tenv loc0 in
    let open TestType in
    let array_sz =
      match t with
      | TyArray (_,sz) -> sz
      | _ -> 1
    and sz_e = E.get_mem_size e in
    match si with
    | {Constant.name=s; offset=idx;_}
      ->
        let sz_s = A.look_size senv (Constant.Symbol.pp s) in
        let nbytes_s = MachSize.nbytes sz_s in
        if MachSize.less_than_or_equal sz_e sz_s then begin
          let ncell = idx / nbytes_s and idx0 = idx mod nbytes_s in
          0 <= ncell && ncell < array_sz
          &&  List.exists (Misc.int_eq idx0) (MachSize.get_off sz_s sz_e)
        end else begin
            idx >= 0 &&
            (let nbytes_e = MachSize.nbytes sz_e in
             idx mod nbytes_e = 0 &&
             (let idx_max = idx/nbytes_s + (nbytes_e/nbytes_s) in
             idx_max <= array_sz))
        end
end
