(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Basic model run (uniproc only) *)

open Printf

module type Config = sig
  val verbose : int
end

module Make (O:Config) (C:ArchRun.S) :
    sig
      val run : C.C.event list list -> C.A.location C.C.EventMap.t
        -> (C.A.location * Code.v) list list

      val dump_cond :  (C.A.location * Code.v) list list -> string
    end
    =
  struct
    module A = C.A
    module C = C.C

    module Rel = InnerRel.Make(C.OrderedEvent)
    module ESet = MySet.Make(C.OrderedEvent)

(* Minimal event structure *)

    let order_to_rel =
      let rec do_rec k = function
        | []|[_] -> k
        | x::xs ->
            let k =
              List.fold_left
                (fun k y -> (x,y)::k)
                k xs in
            do_rec k xs in
      fun es -> Rel.of_list (do_rec [] es)

    let make_pox evts =
      let rec do_one k = function
        | [] -> k
        | e::es ->
            let k = 
              List.fold_left
                (fun k f ->
                  if Misc.string_eq e.C.loc f.C.loc then
                    (e,f)::k
                  else k)
                k es in
            do_one k es in
      let xs =  List.fold_left do_one [] evts in
      Rel.of_list xs

    module Locs = MySet.Make(String)

    let make_evts ess =
      let es = List.map ESet.of_list ess in
      let es = ESet.unions es in
      (* Beurk alloc well behaved evt identifiers *)
      let idx = (ESet.max_elt es).C.idx+1 in
      let locs =
        ESet.fold
          (fun e -> Locs.add e.C.loc)
          es Locs.empty in
      let wsi,_ =
        Locs.fold
          (fun x (wsi,idx) -> C.make_wsi idx x::wsi,idx+1)
          locs ([],idx) in
      ESet.of_list wsi,es

    type str =
        { evts : ESet.t ; wsi : ESet.t ;
          pox : Rel.t ; }
          

    let make_str evts =
      let wsi,evts = make_evts evts
      and pox = make_pox evts in
      { pox; evts=ESet.union evts wsi; wsi; }

(* Communication edges *)


    module LocMap = MyMap.Make(String)
    module State =
      MyMap.Make
        (struct
          type t = A.location
          let compare = A.location_compare
        end)

    module StateSet =
      MySet.Make
        (struct
          type t = Code.v State.t
          let compare = State.compare Misc.int_compare              
        end)

    let by_loc pred evts =
      ESet.fold
        (fun e m ->
          if pred e then
            let loc = e.C.loc in
            let old = LocMap.safe_find ESet.empty loc m in
            LocMap.add loc (ESet.add e old) m
          else m)
        evts LocMap.empty

    let get_possible m =
      LocMap.fold
        (fun _ es k ->
          let rs,ws = ESet.partition (fun e -> e.C.dir = Code.R) es in
          let rs = ESet.elements rs
          and ws = ESet.elements ws in
          List.fold_left (fun k r -> (r,ws)::k) k rs)
        m []

        
    let gen_rfm kont str =
      let rs,ws =
        List.split (get_possible (by_loc (fun _ -> true) str.evts)) in
      Misc.fold_cross ws
        (fun ws k ->
          let rfm = List.fold_right2 C.EventMap.add rs ws C.EventMap.empty in
          kont str rfm k)

    let process_rfm kont str rfm =
      let rf =
        C.EventMap.fold
          (fun r w rf -> (w,r)::rf)
          rfm [] in
      let rf = Rel.of_list rf in
      let ws_by_loc =
        by_loc
          (fun e -> match e.C.dir with Code.W -> true | Code.R -> false)
          str.evts in
      let wsi_by_loc =
        ESet.fold
          (fun w k -> LocMap.add w.C.loc w k)
          str.wsi LocMap.empty in
      let orders =
        LocMap.fold
          (fun loc ws k ->
            let wi =
              try LocMap.find loc wsi_by_loc
              with Not_found -> assert false in
            let vb_loc =
              ESet.fold
                (fun w k ->
                  if C.OrderedEvent.compare wi w = 0 then k
                  else (wi,w)::k)
                ws [] in
            let orders_loc = Rel.all_topos
                (O.verbose > 1) ws (Rel.of_list vb_loc) in
            orders_loc::k)
          ws_by_loc [] in
      
      Misc.fold_cross_gen (fun x xs -> x::xs) []
        orders
        (kont str rfm rf)


    let make_com rfm co =
      let co = Rel.unions (List.map order_to_rel co) in
      let co_map =
        Rel.fold
          (fun (w1,w2) k ->
            let old = C.EventMap.safe_find ESet.empty w1 k in
            C.EventMap.add w1 (ESet.add w2 old) k)
          co C.EventMap.empty in
      let rf =
        C.EventMap.fold
          (fun r w k -> (w,r)::k)
          rfm [] in
      let rf = Rel.of_list rf in
      let fr =
        C.EventMap.fold
          (fun r w0 ->
            let ws =  C.EventMap.safe_find ESet.empty w0 co_map in
            ESet.fold (fun w k -> (r,w)::k) ws)
          rfm [] in
      let fr = Rel.of_list fr in
      Rel.unions [rf;co;fr]
        

    let process_co kont str rfm _rf co k =
      let com = make_com rfm co in
      let seq = Rel.union com str.pox in
      if Rel.is_acyclic seq then
        kont rfm co k
      else k



    let kont m rfm co k =
      let fs =
        List.fold_left
          (fun fs ws -> match ws with
          | []|[_] -> fs
          | _ ->
              let w = Misc.last ws in
              State.add (A.of_loc w.C.loc) w.C.v fs)
          State.empty co in
      let fs = 
        C.EventMap.fold
          (fun r reg fs ->
            try
              let w = C.EventMap.find r rfm in
              (* Can fail because some registers steems from
                 atomic writes... *)
              let v = w.C.v in
              State.add reg v fs
            with Not_found -> fs)
          m fs in
(*
      pp_state stderr fs ;
      eprintf "\n" ;
*)
      StateSet.add fs k

    let run evts m =
      let str = make_str evts in
      let kont = kont m in
      let process_co = process_co kont in
      let process_rfm = process_rfm process_co in
      let sts = gen_rfm process_rfm str StateSet.empty in
(*      eprintf "Candidates: %i\n" (StateSet.cardinal sts) ; *)
      List.map
        State.bindings
        (StateSet.elements sts)

(* Dump condition *)
    type cond =
      | Atom of A.location * Code.v
      | Or of cond list
      | And of cond list


    module OV = struct
      type t = Code.v
      let compare = Misc.int_compare
    end

    module VSet = MySet.Make(OV)
    module VMap = Map.Make(OV)

    let best_col m =
      let mt = Misc.transpose m in
      let cs =
        List.map
          (fun col ->
            let vs = List.map (fun (_,v) -> v) col in
            VSet.cardinal (VSet.of_list vs))
          mt in
      let rec best_rec k (kb,b as p) = function
        | [] -> kb
        | c::cs ->
            if c < b then best_rec (k+1) (k,c) cs
            else best_rec (k+1) p cs in
      best_rec 0 (-1,max_int) cs

    let swap_list =
      let rec swap_list k prev xs = match xs with
      | [] -> assert false
      | x::xs ->
          if k <= 0 then
            x::List.rev_append prev xs
          else
            swap_list (k-1) (x::prev) xs in
      fun k xs -> swap_list k [] xs

          
    let swap_col k m =
      let mt = Misc.transpose m in
      let mt = swap_list k mt in
      Misc.transpose mt
        
    let extract_column xss = match xss with
    | []|[]::_ -> assert false
    | ((loc0,_)::_)::_ ->
        loc0,
        List.map
          (fun row -> match row with
          | (loc,v)::ps ->
              assert (A.location_compare loc0 loc = 0) ;
              v,ps
          | [] -> assert false)
          xss

    let group_rows ps =
      List.fold_left
        (fun m (v,ps) ->
          let pss =
            try VMap.find v m
            with Not_found -> [] in
          VMap.add v (ps::pss) m)
        VMap.empty ps
        
    let rec compile_cond m = 
      let k = best_col m in
      let loc,ps = extract_column (swap_col k m) in
      let m = group_rows ps in
      match ps with
      | [] -> assert false
      | (_,[])::_ ->
          Or 
            (VMap.fold
               (fun v _ k -> Atom (loc,v)::k)
               m [])
      | _ ->
          Or
            (VMap.fold
               (fun v m k -> And [Atom (loc,v);compile_cond m]::k)
               m [])

    let cond_of_finals fs = compile_cond fs

    let rec do_dumps op fs = match fs with
    | [] -> assert false
    | [f] -> do_dump f
    | _ ->
        let pp = String.concat op (List.map do_dump fs) in
        sprintf "(%s)" pp

    and do_dump = function
      | Or [] -> "false"
      | And [] -> "true"
      | Or fs -> do_dumps " \\/ " fs
      | And fs -> do_dumps " /\\ " fs
      | Atom (loc,v) -> sprintf "%s=%i" (A.pp_location loc) v

    let dump_cond fs = do_dump (cond_of_finals fs)


  end

