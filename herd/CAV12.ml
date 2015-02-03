(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Sela Haim's axiomatic model for PPC, second version *)

open Printf	

module type Config =
  sig
    val opt : Model.cav12_opt
    include Model.Config
  end
  
module Make
    (O:Config)
    (S:Sem.Semantics)
    (B:AllBarrier.S with type a = S.barrier)
    =
  struct
(************************)
(* Static configuration *)
(************************)

(* Compute fixpoint completely, even for cyclic rels *)
    let doall = true

(* Opens and module definitions *)
    open Model

    module S = S
    module A = S.A
    module E = S.E
    module U = MemUtils.Make(S)
    module MU = ModelUtils.Make(O)(S)

    let is_fence x =
      let open B in
      match E.barrier_of x with
      | Some a ->
          begin match B.a_to_b a with
          | SYNC|LWSYNC|EIEIO
          | DMB|DSB|DMBST|DSBST
          | MFENCE|LFENCE|SFENCE
            -> true
          | _ -> false
          end
      | None -> false


    module SE =
      SelaEvent.Make(S)
        (struct
          type event = S.event
          let visible_fence = is_fence            
        end)



(* Pretty print *)
    let show_failure test conc msg vb_pp =
      if O.debug then
        let module PP = Pretty.Make(S) in
        let legend =
          sprintf "%s: %s" test.Test.name.Name.name msg in
        eprintf "%s\n%!" legend ;
        PP.show_legend test  legend conc (Lazy.force vb_pp)

(* Utilities *)
    let proc_eq = Misc.int_eq
    let one_store (e1,e2) = E.is_mem_store e1 || E.is_mem_store e2
    and same_proc (e1,e2) = E.same_proc e1 e2
    and diff_proc (e1,e2) = not  (E.same_proc e1 e2)
    and get_proc e = match E.proc_of e with
    | Some p -> p
    | None -> -1

    let is_fence_exe e = match e.SE.nature with
    | SE.Exe ->
        begin match E.barrier_of e.SE.event with
        | Some _ -> true
        | None -> false
        end
    | _ -> false

    let is_self_propagate e = match e.SE.nature with
    | SE.Prop j -> proc_eq j (get_proc e.SE.event)
    | _ -> false

          (* Meaningless events *)
    let is_bad e = is_fence_exe e || is_self_propagate e

    let is_that_fence b x =  match E.barrier_of x with
    | Some a -> B.a_to_b a = b
    | None -> false

    let is_eieio x = is_that_fence B.EIEIO x

    let is_strong x =
      is_that_fence B.SYNC x ||
      is_that_fence B.DMB x ||
      is_that_fence B.DSB x ||
      is_that_fence B.MFENCE x ||
      (O.opt.strongst &&
       (is_that_fence B.DSBST x || is_that_fence B.DMBST x))

(*
    let is_light x =
      is_that_fence B.LWSYNC x ||
      is_that_fence B.EIEIO x ||
      (not O.opt.strongst &&
       (is_that_fence B.DSBST x || is_that_fence B.DMBST x))
*)
    let is_isync x =
      is_that_fence B.ISYNC x ||
      is_that_fence B.ISB x

(* Parameters of Sela's "generic" model *)
    let fbefore (x,y) =
      (E.is_mem_store x && is_fence y) ||
      (is_fence x && E.is_mem_store y) 

    let fafter (x,y) = 
      E.event_compare x y <> 0 &&
      is_strong x && is_strong y

(****************************************************)
(* Main relation builders of Sela's "generic" model *)
(****************************************************)

(* evord computation, it's recursive *)
    let fixpoint pp eq f =
      let rec fix k v0 =
        if O.debug && O.verbose > 2 then pp k v0 ;
        let v1 = f v0 in
        if eq v0 v1 then v0
        else fix (k+1) v1 in
      fix 0

    let eq_st (r0,(a0,b0)) (r1,(a1,b1)) =
      SE.SplittedRel.subset r1 r0 &&
(* checking after/before is optional, gives beter images *)
      begin
        if not doall then
          E.EventRel.subset a1 a0 &&
          E.EventRel.subset b1 b0
        else true
      end
        

    let before_sources m evord =
      SE.SplittedRel.fold
        (fun (x1e,y1e) k ->
          let x = x1e.SE.event
          and y = y1e.SE.event in
          let py = get_proc y in
          if
            m.SE.fbefore (x,y) &&
            SE.relevant_to_proc x1e py && 
            SE.relevant_to_proc y1e py
          then (x,y)::k
          else k)
        evord []

    let before_of m evord = E.EventRel.of_list (before_sources m evord)

    let after_sources m evord =
      SE.SplittedRel.fold
        (fun (x1e,y1e) k ->
          let x = x1e.SE.event
          and y = y1e.SE.event in
          let px = get_proc x in
          
          if
            m.SE.fafter (x,y) &&
            SE.relevant_to_proc x1e px
          then (x,y)::k
          else k)
        evord []

    let show_reduced = true

(* Pretty print of evord *)
    let vb_pp_reduced br ar evord vb_pp =
      let rt = SE.SplittedRel.remove_transitive_edges in
      if show_reduced then
        let evord =
          SE.SplittedRel.filter
            (fun (e1,e2) ->
              let x = e1.SE.event
              and y = e2.SE.event in
              not
                (E.EventRel.mem (x,y) ar ||
                E.EventRel.mem (x,y) br))
            (rt evord) in
        ("Before",br)::("After",ar)::
        SE.vb_pp_splitted evord @ Lazy.force vb_pp
      else
        ("Before",br)::("After",ar)::
        SE.vb_pp_splitted (rt evord) @ Lazy.force vb_pp

(* Compute nature of relevant events *)
    let nature_of_relevant i x =
      if SE.globally_visible x then begin
        if proc_eq (get_proc x) i then
          SE.Com
        else
          SE.Prop i
      end else if E.is_mem_load x then
        SE.Exe
      else
        raise Exit

    let add_relevants ps xys =
      List.fold_left
        (fun k (x,y) ->
          List.fold_left
            (fun k i ->
              try
                let nx = nature_of_relevant i x
                and ny = nature_of_relevant i y in
                ({ SE.nature=nx ; event=x; },{ SE.nature=ny ; event=y; })::k
              with Exit -> k)
            k ps)
        [] xys

    let seq2 r = SE.SplittedRel.union r (SE.SplittedRel.sequence r r)          


    let mk_evord m test conc evts vb_pp _po com rf _co loc_ord =
      let pp_st k (evord,(br,ar)) =
        if O.debug && O.verbose > 2 then begin
          let pp = vb_pp_reduced br ar evord vb_pp in
          let cy = match SE.SplittedRel.get_cycle evord with
          | Some cy ->
              eprintf "Cycle:" ;
              List.iter
                (fun e -> eprintf " %s" (SE.pp_splitted e))
                cy ;
              eprintf "\n" ;
              let rec to_rel = function
                | [_]|[] -> E.EventRel.empty
                | x::(y::_ as rem) ->
                    E.EventRel.add (y.SE.event,x.SE.event) (to_rel rem) in
              to_rel cy
          | None -> E.EventRel.empty in
          let pp = ("CY",cy)::pp in
          show_failure test conc (sprintf "Step %i" k) (lazy pp)
        end in
      pp_st (-1) (loc_ord,(E.EventRel.empty,E.EventRel.empty)) ;
      let ps = E.procs_of conc.S.str in
(* Initial evord *)
      let evord0 =
(* Local edges *)
        let r1 = loc_ord
(* Read-from executed *)
        and r2 =
          let pairs =
            E.EventRel.fold
              (fun (w,r) k ->
                if E.same_proc w r then
                  ({SE.nature=SE.Exe; event=w;},{SE.nature=SE.Exe; event=r;})::k
                else k)
              rf [] in
          SE.SplittedRel.of_list pairs
(* Complete-after-execute *)
        and r3 =
          let pairs =
            E.EventSet.fold
              (fun e k ->
                if is_fence e then k else
                ({SE.nature=SE.Exe; event=e;},{SE.nature=SE.Com; event=e;})::k)
              evts [] in
          SE.SplittedRel.of_list pairs
(* Propagate after complete *)
        and r4 =
          let pairs =
            E.EventSet.fold
              (fun e k -> 
                if SE.globally_visible e then
                  let j = get_proc e
                  and ecom = { SE.nature = SE.Com ; event = e; } in
                  List.fold_left
                    (fun k i ->
                      if proc_eq j i then k
                      else (ecom,{ SE.nature=SE.Prop i ; event = e; })::k)
                    k ps
                else k)
              evts [] in
          SE.SplittedRel.of_list pairs
(* Communication edges *)
        and r5 =
          let pairs =
            E.EventRel.fold
              (fun (x,y as p) k ->
                if diff_proc p then
                  let open E in
                  match get_mem_dir x, get_mem_dir y with
                  | Dir.R,Dir.R -> assert false
                  | Dir.R,Dir.W ->
                      ({ SE.nature = SE.Exe ; event = x ; },
                       { SE.nature = SE.Prop (get_proc x) ; event = y ; })::k
                  | Dir.W,Dir.R ->
                      ({ SE.nature = SE.Prop (get_proc y) ; event = x ; },
                       { SE.nature = SE.Exe ; event = y ; })::k
                  | Dir.W,Dir.W ->
                      if !Misc.switch then k
                      else
                      ({ SE.nature = SE.Com ; event = x ; },
                       { SE.nature = SE.Prop (get_proc x) ; event = y ; })::k
                else k)
              com [] in
          SE.SplittedRel.of_list pairs in
        SE.SplittedRel.unions [r1;r2;r3;r4;r5;] in
      let evord0 =
        SE.SplittedRel.filter
          (fun (x,y) -> not (is_bad x || is_bad y))
          evord0 in

(*      eprintf "EVORD0: %s\n" (SE.pp_splitted_rel evord0) ; *)
      let f (evord,_ as p) =
        (* Simplification (for pictures) if already cyclic, no need to go on *)
        if
          not doall && SE.SplittedRel.is_cyclic evord
        then p
        else
          let br,bef =
            let srcs = before_sources m evord in
            let pairs = add_relevants ps srcs in
            E.EventRel.of_list srcs,SE.SplittedRel.of_list pairs
          and ar,aft =
            let srcs = after_sources  m evord in
            let pairs = add_relevants ps srcs in
            E.EventRel.of_list srcs,SE.SplittedRel.of_list pairs in
          let evord,(br,ar) =
            SE.SplittedRel.unions [evord; aft; bef; ],(br,ar) in
          seq2 evord,(br,ar) in
      

      fixpoint pp_st eq_st f (evord0,(E.EventRel.empty,E.EventRel.empty))

(* Compute cord *)
    let mk_cord m co evord =
      let r1 = co
      and r2 =
        let srcs = before_sources m evord in
        E.EventRel.of_list srcs in
      E.EventRel.unions [r1;r2;]

(***************)
(* Entry point *)
(***************)

    let check_event_structure test conc =

      if E.EventSet.exists
          (fun x ->
            (is_that_fence B.DSBST x || is_that_fence B.DMBST x))
          conc.S.str.E.events
      then
        Warn.user_error "ST qualifier not handled by CAV12" ;
      
      let pr = MU.make_procrels is_isync conc in
      if O.debug && O.verbose > 0 then begin
        let module PP = Pretty.Make(S) in
        let vb_pp = MU.pp_procrels B.pp_isync pr in
        let legend =
          sprintf "%s: dependencies" test.Test.name.Name.name in
        eprintf "%s\n%!" legend ;
        PP.show_legend test  legend conc vb_pp

      end ;

      let flocal xe ye ze =
        let mem = E.EventRel.mem in
        let open SE in
        let x = xe.event and y = ye.event in
        let p = (x,y) in        
        begin
          SE.is_satisfy xe && (SE.is_satisfy ye || SE.is_init ye) &&
          (mem p pr.S.addr || mem p pr.S.data)
        end ||
        begin
          SE.is_satisfy xe && SE.is_satisfy ye &&
          is_that_fence B.LWSYNC ze.SE.event (* Ok for lwsync here: read/read *)
        end ||
        begin
          SE.is_commit xe && SE.is_commit ye &&
          (mem p pr.S.addr || mem p pr.S.data || mem p pr.S.ctrl)
        end ||
        begin
          E.is_mem x && E.is_mem y &&
          SE.is_commit xe && SE.is_commit ye &&
          E.same_location x y
        end ||
        begin          
          SE.is_commit xe && SE.is_commit ye &&
          (is_fence x || is_fence y) &&
          (not
             ((E.is_load x && is_eieio y) ||
             (E.is_load y && is_eieio x)))
        end ||
        begin
          SE.is_commit xe && SE.is_commit ye &&
          E.is_commit x
        end ||
        begin
          E.is_mem x && E.is_mem y &&
          SE.is_commit xe && SE.is_commit ye &&
          mem (x,ze.SE.event) pr.S.addr
        end ||
(* USELESS and dangerous (should be e->e anyway by detour
   begin
   SE.is_commit xe &&
   E.is_load x && E.is_load y &&
   U.rext conc y &&
   E.same_location x y && not (U.same_source conc x y)
   end ||
 *)
        begin
          SE.is_commit xe && SE.is_satisfy ye &&
          (is_isync x || is_strong x)          
        end in
(****************)
(* Model proper *)
(****************)
(* Group Sela's parameters *)
      let m = {SE.fbefore;fafter;flocal;} in
(* Restrict to relevant *)
      let po = E.EventRel.restrict_domain SE.evt_relevant conc.S.po in
      let evts = E.EventSet.filter SE.evt_relevant conc.S.str.E.events in
(* No co dependence *)      
      let rf = U.make_rf conc in
      let loc_ord = SE.splitted_loc_ord m conc evts rf po in
      fun kont res ->  
        let process_co co res =
          let co = S.tr co in
          let fr = U.make_fr conc co in
          let com = E.EventRel.unions [rf;fr;co] in
          let vb_pp = lazy begin
            ("fr",fr)::("co",S.rt co)::[]
          end in
          (* Sela's coherence tests *)
          if
            match O.through with
            | ThroughAll -> false
            | ThroughInvalid|ThroughNone ->
                let complus = S.tr com in
                E.EventRel.exists
                  (fun (e1,e2 as p) ->
                    E.same_proc e1 e2 && not (E.EventRel.mem p po))
                  complus
          then begin
            show_failure
              test conc
              "Failure of coherence"
              vb_pp ;
            res
          end else begin
            let evord,(br,ar) = mk_evord m test conc evts vb_pp po com rf co loc_ord in
            let cord = mk_cord m co evord in
            let ok_cord = not O.opt.cord || E.EventRel.is_acyclic cord in
            if
              match O.through with
              | ThroughAll|ThroughInvalid -> false
              | ThroughNone -> not ok_cord
            then begin
              let vb_pp =
                lazy begin
                  let cord =
                    S.rt (E.EventRel.diff cord (E.EventRel.union br ar)) in
                  ("cord",cord)::
                  ("Before",br)::
                  Lazy.force vb_pp end in
              show_failure
                test conc
                "cord is cyclic"
                vb_pp ;
              res
            end else
              let vb_pp =
                lazy begin
                  vb_pp_reduced br ar evord vb_pp
                end in
              let ok_evord = SE.SplittedRel.is_acyclic evord in
              if
                match O.through with
                | ThroughAll|ThroughInvalid -> false
                | ThroughNone -> not ok_evord then begin
                    show_failure
                      test conc
                      "evord is cyclic"
                      vb_pp ;
                    res
                end else begin           
                  kont conc conc.S.fs vb_pp None res
                end
          end in
        U.apply_process_co test conc process_co res

  end
