open Printf

module type Cfg = sig
  val skipchecks : StringSet.t
  include Mem.S
end

module Make (M:Cfg)
  =
  struct
    module S = M.S
    module E = S.E
    module A = S.A
    module U = MemUtils.Make(S)
    module Sol = E.Act.A.V.Solution

    type exec = {
        po : E.EventRel.t;
        iico : E.EventRel.t;
        mo : E.EventRel.t;
        rf : E.EventRel.t;
        toadd : (int * E.EventSet.t) list;
        added : E.EventSet.t;
        revisit : E.EventSet.t;
        safe : E.EventSet.t;
        rmws : E.EventRel.t;
        exvals : E.event -> bool;
        rfm : M.S.rfmap;
        flags : Flag.Set.t;
        psc : E.EventRel.t;
        debug_rels : (string * E.EventRel.t) list;
      }

              (* debug *)

    let debug = false

    let debug_proc chan p = fprintf chan "%i" p

    let debug_event chan e = fprintf chan "%s:%s " (E.pp_eiid e) (S.E.Act.pp_action e.E.action)

    let debug_event_set chan e =
      let _ = print_string "[" in
      let _ = E.EventSet.iter
                (fun x -> debug_event chan x)
                e in
      print_string "]\n"

    let debug_cnstrnts chan e = fprintf chan "\n[ %s ]" (S.M.VC.pp_cnstrnts e)

    let debug_set chan r =
      E.EventRel.pp chan ","
        (fun chan (e1, e2) -> fprintf chan "%a -> %a"
                                debug_event e1 debug_event e2)
        r

    let debug_rel chan r =
      E.EventRel.pp chan ","
        (fun chan (e1, e2) -> fprintf chan "%a -> %a"
                                debug_event e1 debug_event e2)
        r

    let debug_procs chan procs =
      List.iter
        (fun (x, y) ->
          let _ = printf "proc %d \t:" x in
          debug_event_set chan y) procs

    let debug_rf chan wt rf =
      let _ = match wt with
        | M.S.Final loc -> fprintf chan "Final %s ->" (A.pp_location loc)
        | M.S.Load ev -> debug_event chan ev in
      match rf with
      | M.S.Init -> fprintf chan "init\n"
      | M.S.Store ev ->let _ = debug_event chan ev in
                       print_string "\n"

    let debug_exec chan ex =
      let _ = fprintf chan "execution\n\npo : " in
      let _ = debug_rel chan (E.EventRel.remove_transitive_edges (E.EventRel.restrict_domains E.is_mem E.is_mem ex.po)) in
      let _ = fprintf chan "\n\nmo : " in
      let _ = debug_rel chan (E.EventRel.remove_transitive_edges ex.mo) in
      let _ = fprintf chan "\n\nrf : " in
      let _ = debug_rel chan ex.rf in
      let _ = fprintf chan "\n\nadded : " in
      let _ = debug_event_set chan ex.added in
      let _ = fprintf chan "\ntoadd : " in
      let _ = debug_procs chan ex.toadd in
      let _ = fprintf chan "\nrevisit : " in
      let _ = debug_event_set chan ex.revisit in
      let _ = printf "rmws = %a\n" debug_rel ex.rmws in
      fprintf chan "\n--------------------\n\n"



      (* relations and events *)

    let aux = fun x ->
      try List.assoc x S.E.Act.arch_sets
      with Not_found ->
        Warn.fatal "annotation %s not found" x

    let rmw = aux "RMW"
    let rlx = aux "RLX"
    let acq = aux "ACQ"
    let rel = aux "REL"
    let acq_rel = aux "ACQ_REL"
    let sc = aux "SC"
    let na = aux "NA"
    let fence = E.is_barrier
    let atomic = aux "A"


    (* a; b? *)
    let seq_union a b = E.EventRel.union a  (E.EventRel.sequence a b)

    let added a r = E.EventRel.restrict_domains
                      (fun x -> E.EventSet.mem x a)
                      (fun x -> E.EventSet.mem x a)
                      r

                      (* sbr = (sb | rf)+ *)
    let sbrf ex =
      added ex.added (E.EventRel.transitive_closure (E.EventRel.union (added ex.added ex.po) ex.rf))

            (* rseq = [W]; (sb & loc)?; [W & (RLX | REL | ACQ_REL | ACQ | SC)]; (rf; rmw)* *)
    let rseq ex =
      let rs0 = E.EventRel.restrict_rel
                  E.same_location
                  ex.po in
      let rs1 = E.EventRel.set_to_rln
                  (E.EventSet.filter E.is_mem_store ex.added) in
      let rs2 = E.EventRel.sequence rs1 rs0 in
      let rs3 = E.EventRel.set_to_rln
                  (E.EventSet.filter (fun x -> E.is_mem_store x
                                               && (rlx x.E.action
                                                   || rel x.E.action
                                                   || acq x.E.action
                                                   || acq_rel x.E.action
                                                   || sc x.E.action))
                     ex.added) in
      let rs4 = E.EventRel.sequence rs2 rs3 in
      let rs5 = E.EventRel.inter rs1 rs3 in
      let rs6 = E.EventRel.union rs4 rs5 in
      let rs7 = E.EventRel.transitive_closure (E.EventRel.sequence ex.rf (added ex.added ex.rmws)) in
      E.EventRel.union rs6 (E.EventRel.sequence rs6 rs7)

                       (* sw = [(REL | ACQ_REL | SC)]; ([F]; sb)?; rs; rf; [R & (RLX | REL | ACQ | ACQ_REL | SC)]; (sb; [F])?; [(ACQ | ACQ_REL | SC)] *)
      let sw ex =
      let mpo = ex.po in
      let rseq = rseq ex in
      let fences = E.EventSet.filter
                     fence
                     ex.added in

      let sw0 = E.EventRel.sequence rseq ex.rf in
      let sw1 = E.EventRel.set_to_rln (E.EventSet.filter
                                         (fun x -> rel x.E.action
                                                   || acq_rel x.E.action
                                                   || sc x.E.action)
                                         ex.added) in
      let sw2 = E.EventRel.sequence (E.EventRel.set_to_rln fences) mpo in
      let sw3 = E.EventRel.union sw1 (E.EventRel.sequence sw1 sw2) in
      let sw4 = E.EventRel.sequence sw3 sw0 in
      let sw5 = E.EventRel.set_to_rln (E.EventSet.filter
                                         (fun x -> (rlx x.E.action
                                                    || rel x.E.action
                                                    || acq x.E.action
                                                    || acq_rel x.E.action
                                                    || sc x.E.action)
                                                   && E.is_mem_load x)
                                         ex.added) in
      let sw6 = E.EventRel.sequence sw4 sw5 in
      let sw7 = E.EventRel.sequence mpo (E.EventRel.set_to_rln fences) in
      let sw8 = E.EventRel.set_to_rln (E.EventSet.filter
                                     (fun x -> acq x.E.action
                                               || acq_rel x.E.action
                                               || sc x.E.action)
                                     ex.added) in
      let sw9 = E.EventRel.union sw8
              (E.EventRel.sequence sw7 sw8) in
      let sw = E.EventRel.sequence sw6 sw9 in
      sw

      (* hb = (sb | sw)+ *)
    let hb ex =

      let sw0 = sw ex in
      let hb0 = E.EventRel.union ex.po sw0 in
      E.EventRel.transitive_closure hb0

    (* is part of a rmw operation *)
    let is_exclusive rmws e = E.EventRel.exists
                                (fun (x, y) -> x = e
                                               || y = e)
                                rmws



      (* preproc *)


    let solve test es cs =
      match M.solve_regs test es cs with
      | None -> (es, M.S.RFMap.empty, cs)
      | Some (es, rfm, cs) -> (es, rfm, cs)



      (* postproc *)

    let clean_exec e =
      let nrf = E.EventRel.remove_transitive_edges (E.EventRel.restrict_domains E.is_mem E.is_mem e.rf) in
      let nmo = E.EventRel.remove_transitive_edges (E.EventRel.restrict_domains E.is_mem E.is_mem e.mo) in
      let npo = E.EventRel.remove_transitive_edges (E.EventRel.restrict_domains E.is_mem E.is_mem e.po) in
      {e with rf = nrf; po = npo; mo = nmo}



      (* assigning variables *)

    let is_final_write w ex =
      E.is_mem_store w
      && not (E.EventRel.exists
                (fun (x, _) -> x = w)
                ex.mo)


    let make_cnstrnts ex =
        E.EventRel.fold
          (fun rel cns ->
            match (E.written_of (fst rel), E.read_of (snd rel)) with
            | Some w, Some v ->
               M.S.M.VC.Assign (v, M.S.M.VC.Atom w) :: cns
            | _ -> cns) ex.rf []



      (* stateless algorithm aux functions *)

      (* next event : pending write if there is one or next event according to thread number ascending order and po *)
    let nextp exec po revisit pending =
      let rec aux e p = begin
          match e with
          | [] -> None
          | (_, e) :: tl ->
             try
               match E.EventRel.roots
                       (E.EventRel.restrict_rel
                          (fun x y -> E.EventSet.mem x e
                                      && E.EventSet.mem y e)
                          p) with
               | x when E.EventSet.is_empty x
                        && E.EventSet.is_empty e -> aux tl p
               | x when E.EventSet.is_empty x -> Some (E.EventSet.choose e)
               | x -> Some (E.EventSet.choose x)
             with Not_found -> None end in
      try match E.EventSet.cardinal pending with
          | 2 -> (Some (E.EventSet.find (fun x -> not (E.EventSet.mem x revisit)) pending))
          | 1 -> (Some (E.EventSet.choose pending))
          | 0 -> aux exec po
          | _ -> assert false
      with Not_found -> None

    let extract_event exec po revisit pending = match nextp exec po revisit pending with
      | None -> (None, exec)
      | Some e -> (Some e, List.map
                             (fun (x, y) -> (x, E.EventSet.remove e y))
                             exec)
                    (* pending write : not yet added write of a rmw whose read has been added *)
    let pending ex =
      let cpo =
        E.EventRel.restrict_codomain
          (fun x -> E.EventSet.mem x ex.added)
          ex.po in
      let crmw = E.EventRel.restrict_domain
                   (fun x -> E.EventSet.mem x ex.added) ex.rmws in
      E.EventRel.codomain
        (E.EventRel.restrict_domains
           (fun x -> not (E.EventRel.exists
                            (fun (y,_) -> E.event_compare y x = 0)
                            cpo))
           (fun x -> not (E.EventSet.mem x ex.added))
        crmw)

    let check_revisit ex =
      let t0 = (E.EventRel.codomain (added ex.added (E.EventRel.sequences [E.EventRel.set_to_rln ex.revisit; sbrf ex; E.EventRel.set_to_rln (E.EventSet.filter (fun x -> E.is_mem_load x && not (E.is_mem_store x)) ex.added)]))) in
      if
        not (E.EventSet.subset t0 ex.revisit)
      then
        let _ = if debug then printf "4.1.1\n" else () in
        false
      else
        true

    let check_exec ex =
      let c1 = E.EventRel.filter
                 (fun (x, y) -> not (E.EventSet.mem x ex.added) || not (E.EventSet.mem y ex.added))
                 (E.EventRel.union ex.rf ex.mo) in
      if not (E.EventRel.is_empty c1)
      then
        let _ = if debug then printf "%a\n%a\n" debug_exec ex debug_rel c1 else () in
        false
      else if E.EventSet.exists
                   (fun x -> E.EventRel.cardinal
                           (E.EventRel.restrict_codomain (fun y -> y = x) ex.rf)
                           != 1)
                   (E.EventSet.filter E.is_mem_load ex.added)
      then
        let _ = if debug then printf "2 : %a\n%a\n" debug_rel ex.rf debug_event_set (E.EventSet.filter (fun x -> E.is_mem_load x && not (E.is_mem_store x)) ex.added) else () in
        false
      else
        let grp = E.EventSet.filter
                    (is_exclusive ex.rmws)
                    (pending ex) in
        let t1 = E.EventRel.sequences [E.EventRel.set_to_rln grp;
                                       E.EventRel.inverse ex.rf;
                                       ex.rf;
                                       E.EventRel.set_to_rln (E.EventSet.diff
                                                                (E.EventSet.filter
                                                                   (is_exclusive ex.rmws)
                                                                   ex.added)
                                                                grp)] in
        if not (E.EventRel.is_empty
                  (E.EventRel.restrict_rel
                     (fun x y -> not (E.EventSet.mem y ex.revisit)
                                 || E.EventSet.mem x ex.revisit)
                     t1))
        then
          false
        else
          true

      let check_cns ex cs =
        let ncs = (List.append cs (make_cnstrnts
                                     {ex with rf = added ex.safe ex.rf})) in
        match M.S.M.VC.solve ncs with
        | M.S.M.VC.NoSolns -> false
        | _ -> true



    (* return events in g to their respective threads in toadd *)
    let return_events toadd g =
      let insert_event toadd e =
        List.map (fun (x, y) ->
            match E.proc_of e with
            | Some p when x = p -> (x, E.EventSet.add e y)
            | _ -> (x, y)) toadd in
      E.EventSet.fold (fun x y -> insert_event y x) g toadd

    (* modify ex.rf such that all the reads in r read from w *)
    let set_rf ex w r =
      let nr = E.EventSet.inter ex.added r in
      let nrf0 = E.EventRel.restrict_codomain
                   (fun x -> not (E.EventSet.mem x nr))
                   ex.rf in
      let nrf = E.EventRel.union
                  (E.EventRel.cartesian (E.EventSet.of_list [w]) nr)
                  nrf0 in
      let out = {ex with rf = nrf} in
      out

    (* returns the event a such that (e,a) in mo but there exists no b such that (e,b) and (e,a) in mo *)
    let succ mo e =
      let s0 = E.EventRel.restrict_domain
                 (fun x -> x = e)
                 mo in
      let s1 = E.EventRel.restrict_domains
                 (fun x -> E.EventSet.mem x (E.EventRel.codomain s0))
                 (fun x -> E.EventSet.mem x (E.EventRel.codomain s0))
                 mo in
      let out = E.EventRel.roots s1 in
      match E.EventSet.cardinal out with
      | 0 | 1 -> out
      | _ -> assert false

    (* put wp after w in mo *)
    let insert_mo ex wp w =
      let m = E.EventRel.restrict_rel
                (fun x y -> x != w && y != w)
                ex.mo in

      let mo0 = E.EventRel.restrict_codomain
                  (fun x -> x = wp)
                  m in
      let mo1 = E.EventRel.domain mo0 in
      let mo2 = E.EventSet.add wp mo1 in
      let mo3 = E.EventRel.cartesian mo2 (E.EventSet.of_list [w]) in

      let mo4 = E.EventRel.restrict_domain
                  (fun x -> x = wp)
                  m in
      let mo5 = E.EventRel.codomain mo4 in
      let mo6 = E.EventRel.cartesian (E.EventSet.of_list [w]) mo5 in

      let mo7 = E.EventRel.union mo3 mo6 in

      let out = {ex with mo = E.EventRel.union ex.mo mo7} in
      out

      let extend_safe ex w =
        let sbr = sbrf ex in
        let r0 = E.EventRel.restrict_domains
               (fun x -> E.EventSet.mem x ex.revisit)
               (fun x -> x = w)
               sbr in
        if E.EventRel.is_empty r0
        then (true, {ex with safe = E.EventSet.add w ex.safe})
        else (false, ex)



      (* the algorithm itself *)


      let rec visit_write ex cs w kont res =
        let _ = assert (E.EventSet.is_empty (E.EventSet.diff ex.safe ex.added)) in
        (*      let _ = printf "visiting write %a\n" debug_event w in*)
        (* if w is part of a rmw that reads from a write wp then w goes after wp in ex.mo *)
        let r0 = E.EventRel.sequence ex.rf (added ex.added ex.rmws) in
        let r1 = E.EventRel.restrict_codomain
                   (E.event_equal w)
                   r0 in

        if not (E.EventRel.is_empty r1) then
          (*        let _ = printf "case 1\n" in*)
          let wp = E.EventSet.choose (E.EventRel.domain r1) in
          revisit_reads (insert_mo ex wp w) cs w kont res

        else
          (*       let _ = printf "case 2\n" in*)
          (* w0 = ex.mo maximal write to the same location as w  *)
          let m1 = E.EventRel.restrict_domain
                     (E.same_location w)
                     ex.mo in
          let w0 = try
              E.EventSet.choose (E.EventRel.leaves m1)
            with Not_found -> E.EventSet.choose (E.EventSet.filter (fun x -> E.is_mem_store_init x && E.same_location x w) ex.added) in
          (*        let _ = printf "rvr\n" in*)
          let nex = insert_mo ex w0 w in
          let (b, nex0) = (extend_safe nex w) in
          let rvr = if  b && not (check_cns nex0 cs)
                    then (*let _ = printf "prune3\n" in*) res
                    else (revisit_reads
                            nex0 cs
                            w kont res)
          in

          (* revisit every execution extension *)
          let hb0 = hb ex in
          let s0 = E.EventRel.union ex.mo (E.EventRel.sequence ex.mo ex.rf) in
          let s1 = E.EventRel.sequence s0 hb0 in
          let s3 = E.EventRel.sequence s1 (E.EventRel.singleton (w,w)) in
          let s4 = E.EventRel.sequence ex.rf (E.EventRel.set_to_rln (E.EventSet.inter (E.EventSet.filter (E.same_location w) ex.added) (E.EventRel.domain ex.rmws))) in
          let s5 = E.EventRel.union s3 s4 in
          let s6 = E.EventRel.domain s5 in
          let s7 = E.EventSet.filter
                     (fun x -> E.is_mem_store x
                               && E.same_location w x)
                     ex.added in
          let s = E.EventSet.filter
                    (fun x ->
                      x != w0
                      && not (E.EventSet.mem x s6)
                      && w != x)
                    s7 in

          let sbr = sbrf ex in

          E.EventSet.fold
            (fun wp res1 ->
              (*            let _ = printf "fold\n" in*)
              let sb0 = E.EventRel.set_to_rln (succ ex.mo wp) in
              let sb1 = E.EventRel.add (w, w) sb0 in
              let sb2 = E.EventRel.sequence sbr sb1 in
              let sb3 = E.EventRel.domain sb2 in
              let nrevisit = E.EventSet.filter
                               (fun x -> not (E.EventSet.mem x sb3))
                               ex.revisit in
              let nex = insert_mo {ex with revisit = nrevisit} wp w in
              let (b, nex0) = (extend_safe nex w) in
              if b && not (check_cns nex0 cs)
              then (*let _ = printf "prune3\n" in*) res1
              else (revisit_reads
                      nex0 cs
                      w kont res1)) s rvr

      and revisit_reads ex cs w kont res =
        let _ = assert (E.EventSet.is_empty (E.EventSet.diff ex.safe ex.added)) in
        (*      let _ = printf "revisiting reads %a for %a\n" debug_event_set ex.revisit debug_event w in*)
        let mpo = added ex.added ex.po in

        let r0 = E.EventSet.filter
                   (E.same_location w)
                   ex.revisit in

        let sbr = sbrf ex in
        let hbf = hb ex in

        (* r2 = r0 \ domain(sbr;[w]) otherwise we would have [w];rf;sbr[w] cycles *)
        let r1 = E.EventRel.sequence sbr (E.EventRel.of_list [(w,w)]) in
        let r2 = E.EventSet.diff r0 (E.EventRel.domain r1) in

        (* r6 = r2 \ codomain ([w];mo;rf?;hb?;sb) otherwise we could have [w];mo;rf?;hb?;sb;rf;[w] cycles *)
        let r3 = E.EventRel.union ex.mo (E.EventRel.sequence ex.mo ex.rf) in
        let r4 = E.EventRel.union r3 (E.EventRel.sequence r3 hbf) in
        let r5 = E.EventRel.sequence r4 mpo in
        let r5b = E.EventRel.sequence (E.EventRel.singleton (w,w)) r5 in
        let r6 = E.EventSet.diff r2 (E.EventRel.codomain r5b) in

        (* if w is part of a rmw that reads from the same write as another rmw b, we must revisit the read of b or atomicity will not be maintained *)
        let km0 = E.EventRel.sequences [E.EventRel.inverse ex.rf; ex.rf; added ex.added ex.rmws] in
        let km1 = E.EventRel.restrict_domains
                    (fun x -> E.is_mem_load x
                              && is_exclusive ex.rmws x)
                    (fun x -> x = w)
                    km0 in
        let km2 = (E.EventRel.diff km1 (added ex.added ex.rmws)) in
        let kmust = E.EventSet.inter ex.added (E.EventRel.domain km2) in

        if (not (E.EventRel.is_empty
                   (E.EventRel.sequences
                      [(E.EventRel.set_to_rln kmust);
                       sbr;
                       (E.EventRel.set_to_rln (E.EventSet.singleton w))])))
        then res
        else

          let r7 = E.EventRel.restrict_domain
                     (fun x -> E.EventSet.mem x kmust)
                     sbr in
          let r8 = E.EventSet.union kmust (E.EventRel.codomain r7) in
          let r = E.EventSet.diff r6 r8 in

          (* we compute any subset of r that does not reach itself within sbr and revisit it *)
          let kl0 = E.EventSet.fold
                      (fun e l ->
                        List.append
                          l
                          (List.map
                             (fun x -> E.EventSet.add e x)
                             l))
                      (E.EventSet.inter ex.added r) [E.EventSet.empty] in
          let kl = List.filter
                     (fun x -> not (E.EventRel.exists
                                      (fun (y, z) -> E.EventSet.mem y x
                                                     && E.EventSet.mem z x)
                                      sbr))
                     kl0 in
          List.fold_left
            (fun res1 k1 ->
              let k = if E.EventSet.is_empty kmust then k1 else
                        let k00 = E.EventRel.set_to_rln k1 in
                        let k01 = E.EventRel.sequence k00 sbr in
                        let k02 = E.EventRel.codomain k01 in
                        let k03 = E.EventSet.diff kmust k02 in
                        E.EventSet.union k1 k03 in

              (* we remove sbrf successors of k and make the reads in k read from w *)
              let g0 = E.EventRel.set_to_rln k in
              let g1 = E.EventRel.sequence g0 sbr in
              let g = E.EventRel.codomain g1 in

              let ntoadd = return_events ex.toadd g in
              let nadded = E.EventSet.diff ex.added g in
              let nmo = added nadded ex.mo in
              let nrf = added nadded ex.rf in
              if not (E.EventSet.mem w nadded)
              then let _ = assert false in res1
              else
                let nex = set_rf {ex with added = nadded; toadd = ntoadd; mo = nmo; rf = nrf; revisit = E.EventSet.inter nadded ex.revisit} w k in
                let nsbr = sbrf nex in
                let nrevisit0 = E.EventRel.set_to_rln k1 in
                let nrevisit1 = E.EventRel.union nrevisit0 (E.EventRel.sequence nsbr nrevisit0) in
                let nrevisit2 = E.EventRel.domain nrevisit1 in
                let nrevisit =E.EventSet.filter
                                (fun x -> not (E.EventSet.mem x nrevisit2))
                                nex.revisit in
                let nex0 = {nex with revisit = nrevisit;
                                     debug_rels = List.append nex.debug_rels [("kmust", E.EventRel.set_to_rln kmust)];
                                     safe = E.EventSet.union ex.safe nrevisit2} in
                if not (E.EventSet.is_empty nrevisit2) && not (check_cns nex0 cs) then (*let _ = printf "prune2\n" in*) res1 else
                  visit nex0 cs kont res1)
            res kl

      and visit_read (ex : exec) cs r kont res =
        let _ = assert (E.EventSet.is_empty (E.EventSet.diff ex.safe ex.added)) in
        (*      let _ = printf "visiting read %a\n" debug_event r in*)
        let w0 = E.EventSet.filter (fun x -> E.is_mem_store x && E.same_location r x) ex.added in
        let h = hb ex in

        (* we remove writes that are mo;rf?;hb before r to avoid [r];rf^-1;mo;hb;[r] = [r];eco;hb[r] cycles (coherence) *)
        let c0 = E.EventRel.sequences [ex.mo; ex.rf; h] in
        let c1 = E.EventRel.sequence ex.mo h in
        let c2 = E.EventRel.union c0 c1 in
        let c3 = E.EventRel.sequence c2 (E.EventRel.set_to_rln (E.EventSet.singleton r)) in

        let w1 = E.EventSet.filter (fun x -> not (E.EventSet.mem x (E.EventRel.domain c3))) w0 in


        let sbr = sbrf ex in
        let w2 = if is_exclusive ex.rmws r
                 then
                   let a1 = E.EventSet.filter (E.same_location r) ex.added in

                   let a20 = E.EventRel.set_to_rln
                               (E.EventSet.diff
                                  (E.EventRel.domain
                                     (added (E.EventSet.filter (E.same_location r) ex.added) ex.rmws))
                                  ex.revisit) in
                   let a21 = E.EventRel.set_to_rln
                               (E.EventSet.inter
                                  ex.revisit
                                  (E.EventRel.domain (added (E.EventSet.filter (E.same_location r) ex.added) ex.rmws))) in
                   let a22 = E.EventRel.sequences [a21; sbr; E.EventRel.set_to_rln (E.EventSet.singleton r)] in
                   let a23 = E.EventRel.union a22 a20 in
                   let a24 = E.EventRel.sequence ex.rf a23 in
                   let a2 = E.EventRel.domain a24 in

                   E.EventSet.inter ex.added (E.EventSet.filter (fun x -> not (E.EventSet.mem x a1 && E.EventSet.mem x a2)) w1)
                 else w1 in
        let rec aux w0 =
          let r0 = E.EventRel.set_to_rln w0 in
          let r1 = E.EventRel.sequences [r0;sbr;added ex.added ex.rmws] in
          let r2 = E.EventRel.codomain (E.EventRel.restrict_codomain (E.same_location r) r1) in
          if E.EventSet.is_empty r2
          then w0
          else aux r2 in
        let w3 = E.EventRel.restrict_codomain (fun x -> x = r) sbr in
        let w4 = E.EventSet.inter w2 (E.EventRel.domain w3) in
        let _ = assert (not (E.EventSet.exists (fun x -> not (E.same_location x r)) w4)) in
        let w = aux w4 in

        let wx = try E.EventSet.choose w with Not_found ->
                   E.EventSet.choose (aux w2) in
        let ex0 = (set_rf {ex with revisit = E.EventSet.add r ex.revisit}
                     wx
                     (E.EventSet.of_list [r])) in
        let res0 =  if not (check_cns ex0 cs)
                    then (*let _ = printf "prune0\n" in*) res
                    else visit ex0 cs kont res in

        E.EventSet.fold
          (fun x res1 ->
            let nex = (set_rf ex x (E.EventSet.of_list [r])) in
            let nsbr = sbrf nex in
            let nr0 = E.EventRel.sequence nsbr (E.EventRel.of_list [(r,r);(x,x)]) in
            let nr1 = E.EventRel.domain nr0 in
            let nrevisit =
              E.EventSet.diff nex.revisit nr1 in
            let nex0 = {nex with revisit = nrevisit; safe = E.EventSet.union3 (E.EventSet.of_list [x;r]) nr1 ex.safe} in
            if not (check_cns nex0 cs) then res1 else visit nex0 cs kont res1)
          (E.EventSet.remove wx w2) res0

      and visit (ex : exec) cs kont res =
        let _ = assert (E.EventSet.is_empty (E.EventSet.diff ex.safe ex.added)) in
        let pending = pending ex in
        let a0 = extract_event ex.toadd ex.po ex.revisit pending in
        match a0 with
        | (Some e, ntoadd) -> begin
            let nadded = E.EventSet.add e ex.added in
            let newex = {ex with toadd = ntoadd; added = nadded;} in
            try match e with
                | x when E.is_mem_store x -> visit_write newex cs e kont res
                | x when E.is_mem_load x -> visit_read newex cs e kont res
                | _ -> visit newex cs kont res
            with Not_found -> visit newex cs kont res
          end
        | None, _ ->
           let h = hb ex in
           let r0 = E.EventRel.cartesian ex.added ex.added in
           let r1 = E.EventRel.restrict_rel
                      (fun x y -> E.same_location x y
                                  && (E.is_mem_store x
                                      || E.is_mem_store y)
                                  && not (E.is_mem_store_init x)
                                  && not (E.is_mem_store_init y))
                      r0 in
           let r2 = E.EventRel.restrict_rel
                      (fun x y -> not (E.same_proc x y))
                      r1 in
           let r3 = E.EventRel.diff r2 (E.EventRel.union h (E.EventRel.inverse h)) in

           let realevents = E.EventSet.filter
                              (fun x -> E.is_mem x || fence x)
                              ex.added in

           let dr = E.EventRel.restrict_rel
                      (fun x y -> not (atomic x.E.action
                                       && atomic y.E.action))
                      r3 in
           let sbl = E.EventRel.restrict_rel
                       (fun x y -> not (E.same_location x y)
                                   && E.EventSet.mem x realevents
                                   && E.EventSet.mem y realevents)
                       ex.po in
           let hbl = E.EventRel.restrict_rel
                       (fun x y -> E.same_location x y
                                   && E.EventSet.mem x realevents
                                   && E.EventSet.mem y realevents)
                       h in

           let scb0 = E.EventRel.sequences [sbl; h; sbl] in
           let scb1 = E.EventRel.sequence (E.EventRel.inverse ex.rf) ex.mo in
           let scb = E.EventRel.unions [ex.po; scb0; hbl; ex.mo; scb1] in

           let scfence = (fun x -> fence x && sc x.E.action) in
           let fences = E.EventSet.filter scfence ex.added in
           let fencesrel = E.EventRel.set_to_rln fences in
           let sce = E.EventSet.filter (fun x -> sc x.E.action) ex.added in
           let psc0 = E.EventRel.union fencesrel (E.EventRel.sequence fencesrel h) in
           let psc1 = E.EventRel.union psc0 (E.EventRel.set_to_rln sce) in
           let psc2 = E.EventRel.sequence psc1 scb in
           let psc3 = E.EventRel.sequence h fencesrel in
           let psc4 = E.EventRel.union3 psc3 fencesrel (E.EventRel.set_to_rln sce) in
           let psc5 = E.EventRel.sequence psc2 psc4 in

           let eco0 = E.EventRel.union3 ex.mo
                        ex.rf
                        (E.EventRel.sequence (E.EventRel.inverse ex.rf) ex.mo) in
           let eco = E.EventRel.transitive_closure eco0 in

           let psc10 = E.EventRel.union (E.EventRel.sequences [h; eco; h]) h in
           let psc11 = E.EventRel.restrict_domains
                         scfence
                         scfence
                         psc10 in
           let psc = E.EventRel.union psc5 psc11 in

           if
             not (StringSet.mem "SC" M.skipchecks) && not (E.EventRel.is_acyclic psc)
           then
             res
           else
(*             let _ = printf "done\n" in*)
             let rs10 = E.EventRel.set_to_rln (E.EventSet.filter
                                                 (fun x -> E.is_mem_store x
                                                           && (rlx x.E.action
                                                               || rel x.E.action
                                                               || acq_rel x.E.action
                                                               || acq x.E.action
                                                               || sc x.E.action))
                                                 ex.added) in


             let cyc = match E.EventRel.get_cycle psc with
               | None -> E.EventRel.empty
               | Some x -> E.EventRel.singleton (List.hd x, List.hd x) in


             let nex = {ex with debug_rels = List.append ex.debug_rels
                                               [("psc", psc);
                                                ("eco",eco);
                                                ("scb", scb);
                                                ("pscb", psc5);
                                                ("pscf", psc11);
                                                ("rseq", rseq ex);
                                                ("rs10", rs10);
                                                ("cycle", cyc);
                                                ("sbl", sbl);
                                                ("hbl", hbl);
                                                ("sce", E.EventRel.set_to_rln sce);
                                                ("psc2", psc2);
                                                ("psc4", psc4);
                                                ("fencesrel", fencesrel);
                                                ("psc0", psc0);
                                                ("psc1", psc1);
                                                ("scb0", scb0);
                                                ("scb1", scb1)
                       ]} in
             if E.EventRel.is_empty dr
             then let out = (clean_exec {nex with psc = psc}) in
                  kont out res
             else let out = (clean_exec {nex with flags = Flag.Set.add Flag.Undef ex.flags}) in
                  kont out res

    let finals ex =
      let out = E.EventSet.elements
                  (E.EventSet.filter
                     (fun x -> is_final_write x ex)
                     ex.added) in
      out

    let replace_events ex es =
      let find ev = E.EventSet.find
                      (fun x -> E.event_equal ev x)
                      es in
      let nadded = E.EventSet.map
                     (fun x -> find x)
                     ex.added in
      let nrf = E.EventRel.map
                  (fun (x, y) -> (find x, find y))
                  ex.rf in
      let nmo = E.EventRel.map
                  (fun (x, y) -> (find x, find y))
                  ex.mo in
      let npo = E.EventRel.map
                  (fun (x, y) -> (find x, find y))
                  ex.po in
      let ndebugrels = List.map
                         (fun (str, rel) -> (str, E.EventRel.map
                                              (fun (x, y) -> (find x, find y)) rel))
                         ex.debug_rels in
      {ex with added = nadded; rf = nrf; mo = nmo; po = npo; debug_rels = ndebugrels}


    let mykont test model_kont es cs = (fun e res ->
        let ncs = (List.append cs (make_cnstrnts e)) in
        M.solve_mem test es e.rfm ncs
          (fun es0 rfm0 cs0 res0 ->
            match cs0 with
            | [] ->
               if S.A.reject_mixed then M.check_sizes test es0;
               if not (S.O.optace) || M.check_rfmap es0 rfm0
               then
                 let ne = replace_events e es0.M.S.E.events in
                 let pp_relns =
                   lazy begin
                       ("mo", ne.mo)::
                       ("hb", E.EventRel.remove_transitive_edges (hb ne))::
                       ("rf", ne.rf)::
                       ("po", ne.po)::
                       ("revisit", E.EventSet.fold (fun ev rels -> E.EventRel.add (ev, ev) rels) ne.revisit E.EventRel.empty)::
                       ("rmw", ne.rmws)::
                       ("rs", rseq ne)::
                       ("sw", sw ne)::
                       ne.debug_rels
                     end in
                 let rfm = List.fold_left
                             (fun k w ->
                               M.S.RFMap.add (M.S.Final (M.get_loc w)) (M.S.Store w) k) rfm0 (finals ne) in
                 let fsc = M.compute_final_state test rfm es.E.events in
                 let conc =
                   {
                     S.str = es0;
                     rfmap = rfm;
                     fs = fsc;

                     po = E.EventRel.empty;
                     pos = E.EventRel.empty;
                     pco = E.EventRel.empty;

                     store_load_vbf = E.EventRel.empty;
                     init_load_vbf = E.EventRel.empty;
                     last_store_vbf = E.EventRel.empty;
                     atomic_load_store = E.EventRel.empty
                   } in
                 model_kont conc conc.S.fs
                   (lazy StringMap.empty,pp_relns) e.flags res0
               else begin
                   res0
                 end
            | _ -> M.when_unsolved test es0 rfm0 cs0 (fun c -> c) res0) res)

    let real e = fence e || E.is_mem e

    let check_rfms test rfms _kfail _kont model_kont res =
      let (_, cs0, es0) = rfms in
      let (es, rfm, cs) = solve test es0 cs0 in
      let rmws = M.make_atomic_load_store es in
      let evts = E.EventSet.filter real es.E.events in
      let inits = E.EventSet.filter E.is_mem_store_init evts in
      let po = U.po_iico es in
      let iico = U.iico es in
      let procs = List.map
                    (fun x ->
                      let e = E.EventSet.filter
                                (fun y -> match E.proc_of y with
                                          | Some p when p = x -> true
                                          | _ -> false)
                                evts in
                      x, e, E.EventRel.restrict_rel (fun x y -> E.EventSet.mem x e && E.EventSet.mem y e) po)
                    es.E.procs in
      let toadd = List.map (fun (x, y, _) -> x, y) procs in
      let ex_init = {
          toadd = toadd;
          added = inits;
          po = (E.EventRel.union rmws po);
          iico = iico;
          mo = E.EventRel.empty;
          rf = E.EventRel.empty;
          revisit = E.EventSet.empty;
          safe = inits;
          exvals = (fun _ -> false);
          rmws = rmws;
          rfm = rfm;
          flags = Flag.Set.empty;
          psc = E.EventRel.empty;
          debug_rels = []
        } in
      (visit ex_init cs (mykont test model_kont es cs) res)

    let check_event_structure test (rfms : (_ * S.M.VC.cnstrnts * S.event_structure) list) kfail kont model_kont  (res : 'a) =
      List.fold_left (fun re rf -> check_rfms test rf kfail kont model_kont re) res rfms
  end
