 open Printf

module type Cfg = sig
  include Model.Config
end

module Make (*O:Model.Config*)(*S:Sem.Semantics*)(*SU:SlUtils.S*)(M:Mem.S)
  =
  struct
    module S = M.S
    module E = S.E
    module A = S.A
    module U = MemUtils.Make(S)
  (*  module MU = ModelUtils.Make(O)(S)*)
  (*  module SU = SlUtils.Make(O)(S) *)

    let debug_proc chan p = fprintf chan "%i" p

    let debug_event chan e = fprintf chan "%s:%s " (E.pp_eiid e) (S.E.Act.pp_action e.action)

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

(*    let print_event e = *)

 (*   let is_that_fence b e = match E.barrier_of e with
      | Some a -> a = b.S.barrier
      | None -> false

    let make_procrels conc =
      let is_isync = match S.isync with
        | None -> fun _ -> false
        | Some b -> is_that_fence b in
      MU.make_procrels is_isync conc*)

    let next_evt evts = try
        let e = E.EventSet.min_elt evts in
        let evts = E.EventSet.remove e evts in
        Some (e, evts)
        with Not_found -> None

    let rec my_iter f evts =
      let rec aux = function
        | a when E.EventSet.cardinal a = 0 -> ()
        | a -> match next_evt a with
                    | Some c -> let e, b = c in
                                let _ = f e in
                                aux b
                    | None -> () in
      aux evts

    type exec = {
    (*    procs : (int * E.EventSet.t) list; *)
        events : E.EventSet.t;
        po : E.EventRel.t;
        mo : E.EventRel.t;
        rf : E.EventRel.t;
        toadd : (int * E.EventSet.t) list;
        added : E.EventSet.t;
        revisit : E.EventSet.t;
        exvals : E.event -> bool;
      }

    let debug_exec chan ex =
  (*    let _ = fprintf chan "execution\npo : " in
      let _ = debug_rel chan ex.po in*)
      let _ = fprintf chan "\nmo : " in
      let _ = debug_rel chan ex.mo in
      let _ = fprintf chan "\nrf : " in
      let _ = debug_rel chan ex.rf in
      let _ = fprintf chan "\nadded : " in
      let _ = debug_event_set chan ex.added in
      fprintf chan "\n"



    let val_read r rf = E.EventRel.preds rf r

    let solve_step test es cs =
      match M.solve_regs test es cs with
      | None -> (es, cs)
      | Some (es, rfm, cs) -> (es, cs)

    let rval a =
      match (E.Act.read_of a, E.Act.written_of a) with
      | (Some v, _) | (_, Some v) -> Some v
      | _ -> None

    let is_annot = List.assoc "annot" E.Act.arch_sets

    let remove_events e =
      E.EventSet.filter
        (fun x ->
          match E.Act.value_of x.action with
          | Some Val (Symbolic _) when is_annot x.action (* not (E.is_mem_store_init x*) -> false
          | _ -> true)
      e

    let nextp exec po revisit pending =
      let rec aux e p = begin
          match e with
          | [] -> None
          | (_, e) :: tl -> match E.EventRel.roots (E.EventRel.restrict_rel (fun x y -> E.EventSet.mem x e && E.EventSet.mem y e) p) with
                               | x when E.EventSet.is_empty x && E.EventSet.is_empty e -> aux tl p
                               | x when E.EventSet.is_empty x -> Some (E.EventSet.choose e)
                               | x -> Some (E.EventSet.choose x) end in
      match E.EventSet.cardinal pending with
      | 2 -> (Some (E.EventSet.find (fun x -> not (E.EventSet.mem x revisit)) pending))
      | 1 -> (Some (E.EventSet.choose pending))
      | 0 -> aux exec po
      | _ -> assert false

    let extract_event exec po revisit pending = match nextp exec po revisit pending with
      | None -> (None, exec)
      | Some e -> (Some e, List.map
                (fun (x, y) ->
                  (x,
                   E.EventSet.remove e y)) exec)

    let aux = fun x ->
      try List.assoc x S.E.Act.arch_sets
      with Not_found -> fun x -> true

    let rmw = aux "RMW"
    let rlx = aux "RLX"
    let acq = aux "ACQ"
    let rel = aux "REL"
    let acq_rel = aux "AQU_REL"
    let sc = aux "SC"
    let na = aux "NA"
    let f = E.is_barrier
    let a = aux "A"

    let hb added mo rf po =
      let rseq0 = E.EventRel.restrict_codomain (fun x -> rmw x.action) rf in
      let rseq1 = E.EventRel.transitive_closure rseq0 in
      let rseq2 = E.EventRel.restrict_rel (fun x y -> E.is_mem_store y && E.is_mem_store x && not (na x.action) && E.same_location x y) po in
      let rseq3 = E.EventRel.sequence rseq2 rseq1 in
      let rseq4 = E.EventRel.restrict_domain (fun x -> E.is_mem_store x && not (na x.action)) rseq1 in
      let rseq = E.EventRel.union3 rseq2 rseq3 rseq4 in

      let sw0 = E.EventRel.sequence rseq rf in
      let sw1 = E.EventRel.restrict_codomain (fun x -> rlx x.action || rel x.action || acq x.action || sc x.action) sw0 in
      let sw2 = E.EventRel.restrict_domain (fun x -> f x) po in
      let sw3 = E.EventRel.sequence sw2 sw1 in
      let sw4 = E.EventRel.union sw1 sw3 in
      let sw5 = E.EventRel.restrict_codomain (fun x -> f x) po in
      let sw6 = E.EventRel.sequence sw4 sw5 in
      let sw7 = E.EventRel.union sw4 sw6 in
      let sw = E.EventRel.restrict_domains (fun x -> rel x.action || sc x.action) (fun x -> acq x.action || sc x.action) sw7 in

      let hb0 = E.EventRel.union po sw in
      E.EventRel.transitive_closure hb0

    let make_exvals events =
      let rmws = E.EventSet.filter (fun x -> rmw x.action) events in
      (fun x -> E.EventSet.exists (fun y -> E.same_location x y) rmws)

    let set_rf ex w r =
      let nrf0 = E.EventRel.restrict_codomain (fun x -> x != r) ex.rf in
      let nrf = E.EventRel.add (w, r) nrf0 in
      {ex with rf = nrf}



    let rec visit_write (ex : exec) w =
      let _ = printf "\nvisiting write : " in
      let _ = debug_event stdout w in
      visit ex

    and visit_read (ex : exec) r =
      let _ = printf "\nvisiting read : " in
      let _ = debug_event stdout r in

      let w = E.EventSet.filter (fun x -> E.is_mem_store x && E.same_location r x) ex.events in
      let h = hb ex.added ex.mo ex.rf ex.po in

      let c0 = E.EventRel.sequences [ex.mo; ex.rf; h] in
      let c1 = E.EventRel.sequence ex.mo h in
      let c2 = E.EventRel.union c0 c1 in
      let c3 = E.EventRel.restrict_codomain f c2 in

      let w = E.EventSet.filter (fun x -> not (E.EventSet.mem x (E.EventRel.domain c3))) w in

      let a1 = E.EventSet.filter (fun x -> E.is_mem_store x && rmw x.action) ex.events in

      let a20 = E.EventRel.union ex.po ex.rf in
      let a21 = E.EventRel.transitive_closure a20 in (* sbrf *)
      let a22 = E.EventRel.restrict_rel (fun x y -> E.is_mem_store x && rmw x.action && E.EventSet.mem x ex.revisit && x = r) a21 in
      let a23 = E.EventRel.restrict_codomain (fun x -> rmw x.action && E.is_mem_store x && not (E.EventSet.mem x ex.revisit)) ex.rf in
      let a2 = E.EventRel.domain (E.EventRel.union a22 a23) in

      let w = E.EventSet.filter (fun x -> not (E.EventSet.mem x (E.EventSet.union a1 a2))) w in
      let _ = debug_event_set stdout w in

      let dsbr = E.EventRel.domain (E.EventRel.restrict_codomain (fun x -> x = r) a21) in

      let w0 = E.EventSet.choose (E.EventSet.inter w dsbr) in

      let rec aux wx =
        let t0 = E.EventRel.restrict_rel (fun x y -> x = wx && rmw y.action) ex.rf in
        if ex.exvals wx && not (E.EventRel.is_empty t0)
        then aux (E.EventSet.choose (E.EventRel.codomain t0))
        else wx in

      let wx = aux w0 in

      let _ = visit (set_rf {ex with revisit = E.EventSet.add r ex.revisit} wx r) in
      E.EventSet.iter (fun x ->
          let nrevisit = E.EventSet.diff ex.revisit
                           (E.EventRel.domain
                              (E.EventRel.restrict_codomain
                                 (fun y -> y = r || y = x) a21)) in
                            visit (set_rf {ex with revisit = nrevisit}
                                     x r))
      (E.EventSet.remove wx w)

    and visit (ex : exec) =
      let pending = E.EventSet.empty in
      let a = extract_event ex.toadd ex.po ex.revisit pending in
      match a with
      | (Some e, ntoadd) -> begin
          let nadded = E.EventSet.add e ex.added in
          let newex = {ex with toadd = ntoadd; added = nadded} in
          match e with
          | x when E.is_mem_store x -> visit_write newex e
          | x when E.is_mem_load x -> visit_read newex e
          | _ -> visit newex
        end
      | None, _ -> debug_exec stdout ex



    let check_event_structure test (rfms : (_ * S.M.VC.cnstrnts * S.event_structure) list) =
      let s1 = List.hd rfms in
      match s1
      with (_, cs, es) ->
        let _ = print_string "\ninitial \t: " in
        let _ = debug_cnstrnts stdout cs in
        let _ = debug_event_set stdout es.events in
        let (es, cs) = solve_step test es cs in
        let _ = print_string "\nsolved \t: " in
        let _ = debug_event_set stdout es.events in
        let _ = print_string "\ncleaned \t: " in
        let esc0 = remove_events es.events in
        let _ = debug_event_set stdout esc0 in
        let inits = E.EventSet.filter E.is_mem_store_init esc0 in
        let _ = print_string "\ninits \t: " in
        let _ = debug_event_set stdout inits in
        let annot = E.EventSet.filter (fun x -> is_annot x.action && not (E.is_mem_store_init x)) esc0 in
        let _ = print_string "\nannot \t: " in
        let _ = debug_event_set stdout annot in
        let esc = E.EventSet.filter (fun x -> not (is_annot x.action)) esc0 in
        let _ = print_string "\nesc \t: " in
        let _ = debug_event_set stdout esc in
        let po = E.EventRel.restrict_domains (fun x -> E.EventSet.mem x esc) (fun x -> E.EventSet.mem x esc) (U.po_strict es) in
        let po0 = E.EventRel.cartesian inits esc in
        let pof = E.EventRel.union po po0 in
 (* E.EventSet.fold
               (fun e po ->
                 E.EventSet.fold
                   (fun a p ->
                     if E.po_strict e a
                     then E.EventRel.add (e, a) p
                     else p)
                   esc po)
               (*E.EventSet.union esc*) inits E.EventRel.empty*)
        let _ = print_string "\npo \t: " in
        let _ = debug_rel stdout pof in
        let procs = List.map
                      (fun x ->
                        let e = E.EventSet.filter
                              (fun y ->
                                match E.proc_of y with
                                | Some p when p = x -> true
                                | _ -> false) esc in
                        (x, e, E.EventRel.restrict_domains (fun x -> E.EventSet.mem x e) (fun x -> E.EventSet.mem x e) pof))
                  es.procs in
        let _ = print_string "\n\nprocs\n" in
        let _ = List.iter
              (fun (x, y, _) ->
                let _ = printf "proc %d \t:" x in
                debug_event_set stdout y) procs in
        let _ = print_string "\nextracted \t: " in
        let rec aux0 exec revisit pending n = match extract_event exec pof revisit pending with
          | (Some e, nexec) ->
             let _ = printf "%d : " n in
             let _ = debug_event stdout e in
             aux0 nexec revisit pending (n + 1)
          | (None, _) -> () in
        let procsb = (List.map (fun (x, y, z) -> (x, y)) procs) in
        let _ = aux0 procsb E.EventSet.empty E.EventSet.empty 0 in
        let _ = print_string "\n" in
        let ex = {
            events = E.EventSet.union esc inits;
         (*   procs = procsb;*)
            toadd = procsb;
            added = inits;
            mo = E.EventRel.empty;
            rf = E.EventRel.empty;
            po = pof;
            revisit = E.EventSet.empty;
            exvals = make_exvals (E.EventSet.union esc inits);
          } in
        let _ = visit ex in
        ()

 (*       let proc_ws ws0 res =
          let ws = E.EventRel.transitive_closure ws0 in
          let events = conc.S.str.events in
          let inits = E.EventSet.filter E.is_mem_store_init events in
          let notinits = E.EventSet.diff events inits in
          let _ = print_string "\ninits : " in
          (*let _ = E.EventSet.iter (fun x -> debug_event stdout x) inits in *)
          let _ = my_iter (fun x -> debug_event stdout x) inits in
          let _ = print_string "\nnotinits : " in
          let _ = my_iter (fun x -> debug_event stdout x) notinits in
          (*let _ = E.EventSet.iter (fun x -> debug_event stdout x) notinits in *)
          res in
        U.apply_process_co test conc proc_ws res*)

(*        let aux = fun x ->
          try List.assoc x S.E.Act.arch_sets with Not_found -> fun x -> true in

        let *)

  (*      let pending evts rmw =
          let er = E.EventRel.restrict_domains (fun x -> E.EventSet.mem x evts) (fun x -> not (E.EventSet.mem x evts)) rmw in
          E.EventRel.fold  (fun x y -> E.EventRel.Elts.) er in

        let visit treated rf mo todo revisit =
          let pending = pending treated conc.S.atomic_load_store in
          let nextp g t =
            match E.EventSet.cardinal pending with
            | 1 -> Some (E.EventSet.choose pending)
            | 2 -> Some (E.EventSet.choose (E.EventSet.filter (fun x -> not (E.EventSet.mem x t)) pending))
            | _ -> match (next_evt g) with
                   | None -> None
                   | Some (a, b) -> Some a in
          let a = nextp todo revisit in
          match a with
          | Some x -> begin
              let treated = E.EventSet.add x treated in
              let todo = E.EventSet.remove x todo in
              match x with
              | a when E.is_mem_store a -> visit_read treated rf mo revisit a
              | a when E.is_mem_load a ->
              | _ -> assert false
            end
          | None -> ()*)

 (*   let check_event_structure test (rfms : (_ * S.M.VC.cnstrnts * S.E.event_structure) list) kfail kont res =
      (* let evtstr = List.map (fun (_, _, x) -> x) rfms in *)
      let _ = List.iter
            (fun (_, (x : S.M.VC.cnstrnts), (y : E.event_structure)) ->
               let _ =  E.EventSet.iter
                         (fun z -> debug_event stdout z)
                         y.events in
              let _ = debug_cnstrnts stdout x in printf "\n") rfms in
      let simple = List.map (fun (_, _, (x : E.event_structure))
                               -> E.EventSet.map (fun x -> *)

  end
