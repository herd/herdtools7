open Printf

module type Cfg = sig
  include Model.Config
end

module Make (*O:Model.Config*)(S:Sem.Semantics)(SU:SlUtils)(*O:Top_herd.Config*)
  =
  struct
    module S = S
    module E = S.E
    module A = S.A
    module U = MemUtils.Make(S)
  (*  module MU = ModelUtils.Make(O)(S)*)
  (*  module SU = SlUtils.Make(O)(S) *)

    let debug_proc chan p = fprintf chan "%i" p

    let debug_event chan e = fprintf chan "%s:%s - " (E.pp_eiid e) (S.E.Act.pp_action e.action)

    let debug_cnstrnts chan e = fprintf chan "\n[ %s ]" (S.M.VC.pp_cnstrnts e)

    let debug_set chan r =
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

    type execution = {
        treated_events : E.EventSet.t;
        rf : E.EventRel.t;
        mo : E.EventRel.t;
        todo : E.EventSet.t;
        revisit : E.EventSet.t;
      }



    let val_read r rf = E.EventRel.preds rf r

    let check_event_structure test rfms = ()
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
