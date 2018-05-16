open Printf

module type Cfg = sig
(*  val coherence : bool
  val atomicity : bool
  val sc : bool
  val no_thin_air : bool *)
  include Model.Config
end

module Make (O:Cfg)(S:Sem.Semantics)
  =
  struct
    module S = S
    module E = S.E
    module A = S.A
    module U = MemUtils.Make(S)
    module MU = ModelUtils.Make(O)(S)
    module ER = E.EventRel

    let debug_proc chan p = fprintf chan "%i" p
    let debug_event chan e = fprintf chan "%s" (E.pp_eiid e)
    let debug_set chan s =
      output_char chan '{' ;
      E.EventSet.pp chan "," debug_event s ;
      output_char chan '}'

    let debug_events = debug_set

    let debug_rel chan r =
      ER.pp chan ","
        (fun chan (e1, e2) -> fprintf chan "%a -> %a"
                                debug_event e1 debug_event e2)
        r

    let make_procrels conc =
      MU.make_procrels (fun x -> false) conc

    let pp_failure test conc legend vb_pp =
      if O.debug && O.verbose > 1 then begin
          let module PP = Pretty.Make(S) in
          eprintf "%s\n%!" legend ;
          PP.show_legend test legend conc vb_pp
          end

    let check_event_structure test conc _kfail kont res =
      let pr = make_procrels conc in
      let pp_relns =
        lazy begin
            []
          end in
      let proc_ws ws0 res =
        let ws = ER.transitive_closure ws0 in
        let unv = ER.cartesian conc.S.str.E.events conc.S.str.E.events in
  (*      let nodes = ER.nodes unv in *)
        let rf = pr.rf in
        let mo = ws in
        let sb = conc.S.po in
        let rb = U.make_fr conc ws in
        let loc = ER.restrict_rel E.same_location unv in
        let int = ER.restrict_rel E.same_proc_not_init unv in
        let ext = ER.restrict_rel (fun e1 e2 -> not (E.same_proc e1 e2)) unv in
        let rmw = conc.S.atomic_load_store in

        let aux = fun x ->
          try List.assoc x S.E.Act.arch_sets with Not_found -> fun x -> true in

        let rlx = aux "RLX" in
        let acq = aux "ACQ" in
        let rel = aux "REL" in
        let acq_rel = aux "AQU_REL" in
        let sc = aux "SC" in
        let na = aux "NA" in
      (*  let rlx = ER.Elts.filter E.Act.is_atomic mo_matches MemOrder.Rlx in *)
        let f = E.is_barrier in

        let eco0 = ER.union3 rf mo rb in
        let eco = ER.transitive_closure eco0 in

        let rs0 = ER.sequence rf rmw in
        let rs1 = ER.transitive_closure rs0 in
        let rs2 = ER.restrict_domain (fun x -> E.is_mem_store x && (rlx x.action|| na x.action)) rs1 in
        let rs3 = ER.inter sb loc in
        let rs4 = ER.restrict_domains E.is_mem_store (fun x -> E.is_mem_store x && (rlx x.action|| na x.action)) rs3 in
        let rs5 = ER.sequence rs4 rs2 in
        let rs = ER.union rs2 rs5 in

        let sw0 = ER.sequence rs rf in
        let sw1 = ER.restrict_domain f sb in
        let sw2 = ER.sequence sw1 sw0 in
        let sw3 = ER.union sw0 sw2 in
        let sw4 = ER.restrict_domains (fun x -> rel x.action || acq_rel x.action || sc x.action) (fun x -> E.is_mem_store x && (rlx x.action || na x.action)) sw3 in
        let sw5 = ER.restrict_codomain f sb in
        let sw6 = ER.sequence sw4 sw5 in
        let sw7 = ER.union sw4 sw6 in
        let sw = ER.restrict_codomain (fun x -> rel x.action || acq_rel x.action || sc x.action) sw7 in

        let hb0 = ER.union sb sw in
        let hb = ER.transitive_closure hb0 in

        let sbl = ER.diff sb loc in

        let scb = ER.unions [sb; ER.sequences [sbl; hb; sbl]; ER.inter sb loc; mo; rb] in

        let pscb0 = ER.restrict_domain (fun x -> sc x.action) scb in
        let pscb1 = ER.restrict_domain (fun x -> f x && sc x.action) unv in
        let pscb2 = ER.inter pscb1 hb in
        let pscb3 = ER.sequence pscb2 scb in
        let pscb4 = ER.inter pscb1 scb in
        let pscb5 = ER.union3 pscb0 pscb3 pscb4 in

        let pscb6 = ER.restrict_codomain (fun x -> sc x.action) pscb5 in
        let pscb7 = ER.sequence pscb5 hb in
        let pscb8 = ER.union pscb5 pscb7 in
        let pscb9 = ER.restrict_codomain (fun x -> f x && sc x.action) pscb8 in
        let pscb = ER.union pscb6 pscb9 in

        let pscf0 = ER.union hb (ER.sequences [hb; eco; hb]) in
        let fsc = fun x -> f x && sc x.action in
        let pscf = ER.restrict_domains fsc fsc pscf0 in

        let psc = ER.union pscb pscf in


        let r0 = ER.union hb (ER.sequence hb eco) in
        let coherence = ER.is_irreflexive r0 in

        let r1 = ER.inter rmw (ER.sequence rb mo) in
        let atomicity = ER.is_empty r1 in

        let asc = ER.is_acyclic psc in

        let r2 = ER.union sb rf in
        let nothinair = ER.is_acyclic r2 in

        let ok = coherence && atomicity && asc && nothinair in
        if ok
        then
          kont conc conc.S.fs pp_relns Flag.Set.empty res
        else res in
      U.apply_process_co test conc proc_ws res
  end

