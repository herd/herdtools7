(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Check an event structure against a machine model *)

module type Config = sig
  val m : AST.t
  val bell_model_info : (string * BellModel.info) option
  include Model.Config
end

module Make
    (O:Config)
    (S:Sem.Semantics)
    =
  struct
    let do_deps = O.variant Variant.Deps
    let mixed = O.variant Variant.Mixed
    let memtag = O.variant Variant.MemTag
    let kvm = O.variant Variant.Kvm

    let bell_fname =  Misc.app_opt (fun (x,_) -> x) O.bell_model_info
    let bell_info = Misc.app_opt (fun (_,x) -> x) O.bell_model_info

    module IConfig = struct
      let bell = false
      let bell_fname = bell_fname
      let compat =
        match S.A.arch with
        | `LISA -> O.variant Variant.BackCompat
        | _ -> false
      include O
      let doshow = S.O.PC.doshow
      let showraw = S.O.PC.showraw
      let symetric = S.O.PC.symetric
      let variant = Misc.delay_parse O.variant Variant.parse
    end
    module U = MemUtils.Make(S)
    module MU = ModelUtils.Make(O)(S)

    module IUtils = struct
      let partition_events = U.partition_events
      let loc2events x es =
        let open S in
        let x = A.V.nameToV x in
        E.EventSet.filter
          (fun e -> match E.location_of e with
          | Some (A.Location_global loc) -> A.V.compare loc x = 0
          | None | Some _ -> false)
          es

      let check_through = MU.check_through

      let pp_failure test conc msg vb_pp =
        MU.pp_failure
          test conc
          (Printf.sprintf "%s: %s" test.Test_herd.name.Name.name msg)
          vb_pp

      let pp test conc msg vb_pp =
        MU.pp
          test conc
          (Printf.sprintf "%s: %s" test.Test_herd.name.Name.name msg)
          vb_pp

(* fromto evts fs wher evts are all events and fs are fences *)

      let labels_of e = match e.S.E.iiid with
      | None -> Label.Set.empty
      | Some id -> id.S.A.labels

      let fencerel pbef paft po f =
        let open S in
        let before = E.EventSet.filter pbef (E.EventRel.preds po f)
        and after = E.EventSet.filter paft (E.EventRel.succs po f) in
        E.EventRel.cartesian before after


      let fromto po fs =
        let open S in
        let fs = E.EventSet.filter E.is_barrier fs in
        if E.EventSet.is_empty fs then E.EventRel.empty
        else
          let r =
            E.EventSet.fold
              (fun f r -> match f.E.iiid with
              | None -> assert false (* All fence evts generated by some ins *)
              | Some {A.inst=ins; _} ->
                  let fr = match A.I.fromto_of_instr ins with
                  | None ->
                      fencerel (fun _ -> true) (fun _ -> true) po f
                  | Some (l1,l2) ->
                      let keep lbls e =
                        not (Label.Set.disjoint lbls (labels_of e)) in
                      fencerel (keep l1) (keep l2) po f in
                  fr::r)
              fs [] in
          E.EventRel.unions r

      let same_value e1 e2 = match S.E.value_of e1,S.E.value_of e2 with
      | Some v1,Some v2 -> S.A.V.compare v1 v2 = 0
      | _ -> false

    end

    module I = Interpreter.Make(IConfig)(S)(IUtils)
    module Equiv = EquivSpec.Make(S)
    module E = S.E

(* Local utility: bell event selection *)
    let add_bell_events m pred evts annots =
      I.add_sets m
        (StringSet.fold
           (fun annot k ->
             let tag = BellName.tag2instrs_var annot in
             let rel = lazy begin
               E.EventSet.filter (pred annot) evts
             end in
             if O.debug then
               Printf.eprintf "annotation %s recorded as set %s\n"
                 annot tag ;
             let bd = tag,rel in
             let k = bd::k in
             k)
           annots [])

(* Intepreter call *)
    let (opts,_,prog) = O.m
    let withco = opts.ModelOption.co
    let catdep = opts.ModelOption.catdep
    let run_interpret test  kfail =
      let run =  I.interpret test kfail in
      fun ks m vb_pp kont res ->
        (*Printf.eprintf "vb_pp = {%s}\n%!" (String.concat "," (List.map fst (Lazy.force vb_pp)));*)
        run ks m vb_pp
          (fun st res ->
            if
              not O.strictskip || StringSet.equal st.I.out_skipped O.skipchecks
            then
              let conc = ks.I.conc in
              kont conc conc.S.fs (st.I.out_sets,st.I.out_show) st.I.out_flags res
            else res)
          res

    let choose_spec f1 f2 x = if do_deps then f1 x else f2 x
(* Enter here *)
    let check_event_structure test conc kfail kont res =
      let pr = lazy (MU.make_procrels E.is_isync conc) in
      let vb_pp =
        if O.showsome && O.verbose > 0 then
          lazy (MU.pp_procrels None (Lazy.force pr))
        else
          lazy [] in
      let relevant =
        if do_deps || catdep then fun _ -> true
        else fun e -> not (E.is_reg_any e) in
      let all_evts =  conc.S.str.E.events in
      let evts =
        choose_spec Misc.identity (E.EventSet.filter relevant) all_evts in
      let mem_evts = lazy (E.EventSet.filter E.is_mem evts) in
      let po =
        choose_spec
          Misc.identity
          (E.EventRel.filter (fun (e1,e2) -> relevant e1 && relevant e2))
          conc.S.po in
      let id =
        lazy begin
          E.EventRel.of_list
            (List.rev_map
               (fun e -> e,e)
               (E.EventSet.elements evts))
        end in
      let unv = lazy begin E.EventRel.cartesian evts evts  end in
      let ks = { I.id; unv; evts; conc; po;} in
      let calc_si sca = begin
        if mixed then
          E.EventRel.unions
            (E.EventSetSet.map_list
               (fun sm -> E.EventRel.cartesian sm sm)
               sca)
        else
          E.EventRel.set_to_rln (Lazy.force mem_evts)
      end in
      let si = lazy begin calc_si conc.S.str.E.sca end
      in
      let aligned =
        lazy begin
          let rs =
            List.map
              (fun (mem,sca) ->
                 if U.is_aligned (S.size_env test) mem then
                   calc_si (E.EventSetSet.singleton sca)
                 else
                   E.EventRel.empty)
              conc.S.str.E.aligned in
          E.EventRel.unions rs
        end in
      let rf_reg = lazy (U.make_rf_regs conc) in
(* Initial env *)
      let m =
        I.add_rels
          I.init_env_empty
          ((if O.variant Variant.Success || O.variant Variant.Instr then
            fun k ->
              ("instr",lazy begin
                E.EventRel.of_pred all_evts all_evts E.po_eq
              end)::k
          else Misc.identity)
             (((if do_deps then Misc.identity
             else fun k ->
              ("addr", lazy (Lazy.force pr).S.addr)::
              ("data", lazy (Lazy.force pr).S.data)::
              ("ctrl", lazy (Lazy.force pr).S.ctrl)::k)
             ["id",id;
              "loc", lazy begin
                E.EventRel.restrict_rel
                  E.same_location_with_faults
                  (Lazy.force unv)
              end;
              "int",lazy begin
                E.EventRel.restrict_rel E.same_proc_not_init (Lazy.force unv)
              end ;
              "ext",lazy begin
                E.EventRel.restrict_rel
                  (fun e1 e2 -> not (E.same_proc e1 e2)) (Lazy.force unv)
              end ;
              "rmw",lazy conc.S.atomic_load_store;
              "amo",
              lazy begin
                E.EventRel.filter
                  (fun (r,w) -> E.po_eq r w)
                  conc.S.atomic_load_store
              end;
              "po", lazy  po;
              "depend", lazy (Lazy.force pr).S.depend;
              "success", lazy (Lazy.force pr).S.success;
              "rf", lazy (Lazy.force pr).S.rf;
              "control",lazy conc.S.str.E.control ;
              "sm",si; "si",si;
              "aligned",aligned;
              "iico_data", lazy conc.S.str.E.intra_causality_data;
              "iico_ctrl", lazy conc.S.str.E.intra_causality_control;
              "rf-reg", rf_reg ;
              "same-instr", lazy begin E.EventRel.of_pred all_evts all_evts E.same_instruction end;
              "same-static",
              lazy begin
                E.EventRel.of_pred all_evts all_evts E.same_static_event
              end;
              "same-instance", lazy begin E.EventRel.of_pred all_evts all_evts E.same_instance end;
              "equiv-spec", lazy begin Equiv.build (Lazy.force rf_reg) all_evts end;
            ]))) in
      let m =
        let spec = conc.S.str.E.speculated in
        let is_spec = (fun e -> E.EventSet.mem e spec) in
        let data_ports = conc.S.str.E.data_ports in
        let is_data_port = (fun e -> E.EventSet.mem e data_ports) in
        I.add_sets m
          (("M",mem_evts)::
           List.fold_right
             (fun (k,p) ps ->
               (k,lazy (E.EventSet.filter p (Lazy.force mem_evts)))::ps)
             ["R", E.is_mem_load;
              "W", E.is_mem_store;
              "SPEC", is_spec;
              "EXEC", (fun e -> not (is_spec e));
              "AMO",E.is_amo;
              "I", E.is_mem_store_init;
              "IW", E.is_mem_store_init;
              "FW",
              (let ws = lazy (U.make_write_mem_finals conc) in
              fun e -> E.EventSet.mem e (Lazy.force ws)); ]
             (List.map
                (fun (k,p) -> k,lazy (E.EventSet.filter p evts))
                ["C", E.is_commit;
                 "PoD", E.is_pod;
                 "F", E.is_barrier;
                 "DATA", is_data_port;
                 "NDATA", (fun e -> not (is_data_port e));])) in
      let m =
        I.add_sets m
          (List.map
             (fun (k,a) ->
               k,lazy (E.EventSet.filter (fun e -> a e.E.action) evts))
             E.Act.arch_sets) in
      let m =
        if kvm then
          let my_t0 = lazy begin
            let open DirtyBit in
            let np = test.Test_herd.nice_prog in
            let ha0 =
              Misc.check_same
                Misc.bool_eq
                (fun ((proc,_),_) -> O.dirty.ha proc)
                np
            and hd0 =
              Misc.check_same
                Misc.bool_eq
                (fun ((proc,_),_) -> O.dirty.hd proc)
                np in
            let tr_h h = function
              | None -> fun () ->
                  Warn.fatal "All procs must have the same value of the '%s' flag" h
              | Some h -> fun () -> h in
            {my_ha=tr_h "HA" ha0; my_hd=tr_h "HD" hd0;}
          end in
          I.add_sets m
            (List.map
               (fun (k,a) ->
                 k,lazy begin
                   let open DirtyBit in
                   let tr_proc proc =
                     let my_ha () = O.dirty.ha proc
                     and my_hd () = O.dirty.hd proc in
                     { my_ha; my_hd; } in
                   E.EventSet.filter
                     (fun e ->
                       match E.proc_of e with
                       | Some proc -> a (tr_proc proc) e.E.action
                       | None -> a (Lazy.force my_t0) e.E.action)
                     evts
                 end)
               E.Act.arch_dirty)
        else m in
(* Define empty fence relation
   (for the few models that apply to several archs) *)
      let m = I.add_rels m
          [
(* PTX fences *)
           "membar.cta",lazy E.EventRel.empty;
           "membar.gl", lazy E.EventRel.empty;
           "membar.sys",lazy E.EventRel.empty;
         ] in
(* Override arch specific fences *)

      let m =
        I.add_rels m
          (List.map
             (fun (k,p) ->
               let pred (e1,e2) = p e1.E.action e2.E.action in
               k,lazy (E.EventRel.filter pred (Lazy.force unv)))
             E.Act.arch_rels) in
(* Event sets from proc info *)
      let m = match test.Test_herd.proc_info with
      | [] -> m
      | _::_ as i ->
          let bds = U.lift_proc_info i evts in
          I.add_sets m bds in
(* Event sets from bell_info *)
      let m =
        match bell_info with
        | None -> m
        | Some bi ->
            let m =
              add_bell_events m
                (fun annot e -> E.Act.annot_in_list annot e.E.action)
                evts
                (BellModel.get_mem_annots bi) in
            let open MiscParser in
            begin match test.Test_herd.extra_data with
              (* No region in test, empty regions *)
            | NoExtra|BellExtra {BellInfo.regions=None;_} ->
                I.add_sets m
                  (List.map
                     (fun r -> BellName.tag2instrs_var r,lazy E.EventSet.empty)
                     (StringSet.elements (BellModel.get_region_sets bi)))
            | BellExtra {BellInfo.regions=Some regions;_} ->
                let reg2loc =
                  List.fold_left
                    (fun m (x,rs) ->
                      List.fold_left
                        (fun m r ->
                          let old = StringMap.safe_find StringSet.empty r m in
                          StringMap.add r (StringSet.add x old) m)
                        m rs)
                    StringMap.empty regions in
                let loc2evts = U.collect_mem conc.S.str in
                let loc2evts = U.LocEnv.map E.EventSet.of_list loc2evts in
                I.add_sets m
                  (StringSet.fold
                     (fun region k ->
                       let tag = BellName.tag2instrs_var region in
                       let set = lazy begin
                         let locs =
                           StringMap.safe_find StringSet.empty
                             region reg2loc in
                         let evts =
                           StringSet.fold
                             (fun loc k ->
                               let v = S.A.V.nameToV loc in
                               let x = S.A.Location_global v in
                               U.LocEnv.safe_find E.EventSet.empty
                                 x loc2evts::k)
                             locs [] in
                         E.EventSet.unions evts
                       end in
                       (tag,set)::k)
                     (BellModel.get_region_sets bi) [])
            | CExtra _ -> m (* Ignore CExtra ?? *)
            end in
(* Scope relations from bell info *)
      let m =
        match bell_info with
        | None -> m
        | Some _ ->
            let open MiscParser in
            let extract_tbi e = match test.Test_herd.extra_data with
            | NoExtra|CExtra _ ->
                None (* must be here as, O.bell_mode_info is *)
            | BellExtra tbi -> e tbi in
            let scopes =  extract_tbi (fun tbi -> tbi.BellInfo.scopes) in
            let m = match scopes with
              (* If no scope definition in test, do not build relations, will fail
                 later if the model attempts to use scope relations *)
            | None -> m
                  (* Otherwise, build scope relations *)
            | Some scopes ->
                let rs = U.get_scope_rels evts scopes in
                I.add_rels m
                  (List.map
                     (fun (scope,r) -> BellName.tag2rel_var scope,lazy r)
                     rs)  in
            let lvls = extract_tbi (fun tbi -> tbi.BellInfo.levels) in
            let m = match lvls with
            | None -> m
            | Some lvls ->
                let r,rs = U.get_level_rels evts lvls in
                I.add_rels m
                  ((BellName.nextlevel,lazy r)::
                   List.map
                     (fun (lvl,r) -> BellName.tag2rel_var lvl,lazy r)
                     rs)  in
            m in
(* Now call interpreter, with or without generated co *)
      if withco then
        let process_co co0 res =
          let co = S.tr co0 in
          let fr = U.make_fr conc co in
          let vb_pp =
            if O.showsome then
              lazy (("fr",fr)::("co",co0)::Lazy.force vb_pp)
            else
              lazy [] in
          let m =
            I.add_rels m
              [
               "fr", lazy fr;
               "fre", lazy (U.ext fr); "fri", lazy (U.internal fr);
               "co", lazy co;
               "coe", lazy (U.ext co); "coi", lazy (U.internal co);
             ] in
          run_interpret test kfail ks m vb_pp kont res in
        U.apply_process_co test  conc process_co res
      else
(*        let m = I.add_rels m ["co0",lazy  conc.S.pco] in *)
        run_interpret test kfail ks m vb_pp kont res
  end
