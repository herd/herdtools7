(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Check an event structure against a machine model *)

module type Config = sig
  val m : AST.pp_t
  val bell_model_info : Bell_info.model option
  include Model.Config
end

module Make
    (O:Config)
    (S:Sem.Semantics)
    =
  struct

    module I = Interpreter.Make(O)(S)
    module E = S.E
    module U = MemUtils.Make(S)

(* Local utility: bell event selection *)
    let add_bell_events m pred evts annots =
      I.add_sets m
        (List.map
           (fun annot ->
             let tag = String.uppercase annot in
             tag,
             lazy begin
               E.EventSet.filter (pred annot) evts
             end)
           annots)

(* Intepreter call *)
    let (pp,(opts,_,prog)) = O.m
    let withco = opts.ModelOption.co

    let run_interpret test  =
      let run =  I.interpret test in
      fun ks m vb_pp kont res ->
        run ks m vb_pp
          (fun st res ->
            if
              not O.strictskip || StringSet.equal st.I.skipped O.skipchecks
            then
              let vb_pp = lazy (I.show_to_vbpp st) in
              let conc = st.I.ks.I.conc in
              kont conc conc.S.fs vb_pp st.I.undef res
            else res)
          res

    module MU = ModelUtils.Make(O)(S)

(* Enter here *)
    let check_event_structure test conc kont res =
      let pr = lazy (MU.make_procrels E.is_isync conc) in
      let vb_pp =
        if O.showsome then
          lazy (MU.pp_procrels E.Act.pp_isync (Lazy.force pr))
        else
          lazy [] in
      let evts =
        E.EventSet.filter
          (fun e -> E.is_mem e || E.is_barrier e)
          conc.S.str.E.events in
      let id =
        lazy begin
          E.EventRel.of_list
            (List.rev_map
               (fun e -> e,e)
               (E.EventSet.elements evts))
        end in
      let unv = lazy begin E.EventRel.cartesian evts evts  end in
      let ks = { I.id; unv; evts; conc;} in
(* Initial env *)
      let m =
        I.add_rels
          I.env_empty
          (["id",id;
            "loc", lazy begin
              E.EventRel.restrict_rel E.same_location (Lazy.force unv)
            end;
            "int",lazy begin
              E.EventRel.restrict_rel E.same_proc (Lazy.force unv)
            end ;
            "ext",lazy begin
              E.EventRel.restrict_rel
                (fun e1 e2 -> not (E.same_proc e1 e2)) (Lazy.force unv)
            end ;
           "atom",lazy conc.S.atomic_load_store;
           "po", lazy conc.S.po;
           "addr", lazy (Lazy.force pr).S.addr;
           "data", lazy (Lazy.force pr).S.data;
           "ctrl", lazy (Lazy.force pr).S.ctrl;
           "rf", lazy (Lazy.force pr).S.rf;
          ] @
          (match test.Test.scope_tree with
           | None -> []
           | Some scope_tree ->
        List.fold_left (fun z (k,v) ->
            ("ext-" ^ k, lazy (U.ext_scope v (Lazy.force unv) scope_tree)) ::
            ((* "int-" ^ *) k,
             lazy (U.int_scope v (Lazy.force unv) scope_tree)) ::
            z ) [] [
          "wi", AST.Work_Item; "thread", AST.Work_Item;
          "sg", AST.Sub_Group; "warp", AST.Sub_Group;
          "wg", AST.Work_Group; "block", AST.Work_Group; "cta", AST.Work_Group;
          "kernel", AST.Kernel;
	  "dev", AST.Device; "gl", AST.Device;
	])) in
      let m =
        I.add_sets m
          (List.map
             (fun (k,p) -> k,lazy (E.EventSet.filter p evts))
          [
           "R", E.is_mem_load;
           "W", E.is_mem_store;
           "M", E.is_mem;
	   "F", E.is_barrier;
           "P", (fun e -> not (E.is_atomic e));
           "A", E.is_atomic;
	   "I", E.is_mem_store_init;
         ]) in
      let m =
        I.add_sets m
          (List.map
             (fun (k,a) ->
               k,lazy (E.EventSet.filter (fun e -> a e.E.action) evts))
	  E.Act.arch_sets) in
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
               let pred e = p e.E.action in
               k,lazy (U.po_fence_po conc.S.po pred))
             E.Act.arch_fences) in
(* Event sets from bell_info *)
      let m =
        match O.bell_model_info with
        | None -> m
        | Some bi ->            
            let m =
              add_bell_events m
                (fun annot e -> E.Act.annot_in_list annot e.E.action)
                evts
                (Bell_info.get_mem_annots bi) in
            let m =
              match test.Test.bell_info with
              (* No region in test, no event sets *)
              | None|Some {Bell_info.regions=None;_} -> m
              | Some {Bell_info.regions=Some regions;_} ->
                  add_bell_events m
                    (fun region e -> match E.Act.location_of e.E.action with
                    | None -> false
                    | Some x ->
                       List.mem (E.Act.A.pp_location x, region) regions)
                    evts
                    (Bell_info.get_region_sets bi) in
            m in
(* Scope relations from bell info *)
      let m =
        match O.bell_model_info with
        | None -> m
        | Some bi ->
            let scopes =
              match test.Test.bell_info with
              | None -> assert false (* must be here as, O.bell_mode_info is *)
              | Some tbi -> tbi.Bell_info.scopes in
            begin match scopes with
 (* If no scope definition in test, do not build relations, will fail
    later if the model attempts to use scope relations *)
            | None -> m
 (* Otherwise, build scope relations *)
            | Some scopes ->
                I.add_rels m
                  (List.map
                     (fun scope ->
                       scope,
                       lazy begin
                         U.int_scope_bell scope scopes (Lazy.force unv)
                       end)
                     (Bell_info.get_scope_rels bi))
            end in
(* Now call interpreter, with or without generated co *)
      if withco then
        let process_co co0 res =
          let co = S.tr co0 in
          let fr = U.make_fr conc co in
          let vb_pp =
            if O.showsome then
              lazy begin
                if S.O.PC.showfr then
                  ("fr",fr)::("co",co0)::Lazy.force vb_pp
                else
                  ("co",co0)::Lazy.force vb_pp
              end
            else lazy [] in

          let m =
            I.add_rels m
              [
               "fr", lazy fr;
               "fre", lazy (U.ext fr); "fri", lazy (U.internal fr);
               "co", lazy co;
               "coe", lazy (U.ext co); "coi", lazy (U.internal co);
	     ] in
          run_interpret test ks m vb_pp kont res in
        U.apply_process_co test  conc process_co res
      else
        let m = I.add_rels m ["co0",lazy  conc.S.pco] in
        run_interpret test ks m vb_pp kont res
  end
