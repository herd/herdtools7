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

(** Check an event structure against a CPP11 model *)

module type Config = sig
  val m : AST.pp_t
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

    let (pp,(opts,_,prog)) = O.m

    let withco = opts.ModelOption.co
    let withsc = opts.ModelOption.sc

    let failed_requires_clauses = ref 0

    let run_interpret failed_requires_clause test conc m id vb_pp kont res =
      I.interpret
        failed_requires_clause test conc m id vb_pp
        (fun st res ->
          if not O.strictskip || StringSet.equal st.I.skipped O.skipchecks then
            let vb_pp = lazy (I.show_to_vbpp st) in
            kont conc conc.S.fs vb_pp (Some (!failed_requires_clauses)) res
          else res)
        res

    module MU = ModelUtils.Make(O)(S)

    let check_event_structure test conc kont res =
      let pr = MU.make_procrels (fun _ -> false) conc in
      let vb_pp = lazy (MU.pp_procrels "???" pr) in
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
      let unv =
        lazy begin E.EventRel.cartesian evts evts end in
      let ks = { I.id; unv; evts; } in
(* Initial env *)
      let m =
        I.add_rels I.env_empty
          ["id",id;
	   "unv", unv;
           "atom",lazy conc.S.atomic_load_store;
           "asw",lazy begin
             S.restrict E.is_mem_store_init
	       (fun x -> not (E.is_mem_store_init x))
	       (Lazy.force unv)
           end ;
           "po",lazy begin
             S.restrict E.is_mem E.is_mem conc.S.po
           end ;
           "pos", lazy conc.S.pos;
           "po-loc", lazy conc.S.pos;
	   "loc", lazy begin
             E.EventRel.restrict_rel E.same_location (Lazy.force unv)
           end ;
           "addr", lazy pr.S.addr;
           "data", lazy pr.S.data;
           "ctrl", lazy pr.S.ctrl;
           "ctrlisync", lazy pr.S.ctrlisync;
           "ctrlisb", lazy pr.S.ctrlisync;
           "rf", lazy pr.S.rf;
           "rfe", lazy (U.ext pr.S.rf);
           "rfi", lazy (U.internal pr.S.rf);
         ] in
      let m =
        let m = I.add_sets m ["_",lazy evts;] in
        let lazy_filter p = lazy begin E.EventSet.filter p evts end in
        I.add_sets m
          (List.map
             (fun (k,p) -> k,lazy_filter p)
             (("R", E.is_mem_load)::
              ("W", E.is_mem_store)::
              ("M", E.is_mem)::
              ("P", (fun e -> not (E.is_atomic e)))::
              ("A", E.is_atomic)::
	      ("I", E.is_mem_store_init)::
              ("atomicloc", (fun e ->
                match E.location_of e with
                | Some (E.A.Location_global a) ->
                    Misc.exists_exists (fun p ->
                      p.CAst.param_name = E.A.V.pp_v a &&
                      CType.is_ptr_to_atomic p.CAst.param_ty)
                      test.Test.param_map
                | _ -> false))::
              ("nonatomicloc", (fun e ->
                match E.location_of e with
                | Some (E.A.Location_global a) ->
                    Misc.exists_exists (fun p ->
                      p.CAst.param_name = E.A.V.pp_v a &&
                      not (CType.is_ptr_to_atomic p.CAst.param_ty))
                      test.Test.param_map
                | _ -> false))::
              ("mutexloc", (fun e ->
                match E.location_of e with
                | Some (E.A.Location_global a) ->
                    Misc.exists_exists (fun p ->
                      p.CAst.param_name = E.A.V.pp_v a &&
                      CType.is_mutex p.CAst.param_ty)
                      test.Test.param_map
                | _ -> false))::
              List.map
                (fun (k,a) -> k,(fun e -> a e.E.action))
                E.Act.arch_sets)) in
      let failed_requires_clause () =
	let () = incr failed_requires_clauses in ()
      in
      if withsc then
        let process_sc sc0 res =
          let process_co co0 res =
            let process_lo lo0 res =
              let sc = S.tr sc0 in
              let co = S.tr co0 in
              let lo = lo0 in
              let fr = U.make_fr conc co in
              let vb_pp =
                lazy begin
                  (if StringSet.mem "S" S.O.PC.unshow
                   then [] else [("S",sc0)]) @
                  (if S.O.PC.showfr then [("fr",fr)] else []) @
                  (if StringSet.mem "co" S.O.PC.unshow
                  then [] else [("co",co0)]) @
                  (if StringSet.mem "lo" S.O.PC.unshow
                  then [] else [("lo",lo0)]) @
                  Lazy.force vb_pp
                end in

              let m =
                I.add_rels m
                  [
                   "S", lazy sc;
                   "fr", lazy fr;
                   "fre", lazy (U.ext fr);"fri", lazy (U.internal fr);
                   "co", lazy co;
                   "coe", lazy (U.ext co); "coi", lazy (U.internal co);
                   "lo", lazy lo;
                   "loe", lazy (U.ext lo); "loi", lazy (U.internal lo);
                 ] in
              run_interpret failed_requires_clause test conc m ks vb_pp kont res in
            U.apply_process_lo test conc process_lo res in
          U.apply_process_co test conc process_co res in
        U.apply_process_sc test conc process_sc res
      else
        if withco then
          let process_co co0 res =
            let process_lo lo0 res =
              let co = S.tr co0 in
              let lo = lo0 in
              let fr = U.make_fr conc co in
              let vb_pp =
                lazy begin
                  (if S.O.PC.showfr then [("fr",fr)] else []) @
                  (if StringSet.mem "co" S.O.PC.unshow
                  then [] else [("co",co0)]) @
                  (if StringSet.mem "lo" S.O.PC.unshow
                  then [] else [("lo",lo0)]) @
                  Lazy.force vb_pp
                end in

              let m =
                I.add_rels m
                  [
                   "fr", lazy fr;
                   "fre", lazy (U.ext fr);"fri", lazy (U.internal fr);
                   "co", lazy co;
                   "coe", lazy (U.ext co); "coi", lazy (U.internal co);
                   "lo", lazy lo;
                   "loe", lazy (U.ext lo); "loi", lazy (U.internal lo);
                 ] in
              run_interpret failed_requires_clause test conc m ks vb_pp kont res in
            U.apply_process_lo test conc process_lo res in
          U.apply_process_co test conc process_co res
        else
          let co0 = conc.S.pco in
          let m = I.add_rels m ["co0", lazy co0;] in
          run_interpret failed_requires_clause test conc m ks vb_pp kont res
  end
