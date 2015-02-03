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

(** Check an event structure against an OpenCL model *)

open Printf

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

    let debug_proc chan p = fprintf chan "%i" p
    let debug_event chan e = fprintf chan "%s" (E.pp_eiid e)
    let debug_set chan s =
      output_char chan '{' ;
      E.EventSet.pp chan "," debug_event s ;
      output_char chan '}'

    let debug_events = debug_set

    let debug_rel chan r =
      E.EventRel.pp chan ","
        (fun chan (e1,e2) -> fprintf chan "%a -> %a"
            debug_event e1 debug_event e2)
        r

    let withco = opts.ModelOption.co
    let withsc = opts.ModelOption.sc

    let failed_requires_clauses = ref 0

    let run_interpret failed_requires_clause test conc m id vb_pp kont res =
      I.interpret failed_requires_clause test conc m id vb_pp
        (fun st res ->
          if not O.strictskip || StringSet.equal st.I.skipped O.skipchecks then
            let vb_pp = lazy (I.show_to_vbpp st) in
            kont conc conc.S.fs vb_pp (Some (!failed_requires_clauses)) res
          else res)
        res

    module MU = ModelUtils.Make(O)(S)

    let check_event_structure test conc kont res =
      let pr = lazy (MU.make_procrels (fun _ -> false) conc) in
      let vb_pp = lazy (MU.pp_procrels "???" (Lazy.force pr)) in
      let evts = conc.S.str.E.events in
      let id =
        lazy begin
          E.EventRel.of_list
            (List.rev_map
               (fun e -> e,e)
               (E.EventSet.elements evts))
        end in
      let unv = lazy begin E.EventRel.cartesian evts evts end in
      let ks = { I.id; unv; evts; } in
      (* printf "po = {\n%a\n}\n" debug_rel conc.S.po; *)
(* Initial env *)
      let m =
        I.add_rels I.env_empty
          (["id",id;
	    "unv", unv;
            "atom",lazy (conc.S.atomic_load_store);
            "asw", lazy begin
              S.restrict E.is_mem_store_init
	        (fun x -> not (E.is_mem_store_init x))
	        (Lazy.force unv)
            end ;
            "po", lazy (S.restrict E.is_mem E.is_mem conc.S.po);
            "pos", lazy conc.S.pos;
            "po-loc", lazy conc.S.pos;
	    "loc", lazy begin
              E.EventRel.restrict_rel E.same_location (Lazy.force unv)
            end;
            "addr", lazy (Lazy.force pr).S.addr;
            "data", lazy (Lazy.force pr).S.data;
            "ctrl", lazy (Lazy.force pr).S.ctrl;
            "ctrlisync", lazy (Lazy.force pr).S.ctrlisync;
            "ctrlisb", lazy (Lazy.force pr).S.ctrlisync;
            "rf", lazy (Lazy.force pr).S.rf;
            "rfe", lazy (U.ext (Lazy.force pr).S.rf);
            "rfi", lazy (U.internal (Lazy.force pr).S.rf);
            "same_B", lazy begin
              E.EventRel.restrict_rel E.same_barrier_id (Lazy.force unv)
            end;
          ] @
           (match test.Test.scope_tree with
           | None -> []
           | Some scope_tree ->
               List.fold_left (fun z (k,v) ->
                 ("ext-" ^ k,
                  lazy (U.ext_scope v (Lazy.force unv) scope_tree)) ::
                 ((*"int-" ^ *) k,
                  lazy (U.int_scope v (Lazy.force unv) scope_tree)) ::
                 z ) [] [
               "wi", AST.Work_Item;
               "thread", AST.Work_Item;
               "sg", AST.Sub_Group; "warp", AST.Sub_Group;
               "wg", AST.Work_Group;
               "block", AST.Work_Group;
               "cta", AST.Work_Group;
	       "kernel", AST.Kernel;
	       "dev", AST.Device;
	     ])) in
      let m =
        I.add_sets m
          (List.map
             (fun (k,p) -> k,lazy (E.EventSet.filter p evts))
             ([
              "_", (fun _ -> true);
              "R", E.is_mem_load;
              "W", E.is_mem_store;
              "M", E.is_mem;
              "P", (fun e -> not (E.is_atomic e));
              "A", E.is_atomic;
	      "I", E.is_mem_store_init;
              "G", (fun e ->
                E.is_global_fence e ||
                (match E.location_of e with
                | Some (E.A.Location_global a) ->
                    Misc.exists_exists (fun p ->
                      p.CAst.param_name = E.A.V.pp_v a &&
                      CType.is_ptr_to_global p.CAst.param_ty)
                      test.Test.param_map
                | _ -> false));
              "L", (fun e ->
                E.is_local_fence e ||
                (match E.location_of e with
                | Some (E.A.Location_global a) ->
                    Misc.exists_exists (fun p ->
                      p.CAst.param_name = E.A.V.pp_v a &&
                      CType.is_ptr_to_local p.CAst.param_ty)
                      test.Test.param_map
                | _ -> false));
              "atomicloc", (fun e ->
                match E.location_of e with
                | Some (E.A.Location_global a) ->
                    Misc.exists_exists (fun p ->
                      p.CAst.param_name = E.A.V.pp_v a &&
                      CType.is_ptr_to_atomic p.CAst.param_ty)
                      test.Test.param_map
                | _ -> false);
              "nonatomicloc", (fun e ->
                match E.location_of e with
                | Some (E.A.Location_global a) ->
                    Misc.exists_exists (fun p ->
                      p.CAst.param_name = E.A.V.pp_v a &&
                      not (CType.is_ptr_to_atomic p.CAst.param_ty))
                      test.Test.param_map
                | _ -> false);
            ])) in
      let m =
        I.add_sets m
          (List.map
             (fun (k,a) ->
               k,lazy  begin
                 E.EventSet.filter (fun e -> a e.E.action) evts
               end)
	     E.Act.arch_sets) in
      let failed_requires_clause () =
	let () = incr failed_requires_clauses in ()
      in
      if withco then
        let process_co co0 res =
          if withsc then
            let process_sc sc0 res =
              let co = S.tr co0 in
              let sc = S.tr sc0 in
              let fr = U.make_fr conc co in
              let vb_pp =
                lazy begin
                  (if S.O.PC.showfr then [("fr",fr)] else []) @
                  (if StringSet.mem "co" S.O.PC.unshow
	          then [] else [("co",co0)]) @
                  (if StringSet.mem "sc" S.O.PC.unshow
	          then [] else [("S",sc0)]) @
	          Lazy.force vb_pp
	        end in

              let m =
                I.add_rels m
                  [
                   "fr", lazy fr;
                   "fre", lazy (U.ext fr); "fri", lazy (U.internal fr);
                   "co", lazy co;
                   "coe", lazy (U.ext co); "coi", lazy (U.internal co);
                   "S", lazy sc;
	         ] in
              run_interpret failed_requires_clause test conc m ks vb_pp kont res
            in
            U.apply_process_sc test conc process_sc res
          else
            let co = S.tr co0 in
            let fr = U.make_fr conc co in
            let vb_pp =
              lazy begin
                (if S.O.PC.showfr then [("fr",fr)] else []) @
                (if StringSet.mem "co" S.O.PC.unshow
	        then [] else [("co",co0)]) @
	        Lazy.force vb_pp
	      end in

            let m =
              I.add_rels m
                [
                   "fr", lazy fr;
                   "fre", lazy (U.ext fr); "fri", lazy (U.internal fr);
                   "co", lazy co;
                   "coe", lazy (U.ext co); "coi", lazy (U.internal co);
	         ] in
            run_interpret failed_requires_clause test conc m ks vb_pp kont res
        in
        U.apply_process_co test conc process_co res
      else
        let m = I.add_rels m ["co0",lazy conc.S.pco;] in
        run_interpret failed_requires_clause test conc m ks vb_pp kont res
  end
