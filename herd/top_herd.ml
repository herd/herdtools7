(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc aranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Top level loop : execute test according to model *)

module type CommonConfig = sig
  val show : PrettyConf.show
  val nshow : int option
  val restrict : Restrict.t
  val outcomereads : bool
  val badexecs : bool
  val badflag : string option
  val throughflag : string option
  include Mem.CommonConfig
  val statelessrc11 : bool
  val skipchecks : StringSet.t
  val dumpallfaults : bool
end

module type Config = sig
  include CommonConfig
  val byte : MachSize.sz
  val dirty : DirtyBit.t option
end

module TestResult = struct
  type observation = Never | Always | Sometimes

  let pp_observation = function
    | Never -> "Never"
    | Always -> "Always"
    | Sometimes -> "Sometimes"

  type 'stset stats =
    { states : 'stset;
      cfail : int;
      cands : int;
      pos : int;
      neg : int;
      observation : observation;
      flagged : int list Flag.Map.t ;
      cutoff : string option;
    }

  let states x = x.states
  let failed_candidates x = x.cfail
  let candidates x = x.cands
  let positive x = x.pos
  let negative x = x.neg
  let observation x = x.observation
  let flagged x = x.flagged
  let cutoff x = x.cutoff

  let has_bad_execs ~badflag c =
    Flag.Map.mem Flag.Undef c.flagged ||
    (match badflag with
    | None -> false
    | Some f -> Flag.Map.mem (Flag.Flag f) c.flagged)

  type ('conc, 'sets, 'rels) execution =
    { concrete : 'conc;
      passes_check : bool;
      flags : Flag.Set.t;
      sets : 'sets Lazy.t;
      rels : 'rels Lazy.t;
    }

  let concrete x = x.concrete
  let passes_check x = x.passes_check
  let flags x = x.flags
  let sets x = Lazy.force x.sets
  let relations x = Lazy.force x.rels

  module Make (S : SemExtra.S) = struct
    type nonrec stats = S.A.StateSet.t stats
    type nonrec execution = (S.concrete, S.set_pp, S.rel_pp) execution
    type t = S.event_structure list * ((execution -> unit) -> stats)
  end
end

module type PrinterConfig = sig
  module PC : PrettyConf.S
  val candidates : bool
  val showkind : bool
  val shortlegend : bool
  val verbose : int
  val show : PrettyConf.show
  val speedcheck : Speed.t
  val badflag : string option
end

module Printer (O : PrinterConfig) (S : SemExtra.S) = struct
  module A = S.A
  module C = S.Cons
  module T = Test_herd.Make (S.A)

(* Location out printing *)
  let tr_out test = OutMapping.info_to_tr  test.Test_herd.info

(* Check condition *)

  let check_cond cstr c =
    let open ConstrGen in
    match cstr with
    | ExistsState _ -> c.TestResult.pos > 0
    | NotExistsState _-> c.TestResult.pos = 0
    | ForallStates _  -> c.TestResult.neg = 0

  let check_wit cstr c =
    let open ConstrGen in
    match cstr with
    | ForallStates _
    | ExistsState _ -> c.TestResult.pos,c.TestResult.neg
    | NotExistsState _-> c.TestResult.neg,c.TestResult.pos

  let pp_stats ~time test c fmt =
    let open TestResult in
    let finals = c.states in
    let nfinals = A.StateSet.cardinal finals in
    let tname = test.Test_herd.name.Name.name in
    let cstr = T.find_our_constraint test in

    let open Format in
(* Now output *)
    fprintf fmt "Test %s %s\n" tname (C.dump_as_kind cstr) ;
(**********)
(* States *)
(**********)
    let tr_out = tr_out test in
    fprintf fmt "States %i\n" nfinals ;
    let state_set_str =
      A.StateSet.pp_str "\n" (fun st ->
        A.do_dump_final_state
          test.Test_herd.type_env test.Test_herd.ffaults
          tr_out st) finals
    in
    if nfinals > 0 then
      fprintf fmt "%s\n" state_set_str;
(* Condition result *)
    let has_bad_execs = TestResult.has_bad_execs ~badflag:O.badflag in
    let ok = check_cond cstr c in
    fprintf fmt "%s%s\n"
      (if Misc.is_some c.cutoff then "Loop " else "")
      (if has_bad_execs c then "Undef" else if ok then "Ok" else "No") ;
    fprintf fmt "Witnesses\n" ;
    let pos,neg = check_wit cstr c in
    fprintf fmt "Positive: %i Negative: %i\n" pos neg ;
    begin if O.verbose > 0 then
      Flag.Map.iter
        (fun flag execs ->
          fprintf fmt "Flag %s: %s \n"
            (Flag.pp flag)
            (List.fold_right
               (fun i s -> s ^ (if s="" then "" else ",") ^ Format.sprintf "%i" i)
               execs ""))
      c.flagged
    else
       Flag.Map.iter
        (fun flag _ ->  Format.fprintf fmt "Flag %s\n" (Flag.pp flag))
        c.flagged
    end ;
    fprintf fmt "Condition %s\n" (C.do_constraints_to_string tr_out cstr) ;
    fprintf fmt "Observation %s %s %i %i\n%!"
      tname (TestResult.pp_observation c.observation) c.pos c.neg ;
    fprintf fmt "Time %s %0.2f\n" tname time ;
    if O.candidates then
      fprintf fmt "Candidates %s %i\n" tname (c.cfail+c.cands) ;
(* Auto info or Hash only*)
    List.iter
      (fun (k,v) ->
        if Misc.string_eq k "Hash" then
          fprintf fmt "%s=%s\n" k v)
      test.Test_herd.info ;

  module PP = Pretty.Make (S)

  let dump_exec_graph model test exec chan =
    let cstr = T.find_our_constraint test in
    let legend =
      let open Printf in
      let pp_flag =
        match O.show with
        | PrettyConf.ShowFlag f -> sprintf ", flag %s" f
        | _ -> "" in
      let name = Test_herd.readable_name test in
      let pp_model = sprintf "%s" (Model.pp model) in
      if O.shortlegend then name
      else if O.showkind then
        if O.PC.texmacros then
          sprintf
            "\\mylegendkind{%s}{%s}{%s}"
            name
            (C.dump_as_kind cstr)
            pp_model
        else
          sprintf "Test %s%s%s%s"
            name
            (sprintf ": %s" (C.dump_as_kind cstr))
            (match pp_model with
            | "" -> ""
            | _ -> sprintf " (%s)" pp_model)
            pp_flag
      else begin
        if O.PC.texmacros then
          sprintf
            "\\mylegend{%s}{%s}"
            name
            pp_model
        else
          sprintf "Test %s%s%s" name
            (match pp_model with
            | "" -> ""
            | _ -> sprintf ", %s" pp_model)
            pp_flag
      end
    in
    TestResult.(
      PP.dump_legend chan test legend (concrete exec)
        ~sets:(sets exec) (relations exec))
end

module Make(O:Config)(M:XXXMem.S) =
  struct
    open Printf
    module S = M.S
    module PC = S.O.PC
    module MC = Mem.Make(O)(S)
    module C = S.Cons
    module A = S.A
    module AM = A.Mixed(O)

    module T = Test_herd.Make(A)
    module W = Warn.Make(O)

    let showcutoff = O.variant Variant.CutOff

(* Utilities *)
    open Restrict

    let do_observed = match O.restrict with
    | Observed -> true
    | No|NonAmbiguous|CondOne -> false

(* Cond checking *)
    module CM = C.Mixed(O)

    let check_prop test =
      let c = T.find_our_constraint test in
      let p = ConstrGen.prop_of c in
      let senv = S.size_env test
      and tenv = S.type_env test in
      fun st -> CM.check_prop p tenv senv st

(* Test result *)
    module Count = struct
      type t =
        { states : A.StateSet.t;
          cfail : int ;
          cands : int ;
(* NB: pos and neg are w.r.t. proposition *)
          pos : int ;
          neg : int ;
(* flagged executions *)
          flagged : int list Flag.Map.t ;
(* shown executions *)
          shown : int;
(* registers that read memory *)
          reads : S.loc_set;
(* Too much loop unrolling *)
          cutoff : string option;
        }
    end

    type count = Count.t

    let start =
      let open Count in
      { states = A.StateSet.empty; cfail=0; cands=0; pos=0; neg=0;
        flagged=Flag.Map.empty; shown=0;
        reads = A.LocSet.empty; cutoff=None; }

    let bad_flag = match O.badflag with
    | None ->
        (function
          | Flag.Undef -> true
          | Flag.Flag _ -> false)
    | Some fbad ->
         (function
          | Flag.Undef -> true
          | Flag.Flag f -> String.compare f fbad = 0)

    let is_bad flags = Flag.Set.exists bad_flag flags

    let kfail c = Count.{ c with cfail=c.cfail+1; }

(* rfmap generation and processing, from pre-candidates *)
    let iter_rfms test rfms owls kont k =
      let kont =
        if O.verbose > 0 then
          fun conc k ->
            eprintf ".%!" ;
            let k = kont conc k in
            k
        else kont in
      let k =
        List.fold_left
          (fun res (_i,cs,es) ->
            MC.calculate_rf_with_cnstrnts test owls es cs
              kont res)
          k rfms in
      k

    exception Over of count (* internal use, to stop everything *)

    module PU = PrettyUtils.Make(S)

    let all_observed test conc =
      let es = conc.S.str in
      let obs = PU.observed test es
      and loads = S.E.mem_loads_of es.S.E.events in
      S.E.EventSet.subset loads obs

(* Called by model simulator in case of success *)
    let model_kont f test do_restrict =
      let open ConstrGen in

      let cstr = T.find_our_constraint test in
      let check = check_prop test in

      fun conc (st,flts) (set_pp,vbpp) flags c ->
        if do_observed && not (all_observed test conc) then c
        else if
          match O.throughflag with
          | None -> false
          | Some flag -> not (Flag.Set.mem (Flag.Flag flag) flags)
        then c
        else
          let flts =
            if O.debug.Debug_herd.pac then flts
            else A.FaultSet.map
              (A.map_fault (A.V.map_const Constant.make_canonical)) flts in
          let st =
            if O.debug.Debug_herd.pac then st
            else A.map_state (A.V.map_const Constant.make_canonical) st in
          let st = A.map_state A.V.printable st in
          let fsc = st,flts in
          let ok = check fsc in
          let show_exec =
            let open PrettyConf in
            match O.show with
            | ShowProp -> ok
            | ShowNeg -> not ok
            | ShowCond ->
                begin
                  match cstr with
                  | ExistsState _|ForallStates _-> ok
                  | NotExistsState _ -> not ok
                end
            | ShowWit ->
                begin
                  match cstr with
                  | ExistsState _|NotExistsState _ -> ok
                  | ForallStates _  -> not ok
                end
            | ShowAll -> true
            | ShowNone -> false
            | ShowFlag f -> Flag.Set.mem (Flag.Flag f) flags in
          begin if show_exec then
            let exec =
              {
                TestResult.concrete = conc;
                passes_check = ok;
                flags;
                sets = set_pp;
                rels = vbpp;
              }
            in
            f exec
          end;
          let fsc = do_restrict test fsc in
          let r =
            Count.{
              cands = c.cands+1;
              cfail = c.cfail;
              states = A.StateSet.add fsc c.states;
              pos = if ok then c.pos+1 else c.pos;
              neg = if ok then c.neg else c.neg+1;
              flagged = begin
                let add flag k =
                  let old = Flag.Map.safe_find [] flag k in
                  Flag.Map.add flag (c.cands::old) k in
                Flag.Set.fold add flags c.flagged;
              end;
              shown = if show_exec then c.shown+1 else c.shown;
              reads =
                if O.outcomereads then
                  A.LocSet.union (PU.all_regs_that_read conc.S.str) c.reads
                else c.reads;
              cutoff =  c.cutoff;
            } in
          if not O.badexecs && is_bad flags then raise (Over r) ;
          let r = match O.nshow with
          | None -> r
          | Some m ->
              if r.Count.shown >= m then raise (Over r)
              else r in
          let stop_now =
            match O.speedcheck  with
            | Speed.True|Speed.False -> false
            | Speed.Fast ->
                begin match cstr with
                | ExistsState _|NotExistsState _ -> ok
                | ForallStates _ -> not ok
                end in
          if stop_now then raise (Over r) else r

    (* Performed delayed checks and warnings *)
    let check_failed_model_kont
          cutoff cs
          f test do_restrict
          conc (st,flts) (set_pp,vbpp) flags c  =

      let open S.M.VC in
      match cs with
      | Some (Failed e) ->
         (* Perform error *)
          if O.debug.Debug_herd.top then
            model_kont
              f test do_restrict
              conc (st,flts) (set_pp,vbpp) flags c
          else raise e
      | Some (Warn msg) ->
         (* Warn and ignore *)
         Warn.warn_always "%s, legal outcomes may be missing" msg ;
         c
      | Some (Assign _)|None ->
          if not showcutoff && Misc.is_some cutoff then c
          else
            model_kont
              f test do_restrict
              conc (st,flts) (set_pp,vbpp) flags c

    type test_results = TestResult.Make(M.S).t

    (* Driver *)
    let run test =
      let { MC.event_structures=rfms; MC.overwritable_labels=owls; },test =
        MC.glommed_event_structures ~is_pgm:true test in

      let restrict_faults =
        if !Opts.dumpallfaults then
          Fun.id
        else if A.FaultAtomSet.is_empty test.Test_herd.ffaults then
          fun _ -> A.FaultSet.empty
        else
          A.FaultSet.filter
            (fun flt ->
              A.FaultAtomSet.exists
                (fun ((p,lab),loc,ftype) ->
                  A.check_one_fatom flt ((p,lab),loc,ftype))
                test.Test_herd.ffaults) in

      let final_state_restrict_locs test fsc =
        let dlocs = S.displayed_rlocations test
        and senv = S.size_env test
        and tenv = S.type_env test in
        let fsc,flts = fsc in
        AM.state_restrict_locs O.outcomereads dlocs tenv senv fsc,
        restrict_faults flts in

      let observation : 'p ConstrGen.constr -> pos:int -> neg:int -> TestResult.observation =
        let open ConstrGen in
        let open TestResult in
        match O.speedcheck with
        | Speed.False ->
            fun _test ~pos ~neg ->
              if pos = 0 then Never
              else if neg = 0 then Always
              else Sometimes
        | Speed.(True | Fast) -> (
            fun cond ->
              match cond with
              | ForallStates _ ->
                  fun ~pos ~neg ->
                    (* we have discarded all possible positive states. *)
                    assert (pos = 0);
                    (* This means that we can't answer "Never", as we don't
                       know about the positive cases. *)
                    if neg = 0 then Always else Sometimes
              | ExistsState _ | NotExistsState _ ->
                  fun ~pos ~neg ->
                    (* We have discarded all negative states. *)
                    assert (neg = 0);
                    (* This means that we can't answer 'Always', as we dont'
                       know about the negative cases. *)
                    if pos = 0 then Never else Sometimes)
      in

      let event_structures = List.map (fun (_, _, es) -> es) rfms in

      let i = fun f ->
        (* Thanks to the existence of check_test, XXMem modules
           apply their internal functors once *)
        let call_model conc ofail c =
        let check_test = M.check_event_structure test in
        (* Checked pruned executions before even calling model *)
        let cutoff =  S.find_cutoff conc.S.str.S.E.events in
        let c =
          if Misc.is_some cutoff then Count.{ c with cutoff = cutoff }
          else c in
        (* Discard pruned executions if not explicitely required *)
        check_test
          conc kfail
          (check_failed_model_kont cutoff
             ofail f test final_state_restrict_locs) c in
      let c =
        if O.statelessrc11
        then let module SL = Slrc11.Make(struct include MC let skipchecks = O.skipchecks end) in
             SL.check_event_structure test rfms kfail (fun _ c -> c)
          (model_kont f test final_state_restrict_locs) start
        else
        try iter_rfms test rfms owls call_model start
        with
        | Over c -> c
        in
(* Reduce final states, so as to show relevant locations only *)
      let finals =
        let open Count in
        if O.outcomereads then
          let do_restrict (st,flts) =
            let st =
              A.rstate_filter
                (fun rloc ->
                  let loc = ConstrGen.loc_of_rloc rloc in
                  match loc with
                  | A.Location_global _ -> true
                  | A.Location_reg _ -> A.LocSet.mem loc c.reads)
                st in
                st,flts in
          A.StateSet.map do_restrict c.states
        else c.states in
        {
          states = finals;
          TestResult.cfail = c.Count.cfail;
          TestResult.cands = c.Count.cands;
          TestResult.pos = c.Count.pos;
          TestResult.neg = c.Count.neg;
          TestResult.observation =
            observation test.Test_herd.cond ~pos:c.Count.pos ~neg:c.Count.neg;
          TestResult.flagged = c.Count.flagged;
          TestResult.cutoff = c.Count.cutoff;
        }
      in
      test, (event_structures, i)
  end
