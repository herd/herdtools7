(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
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
  val auto : bool
  val candidates : bool
  val show : PrettyConf.show
  val nshow : int option
  val restrict : Restrict.t
  val showkind : bool
  val shortlegend : bool
  val outcomereads : bool
  val outputdir : PrettyConf.outputdir_mode
  val suffix : string
  val dumpes : bool
  val badexecs : bool
  val badflag : string option
  val throughflag : string option
  include Mem.CommonConfig

  val statelessrc11 : bool
  val skipchecks : StringSet.t
end

module type Config = sig
  include CommonConfig
  val byte : MachSize.sz
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

(* Utilities *)
    open Restrict

    let do_observed = match O.restrict with
    | Observed -> true
    | No|NonAmbiguous|CondOne -> false

(* Location out printing *)
    let tr_out test = OutMapping.info_to_tr  test.Test_herd.info

(* Cond checking *)
    module CM = C.Mixed(O)

    let check_prop test =
      let c = T.find_our_constraint test in
      let p = ConstrGen.prop_of c in
      let senv = S.size_env test in
      fun st -> CM.check_prop p senv st

    let count_prop test sts =
      let c = T.find_our_constraint test in
      let p = ConstrGen.prop_of c in
      let senv = S.size_env test in
      A.StateSet.fold
        (fun st n ->
          if  CM.check_prop p senv st then n+1 else n)
        sts
        0

(* Test result *)
    type count =
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
        }

    let start =
      { states = A.StateSet.empty; cfail=0; cands=0; pos=0; neg=0;
        flagged=Flag.Map.empty; shown=0;
        reads = A.LocSet.empty; }

    let kfail c = { c with cfail=c.cfail+1; }

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

    let has_bad_execs c =
      Flag.Map.mem Flag.Undef c.flagged ||
      (match O.badflag with
      | None -> false
      | Some f -> Flag.Map.mem (Flag.Flag f) c.flagged)

(* Check condition *)
    open ConstrGen

    let check_cond test c =
      let cstr = T.find_our_constraint test in
      match cstr with
      | ExistsState _ -> c.pos > 0
      | NotExistsState _-> c.pos = 0
      | ForallStates _  -> c.neg = 0


    let check_wit test c =
      let cstr = T.find_our_constraint test in
      match cstr with
      | ForallStates _
      | ExistsState _ -> c.pos,c.neg
      | NotExistsState _-> c.neg,c.pos


(* rfmap generation and processing, from pre-candidates *)
    let iter_rfms test rfms kont kont_loop k =
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
	    MC.calculate_rf_with_cnstrnts test es cs
	      kont kont_loop res)
	  k rfms in
      k

(* Open a dot outfile or not *)
    let open_dot test =
      match O.outputdir with
      | PrettyConf.NoOutputdir ->
          if S.O.PC.gv || S.O.PC.evince then
            begin try
              let f,chan = Filename.open_temp_file "herd" ".dot" in
              Some (chan,f)
            with  Sys_error msg ->
              W.warn "Cannot create temporary file: %s" msg ;
              None
            end else None
      | PrettyConf.StdoutOutput ->
	 let fname = Test_herd.basename test in
	 fprintf stdout "\nDOTBEGIN %s\n" fname;
	 fprintf stdout "DOTCOM %s\n"
           (let module G = Show.Generator(PC) in
           G.generator) ;
	 Some (stdout, fname)
      | PrettyConf.Outputdir d ->
          let base = Test_herd.basename test in
          let base = base ^ O.suffix in
          let f = Filename.concat d base ^ ".dot" in
          try Some (open_out f,f) with
          | Sys_error msg ->
              W.warn "Cannot create %s: %s" f msg ;
              None

    let close_dot = function
      | None -> ()
      | Some (chan,fname) ->
	 match O.outputdir with
	 | PrettyConf.NoOutputdir | PrettyConf.Outputdir _ ->
            if S.O.PC.debug then eprintf "close %s\n" fname ;
            close_out chan
	 | PrettyConf.StdoutOutput ->
	    fprintf stdout "\nDOTEND %s\n" fname

    let my_remove name =
      try Sys.remove name
      with e ->
        W.warn "remove failed: %s" (Printexc.to_string e)

    let erase_dot = match S.O.PC.debug,O.outputdir with
    | false,PrettyConf.NoOutputdir -> (* Erase temp file *)
        (function Some (_,f) -> my_remove f | None -> ())
    | (_,PrettyConf.Outputdir _)|(_,PrettyConf.StdoutOutput)|(true,PrettyConf.NoOutputdir) -> (function _ -> ())

    exception Over of count (* internal use, to stop everything *)

    module PU = PrettyUtils.Make(S)

    let all_observed test conc =
      let es = conc.S.str in
      let obs = PU.observed test es
      and loads = S.E.mem_loads_of es.S.E.events in
      S.E.EventSet.subset loads obs

(* Called by model simulator in case of success *)
    let model_kont ochan test cstr =
      let check = check_prop test in
      fun conc fsc vbpp flags c ->
        if do_observed && not (all_observed test conc) then c
        else if
          match O.throughflag with
          | None -> false
          | Some flag -> not (Flag.Set.mem (Flag.Flag flag) flags)
        then c
        else
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

          begin match ochan with
          | Some (chan,_) when show_exec ->
              let legend =
                let pp_flag = match O.show with
                | PrettyConf.ShowFlag f -> sprintf ", flag %s" f
                | _ -> "" in
                let name = Test_herd.readable_name test in
                let pp_model = match M.model with
                | Model.Minimal false -> ""
                | _ -> sprintf "%s" (Model.pp M.model) in
                if O.shortlegend then name
                else if O.showkind then
                  if PC.texmacros then
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
                  if PC.texmacros then
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
                end in
              let module PP = Pretty.Make(S) in
              PP.dump_legend chan test legend conc (Lazy.force vbpp)
          | _ -> ()
          end ;
          let fsc =
            if O.outcomereads then fsc
            else begin
              let dlocs = S.displayed_locations test
              and senv = S.size_env test in
              AM.state_restrict_locs dlocs senv fsc
            end in
          let r =
            { cands = c.cands+1;
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
            } in
          if not O.badexecs && is_bad flags then raise (Over r) ;
          let r = match O.nshow with
          | None -> r
          | Some m ->
              if r.shown >= m then raise (Over r)
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


(* Driver *)
    let run start_time test =
      let cstr = T.find_our_constraint test in
      let { MC.event_structures=rfms; too_far=loop; } =
        MC.glommed_event_structures test in
(* Open *)
      let ochan = open_dot test in
(* So small a race condition... *)
      Handler.push (fun () -> erase_dot ochan) ;
(* Dump event structures ... *)
      if O.dumpes then begin
        match ochan with
        | None -> ()
        | Some (chan,fname) ->
            let module PP = Pretty.Make(S) in
            List.iter
              (fun (_i,_cs,es) -> PP.dump_es chan test es)
              rfms ;
            close_dot ochan ;
            if S.O.PC.gv || S.O.PC.evince then begin
              let module SH = Show.Make(S.O.PC) in
              SH.show_file fname
            end ;
            erase_dot ochan ;
            Handler.pop ()
      end else
        (* Thanks to the existence of check_test, XXMem modules
           apply their internal functors once *)
        let check_test =
            M.check_event_structure test in
        let call_model conc =
          check_test
            conc kfail (model_kont ochan test cstr) in
      let c =
        if O.statelessrc11
        then let module SL = Slrc11.Make(struct include MC let skipchecks = O.skipchecks end) in
             SL.check_event_structure test rfms kfail (fun _ c -> c) (model_kont ochan test cstr) start
        else
        try iter_rfms test rfms call_model (fun c -> c) start
        with
        | Over c -> c
        | e ->
            close_dot ochan  ; (* Close *)
            raise e in
(* Close *)
      close_dot ochan ;
      let do_show () =
(* Show if something to show *)
        begin match ochan with
        | Some (_,fname) when c.shown > 0 ->
            let module SH = Show.Make(S.O.PC) in
            if S.O.PC.debug then eprintf "show %s file\n" fname ;
            SH.show_file fname
        | Some _|None -> ()
        end ;
(* Erase *)
        erase_dot ochan ;
        Handler.pop () in
(* Reduce final states, so as to show relevant locations only *)
      let finals =
        if O.outcomereads then
          let locs =
            A.LocSet.union
              (S.displayed_locations test)
              c.reads in
          let senv  = S.size_env test in
          A.StateSet.map
            (fun st -> AM.state_restrict_locs locs senv st)
            c.states
        else c.states in
      let nfinals = A.StateSet.cardinal finals in
      match O.restrict with
      | Observed when c.cands = 0 -> do_show ()
      | NonAmbiguous when c.cands <> nfinals -> do_show ()
      | CondOne when c.pos <> count_prop test finals -> do_show ()
      | _ ->
        try begin
(* Header *)
        let tname = test.Test_herd.name.Name.name in
        let is_bad = has_bad_execs c in
        if not O.badexecs &&  is_bad then raise Exit ;
        printf "Test %s %s\n" tname (C.dump_as_kind cstr) ;
(**********)
(* States *)
(**********)
        let tr_out = tr_out test in
        printf "States %i\n" nfinals ;
        A.StateSet.pp stdout ""
          (fun chan st ->  fprintf chan "%s\n" (A.do_dump_state tr_out st))
          finals ;
(* Condition result *)
        let ok = check_cond test c in
        printf "%s%s\n"
          (if loop then "Loop " else "")
          (if is_bad then "Undef" else if ok then "Ok" else "No") ;
        let pos,neg = check_wit test c in
        printf "Witnesses\n" ;
        printf "Positive: %i Negative: %i\n" pos neg ;
        begin if O.verbose > 0 then
          Flag.Map.iter
            (fun flag execs ->
              printf "Flag %s: %s \n"
                (Flag.pp flag)
                (List.fold_right
                   (fun i s -> s ^ (if s="" then "" else ",") ^ sprintf "%i" i)
                   execs ""))
          c.flagged
        else
           Flag.Map.iter
            (fun flag _ ->  printf "Flag %s\n" (Flag.pp flag))
            c.flagged
        end ;
        printf "Condition %a\n" (C.do_dump_constraints tr_out) cstr ;
        printf "Observation %s %s %i %i\n%!" tname
          (if c.pos = 0 then "Never"
          else if c.neg = 0 then "Always"
          else "Sometimes") c.pos c.neg ;
        do_show () ;
        printf "Time %s %0.2f\n" tname (Sys.time () -. start_time) ;
        if O.candidates then
          printf "Candidates %s %i\n" tname (c.cfail+c.cands) ;
(* Auto info or Hash only*)
        if O.auto then begin
          List.iter
            (fun (k,v) ->
              if Misc.string_eq k "Relax" then
                printf "Relax %s %s %s\n"
                  tname
                  (if ok then "Ok" else "No") v
              else if Misc.string_eq k "Cycle" ||  Misc.string_eq k "Safe" then
                fprintf stdout "%s=%s\n" k v)
            test.Test_herd.info
        end  else
          List.iter
            (fun (k,v) ->
              if Misc.string_eq k "Hash" then
                printf "%s=%s\n" k v)
            test.Test_herd.info ;
        print_newline ()
      end with Exit -> () ;
      ()
  end
