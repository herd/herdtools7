(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Top level loop : execute test according to model *)

module type Config = sig
  val auto : bool
  val show : PrettyConf.show
  val nshow : int option
  val restrict : Restrict.t
  val showkind : bool
  val shortlegend : bool
  val outcomereads : bool
  val outputdir : string option
  val suffix : string
  val dumpes : bool
  val dumplem : bool
  val dumptex : bool
  val badexecs : bool
  include Mem.Config
end

module Make(O:Config)(M:XXXMem.S) = 
  struct
    open Printf
    module S = M.S
    module PC = S.O.PC
    module MC = Mem.Make(O)(S)
    module C = S.C
    module A = S.A
    module T = Test.Make(A)
    module W = Warn.Make(O)

(* Utilities *)
    open Restrict

    let do_observed = match O.restrict with
    | Observed -> true
    | No|NonAmbiguous|CondOne -> false

(* Cond checking *)
    let check_prop test st =
      let c = T.find_our_constraint test in
      let p = ConstrGen.prop_of c in
      C.check_prop p st

    let count_prop test sts =
      let c = T.find_our_constraint test in
      let p = ConstrGen.prop_of c in
      A.StateSet.fold
        (fun st n ->
          if  C.check_prop p st then n+1 else n)
        sts
        0

    let report_bad_executions = ref false
    (* Identifies the current execution. The use of mutable state
       is not very nice. I need to find another way to find out
       which execution is currently being tested. *)
    let execution_idx = ref 0

(* Test result *)
    type count =
        { states : A.StateSet.t;
          cands : int ;
(* NB: pos and neg are w.r.t. proposition *)
          pos : int ;
          neg : int ;
(* list of executions that fail at least one requires-clause *)
	  bad : int list ;
(* shown executions *)
          shown : int;
(* registers that read memory *)
          reads : S.loc_set;
        }

    let start =
      { states = A.StateSet.empty; cands=0; pos=0; neg=0; bad=[]; shown=0;
        reads = A.LocSet.empty; }

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
      | None ->
          if S.O.PC.gv || S.O.PC.evince then
            begin try
              let f,chan = Filename.open_temp_file "herd" ".dot" in
              Some (chan,f)
            with  Sys_error msg ->
              W.warn "Cannot create temporary file: %s" msg ;
              None
            end else None
      | Some d ->
          let base = Test.basename test in
          let base = base ^ O.suffix in
          let f = Filename.concat d base ^ ".dot" in
          try Some (open_out f,f) with
          | Sys_error msg ->
              W.warn "Cannot create %s: %s" f msg ;
              None

    let close_dot = function
      | None -> ()
      | Some (chan,_) -> close_out chan

    let my_remove name =
      try Sys.remove name
      with e ->
        W.warn "remove failed: %s" (Printexc.to_string e)
          
    let erase_dot = match O.outputdir with
    | None -> (* Erase temp file *)
        (function Some (_,f) -> my_remove f | None -> ())
    | Some _ -> (function _ -> ())

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
      fun conc fsc vbpp failed_requires_clauses c ->
        if do_observed && not (all_observed test conc) then c
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
            | ShowNone -> false in
          begin match ochan with
          | Some (chan,_) when show_exec ->            
              let legend = 
                let name = Test.readable_name test in
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
                    sprintf "Test %s%s%s"
                      name
                      (sprintf ": %s" (C.dump_as_kind cstr))
                      (match pp_model with
                      | "" -> ""
                      | _ -> sprintf " (%s)" pp_model)
                else begin
                  if PC.texmacros then
                    sprintf
                      "\\mylegend{%s}{%s}"
                      name
                      pp_model
                  else
                    sprintf "Test %s%s" name
                      (match pp_model with
                      | "" -> ""
                      | _ -> sprintf ", %s" pp_model)
                end in
              let module PP = Pretty.Make(S) in
              PP.dump_legend chan test legend conc (Lazy.force vbpp)
          | _ -> ()
          end ;
	  report_bad_executions := (failed_requires_clauses != None);
          execution_idx := (!execution_idx) + 1;
          let r =
            { cands = c.cands+1;
              states = A.StateSet.add fsc c.states;
              pos = if ok then c.pos+1 else c.pos;
              neg = if ok then c.neg else c.neg+1;
              bad = (match failed_requires_clauses with 
		    | Some n when n > 0 -> ((!execution_idx) :: c.bad) 
		    | _ -> c.bad);
              shown = if show_exec then c.shown+1 else c.shown;
              reads = 
                if O.outcomereads then
                  A.LocSet.union (PU.all_regs_that_read conc.S.str) c.reads
                else c.reads;
            } in
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
    let run test =
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
      let call_model conc =
        M.check_event_structure
          test conc (model_kont ochan test cstr) in
      let c =
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
            SH.show_file fname 
        | Some _|None -> ()
        end ;
(* Erase *)
        erase_dot ochan ;
        Handler.pop () in
(* Reduce final states, so as to show relevant locations only *)
      let finals =
        let locs = 
          A.LocSet.union
            (S.outcome_locations test)
            c.reads in
        A.StateSet.map
          (fun st ->
            A.LocSet.fold
              (fun loc r -> A.state_add r loc (A.look_in_state st loc))
              locs A.state_empty)
          c.states in
      let nfinals = A.StateSet.cardinal finals in
      match O.restrict with
      | Observed when c.cands = 0 -> do_show ()
      | NonAmbiguous when c.cands <> nfinals -> do_show ()
      | CondOne when c.pos <> count_prop test finals -> do_show ()
      | _ ->
        try begin
(* Header *)
        let tname = test.Test.name.Name.name in
        if not O.badexecs &&  c.bad <> [] then raise Exit ;
        printf "Test %s %s\n" tname (C.dump_as_kind cstr) ;        
(**********)
(* States *)
(**********)
        printf "States %i\n" nfinals ;
        A.StateSet.pp stdout ""
          (fun chan st ->  fprintf chan "%s\n" (A.dump_state st))
          finals ;
(* Condition result *)
        let ok = check_cond test c in
        printf "%s%s\n" (if loop then "Loop " else "") (if ok then "Ok" else "No") ;
        let pos,neg = check_wit test c in
        printf "Witnesses\n" ;
        printf "Positive: %i Negative: %i\n" pos neg ;
	if (!report_bad_executions) then begin
          printf "Bad executions (%i in total): %s \n" 
            (List.length c.bad) 
            (List.fold_right 
               (fun i s -> s ^ (if s="" then "" else ",") ^ sprintf "%i" i) 
               c.bad "")
        end;
        printf "Condition %a\n" C.dump_constraints cstr ;
        printf "Observation %s %s %i %i\n%!" tname
          (if c.pos = 0 then "Never"
          else if c.neg = 0 then "Always"
          else "Sometimes") c.pos c.neg ;
        do_show () ;
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
            test.Test.info
        end  else 
          List.iter
            (fun (k,v) ->
              if Misc.string_eq k "Hash" then
                printf "%s=%s\n" k v)
            test.Test.info ;
        print_newline ()
      end with Exit -> () ;
      ()
  end
