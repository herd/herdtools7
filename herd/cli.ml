(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Herd_core
module TR = Top_herd.TestResult

let iter_count (i : ('a -> unit) -> 'b) : ('a -> unit) -> int * 'b =
  fun f ->
    let c = ref 0 in
    let b = i (fun x -> f x; c := !c + 1) in
    !c, b

module Make (O : sig
  include RunTest.Config
  include Top_herd.PrinterConfig
  val timeout : float option
  val outputdir : PrettyConf.outputdir_mode
  val output_format : PrettyConf.output_format
  val invoked_with_cli : string list option
  val suffix : string
  val dumpes : bool
end) = struct
  module PC = O.PC

(* Open a dot outfile or not *)
  let open_dot test =
    match O.outputdir with
    | PrettyConf.NoOutputdir ->
       begin
         match O.PC.view with
         | Some _ ->
          begin try
            let f,chan = Filename.open_temp_file "herd" ".dot" in
            Some (chan,f)
          with  Sys_error msg ->
            Warn.warn_always "Cannot create temporary file: %s" msg ;
            None
          end
         | None -> None
       end
    | PrettyConf.StdoutOutput ->
       let fname = Test_herd.basename test in
       Printf.fprintf stdout "\nDOTBEGIN %s\n" fname;
       Printf.fprintf stdout "DOTCOM %s\n"
         (let module G = Show.Generator(PC) in
         G.generator) ;
       Some (stdout, fname)
    | PrettyConf.Outputdir d ->
        let base = Test_herd.basename test in
        let base = base ^ O.suffix in
        let f = Filename.concat d base ^ ".dot" in
        try Some (open_out f,f) with
        | Sys_error msg ->
            Warn.warn_always "Cannot create %s: %s" f msg ;
            None

  let close_dot = function
    | None -> ()
    | Some (chan,fname) ->
       match O.outputdir with
       | PrettyConf.NoOutputdir | PrettyConf.Outputdir _ ->
          if O.PC.debug then Printf.eprintf "close %s\n%!" fname ;
          close_out chan
       | PrettyConf.StdoutOutput ->
          Printf.fprintf stdout "\nDOTEND %s\n" fname

  let json_output_file test =
    let base = Test_herd.basename test ^ O.suffix in
    match O.outputdir with
    | PrettyConf.Outputdir dir ->
        Some (`File (Filename.concat dir (base ^ ".json")))
    | PrettyConf.StdoutOutput -> Some (`Stdout base)
    | PrettyConf.NoOutputdir -> None

  let write_json_output test json =
    match json_output_file test with
    | None -> ()
    | Some (`Stdout base) ->
        Printf.printf "\nJSONBEGIN %s\n" base;
        print_endline (Json.pretty_to_string json);
        Printf.printf "JSONEND %s\n" base
    | Some (`File file) ->
        begin try
          let chan = open_out file in
          Fun.protect
            ~finally:(fun () -> close_out chan)
            (fun () -> output_string chan (Json.pretty_to_string json);
                       output_char chan '\n')
        with Sys_error msg -> Warn.warn_always "Cannot create %s: %s" file msg
        end

  let warn_cutoff test c =
    match TR.cutoff c with
    | Some msg ->
        Warn.warn_always
          "%a: unrolling limit exceeded at %s, legal outcomes may be missing."
          Pos.pp_pos0 test.Test_herd.name.Name.file msg
    | None -> ()

  let dump_json_results ~start_time (module R : RunTest.Outcome) =
    let open R in
    let module S = M.S in
    let module A = S.A in
    let module PP = Top_herd.Printer (O) (S) in
    let json_test test =
      let module C = S.Cons in
      let instruction_rows code =
        let rec instruction labels rows = function
          | A.Label (label, rest) ->
              instruction (Label.pp label :: labels) rows rest
          | A.Instruction (_, ins) ->
              let labels =
                if labels = [] then []
                else ["labels", Json.list (List.rev_map Json.string labels)] in
              let row = Json.assoc
                (["static_poi", Json.int ins.A.CodeInstr.static_poi;
                  "instruction", Json.string
                    (A.pp_instruction PPMode.Ascii ins.A.CodeInstr.instr)] @ labels) in
              [], row :: rows
          | A.Nop | A.Symbolic _ | A.Macro _ | A.Pagealign | A.Skip _ ->
              labels, rows in
        let labels, rows = List.fold_left
          (fun (labels, rows) pseudo -> instruction labels rows pseudo)
          ([], []) code in
        (* Trailing labels have no instruction or static program-order index. *)
        let rows = match labels with
          | [] -> rows
          | _ ->
              Json.assoc
                ["labels", Json.list (List.rev_map Json.string labels)] :: rows in
        List.rev rows in
      let program =
        List.map
          (fun (proc, code) ->
            let function_name = match MiscParser.proc_func proc with
            | MiscParser.Main -> "main"
            | MiscParser.FaultHandler -> "fault_handler" in
            Json.assoc
              ["proc", Json.int (MiscParser.proc_num proc);
               "function", Json.string function_name;
               "instructions", Json.list (instruction_rows code)])
          test.Test_herd.annotated_prog in
      let tr_out = OutMapping.info_to_tr test.Test_herd.info in
      let filter = match test.Test_herd.filter with
      | None -> []
      | Some prop ->
          ["filter", Json.string
             (C.do_constraints_to_string tr_out
                (ConstrGen.ExistsState prop))] in
      Json.assoc
        (["name", Json.string test.Test_herd.name.Name.name;
         "kind", Json.string (C.dump_as_kind test.Test_herd.cond);
         "architecture", Json.string (Archs.pp test.Test_herd.arch);
         "info", Json.list
           (List.map
              (fun (key,value) ->
                Json.assoc
                  ["key", Json.string key; "value", Json.string value])
              test.Test_herd.info);
         "program", Json.list program;
         "init", Json.string (A.dump_state test.Test_herd.init_state);
         "condition", Json.string
           (C.do_constraints_to_string tr_out test.Test_herd.cond)] @ filter) in
    let executions = ref [] in
    let collect_execution exec =
      if O.outputdir <> PrettyConf.NoOutputdir then
        executions := exec :: !executions in
    let execution_graph_count, c = iter_count result.TR.exec_iter collect_execution in
    let outcome =
      if TR.positive c = 0 then "Never"
      else if TR.negative c = 0 then "Always"
      else "Sometimes" in
    let states = A.StateSet.elements (TR.states c) in
    let state_ids =
      List.mapi
        (fun i state ->
          PP.dump_final_state test state, Printf.sprintf "state-%i" i)
        states in
    (* The aggregate outcomes already contain the final -outcomereads
       projection. Use their locations to apply it to individual executions. *)
    let displayed_locations =
      List.fold_left
        (fun locs (state,_,_) ->
          List.fold_left
            (fun locs (loc,_) -> A.RLocSet.add loc locs)
            locs (A.rstate_to_list state))
        A.RLocSet.empty states in
    let graphs =
      List.mapi
        (fun i exec ->
          let state,faults,solver = TR.final_state exec in
          let state =
            A.rstate_filter
              (fun loc -> A.RLocSet.mem loc displayed_locations) state in
          let state_text = PP.dump_final_state test (state,faults,solver) in
          let final_state_id = List.assoc state_text state_ids in
          let module Pretty = Pretty.Make(S) in
          Pretty.Json.graph ~id:(Printf.sprintf "execution-%i" i)
            ~is_valid:(TR.is_valid exec)
            ~satisfies_post_condition:(TR.passes_check exec)
            ~final_state_id (TR.concrete exec) (TR.relations exec))
        (List.rev !executions) in
    let final_states =
      List.map
        (fun (value,id) ->
          Json.assoc ["id", Json.string id; "value", Json.string value])
        state_ids in
    let module TRS = TR.Make(S) in
    let result_json = Json.assoc
      ((["model", Json.string (Model.pp M.model);
       "verdict", Json.string (PP.verdict test c);
       "observation", Json.string outcome;
       "positive", Json.int (TR.positive c);
       "negative", Json.int (TR.negative c);
       "candidates", Json.int (TR.candidates c);
       "failed_candidates", Json.int (TR.failed_candidates c);
       "execution_graph_count", Json.int execution_graph_count;
       "final_states", Json.list final_states]) @
       (match O.invoked_with_cli with
       | None -> []
       | Some args ->
           ["invoked_with_cli", Json.list (List.map Json.string args)])) in
    let json = Json.assoc
      ["schema_version", Json.int 1;
       "test", json_test test;
       "result", result_json;
       "execution_graphs", Json.list graphs] in
    let suppress = match O.restrict with
    | Restrict.Observed -> TR.candidates c = 0
    | Restrict.NonAmbiguous ->
        TR.candidates c <> A.StateSet.cardinal (TR.states c)
    | Restrict.CondOne ->
        TR.positive c <> TRS.count_prop ~byte:O.byte test c
    | Restrict.No -> false in
    if not suppress &&
       not (not O.badexecs && TR.has_bad_execs ~badflag:O.badflag c) then begin
          Itimer.stop O.timeout;
          let time = Sys.time () -. start_time in
          Format.printf "%a@." (fun fmt () -> PP.pp_stats ~time test c fmt) ();
          if O.debug.Debug_herd.timers then
            Format.printf "Timers: %a, %a, %a@."
              O.Timer.pp O.Timer.run
              O.Timer.pp O.Timer.semantics
              O.Timer.pp O.Timer.model;
          write_json_output test json;
          warn_cutoff test c
    end

  let my_remove name =
    try Sys.remove name
    with e ->
      Warn.warn_always "remove failed: %s" (Printexc.to_string e)

  let erase_dot = match O.PC.debug, O.outputdir with
  | false,PrettyConf.NoOutputdir -> (* Erase temp file *)
      (function Some (_,f) -> my_remove f | None -> ())
  | (_,PrettyConf.Outputdir _)|(_,PrettyConf.StdoutOutput)|(true,PrettyConf.NoOutputdir) -> (function _ -> ())

  let dump_results ~start_time (module R : RunTest.Outcome) =
    if O.output_format = PrettyConf.Json then
      dump_json_results ~start_time (module R)
    else
    let open R in
    let module S = M.S in
    let module A = S.A in
    let module T = Test_herd.Make (S.A) in
    let module PP = Top_herd.Printer (O) (S) in
    let open ConstrGen in
    let event_structures = result.TR.event_structures in

(* Open *)
    let ochan = open_dot test in
(* So small a race condition... *)
    Handler.push (fun () -> erase_dot ochan) ;
(* Dump event structures ... *)
    if O.dumpes then begin
      match ochan with
      | None -> ()
      | Some (chan, fname) ->
          let module PP = Pretty.Make(S) in
          List.iter
            (fun es -> PP.dump_es chan test es)
            event_structures ;
          close_dot ochan ;
          if Misc.is_some S.O.PC.view then begin
            let module SH = Show.Make(S.O.PC) in
            SH.show_file fname
          end ;
          erase_dot ochan ;
          Handler.pop ()
    end else
    let dump_graph =
      match ochan with
        | Some (chan, _) -> fun exec -> PP.dump_exec_graph M.model test exec chan
        | None -> fun _ -> ()
    in
    let shown, c =
      try iter_count result.TR.exec_iter dump_graph
      with e -> close_dot ochan; raise e
    in
(* Close *)
    close_dot ochan ;
    let do_show () =
(* Show if something to show *)
      begin match ochan with
      | Some (_,fname) when shown > 0 ->
          let module SH = Show.Make(S.O.PC) in
          if O.PC.debug then Printf.eprintf "show %s file\n%!" fname ;
          SH.show_file fname
      | Some _|None -> ()
      end ;
(* Erase *)
      erase_dot ochan ;
      Handler.pop ()
    in
    let finals = TR.states c in
    let nfinals = A.StateSet.cardinal finals in
    let module TRS = TR.Make (S) in
    match O.restrict with
    | Restrict.Observed when TR.candidates c = 0 -> do_show ()
    | Restrict.NonAmbiguous when TR.candidates c <> nfinals -> do_show ()
    | Restrict.CondOne when TR.positive c <> TRS.count_prop ~byte:O.byte test c ->
        do_show ()
    | _ ->
(* Header *)
      if not O.badexecs && TR.has_bad_execs ~badflag:O.badflag c then ()
      else
(* Stop interval timer *)
        Itimer.stop O.timeout ;
(* Now output *)
        let time = Sys.time () -. start_time in
        Format.printf "%a@." (fun fmt () -> PP.pp_stats ~time test c fmt) ();
        if O.debug.Debug_herd.timers then
          Format.printf "Timers: %a, %a, %a@."
            O.Timer.pp O.Timer.run
            O.Timer.pp O.Timer.semantics
            O.Timer.pp O.Timer.model;
        do_show ();
        warn_cutoff test c

  let collect_graph_data = match O.outputdir with
    | PrettyConf.StdoutOutput | PrettyConf.Outputdir _ -> true
    | _ -> false

  let from_file f env =
    let module T = ParseTest.Top (struct
      include O
      let collect_graph_data = collect_graph_data
    end) in
(* Interval timer will be stopped just before output, see dump_results *)
    Itimer.start f O.timeout ;
    let start_time = Sys.time () in
    let env, result = T.from_file f env in
    begin match result with
      | Some result -> dump_results ~start_time result
      | None -> ()
    end;
    env
end
