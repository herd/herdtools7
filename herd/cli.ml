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
          if O.PC.debug then Printf.eprintf "close %s\n" fname ;
          close_out chan
       | PrettyConf.StdoutOutput ->
          Printf.fprintf stdout "\nDOTEND %s\n" fname

  let my_remove name =
    try Sys.remove name
    with e ->
      Warn.warn_always "remove failed: %s" (Printexc.to_string e)

  let erase_dot = match O.PC.debug, O.outputdir with
  | false,PrettyConf.NoOutputdir -> (* Erase temp file *)
      (function Some (_,f) -> my_remove f | None -> ())
  | (_,PrettyConf.Outputdir _)|(_,PrettyConf.StdoutOutput)|(true,PrettyConf.NoOutputdir) -> (function _ -> ())

  let dump_results ~start_time (module R : RunTest.Outcome) =
    let open R in
    let module S = M.S in
    let module A = S.A in
    let module T = Test_herd.Make (S.A) in
    let module PP = Top_herd.Printer (O) (S) in
    let open ConstrGen in
    let event_structures, i = result in

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
      try iter_count i dump_graph
      with e -> close_dot ochan; raise e
    in
(* Close *)
    close_dot ochan ;
    let do_show () =
(* Show if something to show *)
      begin match ochan with
      | Some (_,fname) when shown > 0 ->
          let module SH = Show.Make(S.O.PC) in
          if O.PC.debug then Printf.eprintf "show %s file\n" fname ;
          SH.show_file fname
      | Some _|None -> ()
      end ;
(* Erase *)
      erase_dot ochan ;
      Handler.pop ()
    in
    let finals = TR.states c in
    let nfinals = A.StateSet.cardinal finals in
    let count_prop () =
      let sz = T.compute_size O.byte test in
      let module CM = S.Cons.Mixed (struct let byte = sz end) in
      let cstr = T.find_our_constraint test in
      let p = ConstrGen.prop_of cstr in
      A.StateSet.fold
        (fun st n ->
          if CM.check_prop_rlocs p (S.type_env test) st then n + 1 else n)
        finals 0
    in
    match O.restrict with
    | Restrict.Observed when TR.candidates c = 0 -> do_show ()
    | Restrict.NonAmbiguous when TR.candidates c <> nfinals -> do_show ()
    | Restrict.CondOne when TR.positive c <> count_prop () -> do_show ()
    | _ ->
(* Header *)
      if not O.badexecs && TR.has_bad_execs ~badflag:O.badflag c then ()
      else
(* Stop interval timer *)
        Itimer.stop O.timeout ;
(* Now output *)
        let time = Sys.time () -. start_time in
        Format.printf "%a\n" (fun fmt () -> PP.pp_stats ~time test c fmt) ();
        do_show ();
        begin
          match TR.cutoff c with
          | Some msg ->
              Warn.warn_always
                "%a: unrolling limit exceeded at %s, legal outcomes may be missing."
                Pos.pp_pos0   test.Test_herd.name.Name.file
                msg
          | None -> ()
        end

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
