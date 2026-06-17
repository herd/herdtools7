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

module Make (O : sig
  include RunTest.Config
  include Top_herd.PrinterConfig
  val dumpes : bool
end) = struct
  module G = Show.Generator (O.PC)

  let within_dot fname f =
    Printf.fprintf stdout "\nDOTBEGIN %s\n" fname;
    Printf.fprintf stdout "DOTCOM %s\n" (G.generator);
    let result = f () in
    Printf.fprintf stdout "\nDOTEND %s\n" fname;
    result

  (* TODO: Eventually, downstream JS code should just receive pretty-printed
     stats and DOT graphs directly from this function as strings, instead of
     having those strings printed to stdout only to then parse them back later.
   *)
  let dump_results ~start_time (module R : RunTest.Outcome) =
    let module M = R.M in
    let module Printer = Top_herd.Printer (O) (M.S) in
    let test = R.test in
    let result = R.result in
    let fname = Test_herd.basename test in
    if O.dumpes then
      within_dot fname (fun () ->
        let module PP = Pretty.Make (M.S) in
        result.TR.event_structures |> List.iter (PP.dump_es stdout test))
    else
      let c =
        within_dot fname (fun () ->
          let dump_graph exec = Printer.dump_exec_graph M.model test exec stdout in
          result.TR.exec_iter dump_graph)
      in
      if not O.badexecs && TR.has_bad_execs ~badflag:O.badflag c then ()
      else begin
        let time = Sys.time () -. start_time in
        Format.printf "%a@."
          (fun fmt () -> Printer.pp_stats ~time test c fmt) ();
        match TR.cutoff c with
        | Some msg ->
            Warn.warn_always
              "%a: unrolling limit exceeded at %s, legal outcomes may be missing."
              Pos.pp_pos0 test.Test_herd.name.Name.file msg
        | None -> ()
      end

  let from_file f env =
    let module T = ParseTest.Top (struct
      include O
      let collect_graph_data = true
    end) in
    let start_time = Sys.time () in
    let env, result = T.from_file f env in
    begin match result with
    | Some result -> dump_results ~start_time result
    | None -> ()
    end;
    env
end
