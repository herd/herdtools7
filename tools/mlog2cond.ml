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


open Printf
open LogState

let verbose = ref 0
let logs = ref []
let forall = ref false
let optcond = ref false
let acceptempty = ref false
let hexa = ref false

let options =
  [
  
  ("-q", Arg.Unit (fun _ -> verbose := -1),
   "<non-default> be silent");  
  ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
   ("-forall", Arg.Bool (fun b -> forall := b),
    sprintf
      "<bool> use forall quantifier in place of exists, default %b" !forall);
   ("-optcond", Arg.Bool (fun b -> optcond := b),
    sprintf
      "<bool> optimise conditions, default %b" !optcond);
   ("-acceptempty", Arg.Bool (fun b -> acceptempty := b),
    sprintf
      "<bool> output enmpty conditions, default %b" !acceptempty);
    CheckName.parse_hexa hexa;
  ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mlog2cond"

let () =
  Arg.parse options
    (fun s -> logs := !logs @ [s])
    (sprintf "Usage %s [options]* [log]
log is a log file names.
Options are:" prog)

let verbose = !verbose
let hexa = !hexa
let log = match !logs with
| [log;] -> Some log
| [] -> None
| _ ->
    eprintf "%s takes one argument\n" prog ;
    exit 2

module Verbose = struct let verbose = verbose end

let do_rename name = name
let select_name = fun _ -> true

module LS = LogState.Make(Verbose)
module LL =
  LexLog.Make
    (struct
      let verbose = verbose
      let rename = do_rename
      let ok = select_name
      let hexa = hexa
    end)

let acceptempty = !acceptempty
let quant = if !forall then "forall" else "exists"
let zyva log =
  let test = match log with
  | None -> LL.read_chan "stdin" stdin
  | Some log -> LL.read_name log in
  
(* Dumping of condition *)
  let pp_cond =
    if !optcond then CondPP.pp_opt
    else CondPP.pp_simple in

  let pp_cond name bdss =
    let pp = pp_cond bdss in
    sprintf "%s \"%s %s\"" name quant pp in

  let dump_test chan t =
    let bdss = LS.get_bindings t.states in
    match bdss with
    | [] ->
        if acceptempty then
          fprintf chan "%s \"%s false\"\n" t.tname quant
    | _ ->
        fprintf chan "%s\n" (pp_cond t.tname bdss) in

  let dump_log chan =  Array.iter (dump_test chan) in

  dump_log stdout test.tests ;
  flush stdout ;
  ()

let () =
  try zyva log
  with Misc.Fatal msg ->
    eprintf "Fatal error: %s\n%!" msg

