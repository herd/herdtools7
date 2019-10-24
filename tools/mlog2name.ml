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

(** Extract list of names from log(s), only names with successfull outcomes
    are listed *)
open Printf
open LogState

let verbose = ref 0
let logs = ref []
let acceptempty = ref false

let options =
  [  
  ("-q", Arg.Unit (fun _ -> verbose := -1),
   "<non-default> be silent");  
  ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
   ("-acceptempty", Arg.Bool (fun b -> acceptempty := b),
    sprintf
      "<bool> output empty conditions, default %b" !acceptempty);
   ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mlog2name"

let () =
  Arg.parse options
    (fun s -> logs := !logs @ [s])
    (sprintf "Usage %s [options]* [log]
log is a log file names.
Options are:" prog)

let verbose = !verbose
let acceptempty = !acceptempty

module Verbose = struct let verbose = verbose end

module LS = LogState.Make(Verbose)

module LL =
  LexLog_tools.Make
    (struct
      let verbose = verbose
      let rename n = n
      let ok _ = true
      let hexa = false
      let int32 = true
      let acceptBig = true
    end)

let add_name =
  if acceptempty then fun k st -> st.LogState.s_tname::k
  else
    fun k st ->
      if LS.is_empty_simple st then k
      else  st.LogState.s_tname::k

        let get_names log =
  let xs = List.fold_left add_name [] log.LogState.s_tests in
  StringSet.of_list xs

let zyva_names names =
  let logs = LL.read_names_simple names in
  StringSet.unions (List.rev_map get_names logs)

let zyva_stdin () =
  let log = LL.read_chan_simple "stdin" stdin in
   get_names log

let zyva names = match names with
| [] -> zyva_stdin ()
| _::_ -> zyva_names names

let () =
  try
    let names = zyva !logs in
    StringSet.iter print_endline names
  with Misc.Fatal msg|Misc.UserError msg ->
    eprintf "Fatal error: %s\n%!" msg

