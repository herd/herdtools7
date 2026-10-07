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

let verbose = ref 0
let logs = ref []
let hexa = ref false
let int32 = ref true
let faulttype = ref true
let datafault = ref true

let options =
  LibOpts.parse_verbose verbose
  @ [
  ToolsOpts.parse_hexa hexa;
  ToolsOpts.parse_int32 int32;
  ToolsOpts.parse_faulttype faulttype;
  ToolsOpts.parse_datafault datafault;
  ]@OptNames.parse_withselect


let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "moutcome"

let () =
  Arg.parse options
    (fun s -> logs := !logs @ [s])
    (sprintf "Usage %s [options]* log
log is a log file names.
Options are:" prog)

open OptNames

let select = !select
let names = !names
let oknames = !oknames
let excl = !excl
let nonames = !nonames
let verbose = !verbose
let hexa = !hexa
let int32 = !int32

let log = match !logs with
| [log;] -> Some log
| [] -> None
| _ ->
    eprintf "%s takes at most one argument\n" prog ;
    exit 2
let faulttype = !faulttype
let datafault = !datafault

module Verbose = struct let verbose = verbose end

module LS = LogState.Make(Verbose)
module LL =
  LexLog_tools.Make
    (struct
      let verbose = verbose
      include CheckName.Make
          (struct
            let verbose = verbose
            let rename = []
            let select = select
            let names = names
            let oknames = oknames
            let excl = excl
            let nonames = nonames
          end)
      let hexa = hexa
      let int32 = int32
      let acceptBig = true
      let faulttype = faulttype
      let datafault = datafault
    end)


let zyva log =
  let test = match log with
  | None -> LL.read_chan "stdin" stdin
  | Some log -> LL.read_name log in

  let n = LS.count_outcomes test  in
  printf "%i\n" n ;
  ()

let () =
  try zyva log
  with Misc.Fatal msg|Misc.UserError msg ->
    eprintf "Fatal error: %s\n%!" msg
