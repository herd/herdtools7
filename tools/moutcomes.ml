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
let rename = ref []
let select = ref []
let names = ref []
let excl = ref []
let hexa = ref false

let options =
  let open CheckName in
  [  
  ("-q", Arg.Unit (fun _ -> verbose := -1),
   "<non-default> be silent");  
  ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
     parse_hexa hexa;
     parse_rename rename;
     parse_select select;
     parse_names names;
     parse_excl excl;
  ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "moutcome"

let () =
  Arg.parse options
    (fun s -> logs := !logs @ [s])
    (sprintf "Usage %s [options]* log
log is a log file names.
Options are:" prog)

let rename = !rename
let select = !select
let names = !names
let excl = !excl
let verbose = !verbose
let hexa = !hexa
let log = match !logs with
| [log;] -> Some log
| [] -> None
| _ ->
    eprintf "%s takes at most one argument\n" prog ;
    exit 2

module Verbose = struct let verbose = verbose end

module LS = LogState.Make(Verbose)
module LL =
  LexLog.Make
    (struct
      let verbose = verbose
      include CheckName.Make
          (struct
            let verbose = verbose
            let rename = rename
            let select = select
            let names = names
            let excl = excl
          end)
      let hexa = hexa
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
  with Misc.Fatal msg ->
    eprintf "Fatal error: %s\n%!" msg

