(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


open Printf


let verbose = ref 0
let logs = ref []
let exclude = ref None
let select = ref []


let options =
  [
  
  ("-q", Arg.Unit (fun _ -> verbose := -1),
   "<non-default> be silent");  
  ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
  ("-excl", Arg.String (fun s -> exclude := Some s),
   "<regexp> exclude tests whose name matches <regexp>");
  ("-select",
    Arg.String (fun s ->  select := !select @ [s]),
   "<name> specify test or test index  file, can be repeated") ;
  ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mfilter"

let () =
  Arg.parse options
    (fun s -> logs := !logs @ [s])
    (sprintf "Usage %s [options]* log
log is a log file names.
Options are:" prog)

let exclude = !exclude
let select = !select
let verbose = !verbose
let log = match !logs with
| [log;] -> Some log
| [] -> None
| _ ->
    eprintf "%s takes at most one argument\n" prog ;
    exit 2

module Verbose = struct let verbose = verbose end

let select_name =
  match select with
  | [] -> fun _ -> true
  | args ->
      let names = Names.from_fnames (Misc.expand_argv args) in
      let set = StringSet.of_list names in
      fun name -> StringSet.mem name set


let select_name = match exclude with
| None -> select_name
| Some e ->
    let re = Str.regexp e in
    (fun name -> 
      not (Str.string_match re name 0) &&
      select_name name)



module LS = LogState.Make(Verbose)
module LL =
  LexLog.Make
    (struct
      let verbose = verbose
      let rename x = x
      let ok = select_name
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

