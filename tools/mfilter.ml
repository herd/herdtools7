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
open LogState

let verbose = ref 0
let logs = ref []
let exclude = ref None
let select = ref []
let rename = ref []
let conds = ref []
let inverse = ref false

let options =
  [
  
  ("-q", Arg.Unit (fun _ -> verbose := -1),
   "<non-default> be silent");  
  ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
  ("-inverse", Arg.Bool (fun b -> inverse := b),
   Printf.sprintf "<bool> inverse selection, default %b" !inverse) ;
  ("-excl", Arg.String (fun s -> exclude := Some s),
   "<regexp> exclude tests whose name matches <regexp>");
  ("-select",
    Arg.String (fun s ->  select := !select @ [s]),
   "<name> specify test or test index  file, can be repeated") ;
  ("-conds",
    Arg.String (fun s -> conds := !conds @ [s]),
   "<name> specify condition to apply to outcomes, can be repeated") ;
  ("-rename", Arg.String (fun s -> rename := !rename @ [s]),     
    "<name> specify a rename mapping, for renaming some tests, hashes checked") ;
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
let rename = !rename
let verbose = !verbose
let conds = !conds
let log = match !logs with
| [log;] -> Some log
| [] -> None
| _ ->
    eprintf "%s takes at most one argument\n" prog ;
    exit 2
let inverse = !inverse

module Verbose = struct let verbose = verbose end

module LR = LexRename.Make(Verbose)
let conds = LR.read_from_files conds LogConstr.parse
let rename = LR.read_from_files rename (fun s -> Some s)

let do_rename name =
  try TblRename.find_value rename name
  with Not_found -> name

let select_name =
  match select with
  | [] -> fun _ -> true
  | args ->
      let names = Names.from_fnames (Misc.expand_argv args) in
      let names = List.rev_map do_rename names in
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
      let rename = do_rename
      let ok = select_name
    end)


let zyva log =
  let test = match log with
  | None -> LL.read_chan "stdin" stdin
  | Some log -> LL.read_name log in

  let is_litmus = test.LogState.is_litmus in
(* Dumping of log files *)


  let is_reliable k = match k with
  | Allow|Require|Forbid -> true
  | _ -> false in

  let dump_hash chan = function
  | None -> ()
  | Some h -> fprintf chan "Hash=%s\n" h in

  let dump_condition chan v c = match c,v with
  | Some c,(Ok|No) ->
      fprintf chan
        "Condition %a is%s validated\n"
        LogConstr.dump c
        (if v = Ok then "" else " not")
  | _,_ -> () in

  let dump_prop chan c = match c with
  | Some c ->
      fprintf chan
        "Condition (%a)\n"
        LogConstr.dump_prop (ConstrGen.prop_of c)
  | None -> () in

  let dump_test chan t =
    fprintf chan "Test %s%s\n" t.tname
      (if is_reliable t.kind then " "^LS.pp_kind t.kind else "") ;
    LS.dump_states_cond chan is_litmus t.states ;
    if is_reliable t.kind then begin
      fprintf chan "%s\n" (LS.pp_validation t.validation) ;
      fprintf chan "Witnesses\n" ;
      let p,n = t.witnesses in
      fprintf chan "Positive: %s Negative: %s\n"
        (Int64.to_string p) (Int64.to_string n) ;
      dump_condition chan t.validation t.condition 
    end else begin
      fprintf chan "??\n" ;
      dump_prop chan t.condition       
    end ;
    dump_hash chan t.hash ;    
    begin match t.time with
    | None -> ()
    | Some time -> 
        fprintf chan "Time %s %0.2f\n" t.tname time
    end ;
    output_char chan '\n';
    () in

  let dump_log chan =  Array.iter (dump_test chan) in

  let filtered = LS.filter inverse conds test in
  dump_log stdout filtered ;
  flush stdout ;
  ()

let () =
  try zyva log
  with Misc.Fatal msg ->
    eprintf "Fatal error: %s\n%!" msg

