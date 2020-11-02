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
let select = ref []
let rename = ref []
let names = ref []
let excl = ref []
let hexa = ref false
let int32 = ref true
let emptyok = ref false

let parse_emptyok r =
  "-emptyok", Arg.Bool (fun b -> r := b),
  (Printf.sprintf "<bool> keep tests with empty outcome in output, default %b" !hexa)


let options =
  let open CheckName in
  [

  ("-q", Arg.Unit (fun _ -> verbose := -1),
   "<non-default> be silent");
  ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
   parse_hexa hexa; parse_int32 int32;
   parse_emptyok emptyok;
   parse_rename rename;
   parse_select select; parse_names names;
   parse_excl excl;
  ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mdiff"

type act = Diff | Inter

let act =
  let base = Filename.basename prog in
  let base =
    try Filename.chop_extension base with Invalid_argument _ -> base in
  match base with
  | "mdiff"|"mdiff7" -> Diff
  | "minter"|"minter7" -> Inter
  | _ -> assert false

let () =
  Arg.parse options
    (fun s -> logs := !logs @ [s])
    (sprintf "Usage %s [options]* log1 log2
log1 log2 are log file names.
Options are:" prog)

let excl = !excl
let select = !select
let names = !names
let rename = !rename
let verbose = !verbose
let hexa = !hexa
let int32 = !int32
let emptyok = !emptyok

let log1,log2 = match !logs with
| [log1;log2;] -> log1,log2
| _ ->
    eprintf "%s takes two arguments\n" prog ;
    exit 2


module Verbose = struct let verbose = verbose end
module LS = LogState.Make(Verbose)
module LL =
  LexLog_tools.Make
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
      let int32 = int32
      let acceptBig = false
    end)

let readlog log = match log with
| "stdin" -> LL.read_chan log stdin
| _       -> LL.read_name log

module D =
  LogConstr.Dump
    (struct
      let hexa = hexa
      let tr = Misc.identity
    end)
let zyva log1 log2  =

  let test1 = readlog log1 in
  let test2 = readlog log2 in

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
        D.dump c
        (if v = Ok then "" else " not")
  | _,_ -> () in

  let dump_prop chan c = match c with
  | Some c ->
      fprintf chan
        "Condition (%a)\n"
        D.dump_prop (ConstrGen.prop_of c)
  | None -> () in

  let dump_test chan t =
    fprintf chan "Test %s%s\n" t.tname
      (if is_reliable t.kind then " "^LS.pp_kind t.kind else "") ;
    LS.dump_states chan t.states ;
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

  let diff = match act with
  | Diff -> LS.diff_logs emptyok test1 test2
  | Inter -> LS.inter_logs emptyok test1 test2 in
  dump_log stdout diff ;
  flush stdout ;
  ()

let () =
  try zyva log1 log2
  with Misc.Fatal msg|Misc.UserError msg ->
    eprintf "Fatal error: %s\n%!" msg
