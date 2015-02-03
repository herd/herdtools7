(*********************************************************************)
(*                        DIY                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Extract topology information from logs *)

open LogState
open Printf

module type Config = sig
  val verbose : int
  val shownames : bool
  val names : StringSet.t option
end

module Make(O:Config) = struct

  module LL =
    LexLog.Make
      (struct
        let verbose = O.verbose
        let rename s = s
        let ok = match O.names with
        | None -> fun _ -> true
        | Some set -> fun s -> StringSet.mem s set 
      end)

  module LS = LogState.Make(O)

  let dump_test chan t =
    if LS.some_topologies t.topologies then begin
      if O.shownames then fprintf chan "Test %s%s\n" t.tname
          (if is_reliable t.kind then " "^LS.pp_kind t.kind else "") ;
      LS.dump_topologies chan t.topologies
    end ;
    ()

  let zyva ts chan =
    Array.iter (dump_test chan) ts.tests

  let of_chan name ichan ochan =
    zyva (LL.read_chan name ichan) ochan

  let of_name name chan =
    zyva (LL.read_name name) chan
    
end


let names = ref []
let select = ref []
let verbose = ref 0
let shownames = ref true
let log = ref None

let options =
  [
  ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
   ("-shownames", Arg.Bool (fun b -> shownames := b),
    (sprintf "<bool> show test names in output, default %b" !shownames));
   ("-names",
    Arg.String
      (fun s -> names := s :: !names),
    "<name> specify  selected name file, can be repeated") ;       
   ("-select",
    Arg.String
      (fun s -> select := s :: !select),
    "<name> specify selected test file (or index file), can be repeated") ;
 ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mtopos"

let parse_log s = match !log with
| None -> log := Some s
| Some _ -> raise (Arg.Bad "at most one argument")

let () =
  Arg.parse options
    parse_log
    (sprintf "Usage %s [options]* [log]
  - log is a  litmus log
  - options are:" prog)

let names1 = match !names with
| [] -> None
| args ->
    let add t = StringSet.add t in
    Some
      (List.fold_left
         (fun r name -> ReadNames.from_file name add r)
         StringSet.empty args)

let names2 = match !select with
| [] -> None
| args ->
    let names = Names.from_fnames (Misc.expand_argv args) in
    Some (StringSet.of_list names)

let names = match names1,names2 with
| (None,ns)|(ns,None) -> ns
| Some ns1,Some ns2 -> Some (StringSet.union ns1 ns2)

module Config = struct
  let verbose = !verbose
  let shownames = !shownames
  let names = names
end

module X = Make(Config)

let () = match !log with
| None -> X.of_chan "*stdin*" stdin stdout
| Some log -> X.of_name log stdout

