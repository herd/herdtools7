(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Extract topology information from logs *)

open LogState
open Printf

module type Config = sig
  val verbose : int
  val shownames : bool
  val ok : string -> bool
  val hexa : bool
end

module Make(O:Config) = struct

  module LL =
    LexLog_tools.Make
      (struct
        let verbose = O.verbose
        let rename s = s
        let ok = O.ok
        let hexa = O.hexa
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
  let open CheckName in
  [
  ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
   ("-shownames", Arg.Bool (fun b -> shownames := b),
    (sprintf "<bool> show test names in output, default %b" !shownames));
   parse_select select; parse_names names;
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

module Check =
  CheckName.Make
    (struct
      let verbose = !verbose
      let rename = []
      let select = !select
      let names = !names
      let excl = []
    end)

module Config = struct
  let verbose = !verbose
  let shownames = !shownames
  let ok = Check.ok
  let hexa = false
end

module X = Make(Config)

let () = match !log with
| None -> X.of_chan "*stdin*" stdin stdout
| Some log -> X.of_name log stdout

