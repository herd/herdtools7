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

(* Extract some records from logs. *)
open Printf
open OptNames

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mlogelect"

let arg = ref None

let () =
  Arg.parse
    OptNames.parse_withselect
    (fun s ->
       arg :=
         match !arg with
         | None -> Some s
         | Some _ -> raise (Arg.Bad "one argument at most"))
    (sprintf "usage: %s [options]* log?" prog)

(* Read names *)

module Check =
  CheckName.Make
    (struct
      let verbose = 0
      let rename = []
      let select = !select
      let names = !names
      let oknames = !oknames
      let excl = !excl
      let nonames = !nonames
    end)

let zyva () =
  match !arg with
  | None ->
      LexLogSelect.from_chan Check.ok stdin
  | Some f ->
      Misc.input_protect
        (LexLogSelect.from_chan Check.ok)
        f

let () =
  try zyva ()
  with
  | Misc.(Fatal msg|UserError msg) ->
    Warn.warn_always "Fatal error: %s" msg
  | _ -> assert false
