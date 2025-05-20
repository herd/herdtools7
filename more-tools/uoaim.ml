(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** uoiam is miaou backwards *)

open Printf

let prog =
  if Array.length Sys.argv > 0 then
    Filename.basename Sys.argv.(0)
  else "uoaim7"

let verbose = ref 0
let libdir = ref (Filename.concat Version.libdir "herd")
let includes = ref []
let earley = ref false
let cat = ref "aarch64.cat"
let arg = ref None

let options =
  [
(* Basic *)
    ("-version", Arg.Unit
     (fun () -> printf "%s, Rev: %s\n" Version.version Version.rev ; exit 0),
   " show version number and exit") ;
    ("-libdir", Arg.Unit (fun () -> print_endline !libdir; exit 0),
    " show installation directory and exit");
    ("-set-libdir", Arg.String (fun s -> libdir := s),
    "<path> set installation directory to <path>");
    ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
    ("-q", Arg.Unit (fun _ -> verbose := -1 ),
   "<default> do not show diagnostics");
    ("-I", Arg.String (fun s -> includes := !includes @ [s]),
   "<dir> add <dir> to search path");
   ("-earley", Arg.Set earley, "select earley parser");
    ("-cat", Arg.String (fun s -> cat := s),
     sprintf "<name.cat> set base model, default %s" !cat)
  ]

let () =
  Arg.parse
    options
    (fun s -> arg := Some s)
    (sprintf "Usage: %s [option] [file]" prog)

module Config = struct
  let verbose = !verbose
  let libdir = !libdir
  let includes = !includes
  let cat = !cat
end

let zyva =
  if !earley then
    let module Zyva = EParser.Make(Config) in
    Zyva.zyva
  else
    let module Zyva = StdParser.Make(Config) in
    Zyva.zyva

let zyva name chan =
  try zyva name chan
  with
  | Misc.UserError msg ->
      prerr_endline msg ;
      exit 2
  | Misc.Exit -> exit 2


let arg = !arg
let () =
  let ds =
    match arg with
    | None -> zyva arg stdin
    | Some name -> Misc.input_protect (zyva arg) name in
  if !verbose > 0 then Printf.printf "%a\n%!" PreCat.pp_defs ds
