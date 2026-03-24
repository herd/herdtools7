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

open Uoaim

type opts = {
  verbose : int;
  libdir : string;
  includes : string list;
  cat : string;
  name : string option;
  arg : string option;
  is_hwreq : bool;
  is_def : bool;
  infer_rec : bool;
  irrefl : bool;
}

let parse_args () =
  let verbose = ref 0 in
  let libdir = ref (Filename.concat Version.libdir "herd") in
  let includes = ref [] in
  let cat = ref "aarch64.cat" in
  let name = ref None in
  let arg = ref None in
  let is_hwreq = ref false in
  let is_def = ref false in
  let infer_rec = ref true in
  let irrefl = ref false in
  let options =
    let hwreq_spec = Arg.Unit (fun () -> is_hwreq := true) in
    let def_action = Arg.Unit (fun () -> is_def := true) in
    let no_infer_rec_action = Arg.Unit (fun () -> infer_rec := false) in
    [
      (* Basic *)
      ( "--version",
        [ "-version"; "-V" ],
        Arg.Unit
          (fun () ->
            Printf.printf "%s, Rev: %s\n" Version.version Version.rev;
            exit 0),
        " show version number and exit" );
      ( "--libdir",
        [ "-libdir" ],
        Arg.Unit
          (fun () ->
            print_endline !libdir;
            exit 0),
        " show installation directory and exit" );
      ( "--set-libdir",
        [ "-set-libdir" ],
        Arg.String (fun s -> libdir := s),
        "<path> set installation directory to <path>" );
      ( "-v",
        [],
        Arg.Unit (fun _ -> incr verbose),
        "<non-default> show various diagnostics, repeat to increase verbosity"
      );
      ( "-I",
        [],
        Arg.String (fun s -> includes := !includes @ [ s ]),
        "<dir> add <dir> to search path" );
      ( "--cat",
        [ "-cat" ],
        Arg.String (fun s -> cat := s),
        Printf.sprintf "<name.cat> set base model, default %s" !cat );
      ( "--name",
        [ "-name" ],
        Arg.String (fun s -> name := Some s),
        Printf.sprintf "<name> default name of defined relation" );
      ( "--hwreq",
        [],
        hwreq_spec,
        "interpret input text as a hardware requirement" );
      ("--def", [], def_action, "interpret input text as a definition");
      ( "--no-infer-rec",
        [],
        no_infer_rec_action,
        "do not try to infer recursive bindings" );
      ( "--irrefl",
        [],
        Arg.Unit (fun () -> irrefl := true),
        "print hardware requirements as `irreflexive` tests" );
      ( "--debug",
        [],
        Arg.Unit (fun () -> verbose := 2),
        "be very verbose. Equivalent to `-v -v`." );
    ]
  in
  let prog =
    if Array.length Sys.argv > 0 then Filename.basename Sys.argv.(0)
    else "uoaim7"
  in
  Util.Arg.parse options
    (fun s -> arg := Some s)
    (Printf.sprintf "Usage: %s [option] [file]" prog);
  {
    verbose = !verbose;
    libdir = !libdir;
    includes = !includes;
    cat = !cat;
    name = !name;
    arg = !arg;
    is_hwreq = !is_hwreq;
    is_def = !is_def;
    infer_rec = !infer_rec;
    irrefl = !irrefl;
  }

let () =
  let opts = parse_args () in

  Logs.set_reporter (Logs.format_reporter ());
  let logs_level =
    match opts.verbose with
    | 0 -> Logs.Warning
    | 1 -> Logs.Info
    | _ -> Logs.Debug
  in
  Logs.Src.list ()
  |> List.iter (fun src -> Logs.Src.set_level src (Some logs_level));

  let module NP = NameParser.Make (struct
    let libdir = opts.libdir
    let includes = opts.includes
    let cat = opts.cat
  end) in
  try
    let fname = Option.get opts.arg in
    let contents = Misc.input_protect Util.read_all fname in
    try
      if opts.is_hwreq then
        let module HR = HwReqs.MakeInterpreter (NP) in
        let reqs = HR.top ~irrefl:opts.irrefl ~name:opts.name contents in
        if Misc.List.is_empty reqs then
          raise (Misc.Fatal "Translation produced 0 output items.")
        else reqs |> List.iter print_endline
      else if opts.is_def then
        let module Def = Definition.MakeInterpreter (NP) in
        let defs = Def.top ~infer_rec:opts.infer_rec ~name:opts.name contents in
        if Misc.List.is_empty defs then
          raise (Misc.Fatal "Translation produced 0 output items.")
        else defs |> List.iter print_endline
      else begin
        Printf.eprintf "Must set either `--def` or `--hwreq`\n";
        exit 1
      end
    with
    | Util.Interpret_error { msg; context } ->
        Printf.eprintf "Interpretation error: %s\n  Context: %S\n" msg context
    | Util.Parse_error { msg; pos } ->
        let line, ix = Util.String.line_of_pos contents pos in
        Printf.eprintf "Parse error: %s.\nAt line %d: %S\n" msg (ix + 1) line
  with Misc.Fatal msg -> Logs.err (fun m -> m "%s" msg)
