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

let prog =
  if Array.length Sys.argv > 0 then
    Filename.basename Sys.argv.(0)
  else "cat2table"

module Make
  (O: sig
    val verbose: bool
    val includes: string list
    val libdir: string
  end) =
struct

  let libfind =
    let module ML =
      MyLib.Make
        (struct
          let includes = O.includes
          let env = Some "HERDLIB"
          let libdir = O.libdir
          let debug = O.verbose
        end) in
    ML.find

  module ParserConfig =
    struct
      let debug = false
      let libfind = libfind
    end

  module P = ParseModel.Make(ParserConfig)

  open AST

  let rec tr_ast parsed_files fname =
    Printf.printf "Parsing the AST in file %s.\n" fname;
    let parsed_files = StringSet.add fname parsed_files in
    let (_,_,ast) = P.parse fname in
    List.fold_left tr_ins parsed_files ast

  and tr_ins parsed_files = function
    | Include (_,fname) when not (StringSet.mem fname parsed_files) ->
        tr_ast parsed_files fname
    | _ -> parsed_files

  let execute fname =
    tr_ast StringSet.empty fname
end

let verbose = ref false
let libdir = ref (Filename.concat Version.libdir "herd")
let includes = ref []
let model = ref (Filename.concat !libdir "aarch64.cat")

let options = [
  ("-version", Arg.Unit
    (fun () -> Printf.printf "%s, Rev: %s\n" Version.version Version.rev;
      exit 0), " show version number and exit");
  ("-libdir", Arg.Unit (fun () -> print_endline !libdir; exit 0),
    " show installation directory and exit");
  ("-set-libdir", Arg.String (fun s -> libdir := s),
    "<path> set installation directory to <path>");
  (ArgUtils.parse_string "-model" model "path to cat model");
  (ArgUtils.parse_bool "-v" verbose "show various diagnostics");
]

let arg_handler s =
  raise (Arg.Bad (Printf.sprintf "Unexpected argument: %s" s))

let () =
  try
    Arg.parse options
      arg_handler
      (Printf.sprintf "Usage %s [options], output all chains of relations \
        between e1 and e2." prog)
  with
  | Misc.Fatal msg -> Printf.eprintf "%s: %s\n" prog msg; exit 2

let () =
  let module Run =
    Make
      (struct
        let verbose = !verbose
        let includes = !includes
        let libdir = !libdir
      end) in
  ignore (Run.execute !model)
