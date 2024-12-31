(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2024-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Opts

let args = ref []
let get_cmd_arg s = args := s :: !args

let exec filename =
  let module Parse = ParseDotFile.Make(struct
    let debuglexer = !debug.Debug.lexer
    let instr = !instr
  end) in
  let graphs = Parse.parse_file filename in
  List.iteri (fun i g ->
    Printf.printf "The content of graph %d is:\n%s\n" (i + 1) (DotGraph.describe g)
  ) graphs

let options = [
  ArgUtils.parse_tags
    "-debug"
    (fun tag -> match Debug.parse !debug tag with
    | None -> false
    | Some t -> debug := t ; true)
    Debug.tags
    "show debug messages for specific parts" ;
  ArgUtils.parse_string_opt "-instr" instr
    "Instance of the instruction being run, used for substitution \
    of register names and/or condition variables";
]

let () =
  try
    Arg.parse options
      get_cmd_arg
      (Printf.sprintf "Usage %s [options] [dot_file]" prog);
    if List.length !args = 0 then
      invalid_arg (Printf.sprintf "%s run with no target dot file" prog);
    if List.length !args > 1 then
      invalid_arg (Printf.sprintf "Cannot run %s on more than one dot file at once" prog);

    begin match !instr with
    | None -> ()
    | Some s ->
      let regex = Str.regexp {|\([A-Z]+\)\( \([][a-zA-Z0-9_]+\)\)?\(,\([][a-zA-Z0-9_]+\)\)*$|} in
      if not (Str.string_match regex s 0) then
        invalid_arg "Invalid format for command. Command must have arguments separated by \
        commas, and with no whitespaces between them. The mnemonic and the first argument \
        are separated by exactly one space (eg. LDR X0,[X1])"
    end;

    let filename = List.hd !args in
    exec filename
  with
  | Misc.Fatal msg -> Printf.eprintf "%s: %s\n" prog msg ; exit 2
