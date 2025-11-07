(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** A tool that runs regression tests of `diy7 -cycleonly true`, against .cycle.expected files. *)

(* Flags. *)

type path = string

type flags = {
  diy       : path ;
  conf      : string ;
  expected  : path ;
  arguments : string list ;
}

let diy_argument flags = ["-conf"; flags.conf; "-cycleonly"; "true"] @ flags.arguments

let show_tests flags =
  Printf.printf "%s %s\n" flags.diy (diy_argument flags |> String.concat " ")

module StringSet = Set.Make(String)

let do_run flags =
  let cycles = ref [] in
  (* we record the cycle *)
  let read_line line =
    let cycle = String.split_on_char ':' line
    |> Fun.flip List.nth 1
    |> String.trim in
    cycles := cycle :: !cycles in
  (* ignore the output to stderr *)
  Command.NonBlock.run ~stdout:read_line ~stderr:(fun _ -> ()) flags.diy (diy_argument flags);
  StringSet.of_list !cycles

let run_tests flags =
  let cycles = do_run flags in
  let expected_result =
    Filesystem.read_file flags.expected Channel.read_lines
    |> StringSet.of_list in
  if not @@ StringSet.equal cycles expected_result then begin
    Printf.eprintf "New cycles:\n%s\n"
    ( StringSet.diff cycles expected_result
      |> StringSet.elements
      |> String.concat "\n" );
    Printf.eprintf "Missing cycles:\n%s\n"
    ( StringSet.diff expected_result cycles
      |> StringSet.elements
      |> String.concat "\n" );
    exit 1
  end


let promote_tests flags =
  let cycles = do_run flags |> StringSet.elements in
  Filesystem.write_file flags.expected (fun o -> Channel.write_lines o cycles)


let usage = String.concat "\n" [
  Printf.sprintf "Usage: %s [options] (show|test|promote)" (Filename.basename Sys.argv.(0)) ;
  "" ;
  "Commands:" ;
  "  show     Print the diy7 commands that would be run." ;
  "  test     Compare the output of diy7 against .cycle.expected files." ;
  "  promote  Update .cycle.expected files to the output of diy7." ;
  "" ;
  "Options:" ;
]

let () =
  (* Required arguments. *)
  let diy = ref "" in
  let conf = ref "" in
  let expected = ref "" in
  (* Optional arguments. *)
  let arguments = ref [] in

  let anon_args = ref [] in

  let options = [
    Args.is_file ("-diy-path", Arg.Set_string diy,           "path to diy7 binary") ;
    Args.is_file ("-conf",     Arg.Set_string conf,          "path to config file to pass to diy7") ;
    Args.is_file ("-expected", Arg.Set_string expected,      "path to diy7 binary") ;
                  "-diy-arg",  Args.append_string arguments, "extra argument to pass to diy7" ;
  ] in
  Arg.parse options (fun a -> anon_args := a :: !anon_args) usage ;

  let exit_with_error msg =
    Printf.printf "%s: %s.\n" Sys.argv.(0) msg ;
    Arg.usage options usage ;
    exit 2
  in

  if !diy = "" then
    exit_with_error "Must set -diy-path" ;
  if !conf = "" then
    exit_with_error "Must set -conf" ;
  if !expected = "" then
    exit_with_error "Must set -expected" ;
  let flags = {
    diy = !diy ;
    conf = !conf ;
    expected = !expected ;
    arguments = !arguments ;
    } in
  match !anon_args with
  | "show" :: [] -> show_tests flags
  | "test" :: [] -> run_tests flags
  | "promote" :: [] -> promote_tests flags
  | _ -> exit_with_error "Must provide one command of: show, test, promote"
