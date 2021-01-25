(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** A tool that runs regression tests of herd7, against .expected files. *)


(* Flags. *)

type path = string

type flags = {
  herd       : path ;
  libdir     : path ;
  litmus_dir : path ;
  variants   : string list ;
  conf       : path option ;
}


(* Utilities. *)

let litmuses_of_dir dir =
  let all_files = Array.to_list (Sys.readdir dir) in
  let only_litmus = List.filter TestHerd.is_litmus all_files in
  let full_paths = List.map (Filename.concat dir) only_litmus in
  List.sort String.compare full_paths


(* Commands. *)

let show_tests flags =
  let litmuses = litmuses_of_dir flags.litmus_dir in
  let command_of_litmus l =
    TestHerd.herd_command ~bell:None ~cat:None
      ~conf:flags.conf
      ~variants:flags.variants
      ~libdir:flags.libdir
      flags.herd [l]
  in
  let commands = List.map command_of_litmus litmuses in
  Channel.write_lines stdout commands

let run_tests flags =
  let litmuses = litmuses_of_dir flags.litmus_dir in
  let expecteds = List.map TestHerd.expected_of_litmus litmuses in
  let expected_failures = List.map TestHerd.expected_failure_of_litmus litmuses in
  let results = List.map
   (fun ((l, e), f) ->
     TestHerd.herd_output_matches_expected ~bell:None ~cat:None
      ~conf:flags.conf
      ~variants:flags.variants
      ~libdir:flags.libdir
      flags.herd l e f)
   (List.combine (List.combine litmuses expecteds) expected_failures)
  in
  let failed r = not r in
  if List.exists failed results then begin
    Printf.printf "Some tests had errors\n" ;
    exit 1
  end

let promote_tests flags =
  let litmuses = litmuses_of_dir flags.litmus_dir in
  let output_of_litmus l =
    TestHerd.run_herd ~bell:None ~cat:None
      ~conf:flags.conf
      ~variants:flags.variants
      ~libdir:flags.libdir
      flags.herd [l]
  in
  let outputs = List.map output_of_litmus litmuses in
  let expecteds = List.map TestHerd.expected_of_litmus litmuses in
  let expected_failures = List.map TestHerd.expected_failure_of_litmus litmuses in
  let write_file (path, (lines,_)) =
    Filesystem.write_file path (fun o -> Channel.write_lines o lines) in
  let write_err_file (path, (_,err_lines)) =
    Filesystem.write_file path (fun o -> Channel.write_lines o err_lines) in
  let not_empty xs = List.length xs > 0
  in
  List.combine expecteds outputs |> List.filter (fun (_,(stdout,_)) -> not_empty stdout) |> List.iter write_file ;
  List.combine expected_failures outputs |> List.filter (fun (_,(_,stderr)) -> not_empty stderr) |> List.iter write_err_file

let usage = String.concat "\n" [
  Printf.sprintf "Usage: %s [options] (show|test|promote)" (Filename.basename Sys.argv.(0)) ;
  "" ;
  "Commands:" ;
  "  show     Print the herd7 commands that would be run." ;
  "  test     Compare the output of herd7 against .expected files." ;
  "  promote  Update .expected and .expected-failure files to the output of herd7." ;
  "" ;
  "Options:" ;
]

let () =
  (* Required arguments. *)
  let herd = ref "" in
  let libdir = ref "" in
  let litmus_dir = ref "" in

  (* Optional arguments. *)
  let conf = ref None in
  let variants = ref [] in

  let anon_args = ref [] in

  let options = [
    Args.is_file ("-herd-path",   Arg.Set_string herd,           "path to herd binary") ;
    Args.is_dir  ("-libdir-path", Arg.Set_string libdir,         "path to herd libdir") ;
    Args.is_dir  ("-litmus-dir",  Arg.Set_string litmus_dir,     "path to directory of .litmus files to test against") ;
    Args.is_file ("-conf",        Args.set_string_option conf,   "path to config file to pass to herd7") ;
                  "-variant",     Args.append_string variants,   "variant to pass to herd7" ;
  ] in
  Arg.parse options (fun a -> anon_args := a :: !anon_args) usage ;

  let exit_with_error msg =
    Printf.printf "%s: %s.\n" Sys.argv.(0) msg ;
    Arg.usage options usage ;
    exit 2
  in

  if !herd = "" then
    exit_with_error "Must set -herd-path" ;
  if !libdir = "" then
    exit_with_error "Must set -libdir-path" ;
  if !litmus_dir = "" then
    exit_with_error "Must set -litmus-dir" ;

  let flags = {
    herd = !herd ;
    libdir = !libdir ;
    litmus_dir = !litmus_dir ;
    conf = !conf ;
    variants = !variants ;
  } in
  match !anon_args with
  | "show" :: [] -> show_tests flags
  | "test" :: [] -> run_tests flags
  | "promote" :: [] -> promote_tests flags
  | _ -> exit_with_error "Must provide one command of: show, test, promote"
