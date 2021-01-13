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

(* Contributed by Ethel Morgan <Ethel.Morgan@arm.com> *)

let litmuses_of_dir dir =
  let all_files = Array.to_list (Sys.readdir dir) in
  let only_litmus = List.filter TestHerd.is_litmus all_files in
  let full_paths = List.map (Filename.concat dir) only_litmus in
  List.sort String.compare full_paths

(* Commands *)

let show_tests herd libdir litmus_dir =
  let litmuses = litmuses_of_dir litmus_dir in
  let commands = List.map (fun l -> TestHerd.herd_command herd libdir [l]) litmuses in
  Channel.write_lines stdout commands

let run_tests herd libdir litmus_dir =
  let litmuses = litmuses_of_dir litmus_dir in
  let expecteds = List.map TestHerd.expected_of_litmus litmuses in
  let expected_failures = List.map TestHerd.expected_failure_of_litmus litmuses in
  let results = List.map
   (fun ((l, e), f) -> TestHerd.herd_output_matches_expected herd libdir l e f)
   (List.combine (List.combine litmuses expecteds) expected_failures)
  in
  let failed r = not r in
  if List.exists failed results then begin
    Printf.printf "Some tests had errors\n" ;
    exit 1
  end

let promote_tests herd libdir litmus_dir =
  let litmuses = litmuses_of_dir litmus_dir in
  let outputs = List.map (fun l -> TestHerd.run_herd herd libdir [l]) litmuses in
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
  Printf.sprintf "Usage: %s [opts] (show|test|promote)" Sys.argv.(0) ;
  "" ;
  " show     Print the herd7 commands that would be run." ;
  " test     Compare the output of herd7 against .expected files." ;
  " promote  Update .expected and .expected-failure files to the output of herd7." ;
]

let () =
  let herd = ref "" in
  let libdir = ref "" in
  let litmus_dir = ref "" in
  let anon_args = ref [] in
  Arg.parse [
    ("-herd-path", Arg.String (fun p -> herd := p), "path to herd binary") ;
    ("-libdir-path", Arg.String (fun p -> libdir := p), "path to herd libdir") ;
    ("-litmus-dir", Arg.String (fun p -> litmus_dir := p), "path to directory of .litmus files to test against") ;
  ] (fun a -> anon_args := a :: !anon_args) usage ;
  if (List.exists (fun a -> a = "") [!herd; !libdir; !litmus_dir]) then begin
    Printf.printf "A flag is missing!\n" ;
    exit 1
  end ;
  match !anon_args with
  | "show" :: [] -> show_tests !herd !libdir !litmus_dir
  | "test" :: [] -> run_tests !herd !libdir !litmus_dir
  | "promote" :: [] -> promote_tests !herd !libdir !litmus_dir
  | _ -> Printf.printf "%s\n" usage ; exit 1
