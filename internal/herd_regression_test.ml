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

let herd_args libdir litmus =
  ["-set-libdir"; libdir; litmus]

let time_re = Str.regexp "^Time "
let without_time lines =
  let not_time l = not (Str.string_match time_re l 0) in
  List.filter not_time lines

let run_herd herd libdir litmus =
  let args = herd_args libdir litmus in
  let lines = Command.run_with_stdout herd args Channel.read_lines in
  without_time lines

let log_compare a b = String.compare (String.concat "\n" a) (String.concat "\n" b)

let expected_of_litmus litmus = litmus ^ ".expected"
let is_litmus path = Filename.extension path = ".litmus"

let run_test herd libdir litmus =
  let output = try
    Some (run_herd herd libdir litmus)
  with _ -> None in
  match output with
  | None -> Printf.printf "Failed %s : Herd returned non-zero\n" litmus ; false
  | Some output ->

  let expected = expected_of_litmus litmus in
  let expected_output = try
    Some (Filesystem.read_file expected Channel.read_lines)
  with _ -> None in
  match expected_output with
  | None -> Printf.printf "Failed %s : Missing file %s\n" litmus expected ; false
  | Some expected_output ->

  if log_compare output expected_output <> 0 then begin
    Printf.printf "Failed %s : Logs do not match\n" litmus ;
    false
  end else
    true

let litmuses_of_dir dir =
  let all_files = Array.to_list (Sys.readdir dir) in
  let only_litmus = List.filter is_litmus all_files in
  let full_paths = List.map (Filename.concat dir) only_litmus in
  List.sort String.compare full_paths

(* Commands *)

let show_tests herd libdir litmus_dir =
  let litmuses = litmuses_of_dir litmus_dir in
  let command_of_litmus l = Command.command herd (herd_args libdir l) in
  let commands = List.map command_of_litmus litmuses in
  Channel.write_lines stdout commands

let run_tests herd libdir litmus_dir =
  let litmuses = litmuses_of_dir litmus_dir in
  let statuses = List.map (run_test herd libdir) litmuses in
  if List.for_all (fun x -> x) statuses
  then
    Printf.printf "Tests OK\n"
  else begin
    Printf.printf "Some tests had errors\n" ;
    exit 1
  end

let promote_tests herd libdir litmus_dir =
  let litmuses = litmuses_of_dir litmus_dir in
  let outputs = List.map (run_herd herd libdir) litmuses in
  let expecteds = List.map expected_of_litmus litmuses in
  let write_file (path, lines) =
    Filesystem.write_file path (fun o -> Channel.write_lines o lines)
  in
  List.combine expecteds outputs |> List.iter write_file

let usage = String.concat "\n" [
  Printf.sprintf "Usage: %s [opts] (show|test|promote)" Sys.argv.(0) ;
  "" ;
  " show     Print the herd7 commands that would be run." ;
  " test     Compare the output of herd7 against .expected files." ;
  " promote  Update .expected files to the output of herd7." ;
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
  match !anon_args with
  | "show" :: [] -> show_tests !herd !libdir !litmus_dir
  | "test" :: [] -> run_tests !herd !libdir !litmus_dir
  | "promote" :: [] -> promote_tests !herd !libdir !litmus_dir
  | _ -> Printf.printf "%s\n" usage ; exit 1
