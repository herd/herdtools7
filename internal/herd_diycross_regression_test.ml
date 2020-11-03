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

(** A tool that generates regression tests for herd7 using diycross7, comparing
 *  the output against .expected files. *)

let list_dir dir = List.sort String.compare (Array.to_list (Sys.readdir dir))
let concat_dir dir names = List.map (Filename.concat dir) names

let without_members cutset xs =
  List.filter (fun x -> not (List.mem x cutset)) xs

let common xs ys =
  List.filter (fun x -> List.mem x ys) xs

let diycross_args libdir arch relaxlists out_dir =
  ["-o"; out_dir; "-set-libdir"; libdir; "-arch"; arch] @ relaxlists


(* Commands *)

type flags = {
  herd : string ;
  diycross : string ;
  libdir : string ;
  expected_dir : string ;
  arch : string ;
  relaxlists : string list
}

let show_tests flags =
  let tmp_dir = Filesystem.new_temp_dir () in
  let args = diycross_args flags.libdir flags.arch flags.relaxlists tmp_dir in
  Command.run flags.diycross args ;
  let litmuses = List.filter TestHerd.is_litmus (list_dir tmp_dir) in

  let litmus_paths = concat_dir tmp_dir litmuses in
  let commands = List.map (TestHerd.herd_command flags.herd flags.libdir) litmus_paths in
  Channel.write_lines stdout commands


let run_tests flags =
  let tmp_dir = Filesystem.new_temp_dir () in
  let args = diycross_args flags.libdir flags.arch flags.relaxlists tmp_dir in
  Command.run flags.diycross args ;
  let litmuses = List.filter TestHerd.is_litmus (list_dir tmp_dir) in

  let expecteds = List.filter TestHerd.is_expected (list_dir flags.expected_dir) in
  let expected_litmuses = List.map TestHerd.litmus_of_expected expecteds in

  let only_in_expected = without_members litmuses expected_litmuses in
  let only_in_got = without_members expected_litmuses litmuses in
  if List.length only_in_expected > 0 then begin
    Printf.printf "Missing files:\n" ;
    List.iter (fun f -> Printf.printf "  %s\n" f) only_in_expected
  end ;
  if List.length only_in_got > 0 then begin
    Printf.printf "Extra files:\n" ;
    List.iter (fun f -> Printf.printf "  %s\n" f) only_in_got
  end ;

  let in_both = common litmuses expected_litmuses in
  let litmus_paths = concat_dir tmp_dir in_both in
  let expected_paths =
    concat_dir flags.expected_dir
    (List.map TestHerd.expected_of_litmus in_both)
  in
  let results = List.map
    (fun (l, e) -> TestHerd.herd_output_matches_expected flags.herd flags.libdir l e)
    (List.combine litmus_paths expected_paths)
  in
  let passed x = x in

  let ok =
    (List.length only_in_expected = 0) &&
    (List.length only_in_got = 0) &&
    (List.for_all passed results) in
  if ok then begin
    (* Clean up and exit cleanly. *)
    Filesystem.remove_recursive tmp_dir
  end else begin
    (* Don't clean up in case the user wants to inspect the errors. *)
    Printf.printf "Some tests had errors\n" ;
    exit 1
  end


let promote_tests flags =
  let old_paths = concat_dir flags.expected_dir (list_dir flags.expected_dir) in
  List.iter Sys.remove old_paths ;

  let tmp_dir = Filesystem.new_temp_dir () in
  let args = diycross_args flags.libdir flags.arch flags.relaxlists tmp_dir in
  Command.run flags.diycross args ;
  let litmuses = List.filter TestHerd.is_litmus (list_dir tmp_dir) in

  let expecteds = List.map TestHerd.expected_of_litmus litmuses in

  let litmus_paths = concat_dir tmp_dir litmuses in
  let expected_paths = concat_dir flags.expected_dir expecteds in

  let outputs = List.map (TestHerd.run_herd flags.herd flags.libdir) litmus_paths in
  List.iter
    (fun (path, lines) -> Filesystem.write_file path (fun o -> Channel.write_lines o lines))
    (List.combine expected_paths outputs) ;
  Filesystem.remove_recursive tmp_dir


let usage = String.concat "\n" [
  Printf.sprintf "Usage: %s [opts] (show|test|promote)" Sys.argv.(0) ;
  "" ;
  " show     Print the diycross7 and herd7 commands that would be run." ;
  " test     Compare the output of running herd7 on generated diycross7 tests against .expected files." ;
  " promote  Update .expected files to the output of herd7." ;
]

let () =
  let flags = ref {
    herd = "" ;
    diycross = "" ;
    libdir = "" ;
    expected_dir = "" ;
    arch = "" ;
    relaxlists = []
  } in
  let anon_args = ref [] in
  Arg.parse [
    ("-herd-path", Arg.String (fun p -> flags := {!flags with herd = p}), "path to herd binary") ;
    ("-diycross-path", Arg.String (fun p -> flags := {!flags with diycross = p}), "path to diycross binary") ;
    ("-libdir-path", Arg.String (fun p -> flags := {!flags with libdir = p}), "path to herd libdir") ;
    ("-expected-dir", Arg.String (fun p -> flags := {!flags with expected_dir = p}), "path to directory of .expected files to test against") ;
    ("-arch", Arg.String (fun a -> flags := {!flags with arch = a}), "arch to test") ;
    ("-relaxlist", Arg.String (fun s -> flags := {!flags with relaxlists = (s :: !flags.relaxlists)}), "relaxlist to cross-product (specify multiple times)") ;
  ] (fun a -> anon_args := a :: !anon_args) usage ;
  if (List.exists (fun a -> a = "") [!flags.herd; !flags.diycross; !flags.libdir; !flags.expected_dir; !flags.arch]) then begin
    Printf.printf "A flag is missing!\n" ;
    exit 1
  end ;
  if (List.length !flags.relaxlists) = 0 then begin
    Printf.printf "Must provide at least one -relaxlist.\n" ;
    exit 1
  end ;
  match !anon_args with
  | "show" :: [] -> show_tests !flags
  | "test" :: [] -> run_tests !flags
  | "promote" :: [] -> promote_tests !flags
  | _ -> Printf.printf "%s\n" usage ; exit 1
