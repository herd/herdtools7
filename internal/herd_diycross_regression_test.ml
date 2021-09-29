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


(* Flags. *)

type path = string

type flags = {
  herd         : path ;
  libdir       : path ;
  diycross     : path ;
  expected_dir : path ;
  arch         : string ;
  relaxlists   : string list ;
  variants     : string list ;
  conf         : path option ;
}


(* Utilities. *)

let list_dir dir = List.sort String.compare (Array.to_list (Sys.readdir dir))
let concat_dir dir names = List.map (Filename.concat dir) names

let without_members cutset xs =
  List.filter (fun x -> not (List.mem x cutset)) xs

let common xs ys =
  List.filter (fun x -> List.mem x ys) xs

let diycross_args libdir arch relaxlists out_dir =
  ["-o"; out_dir; "-set-libdir"; libdir; "-arch"; arch] @ relaxlists


(* Commands *)

let show_tests flags =
  let tmp_dir = Filesystem.new_temp_dir () in
  let args = diycross_args flags.libdir flags.arch flags.relaxlists tmp_dir in
  Command.run flags.diycross args ;
  let litmuses = List.filter TestHerd.is_litmus (list_dir tmp_dir) in

  let litmus_paths = concat_dir tmp_dir litmuses in
  let command_of_litmus l =
    TestHerd.herd_command ~bell:None ~cat:None
      ~conf:flags.conf
      ~variants:flags.variants
      ~libdir:flags.libdir
      flags.herd [l]
  in
  Channel.write_lines stdout (List.map command_of_litmus litmus_paths)


let run_tests flags =
  let tmp_dir = Filesystem.new_temp_dir () in
  let args = diycross_args flags.libdir flags.arch flags.relaxlists tmp_dir in
  Command.run flags.diycross args ;

  let litmuses =
    List.filter TestHerd.is_litmus (list_dir tmp_dir) in

  let expecteds =
    List.filter TestHerd.is_expected (list_dir flags.expected_dir) in
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
    (fun (l, e) ->
      TestHerd.herd_output_matches_expected ~bell:None ~cat:None
        ~conf:flags.conf
        ~variants:flags.variants
        ~libdir:flags.libdir
        flags.herd l e "" "")
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

  let output_of_litmus l =
    TestHerd.run_herd ~bell:None ~cat:None
      ~conf:flags.conf
      ~variants:flags.variants
      ~libdir:flags.libdir
      flags.herd [l]
  in
  let outputs = List.map output_of_litmus litmus_paths in
  let write_file (path, (_,lines,_)) =
    Filesystem.write_file path (fun o -> Channel.write_lines o lines) in
  List.combine expected_paths outputs |> List.iter write_file ;
  Filesystem.remove_recursive tmp_dir


let usage = String.concat "\n" [
  Printf.sprintf "Usage: %s [options] (show|test|promote)" (Filename.basename Sys.argv.(0)) ;
  "" ;
  "Commands:" ;
  "  show     Print the diycross7 and herd7 commands that would be run." ;
  "  test     Compare the output of running herd7 on generated diycross7 tests against .expected files." ;
  "  promote  Update .expected files to the output of herd7." ;
  "" ;
  "Options:" ;
]

let () =
  (* Required arguments. *)
  let herd = ref "" in
  let libdir = ref "" in
  let diycross = ref "" in
  let expected_dir = ref "" in
  let arch = ref "" in
  let relaxlists = ref [] in

  (* Optional arguments. *)
  let conf = ref None in
  let variants = ref [] in

  let anon_args = ref [] in
  let options = [
    Args.is_file ("-herd-path",     Arg.Set_string herd,           "path to herd binary") ;
    Args.is_dir  ("-libdir-path",   Arg.Set_string libdir,         "path to herd libdir") ;
    Args.is_file ("-diycross-path", Arg.Set_string diycross,       "path to diycross binary") ;
    Args.is_dir  ("-expected-dir",  Arg.Set_string expected_dir,   "path to directory of .expected files to test against") ;
                  "-arch",          Arg.Set_string arch,           "arch to test" ;
                  "-relaxlist",     Args.append_string relaxlists, "relaxlist to cross-product (specify multiple times)" ;
    Args.is_file ("-conf",          Args.set_string_option conf,   "path to config file to pass to herd7") ;
                  "-variant",       Args.append_string variants,   "variant to pass to herd7" ;
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
  if !diycross = "" then
    exit_with_error "Must set -diycross-path" ;
  if !expected_dir = "" then
    exit_with_error "Must set -expected-dir" ;
  if !arch = "" then
    exit_with_error "Must set -arch" ;
  if List.length !relaxlists = 0 then
    exit_with_error "Must provide at least one -relaxlist" ;

  let flags = {
    herd = !herd ;
    libdir = !libdir ;
    diycross = !diycross ;
    expected_dir = !expected_dir ;
    arch = !arch ;
    relaxlists = !relaxlists ;
    conf = !conf ;
    variants = !variants ;
  } in
  match !anon_args with
  | "show" :: [] -> show_tests flags
  | "test" :: [] -> run_tests flags
  | "promote" :: [] -> promote_tests flags
  | _ -> exit_with_error "Must provide one command of: show, test, promote"
