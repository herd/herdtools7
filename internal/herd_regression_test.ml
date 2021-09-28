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

module Fun = Base.Fun


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

let litmuses_to_sort = 1000

(** [for_each_litmus_in_dir dir f] applies [f] to each .litmus file in [dir].
 *  It reads [litmuses_to_sort] entries first and sorts them before applying [f],
 *  then applies [f] to entries in an undefined order after that.
 *  This is to balance readability of test output in `make test`,
 *  while allowing the tool to scale to arbitrarily large directories. *)
let for_each_litmus_in_dir dir f =
  let handle = Unix.opendir dir in
  let readdir () =
    try Some (Unix.readdir handle)
    with End_of_file -> None
  in
  let rec next_litmus () =
    match readdir () with
    | Some name ->
        if TestHerd.is_litmus name then
          Some name
        else
          next_litmus ()
    | None ->
        None
  in
  let rec first_n_litmuses n acc =
    if n = 0 then
      acc
    else
      match next_litmus () with
      | Some litmus -> first_n_litmuses (n-1) (litmus :: acc)
      | None -> acc
  in
  let rec for_each_remaining_litmus f =
    match next_litmus () with
    | Some litmus ->
        f (Filename.concat dir litmus) ;
        for_each_remaining_litmus f
    | None ->
        ()
  in
  Fun.protect
    ~finally:(fun () -> Unix.closedir handle)
    (fun () ->
      first_n_litmuses litmuses_to_sort []
        |> List.sort String.compare
        |> List.map (Filename.concat dir)
        |> List.iter f ;

      for_each_remaining_litmus f
    )

let remove_if_exists path =
  if Sys.file_exists path then
    Sys.remove path

let write_file path lines =
  Filesystem.write_file path (fun o -> Channel.write_lines o lines)


(* Commands. *)

let show_tests flags =
  let command_of_litmus l =
    TestHerd.herd_command ~bell:None ~cat:None
      ~conf:flags.conf
      ~variants:flags.variants
      ~libdir:flags.libdir
      flags.herd [l]
  in
  for_each_litmus_in_dir flags.litmus_dir (fun l ->
    command_of_litmus l |> print_string ;
    print_char '\n'
  )

let run_tests flags =
  let test_passes l =
    TestHerd.herd_output_matches_expected ~bell:None ~cat:None
      ~conf:flags.conf
      ~variants:flags.variants
      ~libdir:flags.libdir
      flags.herd l
      (TestHerd.expected_of_litmus l)
      (TestHerd.expected_failure_of_litmus l)
      (TestHerd.expected_warn_of_litmus l)
  in
  let everything_passed = ref true in
  for_each_litmus_in_dir flags.litmus_dir (fun l ->
    if not (test_passes l) then
      everything_passed := false
  ) ;
  if not !everything_passed then begin
    Printf.printf "Some tests had errors\n" ;
    exit 1
  end

let promote_tests flags =
  let output_of_litmus l =
    TestHerd.run_herd ~bell:None ~cat:None
      ~conf:flags.conf
      ~variants:flags.variants
      ~libdir:flags.libdir
      flags.herd [l]
  in
  let everything_ok = ref true in
  for_each_litmus_in_dir flags.litmus_dir (fun l ->
    let expected = TestHerd.expected_of_litmus l in
    let expected_failure = TestHerd.expected_failure_of_litmus l in

    match output_of_litmus l with
    | 0, [], [] ->
        Printf.printf "Failed %s : Returned neither stdout nor stderr\n" l ;
        everything_ok := false

    | 0, out, [] ->
        remove_if_exists expected_failure ;
        write_file expected out

    | r, [], err when r <> 0 ->
        remove_if_exists expected ;
        write_file expected_failure err

    | 0, out, err ->
       write_file expected out ;
       let expected_warn = TestHerd.expected_warn_of_litmus l in
       write_file expected_warn err
    | r, _, _  ->
       Printf.printf "Failed %s : unexpected exit code %i\n" l r ;
       everything_ok := false
  ) ;
  if not !everything_ok then begin
    Printf.printf "Some tests had errors\n" ;
    exit 1
  end


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
