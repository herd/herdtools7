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

(** A tool that generates regression tests for herd7 using diycross7, comparing
 *  the output against .expected files. *)


(* Flags. *)

type path = string

type flags = {
  herd          : path ;
  herd_conf     : path option ;
  libdir        : path ;
  diycross      : path ;
  diycross_args : string list ;
  expected_dir  : path ;
  variants      : string list ;
  nohash        : bool ;
  verbose       : bool ;
}


(* Utilities. *)

let list_dir dir = List.sort String.compare (Array.to_list (Sys.readdir dir))
let concat_dir dir names = List.map (Filename.concat dir) names

let without_members cutset xs =
  List.filter (fun x -> not (List.mem x cutset)) xs

let common xs ys =
  List.filter (fun x -> List.mem x ys) xs

let diycross_args flags out_dir =
  ["-o"; out_dir; "-set-libdir"; flags.libdir; ] @ flags.diycross_args


(* Commands *)
let run_diycross flags =
  let tmp_dir = Filesystem.new_temp_dir () in
  let args = diycross_args flags tmp_dir in
  Command.run flags.diycross args ;
  tmp_dir,List.filter TestHerd.is_litmus (list_dir tmp_dir)


let show_tests ?j flags =
  let tmp_dir,litmuses = run_diycross flags in
  match j with
  | None ->
      let command_of_litmus litmus =
        TestHerd.herd_command
          ~bell:None ~cat:None
          ~variants:flags.variants
          ~conf:flags.herd_conf
          ~libdir:flags.libdir
          flags.herd
          [litmus] in
      let litmus_paths = concat_dir tmp_dir litmuses in
      Channel.write_lines stdout
        (List.map command_of_litmus litmus_paths)
  | Some j ->
      let index = Filename.concat tmp_dir "@all" in
      let args =
        TestHerd.herd_args
          ~bell:None ~cat:None
          ~variants:flags.variants
          ~conf:flags.herd_conf
          ~libdir:flags.libdir
          ~timeout:None ~checkfilter:None ~speedcheck:None in
      let herd_dir = Filename.dirname flags.herd in
      let mapply = Filename.concat herd_dir "mapply7" in
      let args =
        String.concat " " (TestHerd.apply_redirect_args flags.herd j args) in
      Channel.write_lines stdout
        [Printf.sprintf "%s %s %s" mapply args index;]

let run_tests ?j flags =
  let tmp_dir,litmuses = run_diycross flags in

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
  let results =
    let les = List.combine litmus_paths expected_paths in
    match j with
    | None ->
       List.map
         (fun (l, e) ->
           (* check if a `*.expected-warn` exists *)
           let warn_file = Filename.remove_extension e
                           |> TestHerd.expected_warn_of_litmus in
           let warn_file = if Sys.file_exists warn_file then warn_file else "" in
            TestHerd.herd_output_matches_expected
              ~verbose:flags.verbose
              ~nohash:flags.nohash
              ~bell:None ~cat:None
             ~conf:flags.herd_conf
             ~variants:flags.variants
             ~libdir:flags.libdir
             flags.herd l e "" warn_file)
         les
    | Some j ->
       ignore
         (TestHerd.run_herd_concurrent
            ~verbose:flags.verbose ~bell:None ~cat:None
            ~conf:flags.herd_conf
            ~variants:flags.variants
            ~libdir:flags.libdir
            flags.herd ~j:j litmus_paths) ;
       List.map
         (fun (l,e) -> TestHerd.output_matches_expected ~nohash:flags.nohash l e)
         les
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

let promote_tests ?j flags =
  (* Run diycross *)
  let tmp_dir,litmuses = run_diycross flags in

  (* Old reference files *)
  let old_paths =
    concat_dir flags.expected_dir
      (List.filter TestHerd.is_expected (list_dir flags.expected_dir))in
  List.iter Sys.remove old_paths ;

  let litmus_paths = concat_dir tmp_dir litmuses in

  let outputs =
    match j with
    |  None ->
        let output_of_litmus l =
          TestHerd.run_herd ~bell:None ~cat:None
            ~conf:flags.herd_conf
            ~variants:flags.variants
            ~libdir:flags.libdir
            flags.herd [l] in
        List.map (fun l -> output_of_litmus l) litmus_paths
  | Some j ->
     ignore
       (TestHerd.run_herd_concurrent  ~bell:None ~cat:None
          ~conf:flags.herd_conf
          ~variants:flags.variants
          ~libdir:flags.libdir
          flags.herd ~j:j litmus_paths) ;
     List.map
       (fun l ->
         0,
         TestHerd.read_file (TestHerd.outname l),
         TestHerd.read_file (TestHerd.errname l))
     litmus_paths in

  (* New reference expected and warn files *)
  let references = concat_dir flags.expected_dir litmuses
    |> List.map (fun filename ->
        TestHerd.expected_of_litmus filename, TestHerd.expected_warn_of_litmus filename
      ) in

  let write_file ((expected_path,warning_path), (_,lines,errs)) =
    Filesystem.write_file expected_path (fun o -> Channel.write_lines o lines);
    if List.length errs <> 0 then
      Filesystem.write_file warning_path (fun o -> Channel.write_lines o errs)
  in
  List.combine references outputs |> List.iter write_file ;
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
  let diycross_args = ref [] in
  let variants = ref [] in

  (* Optional arguments. *)
  let conf = ref None in
  let j = ref None in
  let nohash = ref false in
  let verbose = ref false in

  let anon_args = ref [] in
  let options = [
    Args.npar j ; Args.nohash nohash ; Args.verbose verbose ;
    Args.is_file ("-herd-path",     Arg.Set_string herd,           "path to herd binary") ;
    Args.is_dir  ("-libdir-path",   Arg.Set_string libdir,         "path to herd libdir") ;
    Args.is_file ("-diycross-path", Arg.Set_string diycross,       "path to diycross binary") ;
    Args.is_dir  ("-expected-dir",  Arg.Set_string expected_dir,   "path to directory of .expected files to test against") ;
    "-diycross-arg", Args.append_string diycross_args,  "one argument for diycross (cumulative)" ;
    Args.is_file ("-conf", Args.set_string_option conf,   "path to config file to pass to herd7") ;
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
  if !diycross = "" then
    exit_with_error "Must set -diycross-path" ;
  if !expected_dir = "" then
    exit_with_error "Must set -expected-dir" ;

  let flags = {
    herd = !herd ;
    herd_conf = !conf ;
    libdir = !libdir ;
    diycross = !diycross ;
    diycross_args = !diycross_args ;
    expected_dir = !expected_dir ;
    variants = !variants ;
    nohash = !nohash ;
    verbose = !verbose ;
    } in
  let j = !j in
  match !anon_args with
  | "show" :: [] -> show_tests ?j flags
  | "test" :: [] -> run_tests ?j flags
  | "promote" :: [] -> promote_tests ?j flags
  | _ -> exit_with_error "Must provide one command of: show, test, promote"
