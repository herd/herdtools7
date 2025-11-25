(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type path = string

type flags = {
  herd : path;
  libdir : path;
  dirs_and_confs_file : path;
  assumptions_file : path;
  debug : bool;
}

let prog = Filename.basename Sys.argv.(0)
let usage = Printf.sprintf "Usage: %s [options]" prog

let get_cat_flags filename libdir =
  let libfind =
    let module ML = MyLib.Make (struct
      let includes = []
      let env = Some "HERDLIB"
      let libdir = libdir
      let debug = false
    end) in
    ML.find
  in
  let module P = ParseModel.Make (struct
    let debug = false
    let libfind = libfind
  end) in
  let open AST in
  let _, _, ast = P.parse filename in
  List.fold_left
    (fun acc ins ->
      match ins with
      | Test ((_, _, _, _, name), Flagged) ->
          Option.fold ~none:acc ~some:(fun n -> StringSet.add n acc) name
      | _ -> acc)
    StringSet.empty ast

(* Extract all flag names from a multi-line string *)
let get_raised_flags s =
  let lines = String.split_on_char '\n' s in
  List.fold_left
    (fun acc line ->
      let line = String.trim line in
      if String.length line >= 6 && Misc.string_starts_with ~prefix:"Flag " line
      then
        let name = String.sub line 5 (String.length line - 5) |> String.trim in
        name :: acc
      else acc)
    [] lines

let get_dirs_and_confs filename =
  Misc.input_protect
    (fun ic ->
      let rec loop acc =
        try
          let line = input_line ic in
          (* Split line by whitespace *)
          let parts = String.split_on_char ' ' (String.trim line) in
          (* Filter out any empty entries (in case of extra spaces) *)
          let parts = List.filter (fun s -> s <> "") parts in
          match parts with
          | [ dir; file ] -> loop ((dir, Some file) :: acc)
          | [ dir ] -> loop ((dir, None) :: acc)
          | _ -> failwith ("Invalid line format: " ^ line)
        with End_of_file -> List.rev acc
      in
      loop [])
    filename

let get_each_litmus_in_dir dir =
  let handle = Unix.opendir dir in
  let readdir () = try Some (Unix.readdir handle) with End_of_file -> None in
  let rec next_litmus () =
    match readdir () with
    | Some name ->
        if TestHerd.is_litmus name then Some (Filename.concat dir name)
        else next_litmus ()
    | None -> None
  in
  let rec loop acc =
    match next_litmus () with
    | Some litmus -> loop (litmus :: acc)
    | None ->
        Unix.closedir handle;
        acc
  in
  loop []

let run flags =
  let cat_flags = get_cat_flags flags.assumptions_file flags.libdir in
  let dirs_and_confs = get_dirs_and_confs flags.dirs_and_confs_file in
  let debug = flags.debug in
  let remaining_flags =
    List.fold_left
      (fun remaining_flags (dir, conf) ->
        Printf.printf "Checking assumptions against %s ...\n%!" dir;
        let litmuses = get_each_litmus_in_dir dir in
        let remaining_flags =
          List.fold_left
            (fun remaining_flags litmus ->
              let _, stdout, stderr =
                TestHerd.run_herd ~bell:None ~cat:(Some flags.assumptions_file)
                  ~conf ~variants:[] ~libdir:flags.libdir flags.herd [ litmus ]
              in
              let stdout = String.concat "\n" stdout in
              let stderr = String.concat "\n" stderr in
              if debug && not (stderr = "") then
                Printf.printf "Stderr for test %s is %s\n\n" litmus stderr;
              let assert_regex =
                Str.regexp "[^ \t\n\r]+[ \t\n\r]+assertion failed"
              in
              try
                ignore (Str.search_forward assert_regex stderr 0);
                let failed_assertion = Str.matched_string stderr in
                Printf.printf "Test %s triggered %s\n" litmus failed_assertion;
                exit 1
              with Not_found ->
                let raised_flags = get_raised_flags stdout in
                List.fold_left
                  (fun remaining_flags flag ->
                    StringSet.remove flag remaining_flags)
                  remaining_flags raised_flags)
            remaining_flags litmuses
        in
        Printf.printf "Done\n\n%!";
        remaining_flags)
      cat_flags dirs_and_confs
  in
  if not (StringSet.is_empty remaining_flags) then begin
    Printf.printf "Remaining flags are:\n";
    StringSet.iter (fun flag -> Printf.printf "%s\n" flag) remaining_flags;
    exit 1
  end

let () =
  let herd = ref "" in
  let libdir = ref "" in
  let dirs_and_confs_file = ref "" in
  let assumptions_file = ref "" in
  let debug = ref false in

  let anon_args = ref [] in

  let options =
    [
      Args.is_file ("-herd-path", Arg.Set_string herd, "path to herd binary");
      Args.is_dir ("-libdir-path", Arg.Set_string libdir, "path to herd libdir");
      Args.is_file
        ( "-dirs-and-confs-path",
          Arg.Set_string dirs_and_confs_file,
          "path to the file specifying the directory of litmus tests and \
           configurations to use" );
      Args.is_file
        ( "-assumptions-path",
          Arg.Set_string assumptions_file,
          "path to the assumptions cat file" );
      ArgUtils.parse_bool "-debug" debug
        "show debug output for this test runner";
    ]
  in
  Arg.parse options (fun a -> anon_args := a :: !anon_args) usage;

  let exit_with_error msg =
    Printf.printf "%s: %s.\n" prog msg;
    Arg.usage options usage;
    exit 2
  in

  if !herd = "" then exit_with_error "Must set -herd-path";
  if !libdir = "" then exit_with_error "Must set -libdir-path";
  if !dirs_and_confs_file = "" then
    exit_with_error "Must set -dirs-and-confs-path";
  if !assumptions_file = "" then exit_with_error "Must set -assumptions-path";

  let flags =
    {
      herd = !herd;
      libdir = !libdir;
      dirs_and_confs_file = !dirs_and_confs_file;
      assumptions_file = !assumptions_file;
      debug = !debug;
    }
  in

  run flags;
  Printf.printf "Finished successfully!\n"
