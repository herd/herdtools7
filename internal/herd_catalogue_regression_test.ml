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

module Fun = Base.Fun
module Option = Base.Option


(* Flags. *)

type path = string

type flags = {
  herd       : path ;
  libdir     : path ;
  shelf_path : path ;
  kinds_dir  : path ;
  variants   : string list ;
}


(* Permutations. *)

type permutation = {
  cat : string ;
  bell : string option ;
}

let string_of_permutation p =
  match p.bell with
  | None -> Printf.sprintf "cat = %S" p.cat
  | Some bell -> Printf.sprintf "cat = %S ; bell = %S" p.cat bell

let permutations_of_shelf shelf =
  let open Shelf in
  match shelf.bells with
  | None -> List.map (fun cat -> { cat = cat ; bell = None }) shelf.cats
  | Some bells ->
      List.flatten
        (List.map (fun cat ->
          List.map (fun bell ->
            { cat = cat ; bell = Some bell } )
          bells)
        shelf.cats)

let kinds_path_of_permutation kinds_dir p =
  let escape_filename n =
    String.map (fun c -> if c = '/' then '_' else c) n
  in
  let filename_of_permutation p =
    match p.bell with
    | None -> Printf.sprintf "kinds-cat=%s.txt" p.cat
    | Some bell -> Printf.sprintf "kinds-bell=%s&cat=%s.txt" bell p.cat
  in
  Filename.concat kinds_dir (escape_filename (filename_of_permutation p))

let herd_kinds_of_permutation flags shelf_dir litmuses p =
  let prepend path = Filename.concat shelf_dir path in
  let cmd =
    TestHerd.run_herd
      ~bell:(Option.map prepend p.bell)
      ~cat:(Some (prepend p.cat))
      ~conf:None
      ~variants:flags.variants
      ~libdir:flags.libdir
      flags.herd
  in
  match cmd litmuses with
  | 0,stdout, [] ->
      let kind_of_log l = Log.(l.name, Option.get l.kind) in
      List.map kind_of_log (Log.of_string_list stdout)
  | _, _, stderr ->
      failwith (Printf.sprintf "Herd returned stderr: %s" (String.concat "\n" stderr))


(* Shelves. *)

let various_of_shelf shelf_path =
  let shelf = Shelf.of_file shelf_path in
  let shelf_dir = Filename.dirname shelf_path in
  let illustrative_tests = List.map (Filename.concat shelf_dir) shelf.Shelf.illustrative_tests in
  permutations_of_shelf shelf, shelf_dir, illustrative_tests


(* Helpers. *)

let exit_1_if_any_files_missing ~description paths =
  match List.filter (Fun.negate Sys.file_exists) paths with
  | [] -> ()
  | missing ->
      List.iter (Printf.printf "Missing %s: %s\n" description) missing ;
      exit 1


(* Commands. *)

let show_tests flags =
  let permutations, shelf_dir, illustrative_tests = various_of_shelf flags.shelf_path in

  let prepend path = Filename.concat shelf_dir path in
  let command_of_permutation p =
    let cmd =
      TestHerd.herd_command
        ~bell:(Option.map prepend p.bell)
        ~cat:(Some (prepend p.cat))
        ~conf:None
        ~variants:flags.variants
        ~libdir:flags.libdir
        flags.herd
    in
    cmd illustrative_tests
  in
  permutations
    |> List.map command_of_permutation
    |> List.iter (Printf.printf "%s\n")


let run_tests flags =
  let permutations, shelf_dir, illustrative_tests = various_of_shelf flags.shelf_path in
  let kinds_paths = List.map (kinds_path_of_permutation flags.kinds_dir) permutations in

  exit_1_if_any_files_missing ~description:"illustrative test" illustrative_tests ;
  exit_1_if_any_files_missing ~description:"kinds.txt file" kinds_paths ;

  let result_of_permutation (kinds_path, p) =
    let expected = Kinds.of_file kinds_path in
    let actual = herd_kinds_of_permutation flags shelf_dir illustrative_tests p in
    if Kinds.compare actual expected <> 0 then begin
      Printf.printf "Kinds differs: kinds file = %s ; %s\n" kinds_path (string_of_permutation p) ;
      false
    end else
      true
  in
  let passed =
    List.combine kinds_paths permutations
      |> List.map result_of_permutation
      |> List.for_all (fun x -> x)
  in
  if passed then
    Printf.printf "Tests OK\n"
  else
    exit 1


let promote_tests flags =
  let permutations, shelf_dir, illustrative_tests = various_of_shelf flags.shelf_path in
  let kinds_paths = List.map (kinds_path_of_permutation flags.kinds_dir) permutations in

  exit_1_if_any_files_missing ~description:"illustrative test" illustrative_tests ;

  let kinds =
    List.map
      (herd_kinds_of_permutation flags shelf_dir illustrative_tests)
      permutations
  in
  List.map Kinds.to_string kinds
    |> List.combine kinds_paths
    |> List.iter (fun (p, k) -> Filesystem.write_file p (fun o -> output_string o k))


let usage = String.concat "\n" [
  Printf.sprintf "Usage: %s [options] (show|test|promote)" (Filename.basename Sys.argv.(0)) ;
  "" ;
  "Commands:" ;
  "  show     Print the herd7 commands that would be run." ;
  "  test     Compare the output of running herd7 on Catalogue tests against kinds files." ;
  "  promote  Update kinds files to the output of herd7." ;
  "" ;
  "Options:" ;
]

let () =
  (* Required arguments. *)
  let herd = ref "" in
  let libdir = ref "" in
  let shelf_path = ref "" in
  let kinds_dir = ref "" in

  (* Optional arguments. *)
  let variants = ref [] in

  let anon_args = ref [] in

  let options = [
    Args.is_file ("-herd-path",   Arg.Set_string herd,         "path to herd binary") ;
    Args.is_dir  ("-libdir-path", Arg.Set_string libdir,       "path to herd libdir") ;
    Args.is_dir  ("-kinds-dir",   Arg.Set_string kinds_dir,    "path to directory of kinds files to test against") ;
    Args.is_file ("-shelf-path",  Arg.Set_string shelf_path,   "path to shelf.py to test") ;
                  "-variant",     Args.append_string variants, "variant to pass to herd7" ;
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
  if !shelf_path = "" then
    exit_with_error "Must set -shelf-path" ;
  if !kinds_dir = "" then
    exit_with_error "Must set -kinds-dir" ;

  let flags = {
    herd = !herd ;
    libdir = !libdir ;
    shelf_path = !shelf_path ;
    kinds_dir = !kinds_dir ;
    variants = !variants ;
  } in
  match !anon_args with
  | "show" :: [] -> show_tests flags
  | "test" :: [] -> run_tests flags
  | "promote" :: [] -> promote_tests flags
  | _ -> exit_with_error "Must provide one command of: show, test, promote"
