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

type flags = {
  herd_path : string ;
  libdir_path : string ;
  shelf_path : string ;
  kinds_dir : string ;
  variants : string list ;
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
    match p.bell with
    | None -> TestHerd.run_herd ~cat:(prepend p.cat) ~variants:flags.variants flags.herd_path
    | Some bell -> TestHerd.run_herd ~bell:(prepend bell) ~cat:(prepend p.cat) ~variants:flags.variants flags.herd_path
  in
  match cmd flags.libdir_path litmuses with
  | stdout, [] ->
      let kind_of_log l = Log.(l.name, Option.get l.kind) in
      List.map kind_of_log (Log.of_string_list stdout)
  | _, stderr ->
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
      match p.bell with
      | None -> TestHerd.herd_command ~cat:(prepend p.cat) ~variants:flags.variants flags.herd_path
      | Some bell -> TestHerd.herd_command ~bell:(prepend bell) ~cat:(prepend p.cat) ~variants:flags.variants flags.herd_path
    in cmd flags.libdir_path illustrative_tests
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
  Printf.sprintf "Usage: %s [opts] (show|test|promote)" Sys.argv.(0) ;
  "" ;
  " show     Print the herd7 commands that would be run." ;
  " test     Compare the output of running herd7 on Catalogue tests against kinds files." ;
  " promote  Update kinds files to the output of herd7." ;
]

let check_flags flags =
  let check_set value =
    if value = "" then invalid_arg "must be set"
  in
  let dir value =
    if not (Sys.is_directory value) then invalid_arg "must be a directory"
  in
  let file value =
    if not (Sys.file_exists value && not (Sys.is_directory value)) then
      invalid_arg "must be a file"
  in
  List.iter (fun (name, validator, value) ->
    try
      check_set value ;
      validator value
    with Invalid_argument msg -> invalid_arg (Printf.sprintf "Flag %s %s" name msg)
  ) [
    "-herd-path", file, flags.herd_path ;
    "-libdir-path", dir, flags.libdir_path ;
    "-shelf-path", file, flags.shelf_path ;
    "-kinds-dir", dir, flags.kinds_dir ;
  ]

let () =
  let flags = ref {
    herd_path = "" ;
    libdir_path = "" ;
    shelf_path = "" ;
    kinds_dir = "" ;
    variants = [] ;
  } in
  let anon_args = ref [] in
  Arg.parse [
    ("-herd-path", Arg.String (fun p -> flags := {!flags with herd_path = p}), "path to herd binary") ;
    ("-libdir-path", Arg.String (fun p -> flags := {!flags with libdir_path = p}), "path to herd libdir") ;
    ("-kinds-dir", Arg.String (fun p -> flags := {!flags with kinds_dir = p}), "path to directory of kinds files to test against") ;
    ("-shelf-path", Arg.String (fun p -> flags := {!flags with shelf_path = p}), "path to shelf.py to test") ;
    ("-variant", Arg.String (fun v -> flags := {!flags with variants = !flags.variants @ [v]}), "variants") ;
  ] (fun a -> anon_args := a :: !anon_args) usage ;
  check_flags !flags ;
  match !anon_args with
  | "show" :: [] -> show_tests !flags
  | "test" :: [] -> run_tests !flags
  | "promote" :: [] -> promote_tests !flags
  | _ -> Printf.printf "%s\n" usage ; exit 1
