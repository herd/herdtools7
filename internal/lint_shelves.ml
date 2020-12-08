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

(** A tool to lint the contents of one or more Catalogue Shelf files. *)

module Option = Base.Option

let flatten_fields fields =
  let flatten_field (name, values) =
    List.map (fun v -> name, v) values
  in
  List.map flatten_field fields |> List.concat


let lint_shelf shelf_path =
  let shelf = Shelf.of_file shelf_path in
  let shelf_dir = Filename.dirname shelf_path in

  let file_fields =
    let open Shelf in [
      "cat", shelf.cats ;
      "config", shelf.configs ;
      "illustrative test", shelf.illustrative_tests ;
      "bell", Option.value shelf.bells ~default:[] ;
    ]
  in

  let file_missing p =
    not (Sys.file_exists (Filename.concat shelf_dir p))
  in

  let missing_files =
    flatten_fields file_fields
      |> List.filter (fun (_name, path) -> file_missing path)
  in
  match missing_files with
  | [] -> Ok ()
  | missing -> Error missing


let usage = String.concat "\n" [
  Printf.sprintf "Usage: %s <path/to/shelf.py> [more paths ...]" Sys.argv.(0) ;
  "" ;
  "Synopsis: Lint the contents of one or more Catalogue Shelf files."
]

let () =
  let shelves = ref [] in
  Arg.parse [] (fun s -> shelves := s :: !shelves) usage ;
  let shelves = List.rev !shelves in
  if List.length shelves = 0 then begin
    Printf.printf "%s\n" usage ;
    exit 1
  end ;

  let ok = ref true in
  List.iter (fun shelf ->
    match lint_shelf shelf with
    | Ok () -> ()
    | Error errors ->
        ok := false ;
        List.iter (fun (name, path) -> Printf.printf "Shelf %s: Missing %s: %s\n" shelf name path) errors ;
  ) shelves ;
  if !ok then begin
    if Unix.isatty Unix.stdout then
      Printf.printf "Shelves OK\n"
  end else
    exit 1
