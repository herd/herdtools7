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

module Fun = Base.Fun
module Option = Base.Option

exception Error of string

(* Flags. *)

type path = string

type flags = {
  herd       : path ;
  libdir     : path ;
  shelf_path : path ;
  kinds_path : path ;
  conf_path  : path ;
  index_path : path option ;
  variants   : string list ;
  verbose    : bool ;
  fast       : bool ;
}


(* Permutations. *)

type permutation = {
  cat : string ;
  cfg : string option ;
  bell : string option ;
}

let string_of_permutation p =
  match p.bell with
  | None -> Printf.sprintf "cat = %S" p.cat
  | Some bell -> Printf.sprintf "cat = %S ; bell = %S" p.cat bell

let some_head = function
  | None|Some [] -> None
  | Some (x::_) -> Some x

let one_of_shelf shelf =
  let open Shelf in
  let cat =
    match shelf.cats with
    | cat::_ -> cat
    | [] -> raise (Error "no cat file")
  and bell = some_head shelf.bells
  and cfg =
    match shelf.configs with
    | [] -> None
    | x::_ -> Some x in
  { cat; bell; cfg; }

let (>>=) o1 o2 = match o1 with
| "" -> o2
| _ -> Some o1


let herd_kinds_of_permutation ?j ?timeout flags shelf_dir litmuses p =
  let prepend path = Filename.concat shelf_dir path in
  let cmd =
    TestHerd.run_herd
      ~verbose:flags.verbose
      ~bell:(Option.map prepend p.bell)
      ~cat:(Some (prepend p.cat))
      ~conf:(flags.conf_path >>= Option.map prepend p.cfg)
      ~variants:flags.variants
      ~libdir:flags.libdir
      ~speedcheck:(if flags.fast then `Fast else `False)
      flags.herd ?j ?timeout
  in
  match cmd litmuses with
  | 0,stdout, [] ->
      let kind_of_log l = Log.(l.name, Option.get l.kind) in
      List.map kind_of_log (Log.of_string_list stdout)
  | _, _, stderr ->
      let lines = String.concat "\n" stderr in
      let msg = Printf.sprintf "Herd returned stderr:\n%s" lines in
      raise (Error msg)


(* Shelves. *)

let first_of_shelf shelf_path index_path =
  let shelf = Shelf.of_file shelf_path in
  let shelf_dir = Filename.dirname shelf_path in
  let tests =
    match index_path with
    | None ->
        List.map (Filename.concat shelf_dir) shelf.Shelf.tests
    | Some f ->
        Index.of_file f in
  one_of_shelf shelf, shelf_dir, tests

(* Helpers. *)

let exit_1_if_any_files_missing ~description paths =
  match List.filter (Fun.negate Sys.file_exists) paths with
  | [] -> ()
  | missing ->
      List.iter (Printf.printf "Missing %s: %s\n" description) missing ;
      raise (Error "Some files are missing")

(* Commands. *)

let show_tests ?j ?timeout flags =
  let cat, shelf_dir, tests =
    first_of_shelf flags.shelf_path flags.index_path in

  let prepend path = Filename.concat shelf_dir path in

  let command_of_permutation p =
    let cmd =
      TestHerd.herd_command
        ~bell:(Option.map prepend p.bell)
        ~cat:(Some (prepend p.cat))
        ~conf:(flags.conf_path >>= Option.map prepend p.cfg)
        ~variants:flags.variants
        ~libdir:flags.libdir
        ~checkfilter:true
        ~speedcheck:(if flags.fast then `Fast else `False)
        flags.herd ?j ?timeout
    in
    cmd tests
  in
    command_of_permutation cat
    |> Printf.printf "%s\n"

let run_tests ?j ?timeout flags =
  let cat, shelf_dir, tests =
    first_of_shelf flags.shelf_path flags.index_path in

  exit_1_if_any_files_missing ~description:"test" tests ;
  exit_1_if_any_files_missing ~description:"kinds.txt file" [flags.kinds_path] ;

  let result_of_permutation kinds_path p =
    let expected = Kinds.of_file kinds_path in
    let actual =
      herd_kinds_of_permutation ?j ?timeout flags shelf_dir tests p in
    let diff,miss,excess = Kinds.check ~expected ~actual in
    if Misc.consp miss then begin
      let pf =
        match miss with
        | [_] -> Printf.eprintf "Warning: test %s is not in reference kind file %s\n"
        | _ -> Printf.eprintf "Warning: tests %s are not in reference kind file %s\n" in
      pf (String.concat "," miss) kinds_path
      end ;
     if Misc.consp excess then begin
      let pf =
        match excess with
        | [_] -> Printf.eprintf "Warning: test %s is not in test base\n"
        | _ -> Printf.eprintf "Warning: tests %s are not in test base\n" in
      pf (String.concat "," excess) 
      end ;
    match diff with
    | [] -> true
    | rs ->
        let pp =
          List.map
            (fun (n,ke,ka) ->
              Printf.sprintf "%s: expected=%s, actual=%s"
                n (ConstrGen.pp_kind ke) (ConstrGen.pp_kind ka))
            rs in
        Printf.printf "Kinds differs: kinds file = %s ; %s\n"
          kinds_path (string_of_permutation p) ;
        List.iter (Printf.printf "%s\n") pp ;
        false in
  let passed = result_of_permutation flags.kinds_path cat in
  if not passed then exit 1


let promote_tests ?j flags =
  let cat, shelf_dir, tests =
    first_of_shelf flags.shelf_path flags.index_path in
  exit_1_if_any_files_missing ~description:"tests" tests ;

  let kinds =
    herd_kinds_of_permutation ?j flags shelf_dir tests cat
  in
  Filesystem.write_file flags.kinds_path
    (fun o -> output_string o (Kinds.to_string kinds))

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
  let kinds_path = ref "" in
  let conf_path = ref "" in
  let index_path = ref None in

  (* Optional arguments. *)
  let variants = ref [] in
  let j = ref None in
  let timeout = ref None in
  let anon_args = ref [] in
  let verbose = ref false in
  let fast = ref false in

  let options = [
    Args.verbose verbose ;
    Args.npar j ;
    "-herd-timeout",Arg.Float (fun f -> timeout := Some f), "<f> herd timeout";
    Args.is_file ("-herd-path",   Arg.Set_string herd,         "path to herd binary") ;
    Args.is_dir  ("-libdir-path", Arg.Set_string libdir,       "path to herd libdir") ;
    Args.is_file  ("-kinds-path",   Arg.Set_string kinds_path,    "path to directory of kinds files to test against") ;
    Args.is_file  ("-conf-path",   Arg.Set_string conf_path,    "path to configuration files (overrides cfg of shelf)") ;
    Args.is_file ("-shelf-path",  Arg.Set_string shelf_path,   "path to shelf.py to test") ;
    Args.is_file ("-index-path",  Arg.String (fun p -> index_path := Some p),
                  "path to index file of tests") ;
    "-variant",  Args.append_string variants, "variant to pass to herd7" ;
    "-fast", Arg.Set fast, "only check test kinds"
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
  if !kinds_path = "" then
    exit_with_error "Must set -kinds-path" ;

  let flags = {
    herd = !herd ;
    libdir = !libdir ;
    shelf_path = !shelf_path ;
    kinds_path = !kinds_path ;
    conf_path = !conf_path ;
    variants = !variants ;
    index_path = !index_path ;
    verbose = !verbose ;
    fast = !fast ;
    } in
  try
    let j = !j in
    let timeout = !timeout in
    match !anon_args with
    | "show" :: [] -> show_tests ?j ?timeout flags
    | "test" :: [] -> run_tests ?j ?timeout flags
    | "promote" :: [] -> promote_tests ?j flags
    | _ -> exit_with_error "Must provide one command of: show, test, promote"
  with
  | Error msg ->
      Printf.printf "Fatal error: %s\n" msg
