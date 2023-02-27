(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
(* Authors:                                                                 *)
(* Hadrien Renaud, University College London, UK.                           *)
(****************************************************************************)

open Asllib

type args = {
  exec : bool;
  files : string list;
  opn : string option;
  print_ast : bool;
  print_serialized : bool;
  print_typed : bool;
  version : [ `ASLv0 | `ASLv1 ];
}

let parse_args () =
  let target_files = ref [] in
  let exec = ref true in
  let print_ast = ref false in
  let print_serialized = ref false in
  let print_typed = ref false in
  let version = ref `ASLv1 in
  let set_v0 () = version := `ASLv0 in
  let set_v1 () = version := `ASLv1 in
  let opn = ref "" in
  let speclist =
    [
      ("--parse-only", Arg.Clear exec, "Do not execute the asl program.");
      ("--exec", Arg.Set exec, "Execute the asl program.");
      ( "--print",
        Arg.Set print_ast,
        "Print the parsed AST to stdout before executing it." );
      ( "--serialize",
        Arg.Set print_serialized,
        "Print the parsed AST to stdout in the serialized format." );
      ( "--print-typed",
        Arg.Set print_typed,
        "Print the parsed AST after typing and before executing it." );
      ("-0", Arg.Unit set_v0, "Use ASLv0 parser.");
      ("-1", Arg.Unit set_v1, "Use ASLv1 parser. (default)");
      ("--opn", Arg.Set_string opn, "Parse the following opn file as main.");
    ]
  in
  let anon_fun s = target_files := s :: !target_files in
  let usage_msg =
    "ASL parser and interpreter.\n\nUSAGE:\n\tasli [OPTIONS] [FILE]\n"
  in
  let () = Arg.parse speclist anon_fun usage_msg in
  let args =
    {
      exec = !exec;
      files = !target_files;
      opn = (match !opn with "" -> None | s -> Some s);
      print_ast = !print_ast;
      print_serialized = !print_serialized;
      print_typed = !print_typed;
      version = !version;
    }
  in
  let () =
    if
      (not (List.for_all Sys.file_exists args.files))
      && Option.fold ~none:true ~some:Sys.file_exists args.opn
    then
      let () = Arg.usage speclist usage_msg in
      exit 1
  in
  args

let unordered_flat_map f =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (List.rev_append (f x) acc) xs
  in
  aux []

let or_exit f =
  match Error.intercept f () with
  | Ok res -> res
  | Error e ->
      Format.eprintf "%a@." Error.pp_error e;
      exit 1

let () =
  let args = parse_args () in

  let extra_main =
    match args.opn with
    | None -> []
    | Some fname ->
        or_exit @@ fun () -> Builder.from_file ~ast_type:`Opn args.version fname
  in

  let ast =
    or_exit @@ fun () ->
    unordered_flat_map (Builder.from_file args.version) args.files
  in

  let ast = List.rev_append extra_main ast in

  let () = if args.print_ast then Format.printf "%a@." PP.pp_t ast in

  let () =
    if args.print_serialized then print_string (Serialize.t_to_string ast)
  in

  let () =
    if args.print_typed then
      let annotated_ast = or_exit (fun () -> Typing.annotate_ast ast) in
      Format.printf "%a@." PP.pp_t annotated_ast
  in

  let () =
    if args.exec then
      let _ = or_exit (fun () -> Native.interprete ast) in
      ()
  in

  ()
