(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

open Asllib

type args = {
  exec : bool;
  files : string list;
  opn : string option;
  print_ast : bool;
  print_serialized : bool;
  show_rules : bool;
  version : [ `ASLv0 | `ASLv1 ];
  strictness : Typing.strictness;
}

let parse_args () =
  let show_rules = ref false in
  let target_files = ref [] in
  let exec = ref true in
  let print_ast = ref false in
  let print_serialized = ref false in
  let print_typed = ref false in
  let asl_version = ref `ASLv1 in
  let set_v0 () = asl_version := `ASLv0 in
  let set_v1 () = asl_version := `ASLv1 in
  let opn = ref "" in
  let strictness = ref None in
  let set_strictness s () = strictness := Some s in
  let show_version = ref false in

  let speclist =
    [
      ("--exec", Arg.Set exec, " Execute the asl program (default).");
      ("--no-exec", Arg.Clear exec, " Don't execute the asl program.");
      ( "--print",
        Arg.Set print_ast,
        " Print the parsed AST to stdout before executing it." );
      ( "--serialize",
        Arg.Set print_serialized,
        " Print the parsed AST to stdout in the serialized format." );
      ( "--print-typed",
        Arg.Set print_typed,
        " Print the parsed AST after typing and before executing it." );
      ("-0", Arg.Unit set_v0, " Use ASLv0 parser.");
      ("-1", Arg.Unit set_v1, " Use ASLv1 parser. (default)");
      ( "--opn",
        Arg.Set_string opn,
        "OPN_FILE Parse the following opn file as main." );
      ( "--no-type-check",
        Arg.Unit (set_strictness `Silence),
        " Do not type-check, only perform minimal type-inference. Default for \
         v0." );
      ( "--type-check-warn",
        Arg.Unit (set_strictness `Warn),
        " Do not type-check, only perform minimal type-inference. Log typing \
         errors on stderr." );
      ( "--type-check-strict",
        Arg.Unit (set_strictness `TypeCheck),
        " Perform type-checking, Fatal on any type-checking error. Default for \
         v1." );
      ( "--show-rules",
        Arg.Set show_rules,
        " Instrument the interpreter and log to std rules used." );
      ("--version", Arg.Set show_version, " Print version and exit.");
    ]
    |> Arg.align ?limit:None
  in

  let anon_fun s = target_files := s :: !target_files in
  let prog =
    if Array.length Sys.argv > 0 then Filename.basename Sys.argv.(0)
    else "aslref"
  in

  let usage_msg =
    Printf.sprintf
      "ASL parser and interpreter.\n\nUSAGE:\n\t%s [OPTIONS] [FILE]\n" prog
  in
  let () = Arg.parse speclist anon_fun usage_msg in

  let strictness =
    match !strictness with
    | Some s -> s
    | None -> (
        match !asl_version with `ASLv0 -> `Silence | `ASLv1 -> `TypeCheck)
  in

  let args =
    {
      exec = !exec;
      files = !target_files;
      opn = (match !opn with "" -> None | s -> Some s);
      print_ast = !print_ast;
      print_serialized = !print_serialized;
      version = !asl_version;
      strictness;
      show_rules = !show_rules;
    }
  in

  let () =
    let ensure_exists s =
      if Sys.file_exists s then ()
      else
        let () = Printf.eprintf "%s cannot find file %S\n%!" prog s in
        (* Arg.usage speclist usage_msg; *)
        exit 1
    in
    List.iter ensure_exists args.files;
    Option.iter ensure_exists args.opn
  in

  let () =
    if !show_version then
      let () =
        Printf.printf "aslref version %s rev %s\n%!" Version.version Version.rev
      in
      exit 0
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

  let ast = List.rev_append Native.primitive_decls ast in
  let ast = Builder.with_stdlib ast in

  let () = if false then Format.eprintf "%a@." PP.pp_t ast in

  let static_env =
    or_exit @@ fun () ->
    Typing.type_check_ast args.strictness ast StaticEnv.empty
  in

  let exit_code, used_rules =
    if args.exec then
      let instrumentation = if args.show_rules then true else false in
      or_exit @@ fun () ->
      Native.interprete ~instrumentation ~static_env args.strictness ast
    else (0, [])
  in

  let () =
    if args.show_rules then
      let open Format in
      printf "@[<v 3>Used rules:@ %a@]@."
        (pp_print_list ~pp_sep:pp_print_cut Instrumentation.SemanticsRule.pp)
        used_rules
  in

  exit exit_code
