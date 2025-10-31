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
open Typing

type file_type = NormalV1 | NormalV0 | PatchV1 | PatchV0

type args = {
  exec : bool;
  files : (file_type * string) list;
  opn : string option;
  print_ast : bool;
  print_lisp : bool;
  print_serialized : bool;
  print_typed : bool;
  show_rules : bool;
  strictness : strictness;
  output_format : Error.output_format;
  use_field_getter_extension : bool;
  use_fine_grained_side_effects : bool;
  use_conflicting_side_effects_extension : bool;
  override_mode : override_mode;
  no_primitives : bool;
  no_stdlib : bool;
  no_stdlib0 : bool;
  v0_use_split_chunks : bool;
}

exception Exit of int

let running_in_jsoo : bool =
  match Sys.backend_type with
  | Sys.Other s when s = "js_of_ocaml" -> true
  | _ -> false

let parse_args () =
  let show_rules = ref false in
  let target_files = ref [] in
  let exec = ref true in
  let print_ast = ref false in
  let print_serialized = ref false in
  let print_typed = ref false in
  let print_lisp = ref false in
  let opn = ref "" in
  let strictness : strictness ref = ref TypeCheck in
  let set_strictness s () = strictness := s in
  let show_version = ref false in
  let push_file file_type s = target_files := (file_type, s) :: !target_files in
  let output_format = ref Error.HumanReadable in
  let use_field_getter_extension = ref false in
  let override_mode = ref Permissive in
  let set_override_mode m () = override_mode := m in
  let no_primitives = ref false in
  let no_stdlib = ref false in
  let no_stdlib0 = ref false in
  let use_fine_grained_side_effects = ref false in
  let use_conflincting_side_effects_extension = ref false in
  let v0_use_split_chunks = ref false in

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
      ( "--print-lisp",
        Arg.Set print_lisp,
        " Print the parsed and typechecked AST in the Lisp object format." );
      ( "--format-csv",
        Arg.Unit (fun () -> output_format := Error.CSV),
        " Output the errors in a CSV format." );
      ( "--gnu-errors",
        Arg.Unit (fun () -> output_format := Error.GNU),
        " Output the errors using the GNU convention." );
      ( "--opn",
        Arg.Set_string opn,
        "OPN_FILE Parse the following opn file as main." );
      ( "--no-type-check",
        Arg.Unit (set_strictness Silence),
        " Do not type-check, only perform minimal type-inference. Default for \
         v0." );
      ( "--type-check-warn",
        Arg.Unit (set_strictness Warn),
        " Do not type-check, only perform minimal type-inference. Log typing \
         errors on stderr." );
      ( "--type-check-strict",
        Arg.Unit (set_strictness TypeCheck),
        " Perform type-checking, Fatal on any type-checking error. Default for \
         v1." );
      ( "--type-check-no-warn",
        Arg.Unit (set_strictness TypeCheckNoWarn),
        " Perform type-checking, fatal on any type-checking error, but don't \
         show any warnings." );
      ( "--v0-use-field-getter-extension",
        Arg.Set use_field_getter_extension,
        " Instruct the type-checker to use the field getter extension." );
      ( "--use-fine-grained-side-effects-extension",
        Arg.Set use_fine_grained_side_effects,
        " Instruct the type-checker to use the fine-grained side-effects \
         extension." );
      ( "--use-conflicting-side-effects-extension",
        Arg.Set use_conflincting_side_effects_extension,
        " Instruct the type-checker to use the conflicting side-effects \
         extension. Also implies the fine-grained side-effects extension." );
      ( "--show-rules",
        Arg.Set show_rules,
        " Instrument the interpreter and log to std rules used." );
      ( "--patch",
        Arg.String (push_file PatchV1),
        "patch_file Pass patches to the built AST." );
      ( "--patch0",
        Arg.String (push_file PatchV0),
        "patch_file Pass patches to the built AST." );
      ( "-0",
        Arg.String (push_file NormalV0),
        "filename Use ASLv0 parser for this file." );
      ( "-1",
        Arg.String (push_file NormalV1),
        "filename Use ASLv1 parser for this file. (default)" );
      ("--version", Arg.Set show_version, " Print version and exit.");
      ( "--overriding-permissive",
        Arg.Unit (set_override_mode Permissive),
        " Allow both `impdef` and `implementation` functions (default)." );
      ( "--overriding-warn-implementations",
        Arg.Unit (set_override_mode NoImplementations),
        " Warn if any `implementation` functions are defined." );
      ( "--overriding-warn-all-impdefs-overridden",
        Arg.Unit (set_override_mode AllImpdefsOverridden),
        " Warn if any `impdef` functions are not overridden by corresponding \
         `implementation`s." );
      ( "--no-primitives",
        Arg.Set no_primitives,
        " Do not use internal definitions for standard library subprograms." );
      ( "--no-stdlib",
        Arg.Set no_stdlib,
        " Do not use ASL's standard library. Implies `--no-primitives`." );
      ( "--no-stdlib0",
        Arg.Set no_stdlib0,
        " Do not use the ASL0 compatibility standard library. Default if there \
         is no ASLv0 file passed as argument." );
      ( "--v0-use-chunks",
        Arg.Set v0_use_split_chunks,
        " While lexing v0 files, split the files along separator comment \
         lines. Error display might be impacted." );
    ]
    |> Arg.align ?limit:None
  in

  let anon_fun = push_file NormalV1 in
  let prog =
    if Array.length Sys.argv > 0 then Filename.basename Sys.argv.(0)
    else "aslref"
  in

  let usage_msg =
    Printf.sprintf
      "ASL parser and interpreter.\n\nUSAGE:\n\t%s [OPTIONS] [FILE]\n" prog
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
      print_lisp = !print_lisp;
      strictness = !strictness;
      show_rules = !show_rules;
      output_format = !output_format;
      use_field_getter_extension = !use_field_getter_extension;
      use_fine_grained_side_effects = !use_fine_grained_side_effects;
      use_conflicting_side_effects_extension =
        !use_conflincting_side_effects_extension;
      override_mode = !override_mode;
      no_primitives = !no_primitives || !no_stdlib;
      no_stdlib = !no_stdlib;
      no_stdlib0 = !no_stdlib0;
      v0_use_split_chunks = !v0_use_split_chunks;
    }
  in

  let all_v1 =
    List.for_all
      (function
        | (NormalV1 | PatchV1), _ -> true | (NormalV0 | PatchV0), _ -> false)
      args.files
  in
  let args = { args with no_stdlib0 = args.no_stdlib0 || all_v1 } in

  let () =
    let ensure_exists s =
      if running_in_jsoo then ()
      else if Sys.file_exists s then ()
      else
        let () = Printf.eprintf "%s cannot find file %S\n%!" prog s in
        (* Arg.usage speclist usage_msg; *)
        exit 1
    in
    List.iter (fun (_, s) -> ensure_exists s) args.files;
    Option.iter ensure_exists args.opn
  in

  let () =
    if !show_version then
      let () =
        Printf.printf "aslref version %s rev %s\n%!" Version.version Version.rev
      in
      raise (Exit 0)
  in

  let () =
    if ASTUtils.list_is_empty args.files && Option.is_none args.opn then
      let () =
        Printf.eprintf
          "No files supplied! Run `aslref --help` for information on usage.\n"
      in
      raise (Exit 1)
  in

  args

let run_with (args : args) : unit =
  let parser_config =
    let v0_use_split_chunks = args.v0_use_split_chunks in
    let open Builder in
    { v0_use_split_chunks }
  in

  let or_exit f =
    if Printexc.backtrace_status () then f ()
    else
      match Error.intercept f () with
      | Ok res -> res
      | Error e ->
          let module EP = Error.ErrorPrinter (struct
            let output_format = args.output_format
          end) in
          EP.eprintln e;
          raise (Exit 1)
  in

  let extra_main =
    match args.opn with
    | None -> []
    | Some fname ->
        or_exit @@ fun () ->
        Builder.from_file ~ast_type:`Opn ~parser_config `ASLv1 fname
  in

  let ast =
    let folder (ft, fname) ast =
      let version =
        match ft with
        | NormalV0 | PatchV0 -> `ASLv0
        | NormalV1 | PatchV1 -> `ASLv1
      in
      let this_ast = Builder.from_file ~parser_config version fname in
      match ft with
      | NormalV0 | NormalV1 -> List.rev_append this_ast ast
      | PatchV1 | PatchV0 -> ASTUtils.patch ~src:ast ~patches:this_ast
    in
    or_exit @@ fun () -> List.fold_right folder args.files []
  in

  let ast = List.rev_append extra_main ast in

  let () = if args.print_ast then Format.printf "%a@." PP.pp_t ast in

  let () =
    if args.print_serialized then print_string (Serialize.t_to_string ast)
  in

  let ast =
    let open Builder in
    let added_stdlib =
      if args.no_stdlib then ast
      else with_stdlib ast ~no_stdlib0:args.no_stdlib0
    in
    if args.no_primitives then added_stdlib
    else with_primitives Native.DeterministicBackend.primitives added_stdlib
  in

  let () = if false then Format.eprintf "%a@." PP.pp_t ast in

  let () =
    match args.output_format with
    | Error.CSV ->
        Printf.eprintf
          {|"File","Start line","Start col","End line","End col","Exception label","Exception"
|}
    | Error.HumanReadable | Error.GNU -> ()
  in

  let module C = struct
    let output_format = args.output_format
    let check = args.strictness
    let print_typed = args.print_typed || args.print_lisp
    let use_field_getter_extension = args.use_field_getter_extension
    let override_mode = args.override_mode

    let fine_grained_side_effects =
      args.use_fine_grained_side_effects
      || args.use_conflicting_side_effects_extension

    let use_conflicting_side_effects_extension =
      args.use_conflicting_side_effects_extension
  end in
  let module T = Annotate (C) in
  let typed_ast, static_env = or_exit @@ fun () -> T.type_check_ast ast in

  let () =
    if args.print_typed then
      Format.printf "@[<v 2>Typed AST:@ %a@]@." PP.pp_t typed_ast
  in

  let () =
    if args.print_lisp then
      let lisp_ast = ToLisp.of_ast typed_ast in
      let lisp_static_env = ToLisp.of_static_env_global static_env in
      Lispobj.print_obj Format.std_formatter
        (Lispobj.Cons (lisp_static_env, lisp_ast))
  in

  let exit_code, used_rules =
    if args.exec then
      let instrumentation = if args.show_rules then true else false in
      or_exit @@ fun () ->
      let main_name = T.find_main static_env in
      Native.interpret ~instrumentation static_env main_name typed_ast
    else (0, [])
  in

  let () =
    if args.show_rules then
      let open Format in
      printf "@[<v 3>Used rules:@ %a@]@."
        (pp_print_list ~pp_sep:pp_print_cut Instrumentation.SemanticsRule.pp)
        used_rules
  in
  if exit_code != 0 then raise (Exit exit_code)

let () =
  try
    let args = parse_args () in
    run_with args
  with Exit n -> if running_in_jsoo then () else exit n
