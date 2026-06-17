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

(** This module defines the interface to running ASLRef: its arguments and the
    [run_with] function to invoke ASLRef. *)

open Typing

type file_type = NormalV1 | NormalV0 | PatchV1 | PatchV0

type args = {
  exec : bool;
  files : (file_type * string) list;
  opn : string option;
  print_ast : bool;
  print_lisp : bool;
  print_serialized : bool;
  print_serialized_typed : bool;
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

let default_args =
  {
    exec = false;
    files = [];
    opn = None;
    print_ast = false;
    print_lisp = false;
    print_serialized = false;
    print_serialized_typed = false;
    print_typed = false;
    show_rules = false;
    strictness = TypeCheck;
    output_format = Error.HumanReadable;
    use_field_getter_extension = false;
    use_fine_grained_side_effects = false;
    use_conflicting_side_effects_extension = false;
    override_mode = Permissive;
    no_primitives = false;
    no_stdlib = false;
    no_stdlib0 = false;
    v0_use_split_chunks = false;
  }

exception Exit of int

(** Run ASLRef with the supplied arguments. This function never returns: it
    raises an [Exit] exception containing ASLRef's exit code. *)
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

  let () = if args.print_serialized then Serialize.output_to_chan stdout ast in

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

    let print_typed =
      args.print_typed || args.print_lisp || args.print_serialized_typed

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
    if args.print_serialized_typed then
      Serialize.output_to_chan ?newline:(Some true) stdout typed_ast
  in

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
  raise (Exit exit_code)
