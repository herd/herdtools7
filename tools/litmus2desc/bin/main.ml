(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Top = Litmus2desc.Top

let format_descriptions ~latex_compat descriptions =
  let multiple_executions = List.length descriptions > 1 in
  let preamble =
    if multiple_executions then
      "This test asks whether one of the following candidate executions is \
       architecturally Allowed:"
    else
      "This test asks whether the following execution is architecturally \
       Allowed:"
  in
  let exec_sections =
    descriptions
    |> List.mapi (fun ix expl ->
        if multiple_executions then
          let str_ix =
            String.capitalize_ascii (Litmus2desc.Util.verbalize_index ix)
          in
          if latex_compat then
            Format.sprintf "\\paragraph{%s execution}@.@.%s@." str_ix expl
          else Format.sprintf "=== %s execution ===@.@.%s@." str_ix expl
        else expl)
    |> String.concat "\n"
  in
  Format.sprintf "%s@.@.%s@." preamble exec_sections |> String.trim

let () =
  let file_path = ref None in
  let usage = "Usage: litmus2desc [options] FILE" in

  let libdir = ref None in
  let latex = ref false in
  let describe_dep_path = ref false in
  let set_libdir s = libdir := Some s in
  let set_latex () = latex := true in
  let set_describe_dep_path () = describe_dep_path := true in
  let options =
    [
      ("-set-libdir", Arg.String set_libdir, "<path> set libdir");
      ("--set-libdir", Arg.String set_libdir, "<path> set libdir");
      ( "--latex",
        Arg.Unit set_latex,
        "Output test description as LaTeX. Default = disabled." );
      ("-l", Arg.Unit set_latex, "alias of --latex");
      ( "--describe-dep-path",
        Arg.Unit set_describe_dep_path,
        "Include intermediate instructions when describing dependencies. \
         Default = disabled." );
    ]
  in
  let process_arg arg =
    match !file_path with
    | None -> file_path := Some arg
    | Some _ ->
        prerr_endline "Only one FILE argument is allowed";
        Arg.usage options usage;
        exit 2
  in

  Arg.parse options process_arg usage;

  let file_path =
    match !file_path with
    | Some p -> p
    | None ->
        prerr_endline "Missing FILE argument.";
        Arg.usage options usage;
        exit 2
  in

  let contents =
    In_channel.with_open_text file_path (fun ch -> In_channel.input_all ch)
  in
  let descriptions =
    match
      Top.explain_test ?libdir:!libdir ~latex_compat:!latex
        ~describe_dep_path:!describe_dep_path contents
    with
    | Ok descriptions -> descriptions
    | Error reason ->
        Printf.eprintf "litmus2desc: unsupported: %s.\n" reason;
        exit 1
  in
  match descriptions with
  | [] ->
      prerr_endline
        "litmus2desc: no candidate executions satisfy the post-condition.";
      exit 1
  | _ -> print_endline (format_descriptions ~latex_compat:!latex descriptions)
