(****************************************************************************************************************)
(*  SPDX-FileCopyrightText: Copyright 2022-2024 Arm Limited and/or its affiliates <open-source-office@arm.com>  *)
(*  SPDX-License-Identifier: BSD-3-Clause                                                                       *)
(****************************************************************************************************************)

open Carpenter_lib
open Asllib

type 'a target = Itself | BInterp of 'a

let pp_target fmt = function
  | Itself -> Format.fprintf fmt "no"
  | BInterp s -> Format.fprintf fmt "BInterp at %s" s

let isatty chan = Unix.descr_of_out_channel chan |> Unix.isatty
let progress_frequency = 1

let print_progress should_progress =
  if should_progress then fun i n ->
    if i mod progress_frequency == 0 then Printf.eprintf "\r%d/%d%!" i n else ()
  else fun _i _n -> ()

let hide_progress should_progress () =
  if should_progress then
    Printf.eprintf "\r                                          \r%!"

let setup_logs lvl =
  let open Logs in
  set_level lvl;
  set_reporter (format_reporter ());
  debug (fun m ->
      m "Logs initialized at level %a" (Format.pp_print_option pp_level) lvl);
  ()

let setup_random =
  let print seed = Printf.printf "SEED: 0x%x\n%!" seed in
  fun print_seed ->
    let open Random in
    function
    | None ->
        self_init ();
        if print_seed then (
          let seed = bits64 () |> Int64.to_int in
          print seed;
          init seed)
    | Some seed ->
        if print_seed then print seed;
        init seed

(* --------------------------------------------------------------------------
                                Do the real work
   --------------------------------------------------------------------------*)

let print_ast filename ast =
  Logs.info (fun m -> m "Printing AST in %s" filename);
  let oc = open_out filename in
  let fmt = Format.formatter_of_out_channel oc in
  PP.pp_t fmt ast;
  Format.pp_print_flush fmt ();
  close_out oc

let filename =
  let timestamp =
    let open Unix in
    let now = localtime (time ()) in
    Printf.sprintf "%d-%02d-%02dT%02d:%02d:%02d" (1900 + now.tm_year)
      (now.tm_mon + 1) now.tm_mday now.tm_hour now.tm_min now.tm_sec
  in
  fun o i -> Printf.sprintf "%s-%04d.asl" timestamp i |> Filename.concat o

let print_one progress o n i ast =
  print_progress progress i n;
  print_ast (filename o i) ast

let filter_instr = function
  | Some i ->
      fun ast ->
        Logs.debug (fun m -> m "@[<v>Generated AST:@ %a@]" PP.pp_t ast);
        let _res, used_rules = Comparator.get_ref_result_instr ast in
        let res = List.mem i used_rules in
        if not res then
          Logs.debug (fun m ->
              m "Did not use rule %a. Ignoring."
                Instrumentation.SemanticsRule.pp i);
        res
  | None -> Fun.const true

let random_asts (small : bool) config : AST.t Seq.t =
  let asts =
    let module Generator = RandomAST.Typed ((val config : Config.S)) in
    let open QCheck2.Gen in
    let sizes = if small then small_nat else nat in
    sized_size sizes Generator.ast
  in
  let gen_one =
    let rand = Random.State.make_self_init () in
    fun () -> QCheck2.Gen.generate1 ~rand asts
  in
  Seq.forever gen_one

let smallests_asts config : AST.t Seq.t =
  let module IFSeq = Feat.Enum.IFSeq in
  let module Enums = ASTEnums.Make ((val config : Config.S)) in
  let asts_of_size n = IFSeq.to_seq (Enums.asts n) Seq.empty in
  Seq.flat_map asts_of_size (Seq.ints 0)

let generalized_generate asts (config, instr, o, progress) n =
  asts config
  |> Seq.filter (filter_instr instr)
  |> Seq.take n
  |> Seq.iteri (print_one progress o n);
  hide_progress progress ()

let generate_enum c = generalized_generate smallests_asts c
let generate_random small c = generalized_generate (random_asts small) c

let generalized_fuzz (asts : 'a Seq.t) (on_ast : 'a -> AST.t option) progress o
    n =
  let do_one (i, counter) ast =
    print_progress progress i n;
    match on_ast ast with
    | None -> (succ i, counter)
    | Some ast ->
        Logs.info (fun m -> m "Discrepancy found.");
        let filename = filename o counter in
        print_ast filename ast;
        (succ i, succ counter)
  in
  let _i = asts |> Seq.take n |> Seq.fold_left do_one (0, 0) in
  hide_progress progress ()

let random_trees config small =
  let asts =
    let module Generator = RandomAST.Typed ((val config : Config.S)) in
    let open QCheck2.Gen in
    let sizes = if small then small_int else int in
    sized_size sizes Generator.ast
  in
  let gen_one () = QCheck2.Gen.generate_tree asts in
  Seq.forever gen_one

let read_files files =
  List.to_seq files
  |> Seq.filter_map (fun file ->
         try Some (Builder.from_file `ASLv1 file) with _ -> None)

let get_ref_result instr ast =
  match instr with
  | Some i ->
      let res, used_rules = Comparator.get_ref_result_instr ast in
      (res, List.mem i used_rules)
  | None -> (Comparator.get_ref_result ast, true)

let on_ast_itself instr _type_only ast =
  Logs.debug (fun m -> m "@[Trying a new AST:@ %a@]" PP.pp_t ast);
  try
    match get_ref_result instr ast with
    | _, false -> None
    | Error msg, true ->
        if String.starts_with ~prefix:"Uncaught exception" msg then Some ast
        else None
    | Ok (), true -> None
  with _ -> Some ast

let on_ast_binterp binterp instr type_only ast =
  Logs.debug (fun m -> m "Analyzing generated AST with BInterp.");
  let ref_result, instr_match =
    match instr with
    | Some i ->
        let res, used_rules = Comparator.get_ref_result_instr ast in
        (res, List.mem i used_rules)
    | None -> (Comparator.get_ref_result ast, true)
  in
  if not instr_match then None
  else
    let binterp_result = BInterp.run_one_ast binterp type_only ast in
    if Comparator.compare_results ~ref_result ~binterp_result then None
    else Some ast

let with_on_ast target f =
  match target with
  | Itself -> f on_ast_itself
  | BInterp binterp_path ->
      BInterp.with_binterp binterp_path @@ fun binterp ->
      f (on_ast_binterp binterp)

let rec on_ast_tree on_ast ast_tree =
  let ast = QCheck2.Tree.root ast_tree in
  match on_ast ast with
  | None -> None
  | Some ast1 ->
      (* Generate all the children *)
      QCheck2.Tree.children ast_tree
      |> (* Lazily execute them *)
      Seq.map (on_ast_tree on_ast)
      |> (* Append at the end our previous error *)
      Fun.flip Seq.append (Seq.return (Some ast1))
      |> (* Find the first child that has an error. *)
      Seq.find_map Fun.id

let fuzz small (config, instr, type_only, target, o, progress) n =
  with_on_ast target @@ fun on_ast ->
  generalized_fuzz (random_asts small config) (on_ast instr type_only) progress
    o n

let bet (config, instr, type_only, target, o, progress) n =
  with_on_ast target @@ fun on_ast ->
  generalized_fuzz (smallests_asts config) (on_ast instr type_only) progress o n

let quickcheck small (config, instr, type_only, target, o, progress) n =
  with_on_ast target @@ fun on_ast ->
  let on_ast = on_ast_tree (on_ast instr type_only) in
  generalized_fuzz (random_trees config small) on_ast progress o n

let fuzz_files (_config, instr, type_only, target, o, progress) files =
  with_on_ast target @@ fun on_ast ->
  generalized_fuzz (read_files files) (on_ast instr type_only) progress o
    (List.length files + 1)

let execute () type_only target files =
  let open Format in
  let log_result res =
    printf "%a@\n"
      (pp_print_result
         ~ok:(fun f () -> pp_print_string f "ok")
         ~error:(fun f s -> fprintf f "Error: %s" s))
      res
  in
  let on_filename run file =
    printf "// results for file: %s@\n" file;
    file |> run |> log_result;
    printf "// end of results for file: %s@\n@\n" file
  in
  match target with
  | Itself ->
      let on_file file =
        file |> Asllib.Builder.from_file `ASLv1 |> Comparator.get_ref_result
      in
      List.iter (on_filename on_file) files
  | BInterp binterp_path ->
      BInterp.with_binterp binterp_path @@ fun binterp ->
      List.iter (on_filename (BInterp.run_one binterp type_only)) files

(* --------------------------------------------------------------------------
                            Command line interface
   --------------------------------------------------------------------------*)

module Cmd = struct
  open Cmdliner

  (* ----------------------------------------------------------------------- *)
  (* Arguments *)

  let o =
    let doc = "Output directory." in
    let docs = Manpage.s_common_options in
    Arg.(
      value & opt dir "." & info [ "o"; "output-dir" ] ~docv:"DIR" ~doc ~docs)

  let n =
    let doc = "Number of test to generate." in
    Arg.(required & pos 0 (some int) None & info [] ~docv:"N" ~doc)

  let files =
    let doc = "Files to run." in
    Arg.(value & pos_all non_dir_file [] & info [] ~docv:"FILES" ~doc)

  let instr =
    let doc =
      "Instrument ASLRef execution and filter-out ASTs that do not need \
       $(docv) to execute."
    and docs = Manpage.s_common_options
    and docv = "RULE"
    and parse s =
      try Ok (Instrumentation.SemanticsRule.of_string s)
      with Not_found -> Error ("Cannot find instrumentation rule " ^ s)
    and print = Instrumentation.SemanticsRule.pp in
    let open Arg in
    let c = some & conv' ~docv (parse, print) in
    value & opt c None & info [ "instrument"; "rule" ] ~docs ~docv ~doc

  let binterp_path =
    let doc = "Path for the BInterp executable." in
    let docs = Manpage.s_common_options in
    let binterp c =
      let make_binterp s = BInterp s in
      let parse s = Arg.conv_parser c s |> Result.map make_binterp
      and print = pp_target in
      Arg.conv (parse, print)
    in
    let open Arg in
    value
    & opt (binterp non_dir_file) Itself
    & info [ "binterp" ] ~docv:"BINTERP" ~doc ~docs

  let type_only =
    let doc = "Only test the type-checker and do not run the ASTs." in
    let docs = Manpage.s_common_options in
    Arg.(value & flag & info [ "type-only" ] ~doc ~docs)

  let small =
    let doc = "Generate ASTs of smaller sizes." in
    Arg.(value & flag & info [ "small" ] ~doc)

  let log_level =
    let docs = Manpage.s_common_options in
    Logs_cli.level ~docs ()

  let print_seed =
    let doc = "Print seed before running." in
    let docs = Manpage.s_common_options in
    Arg.(value & flag & info [ "print-seed" ] ~doc ~docs)

  let seed =
    let doc =
      "Set seed to $(docv) before running. If absent, uses system-dependant \
       random sources (e.g. /dev/urandom if available)."
    and docs = Manpage.s_common_options
    and docv = "SEED" in
    Arg.(value & opt (some int) None & info [ "seed" ] ~doc ~docs ~docv)

  let config =
    let doc = "File to parse to set generation properties."
    and docs = Manpage.s_common_options
    and docv = "CONFIG_FILE" in
    Arg.(
      value
      & opt (some non_dir_file) None
      & info [ "c"; "config"; "config-file" ] ~doc ~docs ~docv)

  let progress =
    let open Arg in
    let docv = "PROGRESS" and docs = Manpage.s_common_options in
    let progress =
      let doc =
        "Show a progress number as output. (default to true on a tty)"
      in
      (true, info [ "progress" ] ~doc ~docs ~docv)
    and no_progress =
      let doc =
        "Do not show a progress number as output. (default to false on a tty)"
      in
      (false, info [ "no-progress" ] ~doc ~docs ~docv)
    in
    let default = isatty stderr in
    value & vflag default [ progress; no_progress ]

  (* -----------------------------------------------------------------------*)
  (* Terms *)

  let setup_logs = Term.(const setup_logs $ log_level)
  let setup_random = Term.(const setup_random $ print_seed $ seed)

  let common_options =
    let make_config = function
      | Some f -> Config.Parse.of_file f
      | None -> Config.default_config
    in
    let make () () progress config instr o =
      (make_config config, instr, o, progress)
    in
    Term.(
      const make $ setup_logs $ setup_random $ progress $ config $ instr $ o)

  let generate_random =
    Term.(const generate_random $ small $ common_options $ n)

  let generate_enum = Term.(const generate_enum $ common_options $ n)

  let fuzzing_options =
    let t type_only binterp_path (config, instr, o, progress) =
      (config, instr, type_only, binterp_path, o, progress)
    in
    Term.(const t $ type_only $ binterp_path $ common_options)

  let fuzz = Term.(const fuzz $ small $ fuzzing_options $ n)
  let bet = Term.(const bet $ fuzzing_options $ n)
  let quickcheck = Term.(const quickcheck $ small $ fuzzing_options $ n)
  let fuzz_files = Term.(const fuzz_files $ fuzzing_options $ files)

  let execute =
    Term.(const execute $ setup_logs $ type_only $ binterp_path $ files)

  (* -----------------------------------------------------------------------*)
  (* Commands *)

  let cmd_default =
    let help _o = `Help (`Auto, None) in
    Term.(const help $ common_options |> ret)

  let cmd_fuzz_default =
    let help _ = `Help (`Auto, Some "fuzz") in
    Term.(const help $ fuzzing_options |> ret)

  let cmd_generate_random =
    let doc = "Generate N randoms ASTs and write them to DIR." in
    let info = Cmd.info "random" ~doc in
    Cmd.v info generate_random

  let cmd_generate_enum =
    let doc = "Generate the N smallest ASTs and write them to DIR." in
    let info = Cmd.info "enum" ~doc in
    Cmd.v info generate_enum

  let cmd_generate =
    let doc = "ASL programs generation." in
    let info = Cmd.info "generate" ~doc in
    let default = cmd_default in
    Cmd.group ~default info [ cmd_generate_random; cmd_generate_enum ]

  let cmd_fuzz_random =
    let doc = "Fuzz BInterp w.r.t. ASLRef using randomly generated ASTs." in
    let info = Cmd.info "random" ~doc in
    Cmd.v info fuzz

  let cmd_fuzz_enum =
    let doc =
      "Fuzz BInterp w.r.t. ASLRef using an enumeration of ASTs of increasing \
       size."
    in
    let info = Cmd.info "enum" ~doc in
    Cmd.v info bet

  let cmd_fuzz_quickcheck =
    let doc =
      "QuickCheck style fuzzing for BInterp w.r.t. ASLRef using randomly \
       generated ASTs. Once a discrepancy is found, the tool will iterate over \
       all the possible sub-trees until it finds a local minima for the \
       discrepancy"
    in
    let info = Cmd.info "quickcheck" ~doc in
    Cmd.v info quickcheck

  let cmd_fuzz_files =
    let doc =
      "Fuzz on the tests passed as command line. Usual filters apply.\n\n\
       Current limitation: files will be reprocessed by ASLRef before passed \
       to any other interpreter, so any test that is not parsable by ASLRef \
       will be ignored."
    in
    let info = Cmd.info "files" ~doc in
    Cmd.v info fuzz_files

  let cmd_fuzz =
    let doc = "Fuzz BInterp w.r.t. ASLRef." in
    let info = Cmd.info "fuzz" ~doc in
    let default = cmd_fuzz_default in
    Cmd.group ~default info
      [ cmd_fuzz_random; cmd_fuzz_enum; cmd_fuzz_quickcheck; cmd_fuzz_files ]

  let cmd_execute =
    let doc = "Execute tests with BInterp." in
    let info = Cmd.info "execute" ~doc in
    Cmd.v info execute

  let cmd =
    let doc = "Test generation and fuzzing for ASL."
    and man =
      let config_file_text =
        "Configuration for the generated ASTs. Mostly constrains the generated \
         AST by removing/allowing certain constructors."
      in
      [ `Blocks [ `S Manpage.s_files; `I ("CONFIG_FILE", config_file_text) ] ]
    in
    let info = Cmd.info "carpenter" ~doc ~man in
    let default = cmd_default in
    Cmd.group ~default info [ cmd_generate; cmd_fuzz; cmd_execute ]

  let main () = Cmd.eval cmd
end

let () = exit (Cmd.main ())
