(** ASL compliance test runner using ASLRef's structured runner API. *)

exception Runner_error of string

(** [raise_errorf fmt] formats and raises a [Runner_error]. *)
let raise_errorf fmt =
  Printf.ksprintf (fun message -> raise (Runner_error message)) fmt

module Config : sig
  type t = { suite_path : string; promote : bool }
  (** Runner configuration parsed from the command line. *)

  val parse_args : string array -> t
  (** [parse_args argv] parses command-line arguments. Raises [Runner_error] for
      invalid arguments. *)
end = struct
  type t = { suite_path : string; promote : bool }

  (** [usage program_name] returns the command-line usage string. *)
  let usage program_name =
    Printf.sprintf "usage: %s [--suite DIR] [--promote]\n" program_name

  (** [directory_exists path] is [true] when [path] exists and is a directory.
  *)
  let directory_exists path = Sys.file_exists path && Sys.is_directory path

  (** [default_suite_path ()] returns the default suite path for commands run
      from either the repository root or [asllib]. *)
  let default_suite_path () =
    if directory_exists "asllib/compliance" then "asllib/compliance"
    else if directory_exists "compliance" then "compliance"
    else "asllib/compliance"

  let parse_args argv =
    let suite_path = ref (default_suite_path ()) in
    let promote = ref false in
    let specs =
      [
        ("--suite", Arg.Set_string suite_path, "DIR Compliance suite directory");
        ("--promote", Arg.Set promote, "Rewrite expected YAML files");
      ]
    in
    let program_name = if Array.length argv = 0 then "asltest" else argv.(0) in
    try
      Arg.parse_argv argv specs
        (fun arg ->
          raise (Arg.Bad (Printf.sprintf "unexpected argument %S" arg)))
        (usage program_name);
      { suite_path = !suite_path; promote = !promote }
    with Arg.Bad message -> raise_errorf "%s" message
end

module Discovery : sig
  type test = { asl_path : string; metadata_path : string }
  (** One discovered compliance test. *)

  val discover_tests : string -> test list
  (** [discover_tests suite_path] returns all sibling ASL/YAML test pairs.
      Raises [Runner_error] when a source or metadata file is missing its
      sibling. *)
end = struct
  type test = { asl_path : string; metadata_path : string }

  (** [sorted_directory path] returns deterministic directory entries. *)
  let sorted_directory path =
    Sys.readdir path |> Array.to_list |> List.sort String.compare

  (** [missing_metadata_error metadata_path] explains which metadata file is
      missing. *)
  let missing_metadata_error metadata_path =
    Printf.sprintf "%s: expected metadata file is missing" metadata_path

  (** [missing_asl_error asl_path] explains which ASL source file is missing. *)
  let missing_asl_error asl_path =
    Printf.sprintf "%s: expected ASL source file is missing" asl_path

  (** [candidate_base path] returns the test base path represented by [path],
      when [path] is an ASL source or YAML metadata file. *)
  let candidate_base path =
    if String.ends_with ~suffix:".asl" path then
      Some (Filename.chop_suffix path ".asl")
    else if String.ends_with ~suffix:".yaml" path then
      Some (Filename.chop_suffix path ".yaml")
    else None

  (** [collect_candidate_bases directory] returns sorted test base paths found
      under [directory]. *)
  let collect_candidate_bases directory =
    let rec collect directory bases =
      sorted_directory directory
      |> List.fold_left
           (fun bases entry ->
             let path = Filename.concat directory entry in
             if Sys.is_directory path then
               (* The schema directory contains YAML schema files, not test
                  metadata files paired with ASL sources. *)
               if String.equal entry "schema" then bases else collect path bases
             else
               match candidate_base path with
               | None -> bases
               | Some base -> base :: bases)
           bases
    in
    collect directory [] |> List.sort_uniq String.compare

  (** [validate_candidate_base base] checks that [base] has sibling ASL and YAML
      files. *)
  let validate_candidate_base base =
    let asl_path = base ^ ".asl" in
    let metadata_path = base ^ ".yaml" in
    match (Sys.file_exists asl_path, Sys.file_exists metadata_path) with
    | true, true -> Ok { asl_path; metadata_path }
    | false, true -> Error (missing_asl_error asl_path)
    | true, false -> Error (missing_metadata_error metadata_path)
    | false, false ->
        Error
          (Printf.sprintf "%s and %s: expected test files are missing" asl_path
             metadata_path)

  let discover_tests suite_path =
    let bases = collect_candidate_bases suite_path in
    let tests, errors =
      List.fold_right
        (fun base (tests, errors) ->
          match validate_candidate_base base with
          | Ok test -> (test :: tests, errors)
          | Error error -> (tests, error :: errors))
        bases ([], [])
    in
    match (tests, errors) with
    | tests, [] -> tests
    | _, errors -> raise_errorf "%s" (String.concat "\n" (List.rev errors))
end

module Execution : sig
  type result =
    | Completed of { expected : Metadata.t; actual : Metadata.t }
    | Failed of string  (** Result of running one compliance test. *)

  val run_test : Discovery.test -> result
  (** [run_test test] runs one compliance test. *)
end = struct
  type result =
    | Completed of { expected : Metadata.t; actual : Metadata.t }
    | Failed of string

  (** [with_captured_stdout run] runs [run] while capturing stdout. *)
  let with_captured_stdout run =
    let read_file path = In_channel.with_open_text path In_channel.input_all in
    let redirect_fd_and_close ~from_fd ~to_fd =
      Unix.dup2 from_fd to_fd;
      Unix.close from_fd
    in
    let temp_stdout_file = Filename.temp_file "asl-compliance-" ".stdout" in
    let stdout_fd = Unix.descr_of_out_channel stdout in
    (* Keep a duplicate of the original stdout file descriptor so it can be
       restored after [run] finishes or raises. *)
    let saved_stdout_fd = Unix.dup stdout_fd in
    let output_fd =
      Unix.openfile temp_stdout_file
        [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
        0o600
    in
    Fun.protect
      ~finally:(fun () ->
        flush stdout;
        (* Restore stdout even when [run] raises,
          then remove the temporary stdout file. *)
        redirect_fd_and_close ~from_fd:saved_stdout_fd ~to_fd:stdout_fd;
        (try Unix.close output_fd with Unix.Unix_error _ -> ());
        try Sys.remove temp_stdout_file with Sys_error _ -> ())
      (fun () ->
        flush stdout;
        (* Redirect stdout to the temporary file while ASLRef runs. *)
        redirect_fd_and_close ~from_fd:output_fd ~to_fd:stdout_fd;
        let result = run () in
        flush stdout;
        (* Restore stdout before reading the captured output
          and returning to the caller. *)
        Unix.dup2 saved_stdout_fd stdout_fd;
        let output = read_file temp_stdout_file in
        (result, output))

  (** [args_for_test metadata asl_path] builds ASLRef arguments for one test. *)
  let args_for_test metadata asl_path =
    let exec =
      match metadata.Metadata.mode with
      | Metadata.Exec -> true
      | Metadata.No_exec -> false
    in
    let open Asllib.Runner in
    {
      default_args with
      exec;
      files = [ (NormalV1, asl_path) ];
      (* The compliance suite is ASLv1-only. *)
      no_stdlib0 = true;
    }

  (** [successful_metadata expected output] builds successful actual metadata.
  *)
  let successful_metadata expected output =
    let open Metadata in
    {
      mode = expected.mode;
      outcome = Success;
      output;
      error = None;
      error_line = None;
      info = expected.info;
    }

  (** [failing_metadata expected output error_code error_line diagnostic] builds
      failing actual metadata. *)
  let failing_metadata expected output error_code error_line diagnostic =
    let open Metadata in
    {
      mode = expected.mode;
      outcome = Failure;
      output;
      error = error_code;
      error_line;
      info =
        (match expected.info with
        | Some _ -> expected.info
        | None -> diagnostic);
    }

  (** [metadata_of_result expected result output] converts ASLRef's structured
      result and captured stdout to compliance metadata. *)
  let metadata_of_result expected result output =
    match result with
    | Asllib.Runner.RunResult.{ outcome = Success; _ } ->
        successful_metadata expected output
    | Asllib.Runner.RunResult.
        { outcome = Failure; error_code; error_line; diagnostic } ->
        failing_metadata expected output error_code error_line diagnostic

  let run_test test =
    let open Discovery in
    try
      let expected = Metadata.parse_file test.metadata_path in
      let args = args_for_test expected test.asl_path in
      let result, output =
        with_captured_stdout (fun () ->
            Asllib.Runner.RunResult.run_with_result args)
      in
      Completed { expected; actual = metadata_of_result expected result output }
    with
    | Metadata.Metadata_error message ->
        Failed (Printf.sprintf "%s: %s" test.metadata_path message)
    | exn ->
        Failed
          (Printf.sprintf "%s: ASLRef crashed while running this test: %s"
             test.asl_path (Printexc.to_string exn))
end

module Suite : sig
  val run : Config.t -> unit
  (** [run config] checks or promotes all discovered compliance tests. *)
end = struct
  (** [location label path line] formats an editor-friendly labelled source
      location. *)
  let location label path line =
    Printf.sprintf "%s %S, line %d, characters 0-0:" label path line

  (** [actual_asl_location test actual] returns an editor-friendly ASL location,
      using ASLRef's reported source line when available. *)
  let actual_asl_location test actual =
    let line = Option.value actual.Metadata.error_line ~default:1 in
    location "Test" test.Discovery.asl_path line

  (** [mismatch_message test expected actual] explains a metadata mismatch and
      includes jump targets for both the expected YAML and actual ASL source
      location. *)
  let mismatch_message test expected actual =
    let yaml_location = location "Metadata" test.Discovery.metadata_path 1 in
    let asl_location = actual_asl_location test actual in
    let differences =
      Format.asprintf "%a"
        (fun fmt () -> Metadata.pp_result_diff fmt ~expected ~actual)
        ()
    in
    String.concat "\n"
      [ asl_location; yaml_location; "Differences:"; differences ]

  (** [promote_test test actual] writes [actual] to the test metadata file. *)
  let promote_test test actual =
    Metadata.write_file test.Discovery.metadata_path actual

  (** [check_test config test] checks or promotes one compliance test. It
      returns [None] when the test passes or is promoted successfully, and
      [Some message] when the test contributes a failure report. *)
  let check_test config test =
    match Execution.run_test test with
    | Execution.Failed message -> Some message
    | Execution.Completed { expected; actual } ->
        if config.Config.promote then (
          promote_test test actual;
          None)
        else if Metadata.same_result expected actual then None
        else Some (mismatch_message test expected actual)

  let run config =
    let tests = Discovery.discover_tests config.Config.suite_path in
    let failures =
      List.fold_left
        (fun failures test ->
          match check_test config test with
          | None -> failures
          | Some message -> message :: failures)
        [] tests
      |> List.rev
    in
    match failures with
    | [] -> ()
    | failures -> raise_errorf "%s" (String.concat "\n\n" failures)
end

(** Program entry point. *)
let () =
  try
    let config = Config.parse_args Sys.argv in
    Suite.run config
  with
  | Runner_error message | Metadata.Metadata_error message ->
      prerr_endline message;
      exit 1
  | Arg.Help message ->
      print_string message;
      exit 0
