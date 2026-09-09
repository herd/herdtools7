let read_file path = In_channel.with_open_text path In_channel.input_all

type comparison = { expected_path : string; actual_path : string }
type command = Generate_actual of string | Compare of comparison

let parse_args () =
  let command = ref None in
  let set_command command_value =
    match !command with
    | None -> command := Some command_value
    | Some _ -> raise (Arg.Bad "only one command may be specified")
  in
  let compare_expected_path = ref None in
  let specs =
    [
      ( "--base",
        Arg.String (fun test_stem -> set_command (Generate_actual test_stem)),
        "FILE file stem for compliance test (no suffix)" );
      ( "--compare",
        Arg.Tuple
          [
            Arg.String
              (fun expected_path -> compare_expected_path := Some expected_path);
            Arg.String
              (fun actual_path ->
                match !compare_expected_path with
                | Some expected_path ->
                    set_command (Compare { expected_path; actual_path })
                | None -> raise (Arg.Bad "--compare expects EXPECTED ACTUAL"));
          ],
        "EXPECTED ACTUAL compare two metadata files" );
    ]
  in
  let usage =
    let program_name =
      if Array.length Sys.argv = 0 then "asltest" else Sys.argv.(0)
    in
    Printf.sprintf "usage: %s [--base FILESTEM]\n" program_name
  in
  try
    Arg.parse specs
      (fun arg -> raise (Arg.Bad (Printf.sprintf "unexpected argument %S" arg)))
      usage;
    match !command with
    | Some command -> command
    | None -> raise (Arg.Bad "missing command")
  with Arg.Bad message -> failwith message

let fatal ~context s =
  Printf.eprintf "Error when testing %s:\n%s\n" context s;
  exit 1

module TestCase = struct
  type mode = Exec | NoExec

  let mode_exec, mode_noexec = ("exec", "no-exec")
  let string_of_mode = function Exec -> mode_exec | NoExec -> mode_noexec

  let mode_of_string ~context s =
    if String.equal s mode_exec then Exec
    else if String.equal s mode_noexec then NoExec
    else fatal ~context (Printf.sprintf "bad mode: %s" s)

  type outcome = Success | Failure

  let outcome_success, outcome_failure = ("SUCCESS", "FAILURE")

  let string_of_outcome = function
    | Success -> outcome_success
    | Failure -> outcome_failure

  let outcome_of_string ~context s =
    if String.equal s outcome_success then Success
    else if String.equal s outcome_failure then Failure
    else fatal ~context (Printf.sprintf "bad outcome: %s" s)

  type testcase = {
    mode : mode;
    outcome : outcome;
    output : string option;
    error_code : string option;
    error_line : int option;
    info : string option;
  }
end

open TestCase

(** Read a YAML file to a testcase. This is only for proof-of-concept: for the
    actual testsuite, we only need the mode to know how to run the file, and
    diffing will take care of the rest. *)
let yaml_value_to_testcase ~context (yaml : Yaml.value) =
  let bad_key s = fatal ~context (Printf.sprintf "bad YAML key: %s" s) in
  let extract_string key map =
    match List.assoc_opt key map with Some (`String s) -> s | _ -> bad_key key
  in
  let extract_string_opt key map =
    match List.assoc_opt key map with
    | Some (`String s) -> Some s
    | None -> None
    | _ -> bad_key key
  in
  let extract_int_opt key map =
    match List.assoc_opt key map with
    | Some (`Float f) -> Some (Float.to_int f)
    | None -> None
    | _ -> bad_key key
  in
  match yaml with
  | `O map ->
      let mode = mode_of_string ~context (extract_string "mode" map) in
      let outcome = outcome_of_string ~context (extract_string "outcome" map) in
      let output = extract_string_opt "output" map in
      let error_code = extract_string_opt "error" map in
      let error_line = extract_int_opt "error_line" map in
      let info = extract_string_opt "info" map in
      { mode; outcome; output; error_code; error_line; info }
  | _ -> fatal ~context "expected top-level YAML object"

module Validate = struct
  let rec yaml_to_json ~context (yaml : Yaml.value) : Yojson.Basic.t =
    match yaml with
    | `Null -> `Null
    | `Bool b -> `Bool b
    | `Float f -> `Float f
    | `String s -> `String s
    | `A values -> `List (List.map (yaml_to_json ~context) values)
    | `O map ->
        let json_map =
          List.map (fun (k, v) -> (k, yaml_to_json ~context v)) map
        in
        `Assoc json_map

  let check_yaml ~context (yaml : Yaml.value) =
    let schema_json =
      let schema = "../schema.yaml" in
      let schema_yaml =
        match read_file schema |> Yaml.of_string with
        | Ok y -> y
        | Error (`Msg s) -> fatal ~context s
      in
      yaml_to_json ~context schema_yaml
    in
    let validator =
      match
        Jsonschema.create_validator_from_json ~draft:Jsonschema.Draft2020_12
          ~schema:schema_json ()
      with
      | Ok validator -> validator
      | Error err ->
          let msg = Format.asprintf "%a" Jsonschema.pp_compile_error err in
          fatal ~context (Printf.sprintf "failed to compile schema:\n%s" msg)
    in
    let json = yaml_to_json ~context yaml in
    match Jsonschema.validate validator json with
    | Ok () -> ()
    | Error err ->
        let msg = Format.asprintf "%a" Jsonschema.pp_validation_error err in
        fatal ~context (Printf.sprintf "schema validation failed:\n%s" msg)
end

let read_testcase_from_yaml_file path =
  let yaml_value =
    match read_file path |> Yaml.of_string with
    | Ok y -> y
    | Error (`Msg s) -> fatal ~context:path s
  in
  Validate.check_yaml ~context:path yaml_value;
  yaml_value_to_testcase ~context:path yaml_value

let read_testcase_from_file ~test_stem =
  read_testcase_from_yaml_file (test_stem ^ ".yaml")

let output_of_buffer buffer =
  match Buffer.contents buffer with "" -> None | output -> Some output

let run_aslref ~test_stem testcase =
  let stdout_buffer = Buffer.create 256 in
  let stderr_buffer = Buffer.create 256 in
  let exec = match testcase.mode with Exec -> true | NoExec -> false in
  let args =
    Asllib.Runner.
      {
        default_args with
        exec;
        files = [ (NormalV1, test_stem ^ ".asl") ];
        no_stdlib0 = true;
        capture_output = Some (stdout_buffer, stderr_buffer);
      }
  in
  try
    let _exit_code = Asllib.Runner.run args in
    {
      testcase with
      outcome = Success;
      output = output_of_buffer stdout_buffer;
      error_code = None;
      error_line = None;
      info = None;
    }
  with Asllib.Error.ASLException error -> (
    match Asllib.Error.ErrorCode.of_error error with
    | None ->
        fatal ~context:test_stem
          (Printf.sprintf "ASLRef error is missing an error code:\n%s"
             (Asllib.Error.error_to_string error))
    | Some code ->
        let info = Asllib.Error.error_to_string error in
        {
          testcase with
          outcome = Failure;
          output = output_of_buffer stdout_buffer;
          error_code = Some (Asllib.Error.ErrorCode.to_string code);
          error_line =
            (if Asllib.ASTUtils.is_dummy_pos error then None
             else
               let position = error.Asllib.AST.pos_start in
               Some position.Lexing.pos_lnum);
          info = Some info;
        })

let test_case_to_yaml testcase : Yaml.yaml =
  let open Yaml in
  let simple_scalar value style =
    `Scalar
      {
        anchor = None;
        tag = None;
        value;
        plain_implicit = true;
        quoted_implicit = true;
        style;
      }
  in
  let key k = simple_scalar k `Plain in
  let mapping map =
    `O
      {
        m_anchor = None;
        m_tag = None;
        m_implicit = true;
        m_members =
          List.filter_map
            (fun (k, v, style) ->
              Option.map (fun v -> (key k, simple_scalar v style)) v)
            map;
      }
  in
  let to_output =
    mapping
      [
        ("mode", Some (string_of_mode testcase.mode), `Plain);
        ("outcome", Some (string_of_outcome testcase.outcome), `Plain);
        ("output", testcase.output, `Literal);
        ("error", testcase.error_code, `Plain);
        ("error_line", Option.map Int.to_string testcase.error_line, `Plain);
        ("info", testcase.info, `Literal);
      ]
  in
  to_output

let same_compliance_result expected actual =
  (* The generated actual metadata echoes [mode] for readability, but [mode]
     controls how the test is run; it is not an observed ASLRef result. *)
  String.equal
    (string_of_outcome expected.outcome)
    (string_of_outcome actual.outcome)
  && Option.equal String.equal expected.output actual.output
  && Option.equal String.equal expected.error_code actual.error_code
  && Option.equal Int.equal expected.error_line actual.error_line

let compliance_result_diff ~expected_path ~actual_path expected actual =
  let string_of_optional_string = function
    | None -> "<none>"
    | Some value -> value
  in
  let string_of_optional_int = function
    | None -> "<none>"
    | Some value -> Int.to_string value
  in
  let diff_field ~field ~expected ~actual =
    if String.equal expected actual then []
    else
      [
        Printf.sprintf "-%s: %S" field expected;
        Printf.sprintf "+%s: %S" field actual;
      ]
  in
  [
    Printf.sprintf "--- %s" expected_path;
    Printf.sprintf "+++ %s" actual_path;
    "@@";
  ]
  @ diff_field ~field:"outcome"
      ~expected:(string_of_outcome expected.outcome)
      ~actual:(string_of_outcome actual.outcome)
  @ diff_field ~field:"output"
      ~expected:(string_of_optional_string expected.output)
      ~actual:(string_of_optional_string actual.output)
  @ diff_field ~field:"error"
      ~expected:(string_of_optional_string expected.error_code)
      ~actual:(string_of_optional_string actual.error_code)
  @ diff_field ~field:"error_line"
      ~expected:(string_of_optional_int expected.error_line)
      ~actual:(string_of_optional_int actual.error_line)

let compare_testcases ~expected_path ~actual_path =
  let expected = read_testcase_from_yaml_file expected_path in
  let actual = read_testcase_from_yaml_file actual_path in
  if same_compliance_result expected actual then ()
  else
    fatal ~context:expected_path
      (String.concat "\n"
         (compliance_result_diff ~expected_path ~actual_path expected actual))

let generate_actual ~test_stem =
  let testcase = read_testcase_from_file ~test_stem in
  let actual = run_aslref ~test_stem testcase in
  let yaml_of_testcase = test_case_to_yaml actual in
  match Yaml.yaml_to_string yaml_of_testcase with
  | Ok s -> Printf.printf "%s" s
  | Error (`Msg s) ->
      fatal ~context:test_stem
        (Printf.sprintf "failed to serialize generated metadata as YAML: %s" s)

let () =
  match parse_args () with
  | Generate_actual test_stem -> generate_actual ~test_stem
  | Compare { expected_path; actual_path } ->
      compare_testcases ~expected_path ~actual_path
