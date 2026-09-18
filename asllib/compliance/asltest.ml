let read_file path = In_channel.with_open_text path In_channel.input_all

let program_name =
  if Array.length Sys.argv = 0 then "asltest"
  else Filename.basename Sys.argv.(0)

let print_usage_and_exit () =
  Printf.eprintf "usage: %s FILESTEM\n" program_name;
  exit 1

let parse_args () =
  if not (Int.equal (Array.length Sys.argv) 2) then print_usage_and_exit ();
  Sys.argv.(1)

let fatal ~base s =
  Printf.eprintf "Error when testing %s:\n%s\n" base s;
  exit 1

module TestCase = struct
  type mode = Exec | NoExec

  let mode_exec, mode_noexec = ("exec", "no-exec")
  let string_of_mode = function Exec -> mode_exec | NoExec -> mode_noexec

  let mode_of_string ~base s =
    if String.equal s mode_exec then Exec
    else if String.equal s mode_noexec then NoExec
    else fatal ~base (Printf.sprintf "bad mode: %s" s)

  type outcome = Success | Failure

  let outcome_success, outcome_failure = ("SUCCESS", "FAILURE")

  let string_of_outcome = function
    | Success -> outcome_success
    | Failure -> outcome_failure

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

(** Read the mode field from a YAML file. *)
let yaml_value_to_mode ~base (yaml : Yaml.value) =
  let bad_key s = fatal ~base (Printf.sprintf "bad YAML key: %s" s) in
  let extract_string key map =
    match List.assoc_opt key map with Some (`String s) -> s | _ -> bad_key key
  in
  match yaml with
  | `O map -> mode_of_string ~base (extract_string "mode" map)
  | _ -> fatal ~base "expected top-level YAML object"

let read_mode_from_file ~test_stem =
  let path = test_stem ^ ".yaml" in
  let yaml_value =
    match read_file path |> Yaml.of_string with
    | Ok y -> y
    | Error (`Msg s) -> fatal ~base:path s
  in
  yaml_value_to_mode ~base:path yaml_value

let output_of_buffer buffer =
  match Buffer.contents buffer with "" -> None | output -> Some output

let error_line_of_annotated error =
  if Asllib.ASTUtils.is_dummy_pos error then None
  else
    let position = error.Asllib.AST.pos_start in
    Some position.Lexing.pos_lnum

let run_aslref ~test_stem mode =
  let stdout_buffer = Buffer.create 256 in
  let stderr_buffer = Buffer.create 256 in
  let exec = match mode with Exec -> true | NoExec -> false in
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
      mode;
      outcome = Success;
      output = output_of_buffer stdout_buffer;
      error_code = None;
      error_line = None;
      info = None;
    }
  with Asllib.Error.ASLException error -> (
    match Asllib.Error.ErrorCode.of_error error with
    | None ->
        fatal ~base:test_stem
          (Printf.sprintf "ASLRef error is missing an error code:\n%s"
             (Asllib.Error.error_to_string error))
    | Some code ->
        let info = Asllib.Error.error_to_string error in
        {
          mode;
          outcome = Failure;
          output = output_of_buffer stdout_buffer;
          error_code = Some (Asllib.Error.ErrorCode.to_string code);
          error_line = error_line_of_annotated error;
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
            (function
              | k, Some v, style -> Some (key k, simple_scalar v style)
              | _, None, _ -> None)
            map;
      }
  in
  mapping
    [
      ("mode", Some (string_of_mode testcase.mode), `Plain);
      ("outcome", Some (string_of_outcome testcase.outcome), `Plain);
      ("output", testcase.output, `Literal);
      ("error", testcase.error_code, `Plain);
      ("error_line", Option.map Int.to_string testcase.error_line, `Plain);
      ("info", testcase.info, `Literal);
    ]

let generate_actual ~test_stem =
  let mode = read_mode_from_file ~test_stem in
  let actual = run_aslref ~test_stem mode in
  let yaml_of_testcase = test_case_to_yaml actual in
  match Yaml.yaml_to_string yaml_of_testcase with
  | Ok s -> Printf.printf "%s" s
  | Error (`Msg s) ->
      fatal ~base:test_stem
        (Printf.sprintf "failed to serialize generated metadata as YAML: %s" s)

let () =
  let test_stem = parse_args () in
  generate_actual ~test_stem
