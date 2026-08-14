let read_file path = In_channel.with_open_text path In_channel.input_all

let parse_args () =
  let base = ref "" in
  let specs =
    [
      ( "--base",
        Arg.Set_string base,
        "FILE file stem for compliance test (no suffix)" );
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
    !base
  with Arg.Bad message -> failwith message

let fatal ~base s =
  Printf.eprintf "Error when testing %s:\n%s\n" base s;
  exit 1

module TestCase = struct
  type mode = Exec | NoExec

  let mode_exec, mode_noexec = ("exec", "noexec")
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

  let outcome_of_string ~base s =
    if String.equal s outcome_success then Success
    else if String.equal s outcome_failure then Failure
    else fatal ~base (Printf.sprintf "bad outcome: %s" s)

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
let yaml_value_to_testcase ~base (yaml : Yaml.value) =
  let bad_key s = fatal ~base (Printf.sprintf "bad YAML key: %s" s) in
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
      let mode = mode_of_string ~base (extract_string "mode" map) in
      let outcome = outcome_of_string ~base (extract_string "outcome" map) in
      let output = extract_string_opt "output" map in
      let error_code = extract_string_opt "error" map in
      let error_line = extract_int_opt "error_line" map in
      let info = extract_string_opt "info" map in
      { mode; outcome; output; error_code; error_line; info }
  | _ -> fatal ~base "expected top-level YAML object"

let read_testcase_from_file ~base =
  let yaml_value =
    match read_file (base ^ ".yaml") |> Yaml.of_string with
    | Ok y -> y
    | Error (`Msg s) -> fatal ~base s
  in
  yaml_value_to_testcase ~base yaml_value

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

let () =
  let base = parse_args () in
  let asl_code = read_file (base ^ ".asl") in
  let () = Printf.printf "%s\n" asl_code in
  let testcase = read_testcase_from_file ~base in
  let yaml_of_testcase = test_case_to_yaml testcase in
  match Yaml.yaml_to_string yaml_of_testcase with
  | Ok s -> Printf.printf "%s" s
  | Error (`Msg s) -> fatal ~base (Printf.sprintf "YAML to string: %s" s)
