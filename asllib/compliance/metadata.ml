(** Adapter between ASL compliance metadata YAML files and OCaml values. *)

type mode = Exec | No_exec  (** Test execution mode. *)
type outcome = Success | Failure  (** Expected or actual test outcome. *)

type t = {
  mode : mode;
  outcome : outcome;
  output : string;
  error : string option;
  error_line : int option;
  info : string option;
}
(** Flat ASL compliance metadata. *)

exception Metadata_error of string

(** [raise_errorf fmt] formats and raises a [Metadata_error]. *)
let raise_errorf fmt =
  Printf.ksprintf (fun message -> raise (Metadata_error message)) fmt

let mode_field_name = "mode"
let outcome_field_name = "outcome"
let output_field_name = "output"
let error_field_name = "error"
let error_line_field_name = "error_line"
let info_field_name = "info"
let exec_value = "exec"
let no_exec_value = "no-exec"
let success_value = "SUCCESS"
let failure_value = "FAILURE"

(** [string_of_mode mode] returns the YAML spelling of [mode]. *)
let string_of_mode = function Exec -> exec_value | No_exec -> no_exec_value

(** [string_of_outcome outcome] returns the YAML spelling of [outcome]. *)
let string_of_outcome = function
  | Success -> success_value
  | Failure -> failure_value

module Parser : sig
  val parse_file : string -> t
  (** [parse_file path] reads and validates a metadata YAML file. Raises
      [Metadata_error] if [path] is not valid metadata. *)
end = struct
  (** [parse_yaml_string diagnostic_path ~diagnostic_field_name value] parses
      [value] as a YAML string. [diagnostic_path] and [diagnostic_field_name]
      identify the file and field in error messages. *)
  let parse_yaml_string diagnostic_path ~diagnostic_field_name = function
    | `String value -> value
    | _ ->
        raise_errorf "%s: field %s: expected string" diagnostic_path
          diagnostic_field_name

  (** [reject_duplicate_fields diagnostic_path fields] rejects repeated
      top-level fields. *)
  let reject_duplicate_fields diagnostic_path fields =
    let rec first_duplicate seen = function
      | [] -> None
      | (field_name, _) :: fields ->
          if List.mem field_name seen then Some field_name
          else first_duplicate (field_name :: seen) fields
    in
    match first_duplicate [] fields with
    | None -> ()
    | Some field_name ->
        raise_errorf "%s: duplicate field %s" diagnostic_path field_name

  (** [reject_unknown_fields diagnostic_path fields] rejects fields outside the
      schema. *)
  let reject_unknown_fields diagnostic_path fields =
    let allowed =
      [
        mode_field_name;
        outcome_field_name;
        output_field_name;
        error_field_name;
        error_line_field_name;
        info_field_name;
      ]
    in
    match
      List.find_opt
        (fun (field_name, _) -> not (List.mem field_name allowed))
        fields
    with
    | None -> ()
    | Some (field_name, _) ->
        raise_errorf "%s: unknown field %s" diagnostic_path field_name

  (** [find_field name fields] returns the YAML value for [name], if present. *)
  let find_field name fields = List.assoc_opt name fields

  (** [parse_optional_string diagnostic_path name fields] parses an optional
      string field. *)
  let parse_optional_string diagnostic_path name fields =
    match find_field name fields with
    | None -> None
    | Some value ->
        value |> parse_yaml_string diagnostic_path ~diagnostic_field_name:name
        |> fun parsed -> Some parsed

  (** [parse_output diagnostic_path fields] parses the expected or actual
      stdout, defaulting to the empty string when the YAML file omits the field.

      For example, metadata with no [output] field parses as [""]. Metadata with
      [output: "x"] parses as ["x"]. *)
  let parse_output diagnostic_path fields =
    match find_field output_field_name fields with
    | None -> ""
    | Some value ->
        value
        |> parse_yaml_string diagnostic_path
             ~diagnostic_field_name:output_field_name

  (** [parse_mode diagnostic_path fields] parses [mode], defaulting to [Exec].
  *)
  let parse_mode diagnostic_path fields =
    match find_field mode_field_name fields with
    | None -> Exec
    | Some value -> (
        value
        |> parse_yaml_string diagnostic_path
             ~diagnostic_field_name:mode_field_name
        |> function
        | value when String.equal value exec_value -> Exec
        | value when String.equal value no_exec_value -> No_exec
        | value -> raise_errorf "%s: invalid mode %S" diagnostic_path value)

  (** [parse_outcome diagnostic_path fields] parses the required [outcome]
      field. *)
  let parse_outcome diagnostic_path fields =
    match find_field outcome_field_name fields with
    | None ->
        raise_errorf "%s: missing required field %s" diagnostic_path
          outcome_field_name
    | Some value -> (
        value
        |> parse_yaml_string diagnostic_path
             ~diagnostic_field_name:outcome_field_name
        |> function
        | value when String.equal value success_value -> Success
        | value when String.equal value failure_value -> Failure
        | value -> raise_errorf "%s: invalid outcome %S" diagnostic_path value)

  (** [parse_error_line diagnostic_path fields] parses the optional positive
      [error_line]. [Yaml.of_string] exposes YAML numbers through the
      JSON-compatible [`Float] constructor, so [error_line: 18] is parsed from
      [`Float 18.]. *)
  let parse_error_line diagnostic_path fields =
    match find_field error_line_field_name fields with
    | None -> None
    | Some value -> (
        match value with
        | `Float value when Float.is_integer value ->
            let line = int_of_float value in
            if line >= 1 then Some line
            else
              raise_errorf "%s: field %s: expected positive integer"
                diagnostic_path error_line_field_name
        | _ ->
            raise_errorf "%s: field %s: expected integer" diagnostic_path
              error_line_field_name)

  (** [check_metadata_invariants diagnostic_path metadata] enforces schema
      invariants. *)
  let check_metadata_invariants diagnostic_path metadata =
    match metadata with
    | { outcome = Success; error = Some _; _ } ->
        raise_errorf "%s: successful tests must not specify error"
          diagnostic_path
    | { outcome = Success; error_line = Some _; _ } ->
        raise_errorf "%s: successful tests must not specify error_line"
          diagnostic_path
    | { outcome = Failure; error = None; _ } ->
        raise_errorf "%s: failing tests must specify error" diagnostic_path
    | _ -> ()

  (** [parse_yaml diagnostic_path yaml] converts a parsed top-level YAML object
      to metadata. *)
  let parse_yaml diagnostic_path = function
    | `O fields ->
        reject_duplicate_fields diagnostic_path fields;
        reject_unknown_fields diagnostic_path fields;
        let mode = parse_mode diagnostic_path fields in
        let outcome = parse_outcome diagnostic_path fields in
        let output = parse_output diagnostic_path fields in
        let error =
          parse_optional_string diagnostic_path error_field_name fields
        in
        let error_line = parse_error_line diagnostic_path fields in
        let info =
          parse_optional_string diagnostic_path info_field_name fields
        in
        let metadata = { mode; outcome; output; error; error_line; info } in
        check_metadata_invariants diagnostic_path metadata;
        metadata
    | _ -> raise_errorf "%s: expected top-level object" diagnostic_path

  (** [parse_file path] reads and validates one metadata YAML file. *)
  let parse_file path =
    let text =
      try In_channel.with_open_text path In_channel.input_all
      with Sys_error message -> raise_errorf "%s: %s" path message
    in
    let yaml =
      match Yaml.of_string text with
      | Ok yaml -> yaml
      | Error (`Msg message) -> raise_errorf "%s: %s" path message
    in
    parse_yaml path yaml
end

module Writer : sig
  val write_file : string -> t -> unit
  (** [write_file path metadata] serializes [metadata] to [path]. Raises
      [Metadata_error] if [path] cannot be written. *)
end = struct
  open Format

  (** [pp_scalar fmt (key, value)] writes a scalar YAML field. *)
  let pp_scalar fmt (key, value) = fprintf fmt "%s: %s\n" key value

  (** [literal_block_lines value] returns the lines to write in a YAML literal
      block. A final newline terminates the last content line and is not itself
      emitted as an extra blank line.

      For example, ["a\nb\n"] and ["a\nb"] both produce [["a"; "b"]]. *)
  let literal_block_lines value =
    let value =
      if String.ends_with ~suffix:"\n" value then
        String.sub value 0 (String.length value - 1)
      else value
    in
    String.split_on_char '\n' value

  (** [pp_literal_block_lines fmt lines] writes the body of a YAML literal
      block. *)
  let pp_literal_block_lines fmt lines =
    let pp_indented_literal_block_line fmt line = fprintf fmt "  %s" line in
    pp_print_list
      ~pp_sep:(fun fmt () -> pp_print_newline fmt ())
      pp_indented_literal_block_line fmt lines;
    (* Terminate the block body before the next YAML field is printed. *)
    pp_print_newline fmt ()

  (** [pp_block fmt (key, value)] writes a literal block YAML field.

      For example, [("output", "a\nb\n")] is written as [output: |] followed by
      indented [a] and [b] lines. *)
  let pp_block fmt (key, value) =
    let literal_block_indicator value =
      (* Most ASL tests are expected to print complete lines, so keep the ordinary
       literal block style as the default. Use "|-" only when the exact output
       does not end with a newline, so YAML does not add one when parsed again. *)
      if String.ends_with ~suffix:"\n" value then "|" else "|-"
    in
    fprintf fmt "%s: %s\n%a" key
      (literal_block_indicator value)
      pp_literal_block_lines
      (literal_block_lines value)

  (** [optional_scalar_field key render value] prepares an optional scalar
      field. *)
  let optional_scalar_field key render value =
    Option.map (fun value -> (key, render value)) value

  (** [optional_block key value] prepares a non-empty optional block field. *)
  let optional_block key value =
    if String.equal value "" then None else Some (key, value)

  (** [write_metadata fmt metadata] writes [metadata] in the schema field order.

      For example, a successful executable test with empty output starts with
      [mode: exec] followed by [outcome: SUCCESS], and omits [output]. *)
  let write_metadata fmt metadata =
    let mode = (mode_field_name, string_of_mode metadata.mode) in
    let outcome = (outcome_field_name, string_of_outcome metadata.outcome) in
    let output = optional_block output_field_name metadata.output in
    let error = optional_scalar_field error_field_name Fun.id metadata.error in
    let error_line =
      optional_scalar_field error_line_field_name string_of_int
        metadata.error_line
    in
    let info = optional_scalar_field info_field_name Fun.id metadata.info in
    let pp_optional_block = pp_print_option pp_block in
    let pp_optional_scalar = pp_print_option pp_scalar in
    fprintf fmt "%a%a%a%a%a%a" pp_scalar mode pp_scalar outcome
      pp_optional_block output pp_optional_scalar error pp_optional_scalar
      error_line pp_optional_block info

  let write_file path metadata =
    try
      let channel = open_out path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr channel)
        (fun () ->
          let fmt = formatter_of_out_channel channel in
          write_metadata fmt metadata;
          pp_print_flush fmt ())
    with Sys_error message -> raise_errorf "%s: %s" path message
end

module Compare : sig
  val same_result : t -> t -> bool
  (** [same_result expected actual] compares compliance-result fields, ignoring
      [info]. *)

  val pp_result_diff : Format.formatter -> expected:t -> actual:t -> unit
  (** [pp_result_diff fmt ~expected ~actual] prints compliance-result field
      differences, ignoring [info]. *)
end = struct
  open Format

  let equal_mode ~expected ~actual =
    match (expected, actual) with
    | Exec, Exec | No_exec, No_exec -> true
    | _ -> false

  let equal_outcome ~expected ~actual =
    match (expected, actual) with
    | Success, Success | Failure, Failure -> true
    | _ -> false

  let same_result expected actual =
    equal_mode ~expected:expected.mode ~actual:actual.mode
    && equal_outcome ~expected:expected.outcome ~actual:actual.outcome
    && String.equal expected.output actual.output
    && Option.equal String.equal expected.error actual.error
    && Option.equal Int.equal expected.error_line actual.error_line

  let pp_result_diff fmt ~expected ~actual =
    let pp_diff_if_changed ~label ~equal ~render ~expected ~actual =
      if not (equal ~expected ~actual) then
        fprintf fmt "  - %s: expected %s, got %s\n" label (render expected)
          (render actual)
    in
    let render_string value =
      if String.equal value "" then "<empty>" else Printf.sprintf "%S" value
    in
    let render_string_opt = function
      | None -> "<none>"
      | Some value -> render_string value
    in
    let render_int_opt = function
      | None -> "<none>"
      | Some value -> string_of_int value
    in
    pp_diff_if_changed ~label:mode_field_name ~equal:equal_mode
      ~render:string_of_mode ~expected:expected.mode ~actual:actual.mode;
    pp_diff_if_changed ~label:outcome_field_name ~equal:equal_outcome
      ~render:string_of_outcome ~expected:expected.outcome
      ~actual:actual.outcome;
    pp_diff_if_changed ~label:output_field_name
      ~equal:(fun ~expected ~actual -> String.equal expected actual)
      ~render:render_string ~expected:expected.output ~actual:actual.output;
    pp_diff_if_changed ~label:error_field_name
      ~equal:(fun ~expected ~actual ->
        Option.equal String.equal expected actual)
      ~render:render_string_opt ~expected:expected.error ~actual:actual.error;
    pp_diff_if_changed ~label:error_line_field_name
      ~equal:(fun ~expected ~actual -> Option.equal Int.equal expected actual)
      ~render:render_int_opt ~expected:expected.error_line
      ~actual:actual.error_line
end

(** Keep implementation internals in submodules, but expose the documented
    [Metadata] API at module level. *)
let parse_file = Parser.parse_file

let write_file = Writer.write_file
let same_result = Compare.same_result
let pp_result_diff = Compare.pp_result_diff
