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
(** Raised when a metadata file cannot be parsed, validated, or written. *)

val string_of_mode : mode -> string
(** [string_of_mode mode] returns the YAML spelling of [mode]. *)

val string_of_outcome : outcome -> string
(** [string_of_outcome outcome] returns the YAML spelling of [outcome]. *)

val parse_file : string -> t
(** [parse_file path] reads and validates one metadata YAML file. Raises
    [Metadata_error] if [path] is not valid metadata. *)

val write_file : string -> t -> unit
(** [write_file path metadata] serializes [metadata] to [path]. Raises
    [Metadata_error] if [path] cannot be written. *)

val same_result : t -> t -> bool
(** [same_result expected actual] compares compliance-result fields, ignoring
    [info]. *)

val pp_result_diff : Format.formatter -> expected:t -> actual:t -> unit
(** [pp_result_diff fmt ~expected ~actual] prints compliance-result field
    differences, ignoring [info]. *)
