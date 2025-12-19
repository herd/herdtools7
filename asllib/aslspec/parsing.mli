(** A module for parsing aslspec specifications. *)

exception ParseError of string

val parse_spec_from_file : string -> AST.t
(** [parse_spec_from_file filename] parses a specification from the file
    [filename]. Raises [ParseError] on failure. *)

val parse_spec_from_string : spec:string -> filename:string -> AST.t
(** [parse_spec_from_string ~spec ~filename] parses a specification from the
    string [spec] and uses [filename] as the source file name for error
    reporting. Raises [ParseError] on failure. *)
