(** The Typing module is yet a single-entry-point module. It only exports the 
    function [annotate_ast] which fill type-annotations holes in the AST.
    It should provide enough information to disambiguate any type-dependent
    behaviour. *)

(** [typing_error] is the type of exceptions raised by the typer. *)
type typing_error =
  | NotYetImplemented of string
  | UndefinedIdentifier of string
      (** Raised when an unknown variable or type varaible is used. *)
  | TypeError of string
      (** Raised when a conflict appears during type-inference.*)

exception TypingError of typing_error

val annotate_ast : AST.t -> AST.t
(** Main entry point. *)
