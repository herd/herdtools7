(** The Typing module is yet a single-entry-point module. It only exports the 
    function [annotate_ast] which fill type-annotations holes in the AST.
    It should provide enough information to disambiguate any type-dependent
    behaviour. *)

val annotate_ast : AST.t -> AST.t
(** Annotate is a typing inference function for an ASL AST. It does not type check.

    @raise Error.ASLException if the Inferrence is blocked, for example when
    the AST does not typechecks.
*)

type strictness = [ `Silence | `Warn | `TypeCheck ]
(** Possible strictness of type-checking. *)

val type_check_ast : strictness -> AST.t -> AST.t
(** Typechecks the AST, and returns an AST with type inference holes filled.

    @raise Error.ASLException if the AST does not type-checks.
*)
