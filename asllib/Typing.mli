(** The Typing module is yet a single-entry-point module. It only exports the 
    function [annotate_ast] which fill type-annotations holes in the AST.
    It should provide enough information to disambiguate any type-dependent
    behaviour. *)

val annotate_ast : AST.t -> AST.t
(** Main entry point. *)
