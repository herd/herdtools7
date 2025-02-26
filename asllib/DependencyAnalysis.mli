open AST
open ASTUtils

val used_identifiers_decl : decl -> ISet.t
(** [used_identifiers_decl d] is the set of other declared names required to have
    in the environment to be able to type-check d. *)

val used_identifiers : decl list -> ISet.t
