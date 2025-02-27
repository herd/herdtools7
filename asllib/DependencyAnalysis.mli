open AST
open ASTUtils

module Name : sig
  type t = Subprogram of string | Other of string

  val hash : t -> int
  val to_string : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp_print : Format.formatter -> t -> unit
end

module NameSet : sig
  include Set.S with type elt = Name.t

  val pp_print : Format.formatter -> t -> unit
end

val used_identifiers_decl : decl -> NameSet.t
(** [used_identifiers_decl d] is the set of other declared names required to have
    in the environment to be able to type-check d. *)

val used_identifiers_decls : AST.t -> NameSet.t
(** [used_identifiers_decls ds] is the union of [used_identifiers_decl d] for
    all [d] in [ds]. *)
