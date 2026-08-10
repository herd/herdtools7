(** Implements some operations on literals. *)

open AST

val binop_values :
  _ t_annotated ->
  Error.error_handling_time ->
  binop ->
  literal ->
  literal ->
  literal
(** [binop_values loc t op v1 v2] applies [op] to [v1] and [v2], returning a
    single literal.

    @raise [ASLException]
      at location [loc] and handling time [t] if the operation cannot be
      applied. *)

val unop_values :
  _ t_annotated -> Error.error_handling_time -> unop -> literal -> literal
(** [unop_values loc t op v] applies [op] to [v], returning a literal.

    @raise [ASLException]
      at location [loc] and handling time [t] if the operation cannot be
      applied. *)

val literal_to_string : literal -> string
(** Represents a literal as it should be printed by the [print]/[println]
    statements in ASL. *)
