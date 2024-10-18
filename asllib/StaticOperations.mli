open AST

val constraint_binop :
  binop -> int_constraint list -> int_constraint list -> int_constraint list
(** [constraint_binop PLUS cs1 cs2] is the set of constraints given by the
    element wise application of [PLUS]. *)

type strictness = [ `Silence | `Warn | `TypeCheck ]

module type CONFIG = sig
  val fail : unit -> 'a
  val check : strictness
  val output_format : Error.output_format
end

module Make : functor (C : CONFIG) -> sig
  val annotate_constraint_binop :
    loc:'a annotated ->
    StaticEnv.env ->
    binop ->
    int_constraint list ->
    int_constraint list ->
    int_constraint list
end
