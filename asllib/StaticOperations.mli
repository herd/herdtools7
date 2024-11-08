open AST

val constraint_binop :
  binop -> int_constraint list -> int_constraint list -> int_constraint list
(** [constraint_binop op cs1 cs2] is the set of constraints given by the
    element wise application of [op].

    Supported [op]s: [DIV], [DIVRM], [MUL], [PLUS], [MINUS], [SHR], [SHL],
    [MOD], [POW].
*)

module type CONFIG = sig
  val fail : unit -> 'a
  val warn_from : loc:'a annotated -> Error.warning_desc -> unit
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

(* Used by asllib/tests/ConstraintBinops.ml *)

val filter_reduce_constraint_div : int_constraint -> int_constraint option
