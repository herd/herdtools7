open AST

type int3_binop =
  [ `PLUS | `MINUS | `DIV | `DIVRM | `SHR | `SHL | `POW | `MOD | `MUL ]

val constraint_binop :
  int3_binop ->
  int_constraint list ->
  int_constraint list ->
  int_constraint list
(** [constraint_binop op cs1 cs2] is the set of constraints given by the
    element wise application of [op].
*)

module type CONFIG = sig
  val fail : string -> 'a
  val warn_from : loc:'a annotated -> Error.warning_desc -> unit
end

module Make : functor (C : CONFIG) -> sig
  val annotate_constraint_binop :
    loc:'a annotated ->
    StaticEnv.env ->
    int3_binop ->
    int_constraint list ->
    int_constraint list ->
    int_constraint list * precision_loss_flag
end

(* Used by asllib/tests/ConstraintBinops.ml *)

val filter_reduce_constraint_div : int_constraint -> int_constraint option
