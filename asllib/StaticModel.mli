module Symbolic : functor
  (C : sig
     val handle_atc : bool
     (** When [true], discard ATCs when building symbolic representation of
         expressions. For example, [1 as integer {1,2,3}] would be represented
         as [1]. *)
   end)
  -> sig
  val equal_in_env : StaticEnv.env -> AST.expr -> AST.expr -> bool
  val try_normalize : StaticEnv.env -> AST.expr -> AST.expr
  val reduce_to_z_opt : StaticEnv.env -> AST.expr -> Z.t option
  val normalize_opt : StaticEnv.env -> AST.expr -> AST.expr option

  (* used by tests/static.ml *)
  val normalize : StaticEnv.env -> AST.expr -> AST.expr
end
