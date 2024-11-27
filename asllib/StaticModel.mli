val equal_in_env : StaticEnv.env -> AST.expr -> AST.expr -> bool
val try_normalize : StaticEnv.env -> AST.expr -> AST.expr
val normalize_to_bool_opt : StaticEnv.env -> AST.expr -> bool option
val reduce_to_z_opt : StaticEnv.env -> AST.expr -> Z.t option
val normalize_opt : StaticEnv.env -> AST.expr -> AST.expr option

(* used by tests/static.ml *)
val normalize : StaticEnv.env -> AST.expr -> AST.expr
