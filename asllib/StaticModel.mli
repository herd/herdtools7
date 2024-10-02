val equal_in_env : StaticEnv.env -> AST.expr -> AST.expr -> bool
val try_normalize : StaticEnv.env -> AST.expr -> AST.expr

(* used by tests/static.ml *)
val normalize : StaticEnv.env -> AST.expr -> AST.expr
