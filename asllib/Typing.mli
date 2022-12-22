type scope = AST.identifier

type tenv = {
  genv : AST.type_desc ASTUtils.IMap.t;
  funcs : (AST.type_desc list * AST.type_desc option) ASTUtils.IMap.t;
  lenvs : AST.type_desc ASTUtils.IMap.t ASTUtils.IMap.t;
}

type typing_error =
  | NotYetImplemented of string
  | UndefinedIdentifier of string
  | TypeError of string
  | Internal_InvalidScope of string

exception TypingError of typing_error

val infer : tenv -> scope -> AST.expr -> AST.type_desc
val build_tenv : AST.t -> tenv
