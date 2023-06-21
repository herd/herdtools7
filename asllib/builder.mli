open Lexing

type token = Parser.token
type ast_type = [ `Opn | `Ast ]
type version = [ `ASLv0 | `ASLv1 ]
type version_selector = [ `ASLv0 | `ASLv1 | `Any ]

val from_file_result :
  ?ast_type:ast_type -> version -> string -> 'p AST.t Error.result

val from_file : ?ast_type:ast_type -> version -> string -> 'p AST.t

val from_lexer_lexbuf :
  ?ast_type:ast_type -> version -> 'a -> lexbuf -> 'p AST.t Error.result

val from_file_multi_version :
  ?ast_type:ast_type -> version_selector -> string -> 'p AST.t Error.result

val stdlib : unit AST.t Lazy.t
