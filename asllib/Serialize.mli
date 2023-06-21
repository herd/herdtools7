open AST

type buffer = Buffer.t
type 'a printer = buffer -> 'a -> unit

val pp_value : value printer
val pp_t : 'p t printer
val t_to_string : 'p t -> string
val pp_ty : ty printer
val pp_typed_identifier : typed_identifier printer
