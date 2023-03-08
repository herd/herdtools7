open AST

type buffer = Buffer.t
type 'a printer = buffer -> 'a -> unit

val pp_value : value printer
val pp_t : t printer
val t_to_string : t -> string
val pp_ty : ty printer
val pp_typed_identifier : typed_identifier printer
