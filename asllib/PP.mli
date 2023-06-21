open AST

type 'a printer = Format.formatter -> 'a -> unit

(* Available from 4.12.0 *)
val pp_print_seq : ?pp_sep:unit printer -> 'a printer -> 'a Seq.t printer
val pp_pos : 'a annotated printer

val pp_value : value printer
(** Print a value from its components.*)

val value_to_string : value -> string
val binop_to_string : binop -> string
val unop_to_string : unop -> string
val pp_stmt : stmt printer
val pp_expr : expr printer
val pp_lexpr : lexpr printer

val pp_t : t printer
(** Print an AST from printer for a value *)

val t_to_string : t -> string
(** [t_to_string v_to_string ast] is a string representing [ast] with values printed with [v_to_string].*)

val pp_ty : ty printer
val pp_typed_identifier : typed_identifier printer
val ty_to_string : ty -> string
val pp_version : [ `ASLv0 | `ASLv1 | `Any ] printer
val pp_scope : scope printer
