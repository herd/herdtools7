open AST

val pp_value :
  (Format.formatter -> 'i -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'r -> unit) ->
  (Format.formatter -> 'bv -> unit) ->
  Format.formatter ->
  ('i, 'b, 'r, 'bv) value ->
  unit
(** Print a value from its components.*)

val pp_parsed_value : Format.formatter -> parsed_value -> unit
(** Print a parsed value *)

val binop_to_string : binop -> string
val unop_to_string : unop -> string

val pp_t : (Format.formatter -> 'v -> unit) -> Format.formatter -> 'v t -> unit
(** Print an AST from printer for a value *)

val pp_parsed_t : Format.formatter -> parsed_t -> unit
(** Format a parsed AST.*)

val t_to_string : ('v -> string) -> 'v t -> string
(** [t_to_string v_to_string ast] is a string representing [ast] with values printed with [v_to_string].*)

val parsed_t_to_string : parsed_t -> string
(* Print a parsed AST.*)

val pp_type_desc : Format.formatter -> type_desc -> unit
val pp_typed_identifier : Format.formatter -> typed_identifier -> unit
