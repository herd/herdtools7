open AST

type buffer = Buffer.t

val pp_value :
  ('i -> string) ->
  ('b -> string) ->
  ('r -> string) ->
  ('bv -> string) ->
  buffer ->
  ('i, 'b, 'r, 'bv) value ->
  unit
(** Print a value from its components.*)

val pp_parsed_value : buffer -> parsed_value -> unit
(** Print a parsed value *)

val pp_t : (buffer -> 'v -> unit) -> buffer -> 'v t -> unit
(** Serialize an AST from printer for a value *)

val pp_parsed_t : buffer -> parsed_t -> unit
(** Serialize a parsed AST.*)

val t_to_string : ('v -> string) -> 'v t -> string
(** [t_to_string v_to_string ast] is a string representing [ast] with values printed with [v_to_string].*)

val parsed_t_to_string : parsed_t -> string
(* Serialize a parsed AST.*)

val pp_type_desc : buffer -> type_desc -> unit
val pp_typed_identifier : buffer -> typed_identifier -> unit
