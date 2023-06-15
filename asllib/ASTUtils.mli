open AST

module ISet : Set.S with type elt = identifier

module IMap : sig
  include Map.S with type key = identifier

  val of_list : (key * 'a) list -> 'a t

end

val dummy_pos : Lexing.position
val annotated : 'a -> position -> position -> 'a annotated
val add_dummy_pos : 'a -> 'a annotated
val dummy_annotated : unit annotated
val to_pos : 'a annotated -> unit annotated
val add_pos_from_st : 'a annotated -> 'a -> 'a annotated
val with_pos_from_st : 'a annotated -> 'a annotated -> 'a annotated
val map_desc_st : ('a annotated -> 'a) -> 'a annotated -> 'a annotated
val add_pos_from : 'a annotated -> 'b -> 'b annotated
val with_pos_from : 'a annotated -> 'b annotated -> 'b annotated
val map_desc : ('a annotated -> 'b) -> 'a annotated -> 'b annotated

val map2_desc :
  ('a annotated -> 'b annotated -> 'c) ->
  'a annotated ->
  'b annotated ->
  'c annotated

val s_pass : stmt
val s_then : stmt -> stmt -> stmt
val stmt_from_list : stmt list -> stmt
val mask_from_set_bits_positions : int -> int list -> string
val inv_mask : string -> string
val slices_to_positions : ('a -> int) -> ('a * 'a) list -> int list
val used_identifiers : decl list -> ISet.t
val used_identifiers_stmt : stmt -> ISet.t
val canonical_fields : (String.t * 'a) list -> (String.t * 'a) list
val literal : value -> expr
val var_ : identifier -> expr
val binop : binop -> expr -> expr -> expr
val expr_of_lexpr : lexpr -> expr
val fresh_var : string -> string
val big_union : expr list -> expr
val case_to_conds : stmt -> stmt
val slice_as_single : slice -> expr
val getter_prefix : string
val setter_prefix : string
val setter_name : string -> string
val getter_name : string -> string
val num_args : int -> string -> string
val default_t_bits : type_desc

val patch : src:AST.t -> patches:AST.t -> AST.t
(** [patch ~src ~patches] replaces in [src] the global identifiers defined by [patches]. *)
