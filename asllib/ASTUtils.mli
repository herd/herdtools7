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
val use_e : ISet.t -> expr -> ISet.t
val used_identifiers : decl list -> ISet.t
val used_identifiers_stmt : stmt -> ISet.t
val canonical_fields : (String.t * 'a) list -> (String.t * 'a) list
val literal : value -> expr
val var_ : identifier -> expr
val binop : binop -> expr -> expr -> expr
val list_equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
val list_compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int

val list_fold_left_map :
  ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list

val list_cross : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val expr_equal : (expr -> expr -> bool) -> expr -> expr -> bool
val value_equal : value -> value -> bool
val slice_equal : (expr -> expr -> bool) -> slice -> slice -> bool
val slices_equal : (expr -> expr -> bool) -> slice list -> slice list -> bool
val type_equal : (expr -> expr -> bool) -> ty -> ty -> bool

val bitwidth_equal :
  (expr -> expr -> bool) -> bits_constraint -> bits_constraint -> bool

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

val constraint_binop :
  binop -> int_constraints -> int_constraints -> int_constraints

val subst_expr : (identifier * expr) list -> expr -> expr
(** [subst_expr substs e] replaces the variables used inside [e] by their
    associated expression in [substs], if any.

    Warning: constants and statically-evaluated parts are not changed, for
    example:
      [E_Slice (E_Var "y", [Slice_Single (E_Var "y")])]
    will become after [subst_expr [("y", E_Var "x")]]:
      [E_Slice (E_Var "x", [Slice_Single (E_Var "y")])]
*)

val dag_fold :
  (decl -> identifier) ->
  (decl -> ISet.t) ->
  (decl -> 'a -> 'a) ->
  t ->
  'a ->
  'a
(** [dag_fold def use folder ast a] is [a |> f d_1 |> ... f d_n] where [d_i]
    spawns all declarations in AST, but in an order such that [use]/[def]
    relations are respected. *)

val scope_equal : scope -> scope -> bool
val scope_compare : scope -> scope -> int
