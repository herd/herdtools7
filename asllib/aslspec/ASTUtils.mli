open AST

val elem_name : elem -> string
(** [elem_name elem] returns the name of the top-level element [elem]. *)

val vars_of_type_term : Term.type_term -> string list
(** [vars_of_type_term term] returns the list of term-naming variables that
    occur at any depth inside [term]. The list is sorted and contains no
    duplicates. *)

val vars_of_opt_named_type_terms : Term.opt_named_type_term list -> string list
(** [vars_of_opt_named_type_terms terms] returns the list of term-naming
    variables that occur at any depth inside [terms]. *)

val variant_to_label_opt : TypeVariant.t -> string option
(** [variant_to_label_opt variant] returns the label of [variant] if it has one,
    or [None] otherwise. *)

val is_operator : elem -> bool
(** [is_operator elem] returns [true] if [elem] corresponds to an operator, and
    [false] otherwise. *)
