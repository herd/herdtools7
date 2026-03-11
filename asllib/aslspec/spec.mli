(** A module for querying the AST (of the semantics-specification for ASL) and
    for checking its correctness. *)

open AST

(* The _ variable in aslspec. *)
val ignore_var : string

(** Wraps AST nodes that define identifiers that may appear in type terms and
    expression terms: types, constants, relations, type variants, and record
    fields. *)
type definition_node =
  | Node_Type of Type.t
  | Node_Relation of Relation.t
  | Node_TypeVariant of TypeVariant.t
  | Node_Constant of Constant.t
  | Node_RecordField of Term.record_field

val math_macro_opt_for_node : definition_node -> string option
(** [math_macro_opt_for_node node] returns the math macro for [node], if one is
    defined, and [None] otherwise. *)

(** Utility functions for handling layouts. *)
module Layout : sig
  val math_layout_for_node : definition_node -> layout
  (** [math_layout_for_node node] returns the math layout for [node]. If none is
      defined, a default layout is inferred for relation nodes, type variants,
      and record fields; otherwise [Unspecified] is returned. *)

  val for_type_term : Term.t -> layout
  (** [for_type_term term] returns a full default layout for [term]. That is, a
      layout extending to the depth of [term]. *)
end

type t
(** A processed and validated specification. *)

val from_ast : AST.t -> t
(** [from_ast ast] converts an AST into a validated specification. Performs all
    correctness checks, and raises [SpecError] if any fail. *)

val defined_ids : t -> string list
(** [defined_ids spec] returns the list of all identifiers defined in [spec]. *)

val elements : t -> elem list
(** [elements spec] returns the list of all elements in [spec]. *)

val defining_node_for_id : t -> string -> definition_node
(** [defining_node_for_id spec id] returns the defining node for [id] in [spec].
    Assumes that [id] is defined in [spec]. *)

val defining_node_opt_for_id : t -> string -> definition_node option
(** [defining_node_opt_for_id spec id] returns [Some node] if [id] is defined in
    [spec] with defining node [node], and [None] otherwise. *)

val relation_for_id : t -> string -> Relation.t
(** [relation_for_id spec id] returns the relation definition for [id] in
    [spec], assuming it is defined as a relation. *)

val record_variant_for_expr : t -> Expr.t -> TypeVariant.t
(** [record_variant_for_expr spec expr] returns the record type variant
    corresponding to [expr], assuming [expr] is a record expression. *)

val is_defined_id : t -> string -> bool
(** [is_defined_id spec id] returns [true] if [id] is defined in [spec] and
    [false] otherwise. *)

val is_variadic_operator : t -> string -> bool
(** [is_variadic_operator spec id] returns [true] if [id] corresponds to a
    variadic operator in [spec], and [false] otherwise. A variadic operator is
    an operator that can take a variable number of arguments and handle them as
    though they were given in a list. *)

val is_quantifying_operator : t -> string -> bool
(** [is_quantifying_operator spec id] returns [true] if [id] corresponds to a
    quantifying operator in [spec], and [false] otherwise. *)

val is_cond_operator_name : t -> string -> bool
(** [is_cond_operator_name spec id] returns [true] if [id] corresponds to the
    match_cases operator in [spec], and [false] otherwise. *)

(** A module for expanding rules with cases into multiple rules without cases.
*)
module ExpandRules : sig
  type expanded_rule = {
    name_opt : string option;
    judgments : Rule.judgment list;
  }
  (** An expanded rule is a rule with no cases. *)

  val expand : Rule.t -> expanded_rule list
  (** [expand rule] expands [rule] into multiple rules without cases. *)

  val split_absolute_rule_name : string -> string list
  (** [split_absolute_rule_name abs_name] splits an absolute rule name
      [abs_name] into its components by splitting at the '.' character. *)
end

val filter_rule_for_path : Relation.t -> string -> Rule.t
(** [filter_rule_for_path relation path_str] filters the rule given for
    [relation] to only include the cases along the path specified by [path_str].
    The path is a dot-separated, where each name corresponds to the name of the
    case to take at each level of nesting. For example, "name1.name2" means:
    take case [name1] at the top level, and then case [name2] within it.

    @raise [SpecError]
      if [path_str] is not a valid path through the cases of the rule for
      [relation]. *)
