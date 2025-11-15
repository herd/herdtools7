(** A module for querying the AST (of the semantics-specification for ASL) and
    for checking its correctness. *)

open AST

val vars_of_type_term : type_term -> string list
(** [vars_of_type_term term] returns the list of term-naming variables that
    occur at any depth inside [term]. The list is sorted and contains no
    duplicates. *)

val vars_of_opt_named_type_terms : opt_named_type_term list -> string list
(** [vars_of_opt_named_type_terms terms] returns the list of term-naming
    variables that occur at any depth inside [terms]. *)

val variant_to_label_opt : TypeVariant.t -> string option
(** [variant_to_label_opt variant] returns the label of [variant] if it has one,
    or [None] otherwise. *)

(** Wraps AST nodes that define identifiers that may appear in type terms and
    expression terms: types, constants, relations, labels, labelled tuples, and
    labelled records. *)
type definition_node =
  | Node_Type of Type.t
  | Node_Relation of Relation.t
  | Node_TypeVariant of TypeVariant.t
  | Node_Constant of Constant.t
  | Node_RecordField of record_field

val math_macro_opt_for_node : definition_node -> string option
(** [math_macro_opt_for_node node] returns the math macro for [node], if one is
    defined, and [None] otherwise. *)

(** Utility functions for handling layouts. *)
module Layout : sig
  val math_layout_for_node : definition_node -> layout
  (** [math_layout_for_node node] returns the math layout for [node], or a
      default layout based on its type term if no math layout is defined. *)

  val for_type_term : type_term -> layout
  (** [for_type_term style term] returns a full default layout for [term]. That
      is, a layout extending to the depth of [term]. *)
end

val elem_name : elem -> string
(** [elem_name elem] returns the name of the top-level element [elem]. *)

type t
(** A processed and validated specification. *)

val ast : t -> AST.t
(** [ast spec] returns the original AST from which [spec] was created. *)

val from_ast : AST.t -> t
(** [from_ast ast] converts an AST into a validated specification. Performs all
    correctness checks, and raises [SpecError] if any fail. *)

val defined_ids : t -> string list
(** [defined_ids spec] returns the list of all identifiers defined in [spec] in
    order of appearance. *)

val defining_node_for_id : t -> string -> definition_node
(** [defining_node_for_id spec id] returns the defining node for [id] in [spec].
    Raises [SpecError] if [id] is not defined. *)

val defining_node_opt_for_id : t -> string -> definition_node option
(** [defining_node_opt_for_id spec id] returns [Some node] if [id] is defined in
    [spec] with defining node [node], and [None] otherwise. *)

val relation_for_id : t -> string -> Relation.t
(** [relation_for_id spec id] returns the relation definition for [id] in
    [spec]. Raises [SpecError] if [id] is not defined as a relation. *)

val is_defined_id : t -> string -> bool
(** [is_defined_id spec id] returns [true] if [id] is defined in [spec] and
    [false] otherwise. *)

module ExpandRules : sig
  type expanded_rule = {
    name_opt : string option;
    category_opt : Relation.relation_category option;
    judgments : Rule.judgment list;
  }
  (** An expanded rule is a rule with no cases. *)

  val expand : Relation.relation_category option -> Rule.t -> expanded_rule list
  (** [expand category_opt elements] expands the rule whose list of elements is
      [elements] into multiple rules without cases, all having the optional
      category [category_opt]. *)
end
