open AST
open ASTUtils

let spec_error msg = raise (SpecError msg)

(** [spec_error_loc ~loc msg] raises a [SpecError] with a message [msg] and
    source location [loc]. *)
let spec_error_loc ~loc:{ start_pos } msg =
  let pos_in_line loc = 1 + Lexing.(loc.pos_cnum - loc.pos_bol) in
  let msg_with_loc =
    Format.asprintf "%s:%d:%d: %s" start_pos.pos_fname start_pos.pos_lnum
      (pos_in_line start_pos) msg
  in
  spec_error msg_with_loc

(** [spec_error_loc_of_expr expr msg] raises a [SpecError] with a message [msg]
    and the source location of [expr]. *)
let spec_error_loc_of_expr expr msg =
  let loc = loc_of_expr expr in
  spec_error_loc ~loc msg

(** [spec_error_loc_of_term term msg] raises a [SpecError] with a message [msg]
    and the source location of [term]. *)
let spec_error_loc_of_term term msg =
  let loc = loc_of_term term in
  spec_error_loc ~loc msg

let bad_layout term layout ~consistent_layout =
  spec_error_loc_of_term term
  @@ Format.asprintf
       "layout %a is inconsistent with %a. Here's a consistent layout: %a"
       PP.pp_layout layout PP.pp_type_term term PP.pp_layout consistent_layout

let undefined_reference id context =
  spec_error @@ Format.asprintf "Undefined reference to %s in %s" id context

let undefined_element id =
  spec_error @@ Format.asprintf "Encountered undefined element: %s" id

let duplicate_definition id =
  spec_error @@ Format.asprintf "Duplicate definition of %s" id

let unmatched_variables_in_template template unmatched_vars =
  spec_error
  @@ Format.asprintf
       "The prose template \"%s\" contains the following unmatched variables: \
        %s"
       template
       (String.concat ", " unmatched_vars)

let not_type_name_error relation_name term =
  spec_error_loc_of_term term
  @@ Format.asprintf
       "In relation %s, output type term %a is not a type name or a set of \
        constants. All relation output types, except for the first one, must \
        be type names or a set of constants."
       relation_name PP.pp_type_term term

let missing_short_circuit_attribute relation_name term type_name =
  spec_error_loc_of_term term
  @@ Format.asprintf
       "In relation %s, output type term %a references type %s which does not \
        have a short-circuit macro defined. All relation output types, except \
        for the first one, must reference types with short-circuit macros."
       relation_name PP.pp_type_term term type_name

let invalid_application_of_symbol_in_expr name expr =
  spec_error_loc_of_expr expr
    (Format.asprintf "Invalid application of symbol %s to expression %a" name
       PP.pp_expr expr)

let type_subsumption_failure sub super =
  spec_error_loc_of_term sub
  @@ Format.asprintf "Unable to determine that `%a` is subsumed by `%a`"
       PP.pp_type_term sub PP.pp_type_term super

let argument_subsumption_failure sub super ~context_expr =
  spec_error_loc_of_expr context_expr
    (Format.asprintf "Unable to determine that `%a` is subsumed by `%a` in %a"
       PP.pp_type_term sub PP.pp_type_term super PP.pp_expr context_expr)

let non_constant_used_as_constant_set context_term id =
  spec_error_loc_of_term context_term
  @@ Format.asprintf
       "%s is used as a constant even though it is not defined as one" id

let tuple_instantiation_length_failure term args label def_components =
  spec_error_loc_of_term term
  @@ Format.asprintf
       "The type term `%a` cannot be instantiated since it has %i type terms \
        and `%s` requires %i type terms"
       PP.pp_type_term term (List.length args) label
       (List.length def_components)

let tuple_instantiation_failure_not_labelled_tuple term label =
  spec_error_loc_of_term term
  @@ Format.asprintf
       "The type term `%a` cannot be instantiated since %s is not a labelled \
        tuple type"
       PP.pp_type_term term label

let record_instantiation_failure_different_fields term def_term =
  spec_error_loc_of_term term
  @@ Format.asprintf
       "The type term `%a` cannot be instantiated since its list of fields is \
        different to those of %a"
       PP.pp_type_term term PP.pp_type_term def_term

let record_instantiation_failure_not_labelled_record term label =
  spec_error_loc_of_term term
  @@ Format.asprintf
       "The type term `%a` cannot be instantiated since %s is not a labelled \
        record type"
       PP.pp_type_term term label

let instantiation_failure_not_a_type term label =
  spec_error_loc_of_term term
  @@ Format.asprintf
       "The type term `%a` cannot be instantiated since %s is not a type"
       PP.pp_type_term term label

let empty_rule { loc; Relation.name } =
  spec_error_loc ~loc
  @@ Format.asprintf "The rule for relation %s is empty." name

let relation_argument_incorrect_naming relation_name ((_, term) as arg) =
  spec_error_loc_of_term term
  @@ Format.asprintf
       "The term %a in relation %s does not provide a name for at least one of \
        its sub-terms."
       PP.pp_opt_named_type_term arg relation_name

let multiple_output_judgments { loc; Relation.name } rule_name_opt =
  let pp_name_opt fmt = function
    | Some name -> Format.fprintf fmt ": %s" name
    | None -> ()
  in
  spec_error_loc ~loc
  @@ Format.asprintf
       "All but the last judgment in the rule for relation %s must be \
        non-output judgments%a"
       name pp_name_opt rule_name_opt

let missing_output_judgment { loc; Relation.name } expanded_rule_name_opt =
  spec_error_loc ~loc
  @@ Format.asprintf
       "The rule for relation %s does not end with an output judgment in case \
        %s"
       name
       (Option.value ~default:"top-level" expanded_rule_name_opt)

let illegal_lhs_application expr =
  spec_error_loc_of_expr expr
  @@ Format.asprintf
       "The left-hand side of an application must be a relation, type variant, \
        or operator, but found %a"
       PP.pp_expr expr

let invalid_number_of_arguments rel_name expr ~expected ~actual =
  spec_error_loc_of_expr expr
  @@ Format.asprintf
       "The application of relation %s in expression %a has an invalid number \
        of arguments: expected %d but found %d"
       rel_name PP.pp_expr expr expected actual

let invalid_number_of_components expr ~expected ~actual =
  spec_error_loc_of_expr expr
  @@ Format.asprintf
       "The labelled tuple %a has an invalid number of args: expected %d but \
        found %d"
       PP.pp_expr expr expected actual

let invalid_record_field_names expr expr_field_names record_type_field_names =
  spec_error_loc_of_expr expr
  @@ Format.asprintf
       "The record expression %a has missing or invalid field names: expected \
        %s but found %s"
       PP.pp_expr expr
       (String.concat ", " record_type_field_names)
       (String.concat ", " expr_field_names)

let non_field id expr =
  spec_error_loc_of_expr expr
  @@ Format.asprintf "The non-field identifier %s is used as a field in %a" id
       PP.pp_expr expr

let undefined_variable_in_rule ~context_expr id =
  spec_error_loc_of_expr context_expr
  @@ Format.asprintf "The variable %s is used before it is defined" id

let redefined_variable_in_rule ~context_expr id =
  spec_error_loc_of_expr context_expr
  @@ Format.asprintf "The variable %s is re-defined" id

let undefined_field_in_record ~context_expr base_type field_id =
  spec_error_loc_of_expr context_expr
  @@ Format.asprintf "The field %s is not defined in the record type %a in %a"
       field_id PP.pp_type_term base_type PP.pp_expr context_expr

let invalid_list_index_type index_type ~context_expr =
  spec_error_loc_of_expr context_expr
  @@ Format.asprintf
       "The index type %a in %a is not a subtype of the natural numbers type \
        (N)"
       PP.pp_type_term index_type PP.pp_expr context_expr

let invalid_list_base_type base_type ~context_expr =
  spec_error_loc_of_expr context_expr
  @@ Format.asprintf "The type %a in %a is not a list type" PP.pp_type_term
       base_type PP.pp_expr context_expr

let invalid_number_of_arguments_for_map ~expected ~actual ~context_expr =
  spec_error_loc_of_expr context_expr
  @@ Format.asprintf
       "The map application in %a has an invalid number of arguments: expected \
        %d but found %d"
       PP.pp_expr context_expr expected actual

let invalid_argument_type ~arg_expr ~actual_type ~formal_type ~context_expr =
  spec_error_loc_of_expr context_expr
  @@ Format.asprintf
       "The argument or field expression %a has type %a but expected to have a \
        subtype of %a"
       PP.pp_expr arg_expr PP.pp_type_term actual_type PP.pp_type_term
       formal_type

let invalid_map_lhs_type lhs_type ~context_expr:expr =
  spec_error_loc_of_expr expr
  @@ Format.asprintf
       "The map application in %a has an invalid left-hand side type: expected \
        a function type but found %a"
       PP.pp_expr expr PP.pp_type_term lhs_type

let invalid_labelled_type label ~context_expr =
  spec_error_loc_of_expr context_expr
  @@ Format.asprintf
       "The label %s does not correspond to a labelled record or tuple in %a"
       label PP.pp_expr context_expr

let type_instantiation_length_failure formal_type arg_type ~expected_length
    ~actual_length =
  spec_error_loc_of_term arg_type
  @@ Format.asprintf
       "The type term `%a` cannot be instantiated with `%a` since they have \
        different numbers of arguments/fields: expected %d but found %d"
       PP.pp_type_term formal_type PP.pp_type_term arg_type expected_length
       actual_length

let type_operator_instantiation_failure ~relation_name formal_type arg_type =
  spec_error_loc_of_term arg_type
  @@ Format.asprintf
       "The type term `%a` cannot be instantiated with `%a` for operator `%s` \
        since there are incompatible argument types for it"
       PP.pp_type_term formal_type PP.pp_type_term arg_type relation_name

let uninstantiated_parameter_in_relation param relation_name ~context_expr =
  spec_error_loc_of_expr context_expr
  @@ Format.asprintf
       "The type parameter %s of relation %s could not be instantiated in %a"
       param relation_name PP.pp_expr context_expr

let parameter_type_unification_failure ~relation_name parameter_name term1 term2
    =
  spec_error
  @@ Format.asprintf
       "Could not unify types %a and %a for parameter %s of relation %s"
       PP.pp_type_term term1 PP.pp_type_term term2 parameter_name relation_name

let only_single_output_relations_supported name ~context_expr =
  spec_error_loc_of_expr context_expr
  @@ Format.asprintf
       "Only single output relations are supported outside of transition \
        judgments, and %s has more than one output in %a"
       name PP.pp_expr context_expr

let only_relation_transitions_supported ~context_expr =
  spec_error_loc_of_expr context_expr
  @@ Format.asprintf
       "Only relation applications are supported on the left-hand side of \
        transitions in %a"
       PP.pp_expr context_expr

let no_matching_output_type rhs ~context_expr =
  spec_error_loc_of_expr context_expr
  @@ Format.asprintf "No matching output type found for expression %a in %a"
       PP.pp_expr rhs PP.pp_expr context_expr

let ambiguous_output_type rhs candidates ~context_expr =
  spec_error_loc_of_expr context_expr
  @@ Format.asprintf
       "Ambiguous output type for expression %a in %a. Possible candidates: %a"
       PP.pp_expr rhs PP.pp_expr context_expr
       (PP.pp_sep_list ~sep:", " PP.pp_type_term)
       candidates

let invalid_indexed_body_type body_type ~context_expr =
  spec_error_loc_of_expr context_expr
  @@ Format.asprintf "The body type %a in %a is not the Boolean type"
       PP.pp_type_term body_type PP.pp_expr context_expr

let cannot_apply_type_to_expr expr target_type =
  spec_error_loc_of_expr expr
  @@ Format.asprintf "Cannot type %a as %a" PP.pp_expr expr PP.pp_type_term
       target_type

let judgment_not_boolean inferred_judgment_type judgment_expr =
  spec_error_loc_of_expr judgment_expr
  @@ Format.asprintf
       "The type inferred for the judgment %a is %a, which is not the Boolean \
        type"
       PP.pp_expr judgment_expr PP.pp_type_term inferred_judgment_type

let output_type_mismatch output_judgment_type output_types output_expr =
  spec_error_loc_of_expr output_expr
  @@ Format.asprintf
       "The type %a inferred for the output judgment %a does not match any of \
        the output types %a"
       PP.pp_type_term output_judgment_type PP.pp_expr output_expr
       (PP.pp_sep_list ~sep:" | " PP.pp_type_term)
       output_types

let record_update_expression_not_assignable expr =
  spec_error_loc_of_expr expr
  @@ Format.asprintf
       "The record update expression %a cannot be used as an assignable \
        expression"
       PP.pp_expr expr

let invalid_record_update_base_type base_type ~context_expr =
  spec_error_loc_of_expr context_expr
  @@ Format.asprintf "The base type %a in %a is not a record type"
       PP.pp_type_term base_type PP.pp_expr context_expr

let missing_type_for_constant id =
  spec_error @@ Format.asprintf "Missing type for constant %s" id

let missing_case_in_rule case_name relation_name case_path =
  spec_error
  @@ Format.asprintf
       "Case %s does not exist in rule for relation %s for path %s." case_name
       relation_name case_path
