open AST
open ASTUtils
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

(* A variable for discarding of values. *)
let ignore_var = "_"

(* The identifier that should be used to mark bound variables. *)
let bound_variable = "bound_variable"

module Error = struct
  let spec_error msg = raise (SpecError msg)

  let bad_layout term layout ~consistent_layout =
    spec_error
    @@ Format.asprintf
         "layout %a is inconsistent with %a. Here's a consistent layout: %a"
         PP.pp_layout layout PP.pp_type_term term PP.pp_layout consistent_layout

  let undefined_reference id context =
    spec_error @@ Format.asprintf "Undefined reference to '%s' in %s" id context

  let undefined_element id =
    spec_error @@ Format.asprintf "Encountered undefined element: %s" id

  let duplicate_definition id =
    spec_error @@ Format.asprintf "Duplicate definition of '%s'" id

  let unmatched_variables_in_template template unmatched_vars =
    spec_error
    @@ Format.asprintf
         "The prose template '%s' contains the following unmatched variables: \
          %s"
         template
         (String.concat ", " unmatched_vars)

  let not_type_name_error relation_name term =
    spec_error
    @@ Format.asprintf
         "In relation '%s', output type term '%a' is not a type name or a set \
          of constants. All relation output types, except for the first one, \
          must be type names or a set of constants."
         relation_name PP.pp_type_term term

  let missing_short_circuit_attribute relation_name term type_name =
    spec_error
    @@ Format.asprintf
         "In relation '%s', output type term '%a' references type '%s' which \
          does not have a short-circuit macro defined. All relation output \
          types, except for the first one, must reference types with \
          short-circuit macros."
         relation_name PP.pp_type_term term type_name

  let invalid_application_of_symbol_in_expr name expr =
    spec_error
    @@ Format.asprintf "Invalid application of symbol '%s' to expression %a"
         name PP.pp_expr expr

  let type_subsumption_failure sub super =
    spec_error
    @@ Format.asprintf "Unable to determine that `%a` is subsumed by `%a`"
         PP.pp_type_term sub PP.pp_type_term super

  let non_constant_used_as_constant_set id =
    spec_error
    @@ Format.asprintf
         "%s is used as a constant even though it is not defined as one" id

  let tuple_instantiation_length_failure term args label def_components =
    spec_error
    @@ Format.asprintf
         "The type term `%a` cannot be instantiated since it has %i type terms \
          and `%s` requires %i type terms"
         PP.pp_type_term term (List.length args) label
         (List.length def_components)

  let tuple_instantiation_failure_not_labelled_tuple term label =
    spec_error
    @@ Format.asprintf
         "The type term `%a` cannot be instantiated since '%s' is not a \
          labelled tuple type"
         PP.pp_type_term term label

  let record_instantiation_failure_different_fields term def_term =
    spec_error
    @@ Format.asprintf
         "The type term `%a` cannot be instantiated since its list of fields \
          is different to those of %a"
         PP.pp_type_term term PP.pp_type_term def_term

  let record_instantiation_failure_not_labelled_record term label =
    spec_error
    @@ Format.asprintf
         "The type term `%a` cannot be instantiated since '%s' is not a \
          labelled record type"
         PP.pp_type_term term label

  let instantiation_failure_not_a_type term label =
    spec_error
    @@ Format.asprintf
         "The type term `%a` cannot be instantiated since '%s' is not a type"
         PP.pp_type_term term label

  let empty_rule relation_name =
    spec_error
    @@ Format.asprintf "The rule for relation '%s' is empty." relation_name

  let missing_relation_argument_name relation_name =
    spec_error
    @@ Format.asprintf
         "All arguments in the relation '%s' must have names, since it \
          specifies a rule."
         relation_name

  let multiple_output_judgments relation_name rule_name_opt =
    let pp_name_opt fmt = function
      | Some name -> Format.fprintf fmt ": %s" name
      | None -> ()
    in
    spec_error
    @@ Format.asprintf
         "All but the last judgment in the rule for relation %s must be \
          non-output judgments%a"
         relation_name pp_name_opt rule_name_opt

  let missing_output_judgment relation_name expanded_rule_name_opt =
    spec_error
    @@ Format.asprintf
         "The rule for relation %s does not end with an output judgment in \
          case %s"
         relation_name
         (Option.value ~default:"top-level" expanded_rule_name_opt)

  let illegal_lhs_application expr =
    spec_error
    @@ Format.asprintf
         "The left-hand side of an application must be a relation, type \
          variant, or operator, but found expression %a"
         PP.pp_expr expr

  let invalid_number_of_arguments rel_name expr ~expected ~actual =
    spec_error
    @@ Format.asprintf
         "The application of relation '%s' in expression %a has an invalid \
          number of arguments: expected %d but found %d"
         rel_name PP.pp_expr expr expected actual

  let invalid_number_of_components label expr ~expected ~actual =
    spec_error
    @@ Format.asprintf
         "The application of tuple label '%s' in expression %a has an invalid \
          number of args: expected %d but found %d"
         label PP.pp_expr expr expected actual

  let invalid_record_field_names expr expr_field_names record_type_field_names =
    spec_error
    @@ Format.asprintf
         "The record expression %a has invalid field names: expected %s but \
          found %s"
         PP.pp_expr expr
         (String.concat ", " expr_field_names)
         (String.concat ", " record_type_field_names)

  let non_field id expr =
    spec_error
    @@ Format.asprintf
         "The non-field identifier '%s' is used in expression %a as a field" id
         PP.pp_expr expr

  let undefined_variable_in_rule ~context_expr id =
    spec_error
    @@ Format.asprintf "The variable %s is used in %a before it is defined" id
         PP.pp_expr context_expr

  let redefined_variable_in_rule ~context_expr id =
    spec_error
    @@ Format.asprintf
         "The variable %s is defined twice, the second time is in %a" id
         PP.pp_expr context_expr
end

(** A wrapper type for the different kinds of elements in a spec that are
    associated with an identifier. *)
type definition_node =
  | Node_Type of Type.t
  | Node_Relation of Relation.t
  | Node_TypeVariant of TypeVariant.t
  | Node_Constant of Constant.t
  | Node_RecordField of Term.record_field

(** [definition_node_name node] returns the name associated with the definition
    node [node]. *)
let definition_node_name = function
  | Node_Type { Type.name }
  | Node_Relation { Relation.name }
  | Node_Constant { Constant.name }
  | Node_RecordField { name } ->
      name
  | Node_TypeVariant def -> Option.get (variant_to_label_opt def)

(** [pp_definition_node fmt node] pretty-prints the definition node [node]. *)
let pp_definition_node fmt =
  let open PP in
  function
  | Node_Type def -> pp_type_definition fmt def
  | Node_Relation def -> pp_relation_definition fmt def
  | Node_Constant def -> pp_constant_definition fmt def
  | Node_TypeVariant { TypeVariant.term } -> pp_type_term fmt term
  | Node_RecordField { name; term } -> pp_named_type_term fmt (name, term)

(** [args_of_tuple id_to_defining_node label] returns the components of the
    labelled tuple type variant named [label] using [id_to_defining_node] to
    lookup the type variant definition node. *)
let args_of_tuple id_to_defining_node label =
  match StringMap.find label id_to_defining_node with
  | Node_TypeVariant { TypeVariant.term = Tuple { args } } -> args
  | node ->
      let msg =
        Format.asprintf
          "Expected labelled tuple type variant for label %s, found %a." label
          pp_definition_node node
      in
      failwith msg

(** [math_macro_opt_for_node node] returns the optional math macro associated
    with the definition node [node]. *)
let math_macro_opt_for_node = function
  | Node_Type def -> Type.math_macro def
  | Node_Relation def -> Relation.math_macro def
  | Node_TypeVariant def -> TypeVariant.math_macro def
  | Node_Constant def -> Constant.math_macro def
  | Node_RecordField def -> Term.record_field_math_macro def

(** [prose_description_for_node node] returns the optional prose description
    associated with the definition node [node]. *)
let prose_description_for_node = function
  | Node_Type def -> Type.prose_description def
  | Node_Relation def -> Relation.prose_description def
  | Node_TypeVariant def -> TypeVariant.prose_description def
  | Node_Constant def -> Constant.prose_description def
  | Node_RecordField def -> Term.record_field_prose_description def

(** [vars_of_node node] returns the list of term-naming variables that occur at
    any depth inside the definition node [node]. *)
let vars_of_node = function
  | Node_Type { Type.variants; _ } ->
      Utils.list_concat_map
        (fun { TypeVariant.term } -> vars_of_type_term term)
        variants
  | Node_TypeVariant { TypeVariant.term } -> vars_of_type_term term
  | Node_Constant _ -> []
  | Node_Relation { Relation.input; output; _ } ->
      vars_of_opt_named_type_terms input
      @ Utils.list_concat_map vars_of_type_term output
  | Node_RecordField { name; term } -> name :: vars_of_type_term term

(** [is_constant id_to_defining_node id] is true if and only if [id] is either
    defined as a constant directly or as a type variant with a label.

    For example:
    {[
      typedef A = L;
      constant C
    ]}
    defines the constant [L] and the constant [C]. *)
let is_constant id_to_defining_node id =
  match StringMap.find_opt id id_to_defining_node with
  | Some (Node_Constant _) | Some (Node_TypeVariant { term = Label _ }) -> true
  | _ -> false

(** [check_is_constant id_to_defining_node id] raises an error if [id] is not
    defined as a constant directly or as a type variant with a label. *)
let check_is_constant id_to_defining_node id =
  if is_constant id_to_defining_node id then ()
  else Error.non_constant_used_as_constant_set id

(** Utility functions for handling layouts. *)
module Layout = struct
  (** Transforms a relation definition into a type term representing its input
      and output types as a tuple in order to facilitate its rendering. For a
      relation like [ r(a,b,c) -> A|B|C ] generates a term
      [ (r(a,b,c), (A,B,C)) ]. That is, a pair where the first component is a
      labelled tuple with the relation name and arguments and the second
      component is a tuple with all output terms. *)
  let relation_to_tuple { Relation.name; input; output } =
    let opt_named_output_terms = List.map (fun term -> (None, term)) output in
    let open Term in
    make_tuple
      [
        (None, make_labelled_tuple name input);
        (None, make_tuple opt_named_output_terms);
      ]

  let rec for_type_term term =
    let open Term in
    match term with
    | Label _ -> Unspecified
    | TypeOperator { term = _, t } -> for_type_term t
    | Tuple { args = [] | _ :: [] } -> Unspecified
    | Tuple { args } ->
        Horizontal (List.map (fun (_, t) -> for_type_term t) args)
    | Record { fields; _ } ->
        if List.length fields > 1 then
          let layout_per_field =
            List.map (fun { Term.term } -> for_type_term term) fields
          in
          Vertical layout_per_field
        else Unspecified
    | ConstantsSet names -> Horizontal (List.map (fun _ -> Unspecified) names)
    | Function { from_type = _, from_term; to_type = _, to_term; _ } ->
        Horizontal [ for_type_term from_term; for_type_term to_term ]

  let math_layout_for_node = function
    | Node_Type def -> (
        match Type.math_layout def with
        | Some layout -> layout
        | None -> Unspecified)
    | Node_Constant _ -> Unspecified
    | Node_Relation def -> (
        match Relation.math_layout def with
        | Some layout -> layout
        | None -> for_type_term (relation_to_tuple def))
    | Node_TypeVariant ({ TypeVariant.term } as def) -> (
        match TypeVariant.math_layout def with
        | Some layout -> layout
        | None -> for_type_term term)
    | Node_RecordField { Term.term } -> for_type_term term
end

(** Lists nodes that define identifiers and can be referenced. *)
let list_definition_nodes ast =
  List.fold_right
    (fun elem acc_nodes ->
      match elem with
      | Elem_Relation def -> Node_Relation def :: acc_nodes
      | Elem_Type ({ Type.variants } as def) ->
          (* Labelled types directly under this type are considered defined here,
             but labelled types nested further in are considered as type references.
          *)
          let acc_nodes =
            List.fold_right
              (fun ({ TypeVariant.term } as variant) acc_nodes ->
                match term with
                | Label _ | Tuple { label_opt = Some _ } ->
                    Node_TypeVariant variant :: acc_nodes
                | Record { label_opt; fields } ->
                    let field_nodes =
                      List.map (fun field -> Node_RecordField field) fields
                    in
                    let variant_if_labelled =
                      Option.map (fun _ -> Node_TypeVariant variant) label_opt
                      |> Option.to_list
                    in
                    variant_if_labelled @ field_nodes @ acc_nodes
                | Tuple { label_opt = None }
                | Function _ | ConstantsSet _ | TypeOperator _ ->
                    acc_nodes)
              variants acc_nodes
          in
          Node_Type def :: acc_nodes
      | Elem_Constant def -> Node_Constant def :: acc_nodes
      (* Although a render defines an identifier, it does not carry semantic
         meaning, and cannot be referenced elsewhere. *)
      | Elem_RenderTypes _ | Elem_RenderRule _ -> acc_nodes)
    ast []

(** Creates a map from identifiers to the nodes where they are defined. If two
    nodes with the same identifier exist, a [SpecError] is raised. *)
let make_id_to_definition_node definition_nodes =
  List.fold_left
    (fun acc_map node ->
      let name = definition_node_name node in
      StringMap.update name
        (function
          | Some _ -> Error.duplicate_definition name | None -> Some node)
        acc_map)
    StringMap.empty definition_nodes

type t = {
  ast : AST.t;  (** The AST, added with builtin definitions, transformed. *)
  id_to_defining_node : definition_node StringMap.t;
      (** Associates identifiers with the AST nodes where they are defined. *)
  assign : Relation.t;
  reverse_assign : Relation.t;
  bottom_constant : Constant.t;
  none_constant : Constant.t;
  empty_set : Constant.t;
  empty_list : Constant.t;
  _bool_type : Type.t;
  n_type : Type.t;
  z_type : Type.t;
  _q : Type.t;
  _equal : Relation.t;
}

type spec_type = t

let is_builtin_type id (t : Type.t) = String.equal id t.name
let is_builtin_constant id (c : Constant.t) = String.equal id c.name
let _is_builtin_relation id (r : Relation.t) = String.equal id r.name

(** [make_symbol_table ast] creates a symbol table from [ast]. *)
let make_symbol_table ast =
  let definition_nodes = list_definition_nodes ast in
  make_id_to_definition_node definition_nodes

(** [update_spec_ast spec ast] updates the specification [spec] with a new AST
    [ast], returning a new specification. *)
let update_spec_ast spec ast =
  (* With new AST elements, we need to re-generate the symbol table. *)
  { spec with ast; id_to_defining_node = make_symbol_table ast }

let defined_ids self =
  StringMap.bindings self.id_to_defining_node |> List.map fst

(** [iter_defined_nodes self f] applies function [f] to each defined node in
    [self]. *)
let iter_defined_nodes self f =
  StringMap.iter (fun _ node -> f node) self.id_to_defining_node

let defining_node_opt_for_id self id =
  StringMap.find_opt id self.id_to_defining_node

let defining_node_for_id self id =
  match defining_node_opt_for_id self id with
  | Some def -> def
  | None -> Error.undefined_element id

(** [relation_for_id self id] returns the relation definition node for the given
    identifier [id], which is assumed to correspond to a relation definition. *)
let relation_for_id self id =
  match defining_node_for_id self id with
  | Node_Relation def -> def
  | _ -> assert false

let is_defined_id self id = StringMap.mem id self.id_to_defining_node
let elements self = self.ast

(** [symbol_table_for_id id_to_defining_node id] returns the symbol table for
    the scope of the definition with identifier [id]. If [id] corresponds to a
    relation definition, the parameters of the relation definition are added. *)
let symbol_table_for_id id_to_defining_node id =
  match StringMap.find_opt id id_to_defining_node with
  | Some (Node_Relation { Relation.parameters }) ->
      List.fold_left
        (fun curr_table param ->
          let type_for_param = Type.make TypeKind_Generic param [] [] in
          StringMap.add param (Node_Type type_for_param) curr_table)
        id_to_defining_node parameters
  | _ -> id_to_defining_node

(** [is_variadic_operator id_to_defining_node id] checks if [id] represents a
    variadic operator. That is, an operator whose only argument is a list type.
    Variadic operators accept any number of actual arguments. *)
let is_variadic_operator spec id =
  match StringMap.find id spec.id_to_defining_node with
  | Node_Relation { is_operator = true; is_variadic } -> is_variadic
  | _ -> false

(** A module to resolve expressions appearing in rules and constant values. *)
module ResolveApplicationExpr = struct
  (** [resolve_application_expr id_to_defining_node expr] resolves application
      expressions appearing in [expr] by using [id_to_defining_node] to lookup
      type variants and relations. *)
  let rec resolve_application_expr id_to_defining_node expr =
    let resolve_in_context = resolve_application_expr id_to_defining_node in
    let open Expr in
    match expr with
    | Var _ | FieldAccess _ -> expr
    | Tuple { label_opt; args } ->
        let resolved_args = List.map resolve_in_context args in
        Tuple { label_opt; args = resolved_args }
    | Relation { is_operator = true; name; args } ->
        let resolved_args = List.map resolve_in_context args in
        Relation { is_operator = true; name; args = resolved_args }
    | ListIndex { list_var; index } ->
        let resolved_index = resolve_in_context index in
        ListIndex { list_var; index = resolved_index }
    | Record { label_opt; fields } ->
        let resolved_fields =
          List.map
            (fun (field_name, field_expr) ->
              (field_name, resolve_in_context field_expr))
            fields
        in
        Record { label_opt; fields = resolved_fields }
    | UnresolvedApplication { lhs; args } -> (
        let resolved_args = List.map resolve_in_context args in
        match lhs with
        | Var id -> (
            match StringMap.find_opt id id_to_defining_node with
            | Some (Node_Relation { Relation.is_operator; name }) ->
                Relation { is_operator; name; args = resolved_args }
            | Some (Node_TypeVariant { term = Term.Tuple { label_opt } })
            | Some
                (Node_Constant { opt_type = Some (Term.Tuple { label_opt }) })
              ->
                Tuple { label_opt; args = resolved_args }
            | Some (Node_Constant { Constant.name })
            | Some (Node_Type { Type.name }) ->
                Error.invalid_application_of_symbol_in_expr name expr
            | Some (Node_RecordField _) | Some (Node_TypeVariant _) | None ->
                Error.illegal_lhs_application expr)
        | _ -> Map { lhs = resolve_in_context lhs; args = resolved_args })
    | Transition { lhs; rhs; short_circuit } ->
        let resolved_lhs = resolve_in_context lhs in
        let resolved_rhs = resolve_in_context rhs in
        let resolved_short_circuit =
          Option.map (List.map resolve_in_context) short_circuit
        in
        Transition
          {
            lhs = resolved_lhs;
            rhs = resolved_rhs;
            short_circuit = resolved_short_circuit;
          }
    | Indexed ({ body : Expr.t } as indexed_expr) ->
        let resolved_body = resolve_in_context body in
        Indexed { indexed_expr with body = resolved_body }
    | NamedExpr (sub_expr, name) ->
        let resolved_sub_expr = resolve_in_context sub_expr in
        NamedExpr (resolved_sub_expr, name)
    | Relation _ | Map _ ->
        let msg =
          Format.asprintf "unexpected resolved expression: %a" PP.pp_expr expr
        in
        failwith msg

  let rec resolve_rule_element id_to_defining_node rule_element =
    let open Rule in
    let open Expr in
    match rule_element with
    | Judgment ({ expr } as judgment) ->
        let resolved_expr = resolve_application_expr id_to_defining_node expr in
        Judgment { judgment with expr = resolved_expr }
    | Cases cases ->
        let resolved_cases =
          List.map
            (fun { name; elements } ->
              let resolved_elements =
                List.map (resolve_rule_element id_to_defining_node) elements
              in
              { name; elements = resolved_elements })
            cases
        in
        Cases resolved_cases

  let resolve ({ ast; id_to_defining_node } as spec) =
    let open Rule in
    let ast =
      List.map
        (fun elem ->
          match elem with
          | Elem_Type _ | Elem_RenderTypes _ | Elem_RenderRule _ -> elem
          | Elem_Constant ({ Constant.opt_value_and_attributes } as def) -> (
              match opt_value_and_attributes with
              | Some (e, attributes) ->
                  let resolved_e =
                    resolve_application_expr id_to_defining_node e
                  in
                  Elem_Constant
                    {
                      def with
                      Constant.opt_value_and_attributes =
                        Some (resolved_e, attributes);
                    }
              | _ -> elem)
          | Elem_Relation ({ rule_opt = Some elements } as def) ->
              let resolved_def =
                let resolved_elements =
                  List.map (resolve_rule_element id_to_defining_node) elements
                in
                { def with rule_opt = Some resolved_elements }
              in
              Elem_Relation resolved_def
          | Elem_Relation { rule_opt = None } as elem -> elem)
        ast
    in
    update_spec_ast spec ast
end

(** A module for normalizing rules within relations to make them amenable for
    further checking and for rendering. There are three transformations:
    {ul
     {- Aggregates consecutive [Case] blocks into a single [Case] block. }
     {- Resolves [Unresolved] in applicators to either [Node_Relation] or
        [Node_TypeVariant]
     }
     {- Transforms the conclusion judgment from an expression to a transition.
        That is, given
        {[
          relation r(a: A) -> (b:B) =
            p1; ... pk;
            --
            c_expr;
        ]}
        we transform it to
        {[
          relation r(a: A) -> (b:B) =
            p1; ... pk;
            --
            r(a) -> c_expr;
        ]}
        The result is the re-written AST along with an updated map from
        identifiers to definition nodes, updated for the newly created relation
        nodes.
     }
    } *)
module ResolveRules = struct
  open Rule

  (** [resolve_rule_element id_to_defining_node rule_element] performs
      resolution transformations on [rule_element] using [id_to_defining_node]
      to lookup type variants and relations. *)
  let rec resolve_rule_element id_to_defining_node conclusion_lhs rule_element =
    let open Expr in
    match rule_element with
    | Judgment ({ expr; is_output } as judgment) ->
        let resolved_expr =
          if is_output then
            Transition
              {
                lhs = conclusion_lhs;
                rhs = expr;
                short_circuit = Some [];
                (* Output transitions never have alternatives.
                   This ensures alternatives are not inserted. *)
              }
          else expr
        in
        Judgment { judgment with expr = resolved_expr }
    | Cases cases ->
        let resolved_cases =
          List.map
            (fun { name; elements } ->
              let resolved_elements =
                List.map
                  (resolve_rule_element id_to_defining_node conclusion_lhs)
                  elements
              in
              { name; elements = resolved_elements })
            cases
        in
        Cases resolved_cases

  (** [lhs_of_conclusion def] returns an expression representing the LHS of the
      conclusion judgment for the relation definition [def]. This function
      assumes [relation_named_arguments_if_exists_rule] has been called. *)
  let lhs_of_conclusion { Relation.name; is_operator; input } =
    (* Converts an optionally-named type term into an expression. *)
    let rec arg_of = function
      | Some name, _ -> Expr.Var name
      | None, Term.Tuple { label_opt; args } ->
          let args = List.map arg_of args in
          Expr.make_opt_labelled_tuple label_opt args
      | None, Term.Record { label_opt; fields } ->
          let field_exprs =
            List.map (fun { Term.name } -> (name, Expr.Var name)) fields
          in
          Expr.Record { label_opt; fields = field_exprs }
      | None, _ -> Error.missing_relation_argument_name name
    in
    let args = List.map arg_of input in
    Expr.Relation { name; is_operator; args }

  (** In text, a list of [case] elements without non-[case] elements in between
      them are considered to be a single case element with multiple cases.
      Therefore, we make this explicit by merging all consecutive [Case]
      elements into a single [Case] element.

      So for example, the following list of elements:
      {[
        Judgment j1;
        Cases [ case1 ];
        Cases [ case2 ];
        Judgment j2;
        Cases [ case3 ];
        Cases [ case4 ]
      ]}
      is transformed into:
      {[
        Judgment j1;
        Cases [ case1; case2 ];
        Judgment j2;
        Cases [ case3; case4 ]
      ]}

      [aggregate_case_blocks rule_elements] merges all consecutive [Case]
      elements in [rule_elements] into a single [Cases] element, recursing into
      [Case] elements elements as needed. *)
  let rec aggregate_case_blocks rule_elements =
    let open Rule in
    (* First, aggregate by recursing into each Case element separately. *)
    let rule_elements =
      List.map
        (fun rule_elem ->
          match rule_elem with
          | Judgment _ -> rule_elem
          | Cases cases ->
              let aggregated_cases =
                List.map
                  (fun { name; elements } ->
                    { name; elements = aggregate_case_blocks elements })
                  cases
              in
              Cases aggregated_cases)
        rule_elements
    in
    (* Second, aggregate consecutive Cases elements. *)
    let rec aggregate = function
      | [] -> []
      | Rule.Cases cases1 :: Rule.Cases cases2 :: rest ->
          aggregate (Rule.Cases (cases1 @ cases2) :: rest)
      | rule_element :: rest -> rule_element :: aggregate rest
    in
    aggregate rule_elements

  let resolve ({ ast; id_to_defining_node } as spec) =
    let open Rule in
    let ast =
      List.map
        (fun elem ->
          match elem with
          | Elem_Type _ | Elem_RenderTypes _ | Elem_RenderRule _
          | Elem_Constant _ ->
              elem
          | Elem_Relation ({ rule_opt = Some elements } as def) ->
              let resolved_def =
                let elements = aggregate_case_blocks elements in
                let conclusion_lhs = lhs_of_conclusion def in
                let resolved_elements =
                  List.map
                    (resolve_rule_element id_to_defining_node conclusion_lhs)
                    elements
                in
                { def with rule_opt = Some resolved_elements }
              in
              Elem_Relation resolved_def
          | Elem_Relation { rule_opt = None } as elem -> elem)
        ast
    in
    update_spec_ast spec ast
end

(** A module for extending expressions in output configurations of conclusion
    judgments with names derived from type terms. *)
module ExtendNames = struct
  open Expr
  open Rule

  (** [opt_extend] Wraps [expr] with a name if [opt_name] is [Some]. Avoids
      naming a variable expression with its own name, as an optimization. *)
  let opt_extend expr opt_name =
    match (expr, opt_name) with
    | _, None -> expr
    | Var v, Some name when String.equal v name ->
        expr (* Avoid naming a variable with its own name. *)
    | _, Some name -> NamedExpr (expr, name)

  (** [extend_with_names type_term expr ] recursively transforms [expr] by
      adding names from [type_term] to sub-expressions of [expr]. Currently,
      only tuples (labelled or unlabelled) are supported, which is sufficient
      for most output configurations. *)
  let rec extend_with_names type_term expr =
    match (type_term, expr) with
    | Term.Tuple { label_opt = None; args = [ (opt_name, _) ] }, _ ->
        (* An unlabelled tuple with a single component serves as a named reference
           to any type.*)
        opt_extend expr opt_name
    | ( Term.Tuple { label_opt = term_label_opt; args = term_components },
        Expr.Tuple { label_opt = expr_label_opt; args = expr_components } )
      when Option.equal String.equal term_label_opt expr_label_opt ->
        let () =
          if List.compare_lengths term_components expr_components <> 0 then
            let msg =
              Format.asprintf
                "The expression %a cannot be extended with names from the type \
                 term %a since they have different number of args."
                PP.pp_expr expr PP.pp_type_term type_term
            in
            raise (SpecError msg)
          else ()
        in
        let extended_args =
          List.map2
            (fun (opt_name, arg_type) arg ->
              opt_extend (extend_with_names arg_type arg) opt_name)
            term_components expr_components
        in
        Expr.Tuple { label_opt = expr_label_opt; args = extended_args }
    | _ -> expr

  (** [extend_rule_element output_type rule_element] extends output judgments in
      [rule_element] with names from [output_type]. *)
  let rec extend_rule_element output_type rule_element =
    match rule_element with
    | Judgment
        ({ expr = Transition { lhs; rhs; short_circuit }; is_output = true } as
         judgment) ->
        let extended_rhs =
          if auto_name_judgment judgment then extend_with_names output_type rhs
          else rhs
        in
        let extended_expr =
          Transition { lhs; rhs = extended_rhs; short_circuit }
        in
        Judgment { judgment with expr = extended_expr }
    | Judgment _ -> rule_element
    | Cases cases ->
        let extended_cases =
          List.map
            (fun { name; elements } ->
              let extended_elements =
                List.map (extend_rule_element output_type) elements
              in
              { name; elements = extended_elements })
            cases
        in
        Cases extended_cases

  (** [extend ast] extends [ast] by adding names to expressions in output
      configurations. It returns the extended AST and an updated symbol table.
  *)
  let extend ({ ast } as spec) =
    let ast =
      List.map
        (fun elem ->
          match elem with
          | ( Elem_Type _ | Elem_Constant _ | Elem_RenderTypes _
            | Elem_RenderRule _
            | Elem_Relation { rule_opt = None } ) as elem ->
              elem
          | Elem_Relation ({ rule_opt = Some elements; output } as def) ->
              (* The parser ensures that [output] is non-empty. *)
              let first_output_type = List.hd output in
              (* TODO: we currently use the first type variant in the list of
               output types for the relation to assign names.
               A more flexible approach is to find the output type term
               that rhs corresponds to. *)
              let extended_elements =
                List.map (extend_rule_element first_output_type) elements
              in
              Elem_Relation { def with rule_opt = Some extended_elements })
        ast
    in
    update_spec_ast spec ast
end

(** A module for expanding rules with cases into multiple rules without cases.
*)
module ExpandRules = struct
  open Rule

  type expanded_rule = {
    name_opt : string option;
        (** The optional name of the expanded rule, if it originated from a
            case. *)
    judgments : Rule.judgment list;
        (** After all cases have been expanded, the rule is simply a list of
            judgments. *)
  }
  (** An expanded rule is a rule with no [Cases]. *)

  let split_absolute_rule_name abs_name =
    Str.split (Str.regexp_string ".") abs_name

  (** [concat_expanded_rules prefix suffix] concatenates two expanded rules,
      [prefix] and [suffix], combining their optional names as needed. *)
  let concat_expanded_rules
      { name_opt = prefix_name_opt; judgments = prefix_judgments }
      { name_opt = suffix_name_opt; judgments = suffix_judgments } =
    let name_opt =
      match (prefix_name_opt, suffix_name_opt) with
      | None, None -> None
      | Some name, None | None, Some name -> Some name
      | Some prefix_name, Some suffix_name ->
          Some (Rule.join_case_names [ prefix_name; suffix_name ])
    in
    { name_opt; judgments = prefix_judgments @ suffix_judgments }

  (** [product_concat expanded_prefix expanded_suffix] performs a Cartesian
      product concatenation of two lists of expanded rules, [expanded_prefix]
      and [expanded_suffix], returning a list of expanded rules. *)
  let product_concat expanded_prefix expanded_suffix =
    Utils.list_concat_map
      (fun expanded_suffix_rule ->
        List.map
          (fun expanded_case_rule ->
            concat_expanded_rules expanded_case_rule expanded_suffix_rule)
          expanded_prefix)
      expanded_suffix

  (** [expand elements] expands the rule whose list of elements is [elements]
      into multiple rules without cases. *)
  let rec expand elements =
    let open Rule in
    (* Transforms a single case into expanded rules and update the names
       of all resulting expanded cases by prefixing them with the case name
       and setting the optional category.
    *)
    let expand_case { name; elements } =
      let name_as_expanded = [ { name_opt = Some name; judgments = [] } ] in
      let expanded_case_elements = expand elements in
      product_concat name_as_expanded expanded_case_elements
    in
    (* Expand cases by transforming the list bottom-up. *)
    List.fold_right
      (fun rule_elem suffix_expanded ->
        match rule_elem with
        | Judgment judgment ->
            let judgment_as_expanded_rule_list =
              [ { name_opt = None; judgments = [ judgment ] } ]
            in
            product_concat judgment_as_expanded_rule_list suffix_expanded
        | Cases cases ->
            let cases_as_expanded_rule =
              Utils.list_concat_map expand_case cases
            in
            product_concat cases_as_expanded_rule suffix_expanded)
      elements
      [ { name_opt = None; judgments = [] } ]
end

module Check = struct
  (** [check_layout term layout] checks that the given [layout] is structurally
      consistent with the given [term]. If not, raises a [SpecError] describing
      the issue. *)
  let rec check_layout term layout =
    let consistent_layout = Layout.for_type_term term in
    let open Layout in
    let open Term in
    match (term, layout) with
    | Label _, Unspecified -> ()
    | Label _, _ -> Error.bad_layout term layout ~consistent_layout
    | TypeOperator { term = _, t }, _ -> check_layout t layout
    | Tuple { args }, Horizontal cells | Tuple { args }, Vertical cells ->
        if List.compare_lengths args cells <> 0 then
          Error.bad_layout term layout ~consistent_layout
        else
          List.iter2 (fun (_, term) cell -> check_layout term cell) args cells
    | Record { fields }, Horizontal cells | Record { fields }, Vertical cells ->
        if List.compare_lengths fields cells <> 0 then
          Error.bad_layout term layout ~consistent_layout
        else
          List.iter2
            (fun { Term.term } cell -> check_layout term cell)
            fields cells
    | ConstantsSet names, (Horizontal cells | Vertical cells) ->
        if List.compare_lengths names cells <> 0 then
          Error.bad_layout term layout ~consistent_layout
        else List.iter2 (fun _ cell -> check_layout (Label "") cell) names cells
    | ( Function { from_type = _, from_term; to_type = _, to_term; _ },
        (Horizontal cells | Vertical cells) ) ->
        if List.length cells <> 2 then
          Error.bad_layout term layout ~consistent_layout
        else check_layout from_term (List.nth cells 0);
        check_layout to_term (List.nth cells 1)
    | _, Unspecified -> ()

  (** [check_math_layout spec] checks that the math layouts for all defining
      nodes in [spec] are structurally consistent with their type terms. *)
  let check_math_layout spec =
    let check_math_layout_for_definition_node node =
      let open Layout in
      match node with
      | Node_Type { Type.name } ->
          check_layout (Label name) (math_layout_for_node node)
      | Node_Constant { Constant.name } ->
          check_layout (Label name) (math_layout_for_node node)
      | Node_TypeVariant { TypeVariant.term } ->
          check_layout term (math_layout_for_node node)
      | Node_Relation def ->
          check_layout (relation_to_tuple def) (math_layout_for_node node)
      | Node_RecordField { term } ->
          check_layout term (math_layout_for_node node)
    in
    iter_defined_nodes spec check_math_layout_for_definition_node

  (** Returns all the identifiers referencing nodes that define identifiers. *)
  let rec referenced_ids =
    let open Term in
    function
    | Label id -> [ id ]
    | TypeOperator { term = _, t } -> referenced_ids t
    | Tuple { label_opt; args } -> (
        let component_ids =
          List.map snd args |> Utils.list_concat_map referenced_ids
        in
        match label_opt with
        | None -> component_ids
        | Some label -> label :: component_ids)
    | Record { label_opt; fields } -> (
        let fields_ids =
          List.map (fun { Term.term } -> term) fields
          |> Utils.list_concat_map referenced_ids
        in
        match label_opt with
        | None -> fields_ids
        | Some label -> label :: fields_ids)
    | ConstantsSet constant_names -> constant_names
    | Function { from_type = _, from_term; to_type = _, to_term } ->
        referenced_ids from_term @ referenced_ids to_term

  (** [check_no_undefined_ids spec] checks that all identifiers referenced by
      [spec.ast] are keys in [spec.id_to_defining_node]. *)
  let check_no_undefined_ids { ast; id_to_defining_node } =
    let check_no_undefined_ids_in_elem id_to_defining_node elem =
      let referenced_ids_for_list = Utils.list_concat_map referenced_ids in
      let ids_referenced_by_elem =
        match elem with
        | Elem_Constant _ -> []
        | Elem_Type { Type.variants } ->
            referenced_ids_for_list
              (List.map (fun { TypeVariant.term } -> term) variants)
        | Elem_Relation { Relation.input; output } ->
            let input_terms = List.map snd input in
            referenced_ids_for_list (input_terms @ output)
        | Elem_RenderTypes { pointers } ->
            Utils.list_concat_map
              (fun { TypesRender.type_name; variant_names } ->
                type_name :: variant_names)
              pointers
        | Elem_RenderRule { RuleRender.relation_name } -> [ relation_name ]
      in
      let elem_name = elem_name elem in
      (* Update the symbol table by introducing types for parameters. *)
      let id_to_defining_node =
        symbol_table_for_id id_to_defining_node elem_name
      in
      List.iter
        (fun id ->
          if not (StringMap.mem id id_to_defining_node) then
            Error.undefined_reference id elem_name)
        ids_referenced_by_elem
    in
    List.iter (check_no_undefined_ids_in_elem id_to_defining_node) ast

  (** [check_relations_outputs elems id_to_defining_node] checks that, for each
      relation in [elems], the first output type term is arbitrary, and that all
      type terms following it are either type names or sets of constants.
      Furthermore, it checks that all type names used as alternative output type
      terms reference types with the [short_circuit_macro] attribute defined. If
      not, raises a [SpecError] describing the issue. *)
  let check_relations_outputs { ast; id_to_defining_node } =
    let relations_defs =
      List.filter_map (function Elem_Relation def -> Some def | _ -> None) ast
    in
    let check_alternative_outputs { Relation.name; output } =
      let alternative_outputs = ListLabels.tl output in
      List.iter
        (fun term ->
          match term with
          | Term.Label id -> (
              match StringMap.find id id_to_defining_node with
              | Node_Type typedef -> (
                  match Type.short_circuit_macro typedef with
                  | None -> Error.missing_short_circuit_attribute name term id
                  | Some _ -> ())
              | _ -> Error.not_type_name_error name term)
          | Term.ConstantsSet _ -> ()
          | _ -> Error.not_type_name_error name term)
        alternative_outputs
    in
    List.iter check_alternative_outputs relations_defs

  (** A module for checking that each prose template string ([prose_description]
      and [prose_application] attributes) to ensure it does not contain a
      [{var}] where [var] does not name any type term. If it does, LaTeX will
      fail on [{var}], which would require debugging the generated code. This
      check catches such cases and generates an easy to understand explanation.
  *)
  module CheckProseTemplates : sig
    val check : t -> unit
    (** [check spec] checks all prose templates defined in [spec]. *)
  end = struct
    (** [check_prose_template_for_vars template vars] checks that [template]
        does not contain a [{var}] where [var] is not in [vars]. Otherwise,
        raises a [SpecError] detailing the unmatched variables. *)
    let check_prose_template_for_vars template vars =
      let open Latex in
      (* Populate with [{var}] for each [var]. *)
      let template_vars =
        List.fold_left
          (fun acc_map var_str ->
            let template_var = spec_var_to_template_var var_str in
            StringSet.add template_var acc_map)
          StringSet.empty vars
      in
      let template_var_regexp = Str.regexp "{[a-zA-Z0-9_']+}" in
      (* Remove things like [\texttt{a}], which do not (should not) reference variables. *)
      let reduce_template =
        Str.global_replace
          (Str.regexp
             {|\\\([a-zA-Z]+\){[a-zA-Z0-9_']+}\|\(\\hyperlink{[a-zA-Z_\-]*}{[a-zA-Z_\-]*}\)|})
          "" template
      in
      let blocks = Str.full_split template_var_regexp reduce_template in
      let unmatched_vars =
        List.fold_left
          (fun acc block ->
            match block with
            | Str.Text _ -> acc
            | Str.Delim var -> (
                match StringSet.find_opt var template_vars with
                | Some _ -> acc
                | None -> var :: acc))
          [] blocks
      in
      if Utils.list_is_empty unmatched_vars then ()
      else Error.unmatched_variables_in_template template unmatched_vars

    let check_prose_template_for_definition_node defining_node =
      let prose_description = prose_description_for_node defining_node in
      let vars = vars_of_node defining_node in
      let () = check_prose_template_for_vars prose_description vars in
      match defining_node with
      | Node_Type _ | Node_TypeVariant _ | Node_Constant _ | Node_RecordField _
        ->
          ()
      | Node_Relation def ->
          let prose_application = Relation.prose_application def in
          let () = check_prose_template_for_vars prose_application vars in
          ()

    let check spec =
      iter_defined_nodes spec check_prose_template_for_definition_node
  end

  (** A module for conservatively checking that all type terms are well-formed
      and that all type instantiations are valid. That is each type term that
      instantiates another defined type term is subsumed by it.

      For example, given the following definitions:
      {[
        typedef Z;
        typedef Bool = True | False;
        ast literal = L_Int(whole_number: Z) | L_Bool(value: Bool);
        typedef native_value = NV_Literal(l: literal);
        typedef tint = ( NV_Literal(L_Int(v: Z)) );
      ]}
      The type [native_value] defines the labelled tuple type [L_Int] with a
      single component of type [literal].

      The type term for [tint] instantiates the type term for
      [NV_Literal(L_Int(v: Z))] rather than defining it, which can be seen since
      it is parenthesized. The check needs to ensure that
      [NV_Literal(L_Int(v: Z))] is valid.

      This is done by: 1. checking that [NV_Literal(L_Int(v: Z))] is well-formed
      with respect to [NV_Literal(l: literal)], and

      2. checking that all values of [NV_Literal(L_Int(v: Z))] are also value of
      [NV_Literal(l: literal)]. In other words, [NV_Literal(L_Int(v: Z))] is
      subsumed by [NV_Literal(l: literal)].

      To check (1), we verify that the labelled tuple [NV_Literal(L_Int(v: Z))]
      has a single component.

      To check (2), we use structural induction, which means that we need to
      verify that [L_Int(v: Z)] is subsumed by [l: literal]. This is done by
      checking that [L_Int(v: Z)] is subsumed by at least one of the type
      variants of [literal], which is indeed the case since [L_Int(v: Z)] is a
      type variant of [literal]. *)
  module CheckTypeInstantiations : sig
    val check : t -> unit
    (** [check spec] conservatively checks that all type terms referenced in
        [spec.ast] that instantiate type terms in the range of
        [spec.id_to_defining_node] are also subsumed by them. The check assumes
        that [check_no_undefined_ids] has already been run. *)

    val _subsumed : t -> Term.t -> Term.t -> bool
    (** [subsumed id_to_defining_node sub super] conservatively tests whether
        all values in the domain of [sub] are also in the domain of [super].
        [id_to_definition_node] is used to look up types by their names. *)

    val _operator_subsumed : Term.type_operator -> Term.type_operator -> bool
    (** [_operator_subsumed sub_op super_op] is true if any [term], all values
        in the domain of [TypeOperator { op = sub_op; term }] are also in the
        domain of [TypeOperator { op = super_op; term }]. *)

    val _reduce_term : t -> Term.t -> Term.t
    (** [_reduce_term id_to_defining_node term] reduces [term] by applying two
        simplifications:

        - If [term] is an unlabelled singleton tuple (that is, a type
          reference), it is replaced by its single component term. This
          simplifies handling tuple types.

        - If [term] is a label referencing a type with a single variant, it is
          reduced to that variant's term. *)
  end = struct
    open Term

    (** If a type defines a single variant, reduce references to that type to
        the variant's term.

        For example, given:
        {[
          typedef type_error = TypeError (error_code : type_error_code)
        ]}
        then [type_error] is reduced to
        [TypeError(error_code: type_error_code)]. *)
    let rec reduce_single_variant_type spec term =
      let open Term in
      match term with
      | Label id -> (
          match defining_node_opt_for_id spec id with
          | Some
              (Node_Type
                 { Type.variants = [ { TypeVariant.term = variant_term } ]; _ })
            ->
              _reduce_term spec variant_term
          | _ -> term)
      | _ -> term

    (** [reduce_type_reference spec term] reduces [term] if it is an unlabelled
        singleton tuple, which represents a type reference.

        For example, given:
        {[
          typedef tbool = NV_Literal(L_Bool(v: Boolean type));
        ]}
        then the type term for [tbool] is the unlabelled singleton tuple
        [( NV_Literal(L_Bool(v: Boolean type)) )], which is reduced to
        [NV_Literal(L_Bool(v: Boolean type))]. *)
    and reduce_type_reference spec term =
      match term with
      | Tuple { label_opt = None; args = [ (_, referenced_term) ] } ->
          _reduce_term spec referenced_term
      | _ -> term

    and _reduce_term spec term =
      reduce_type_reference spec term |> reduce_single_variant_type spec

    (** [subsumed sub_op super_op] is true if all values in the domain of
        [TypeOperator { op = sub_op; term }] are also in the domain of
        [TypeOperator { op = super_op; term }]. *)
    let _operator_subsumed sub_op super_op =
      match (sub_op, super_op) with
      | Powerset, Powerset
      | Powerset_Finite, (Powerset_Finite | Powerset)
      | List1, (List1 | List0)
      | List0, List0
      | Option, (Option | Powerset_Finite | Powerset)
      (* Option represents sets with 0 or 1 values while Powerset represents sets
         of _any_ number of values. *)
        ->
          true
      | _ -> false

    (** [subsumed id_to_defining_node expanded_types sub super] conservatively
        tests whether all values in the domain of [sub] are also in the domain
        of [super]. Labels that represent types may be expanded, that is,
        replaced by their list of type variants, using [id_to_defining_node]. To
        ensure termination on recursive types, this expansion is done at most
        once by tracking the set of expanded labels in [expanded_types].

        This function assumes that [check_well_formed] has already been run.

        In the example, [M(B, Num)] is not subsumed by [M(A, Num)]. The
        algorithm checks whether [B] is subsumed by [A], which requires checking
        whether [B] is subsumed by [L] and whether [B] is subsumed by
        [M(A, Num)]. The first check fails, but the second check requires
        expanding [B] to check whether all of its variants are subsumed by
        [M(A, Num)], namely whether [M(B, Num)] is subsumed by [M(A, Num)],
        which is the original subsumption test.

        To avoid infinite recursion, the algorithm tracks which types have
        already been expanded and does not expand them again when checking
        subsumption for [B]. Thus, [B] is not expanded again, and the
        subsumption test returns [false].
        {[
          typedef;
          typedef A = L | M(A, Num);
          typedef B = ( M(B, Num) );
        ]} *)
    let rec subsumed_rec spec expanded_types sub super =
      (* In the example above [( M(B, Num) )] is equivalent to [M(B, Num)]. *)
      let sub = _reduce_term spec sub in
      let super = _reduce_term spec super in
      let result =
        match (sub, super) with
        | Label sub_id, _ when is_builtin_constant sub_id spec.bottom_constant
          ->
            (* The bottom constant is a subset of every type. *)
            true
        | Label sub_id, Label super_id
          when is_builtin_type sub_id spec.n_type
               && is_builtin_type super_id spec.z_type ->
            true
        | Label sub_label, TypeOperator { op = List0 | List1 }
          when is_builtin_constant sub_label spec.empty_list ->
            (* The empty list is a subset of every list. *)
            true
        | Label sub_label, TypeOperator { op = Powerset | Powerset_Finite }
          when is_builtin_constant sub_label spec.empty_set ->
            (* The empty set is a subset of every set. *)
            true
        | ( Label sub_label,
            TypeOperator { op = Option | Powerset | Powerset_Finite } )
          when is_builtin_constant sub_label spec.none_constant ->
            (* None is a subset of Option and also every set. *)
            true
        | _, Label super_label ->
            let sub_is_label_case =
              match sub with
              | Label sub_label ->
                  String.equal sub_label super_label
                  ||
                  (* The case where [sub_label] is a type name,
                  like [B] of [M(B, Num)] in the example. *)
                  subsumed_typename_super spec expanded_types sub_label super
              | _ -> false
            in
            sub_is_label_case
            (* The case where [super_label] is a type name,
              like [A] of [M(A, Num)] in the example.
          *)
            || subsumed_sub_typename spec expanded_types sub super_label
        (* From here on the test operates via structural induction. *)
        | ( TypeOperator { op = sub_op; term = _, sub_term },
            TypeOperator { op = super_op; term = _, super_term } ) ->
            _operator_subsumed sub_op super_op
            && subsumed_rec spec expanded_types sub_term super_term
        | ( Tuple { label_opt = sub_label_opt; args = sub_components },
            Tuple { label_opt = super_label_opt; args = super_components } ) ->
            Option.equal String.equal sub_label_opt super_label_opt
            && List.for_all2
                 (fun (_, sub_term) (_, super_term) ->
                   subsumed_rec spec expanded_types sub_term super_term)
                 sub_components super_components
        | ( Record { label_opt = sub_label_opt; fields = sub_fields },
            Record { label_opt = super_label_opt; fields = super_fields } ) ->
            Option.equal String.equal sub_label_opt super_label_opt
            && List.for_all2
                 (fun { term = sub_term; _ } { term = super_term; _ } ->
                   subsumed_rec spec expanded_types sub_term super_term)
                 sub_fields super_fields
        | ( Function { from_type = _, sub_from_term; to_type = _, sub_to_term },
            Function
              { from_type = _, super_from_term; to_type = _, super_to_term } )
          ->
            (* Functions can be partial or total, which require different subsumption tests.
             To make this simple, we require equivalence of the from-terms and to-terms,
             which is sufficient for our needs.
          *)
            let equivalence_test term term' =
              subsumed_rec spec expanded_types term term'
              && subsumed_rec spec expanded_types term' term
            in
            equivalence_test sub_from_term super_from_term
            && equivalence_test sub_to_term super_to_term
        | ConstantsSet sub_names, ConstantsSet super_names ->
            List.for_all (fun name -> List.mem name super_names) sub_names
        | _ ->
            (* false is safely conservative. *)
            false
      in
      let () =
        if false then
          Format.eprintf "Subsumed check: %a <= %a = %b@." PP.pp_type_term sub
            PP.pp_type_term super result
      in
      result

    (** [subsumed_sub_typename id_to_defining_node sub_term typename] checks if
        [sub_term] is subsumed by any of the type variants defined by [typename]
        in [id_to_defining_node]. If [typename] is not a type with type
        variants, returns false. *)
    and subsumed_sub_typename ({ id_to_defining_node } as spec : spec_type)
        expanded_types sub_term typename =
      if StringSet.mem typename expanded_types then false
      else
        (* Prevent infinite recursion on recursive types by expanding a type name at most once. *)
        let expanded_types = StringSet.add typename expanded_types in
        match StringMap.find_opt typename id_to_defining_node with
        | Some (Node_Type { Type.variants; _ }) ->
            (* [sub_term] is subsumed by the type [typename] if it is subsumed by at least
              one of its variants.
              For example [L_Int(v: Z)] is subsumed by [literal] since it is subsumed by the
              variant [L_Int(whole_number: Z)] of [literal].
            *)
            (not (Utils.list_is_empty variants))
            && List.exists
                 (fun { TypeVariant.term = super_term } ->
                   subsumed_rec spec expanded_types sub_term super_term)
                 variants
        | _ -> false

    (** [subsumed_typename_super id_to_defining_node typename super_term] checks
        if all type variants defined by [typename] are subsumed by [super_term].
        If [typename] is not a type with type variants, returns false. *)
    and subsumed_typename_super ({ id_to_defining_node } as spec) expanded_types
        typename super_term =
      if StringSet.mem typename expanded_types then false
      else
        (* Prevent infinite recursion on recursive types by expanding a type name at most once. *)
        let expanded_types = StringSet.add typename expanded_types in
        match StringMap.find_opt typename id_to_defining_node with
        | Some (Node_Type { Type.variants; _ }) ->
            (not (Utils.list_is_empty variants))
            && List.for_all
                 (fun { TypeVariant.term = sub_term } ->
                   subsumed_rec spec expanded_types sub_term super_term)
                 variants
        | _ -> false

    let _subsumed id_to_defining_node sub super =
      subsumed_rec id_to_defining_node StringSet.empty sub super

    (** [check_subsumed_terms_lists id_to_defining_node term label sub_terms
         super_terms] checks that each term in [sub_terms] is subsumed by the
        corresponding term in [super_terms]. Both lists of terms comprise the
        labelled tuple/labelled record [term] with label [label]. If the lists
        have different lengths, or if any term in [sub_terms] is not subsumed by
        the corresponding term in [super_terms], a [SpecError] is raised. *)
    let check_subsumed_terms_lists id_to_defining_node sub_terms super_terms =
      let check_subsumed id_to_defining_node sub super =
        if _subsumed id_to_defining_node sub super then ()
        else Error.type_subsumption_failure sub super
      in
      List.iter2 (check_subsumed id_to_defining_node) sub_terms super_terms

    (** [check_well_typed id_to_defining_node term] checks that every type
        referenced by [term] correctly instantiates its defining type with
        respect to the type definitions in the range of [id_to_defining_node].
        The check assumes that both [check_no_undefined_ids] and that
        [check_well_formed] have already been run.

        The check operates by structural induction on [term], except when a
        label term references another type in which case the type variants of
        the referenced type are considered. *)
    let rec check_well_instantiated spec term =
      match term with
      | TypeOperator { term = _, operator_term } ->
          check_well_instantiated spec operator_term
      | Tuple { label_opt; args } -> (
          let terms = List.map snd args in
          let () = List.iter (check_well_instantiated spec) terms in
          match label_opt with
          | None -> ()
          | Some label -> (
              let variant_def = StringMap.find label spec.id_to_defining_node in
              match variant_def with
              | Node_TypeVariant
                  {
                    TypeVariant.term = Tuple { args = def_opt_named_components };
                  } ->
                  let def_terms = List.map snd def_opt_named_components in
                  check_subsumed_terms_lists spec terms def_terms
              | _ -> assert false))
      | Record { label_opt; fields } -> (
          let terms = List.map (fun { term } -> term) fields in
          let () = List.iter (check_well_instantiated spec) terms in
          match label_opt with
          | None -> ()
          | Some label -> (
              let variant_def = StringMap.find label spec.id_to_defining_node in
              match variant_def with
              | Node_TypeVariant
                  { TypeVariant.term = Record { fields = def_fields; _ } } ->
                  let def_terms = List.map field_type def_fields in
                  check_subsumed_terms_lists spec terms def_terms
              | _ -> assert false))
      | Function { from_type = _, from_term; to_type = _, to_term } ->
          check_well_instantiated spec from_term;
          check_well_instantiated spec to_term
      | ConstantsSet _ | Label _ -> ()

    (** [check_well_formed id_to_defining_node term] checks that [term] is
        well-formed with respect to the type definitions in the range of
        [id_to_defining_node]. That is:
        - tuples have the correct number of components,
        - records have the correct fields,
        - constants sets only reference defined constants,
        - labelled types reference defined labelled types. The check assumes
          that [check_no_undefined_ids] has already been run and operates by
          structural induction on [term]. *)
    let rec check_well_formed id_to_defining_node term =
      match term with
      | TypeOperator { term = _, operator_term } ->
          check_well_formed id_to_defining_node operator_term
      | Tuple { label_opt; args } -> (
          let terms = List.map snd args in
          let () = List.iter (check_well_formed id_to_defining_node) terms in
          match label_opt with
          | None -> ()
          | Some label -> (
              let variant_def = StringMap.find label id_to_defining_node in
              match variant_def with
              | Node_TypeVariant
                  { TypeVariant.term = Tuple { args = def_components } } ->
                  if List.compare_lengths args def_components <> 0 then
                    Error.tuple_instantiation_length_failure term args label
                      def_components
              | _ ->
                  Error.tuple_instantiation_failure_not_labelled_tuple term
                    label))
      | Record { label_opt; fields } -> (
          let terms = List.map field_type fields in
          let () = List.iter (check_well_formed id_to_defining_node) terms in
          match label_opt with
          | None -> ()
          | Some label -> (
              let variant_def = StringMap.find label id_to_defining_node in
              match variant_def with
              | Node_TypeVariant
                  {
                    TypeVariant.term =
                      Record { fields = def_fields; _ } as def_term;
                  } ->
                  let field_names = List.map field_name fields in
                  let def_field_names = List.map field_name def_fields in
                  if
                    not
                      (Utils.list_is_equal String.equal field_names
                         def_field_names)
                  then
                    Error.record_instantiation_failure_different_fields term
                      def_term
                  else ()
              | _ ->
                  Error.record_instantiation_failure_not_labelled_record term
                    label))
      | Function { from_type = _, from_term; to_type = _, to_term } ->
          check_well_formed id_to_defining_node from_term;
          check_well_formed id_to_defining_node to_term
      | ConstantsSet labels ->
          List.iter (check_is_constant id_to_defining_node) labels
      | Label label -> (
          (* Label definitions have been filtered out so this is either a
              reference to a type, which is well-formed, or reference to a label,
              which should only appear in a [constants_set]. *)
          let variant_def = StringMap.find label id_to_defining_node in
          match variant_def with
          | Node_Type _ -> ()
          | _ -> Error.instantiation_failure_not_a_type term label)

    let check_well_typed spec term =
      check_well_formed spec.id_to_defining_node term;
      (* Now check whether [term] instantiates a type
        and if so, check that it is subsumed by it.
      *)
      check_well_instantiated spec term

    let check ({ ast; id_to_defining_node } as spec) =
      List.iter
        (fun elem ->
          try
            match elem with
            | Elem_RenderTypes _ | Elem_RenderRule _
            | Elem_Constant { opt_type = None } ->
                ()
            | Elem_Constant { opt_type = Some type_term } ->
                check_well_typed spec type_term
            | Elem_Relation { name; input; output } ->
                (* The check must be made in a symbol table that contains the relation parameters. *)
                let spec =
                  {
                    spec with
                    id_to_defining_node =
                      symbol_table_for_id id_to_defining_node name;
                  }
                in
                List.iter (fun (_, term) -> check_well_typed spec term) input;
                List.iter (check_well_typed spec) output
            | Elem_Type { Type.variants; _ } ->
                List.iter
                  (fun { TypeVariant.term } ->
                    match term with
                    | Label _ ->
                        () (* A constant label definition is well-formed. *)
                    | _ -> check_well_typed spec term)
                  variants
          with SpecError e ->
            stack_spec_error e
              (Format.asprintf "While checking: %s" (elem_name elem)))
        ast
  end

  (** A module for checking variable usage and inferring types for expressions
      in relation rules. The functions in this module assume that a rule exists
      and that [Check.relation_named_arguments_if_exists_rule] has been run. *)
  module TypeInference = struct
    open Expr

    (** [is_quantifying_operator spec op_name] tests whether [op_name] is a
        quantifying operator (like "forall" or "exists") based on whether its
        first argument is named as [bound_variable]. *)
    let is_quantifying_operator spec op_name =
      let operator = relation_for_id spec op_name in
      match operator.input with
      | (Some arg_name, _) :: _ -> String.equal arg_name bound_variable
      | _ -> false

    (** [vars_of_identifiers id_to_defining_node identifiers] returns the
        sublist of [identifiers] that represent variables. The map
        [id_to_defining_node] is used to filter out constants and labels. *)
    let vars_of_identifiers id_to_defining_node identifiers =
      (* Filters out identifiers that are definitely not variables,
         since they are either constants or type names. *)
      let is_non_var id =
        match StringMap.find_opt id id_to_defining_node with
        | Some (Node_Constant _)
        | Some (Node_Type _)
        | Some (Node_TypeVariant _) ->
            (* Constants and types cannot be used as variables.*)
            true
        | Some (Node_RecordField _)
        | Some (Node_Relation _)
        (* Field names and relation names can be used as variables. *)
        | None ->
            false
      in
      identifiers
      |> List.filter (fun id ->
          not (String.equal id ignore_var || is_non_var id))

    type use_def = { used : StringSet.t; defined : StringSet.t }
    (** An environment data type for tracking used and defined variables. *)

    (** Pretty-prints a [use_def] environment for debugging. *)
    let pp_use_def fmt { used; defined } =
      Format.fprintf fmt "{ used: [%s]; defined: [%s]}"
        (String.concat ", " (StringSet.elements used))
        (String.concat ", " (StringSet.elements defined))

    (** [check_and_add_uses ~context_expr use_def vars] checks that all
        variables in [vars] are already defined in [use_def] and adds them to
        the used set.
        @raise [SpecError]
          if any variable in [vars] is not defined in [use_def]. *)
    let check_and_add_uses ~context_expr use_def vars =
      let undefined_vars =
        StringSet.diff (StringSet.of_list vars) use_def.defined
      in
      let () =
        if not (StringSet.is_empty undefined_vars) then
          Error.undefined_variable_in_rule ~context_expr
            (StringSet.choose undefined_vars)
      in
      let new_used = StringSet.union use_def.used (StringSet.of_list vars) in
      { use_def with used = new_used }

    (** [check_and_add_defined ~context_expr use_def vars] checks that all
        variables in [vars] are not already defined in [use_def] and adds them
        to the defined set.
        @raise [SpecError]
          if any variable in [vars] is already defined in [use_def]. *)
    let check_and_add_defined ~context_expr use_def vars =
      let var_set = StringSet.of_list vars in
      let redefined_vars = StringSet.inter var_set use_def.defined in
      let () =
        if not (StringSet.is_empty redefined_vars) then
          Error.redefined_variable_in_rule ~context_expr
            (StringSet.choose redefined_vars)
      in
      let new_defined =
        StringSet.union use_def.defined (StringSet.of_list vars)
      in
      { use_def with defined = new_defined }

    (** A type for distinguishing whether we are analyzing an expression for
        variable uses or variable definitions. *)
    type use_def_mode = Use | Def

    (** [update_use_def_for_expr mode spec use_def expr] updates the use-def
        environment [use_def] by analyzing [expr]. If [uses] is [true], it
        considers [expr] as an expression that uses variables. Otherwise, it
        considers [expr] as an expression that defines variables. *)
    let rec update_use_def_for_expr mode spec use_def expr =
      let check_and_add_for_expr mode use_def vars =
        match mode with
        | Use -> check_and_add_uses ~context_expr:expr use_def vars
        | Def -> check_and_add_defined ~context_expr:expr use_def vars
      in
      let open Expr in
      match expr with
      | Var id ->
          vars_of_identifiers spec.id_to_defining_node [ id ]
          |> check_and_add_for_expr mode use_def
      | FieldAccess { var } -> (
          match mode with
          | Use ->
              let vars_of_id =
                vars_of_identifiers spec.id_to_defining_node [ var ]
              in
              check_and_add_for_expr Use use_def vars_of_id
          | Def ->
              failwith
                "A field access cannot be used as a variable-defining \
                 expression")
      | ListIndex { list_var; index } ->
          let use_def =
            vars_of_identifiers spec.id_to_defining_node [ list_var ]
            |> check_and_add_for_expr mode use_def
          in
          (* An index expression never defines variables. *)
          update_use_def_for_expr Use spec use_def index
      | Record { fields } ->
          let initializing_exprs = List.map snd fields in
          update_use_def_for_expr_list mode spec use_def initializing_exprs
      | NamedExpr (sub_expr, _) ->
          update_use_def_for_expr mode spec use_def sub_expr
      | Tuple { args } -> update_use_def_for_expr_list mode spec use_def args
      | Map { lhs; args } ->
          update_use_def_for_expr_list mode spec use_def (lhs :: args)
      | Relation { is_operator = true; name; args = [ lhs; rhs ] }
        when String.equal name spec.assign.name ->
          let use_def = update_use_def_for_expr Use spec use_def rhs in
          update_use_def_for_expr Def spec use_def lhs
      | Relation { is_operator = true; name; args = [ lhs; rhs ] }
        when String.equal name spec.reverse_assign.name ->
          let use_def = update_use_def_for_expr Use spec use_def lhs in
          update_use_def_for_expr Def spec use_def rhs
      | Relation { is_operator = true; name; args = Var bound_var :: tail_args }
        when is_quantifying_operator spec name ->
          update_use_def_with_bound_variable mode spec use_def tail_args
            ~context_expr:expr ~bound_var
      | Relation { args } -> update_use_def_for_expr_list mode spec use_def args
      | Transition { lhs; rhs } ->
          let use_def = update_use_def_for_expr Use spec use_def lhs in
          update_use_def_for_expr Def spec use_def rhs
      | Indexed { index; list_var; body } ->
          let use_def = check_and_add_for_expr Use use_def [ list_var ] in
          update_use_def_with_bound_variable mode spec use_def [ body ]
            ~context_expr:expr ~bound_var:index
      | UnresolvedApplication _ ->
          let msg =
            Format.asprintf
              "Unresolved application found when checking use-def in \
               expression: %a."
              PP.pp_expr expr
          in
          failwith msg

    and update_use_def_for_expr_list mode spec use_def exprs =
      List.fold_left
        (fun curr_use_def arg_expr ->
          update_use_def_for_expr mode spec curr_use_def arg_expr)
        use_def exprs

    (** updates [use_def] for the list of expressions [exprs] and the variable
        [bound_var], which is only in scope for [exprs]. *)
    and update_use_def_with_bound_variable mode spec use_def exprs ~context_expr
        ~bound_var =
      (* To avoid confusing specs, we forbid shadowing variables by bound variables. *)
      let use_def = check_and_add_defined ~context_expr use_def [ bound_var ] in
      let use_def = update_use_def_for_expr_list mode spec use_def exprs in
      (* The bound variable goes out of scope. *)
      { use_def with defined = StringSet.remove bound_var use_def.defined }

    (** [check_use_def relation spec expanded_rule] checks that all variables
        used in [expanded_rule] are defined before use and no variables are
        re-defined. *)
    let check_use_def relation spec expanded_rule =
      let open ExpandRules in
      let () =
        if false then
          Format.eprintf "Checking use-def for relation %s case %s@."
            relation.Relation.name
            (Option.value ~default:"top level"
               expanded_rule.ExpandRules.name_opt)
      in
      let premises, conclusion =
        Utils.split_last expanded_rule.ExpandRules.judgments
      in
      let premises_exprs = List.map (fun { Rule.expr } -> expr) premises in
      let output_expr =
        match conclusion.expr with
        | Transition { rhs } ->
            (* If the conclusion expression has been transformed to a transition,
            we need to consider only the right-hand side expression as the output,
            as the variables on the left-hand side are the same variables
            appearing in the input of the relation and are therefore
            implicitly defined - see next.*)
            rhs
        | _ -> conclusion.expr
      in
      (* The input variables are implicitly defined at the start of the rule. *)
      let defined_args =
        vars_of_opt_named_type_terms relation.Relation.input
        |> StringSet.of_list
      in
      let use_def = { used = StringSet.empty; defined = defined_args } in
      let use_def =
        update_use_def_for_expr_list Use spec use_def
          (premises_exprs @ [ output_expr ])
      in
      if false then Format.eprintf "After premises: %a@." pp_use_def use_def
      else ()
  end

  (** A module for checking the correctness of the rules in all relations. The
      checks in this module assume that [ResolveRules.resolve] has already been
      applied to the AST. *)
  module CheckRules = struct
    open Expr

    (** [check_well_formed_expanded relation_name expanded_rule] checks that the
        [expanded_rule] is well-formed, that is, it has at least one judgment,
        and that the last judgment, and only the last judgment, is an output
        judgment. If not, raises a [SpecError] describing the issue.
        [relation_name] is used when reporting errors. *)
    let check_well_formed_expanded relation_name expanded_rule =
      let open ExpandRules in
      (* Reverse the list to easily access the last judgment. *)
      match List.rev expanded_rule.judgments with
      | [] -> Error.empty_rule relation_name
      | { expr = Transition _; is_output = true } :: prefix_rules ->
          List.iter
            (fun { Rule.is_output } ->
              if is_output then
                Error.multiple_output_judgments relation_name
                  expanded_rule.name_opt
              else ())
            prefix_rules
      | _ -> Error.missing_output_judgment relation_name expanded_rule.name_opt

    (** [formals_of_relation id_to_defining_node rel_name] returns the list of
        formal arguments for the relation named [rel_name] using
        [id_to_defining_node] to lookup the relation definition node. *)
    let formals_of_relation id_to_defining_node rel_name =
      match StringMap.find rel_name id_to_defining_node with
      | Node_Relation { Relation.input } -> input
      | _ -> failwith "Expected relation definition node."

    (** [check_expr_well_formed id_to_defining_node expr] checks that the
        expression [expr] is well-formed in the context of
        [id_to_defining_node]. That is, it has the correct structure in terms of
        number of arguments (or fields) with respect to the symbols it
        references. If not, raises a [SpecError] describing the issue. *)
    let rec check_expr_well_formed ({ id_to_defining_node } as spec) expr =
      let is_field id =
        match StringMap.find_opt id id_to_defining_node with
        | Some (Node_RecordField _) -> true
        | _ -> false
      in
      let check_expr_in_context = check_expr_well_formed spec in
      let check_expr_list_in_context = List.iter check_expr_in_context in
      let open Rule in
      match expr with
      | NamedExpr (sub_expr, _) -> check_expr_in_context sub_expr
      | Relation { name; args } ->
          let () = check_expr_list_in_context args in
          let formal_args = formals_of_relation id_to_defining_node name in
          if
            (not (is_variadic_operator spec name))
            (* A variadic operator accepts any number of arguments. *)
            && List.compare_lengths formal_args args <> 0
          then
            Error.invalid_number_of_arguments name expr
              ~expected:(List.length formal_args) ~actual:(List.length args)
      | Map { lhs; args } -> check_expr_list_in_context (lhs :: args)
      | Tuple { label_opt; args } -> (
          let () = check_expr_list_in_context args in
          match label_opt with
          | Some label ->
              let type_components = args_of_tuple id_to_defining_node label in
              if List.compare_lengths args type_components <> 0 then
                Error.invalid_number_of_components label expr
                  ~expected:(List.length type_components)
                  ~actual:(List.length args)
          | None -> ())
      | Record { label_opt; fields } -> (
          let expr_field_names, expr_field_inits = List.split fields in
          let () = check_expr_list_in_context expr_field_inits in
          match label_opt with
          | Some label ->
              let record_type_fields =
                match StringMap.find label id_to_defining_node with
                | Node_TypeVariant { TypeVariant.term = Record { fields } } ->
                    fields
                | _ -> Error.illegal_lhs_application expr
              in
              let record_type_field_names =
                List.map (fun { Term.name } -> name) record_type_fields
              in
              if
                not
                  (Utils.list_is_equal String.equal expr_field_names
                     record_type_field_names)
              then
                Error.invalid_record_field_names expr expr_field_names
                  record_type_field_names
              else ()
          | None -> ())
      | Transition { lhs; rhs; short_circuit } ->
          check_expr_in_context lhs;
          check_expr_in_context rhs;
          Option.iter check_expr_list_in_context short_circuit
      | Indexed { body } -> check_expr_in_context body
      | ListIndex { index } -> check_expr_in_context index
      | Var _ -> ()
      | FieldAccess { fields } ->
          List.iter
            (fun id -> if not (is_field id) then Error.non_field id expr)
            fields
      | UnresolvedApplication _ ->
          failwith "Unexpected unresolved application expression."

    (** [check_rule_for_relation spec relation elements] checks the elements of
        the rule for [relation] in the context of [spec].
        @raise [SpecError]
          if any check fails, describing the issue using [relation.name]. *)
    let check_rule_for_relation spec relation elements =
      let expanded_rules = ExpandRules.expand elements in
      let () =
        List.iter
          (fun expanded_rule ->
            try
              let () =
                check_well_formed_expanded relation.Relation.name expanded_rule
              in
              let () =
                TypeInference.check_use_def relation spec expanded_rule
              in
              let open ExpandRules in
              List.iter
                (fun { Rule.expr } -> check_expr_well_formed spec expr)
                expanded_rule.judgments
            with SpecError err | Failure err ->
              stack_spec_error err
                (Format.asprintf "In rule %s of %s"
                   (Option.value ~default:"(top-level case)"
                      expanded_rule.name_opt)
                   relation.Relation.name))
          expanded_rules
      in
      ()

    (** Checks the rules in all relations. *)
    let check ({ ast } as spec) =
      let open Rule in
      List.iter
        (fun elem ->
          match elem with
          | Elem_Type _ | Elem_Constant _ | Elem_RenderTypes _
          | Elem_RenderRule _
          | Elem_Relation { rule_opt = None } ->
              ()
          | Elem_Relation ({ rule_opt = Some elements } as def) ->
              check_rule_for_relation spec def elements)
        ast
  end

  (** [relation_named_arguments_if_exists_rule ast] checks that for each
      relation in [ast] that has a rule, all its input arguments are either
      named or can be deconstructed into named components. That is, arguments
      are either named, tuples whose components are named or recursively named,
      or records. *)
  let relation_named_arguments_if_exists_rule ast =
    let open Rule in
    let rec is_named_argument =
      let open Term in
      function
      | Some _, _ | None, Record _ -> true
      | None, Tuple { args } -> List.for_all is_named_argument args
      | None, _ -> false
    in
    let check_relation { Relation.name; input; rule_opt } =
      if Option.is_some rule_opt && not (List.for_all is_named_argument input)
      then Error.missing_relation_argument_name name
    in
    List.iter (function Elem_Relation def -> check_relation def | _ -> ()) ast
end

(** [add_default_rule_renders ast] adds default render rules for relations that
    have rules but do not have any rule render associated with them. That is,
    for a relation
    {[
      relation r(A) -> B = ...
    ]}
    without any specified rule render, it adds the rule render
    {[
      render rule r
    ]} *)
let add_default_rule_renders ({ ast } as spec) =
  (* First, gather the relations that have rule renders. *)
  let relations_with_rules =
    List.fold_left
      (fun acc elem ->
        match elem with
        | Elem_RenderRule { RuleRender.relation_name } ->
            StringSet.add relation_name acc
        | _ -> acc)
      StringSet.empty ast
  in
  (* For each relation without a rule render, add a render for all its cases,
      with the same name as the relation. *)
  let generated_elems =
    List.filter_map
      (fun elem ->
        match elem with
        | Elem_Relation { Relation.name; rule_opt = Some _ }
          when not (StringSet.mem name relations_with_rules) ->
            let rule_render = RuleRender.make ~name ~relation_name:name [] in
            Some (Elem_RenderRule rule_render)
        | _ -> None)
      ast
  in
  { spec with ast = ast @ generated_elems }

(** [get_type id_to_defining_node name] retrieves the [Type.t] associated with
    [name] in [id_to_defining_node].
    @raise [SpecError]
      If [name] is not associated with a [Type.t] in [id_to_defining_node]. *)
let get_type id_to_defining_node name =
  match StringMap.find name id_to_defining_node with
  | Node_Type def -> def
  | node ->
      let msg =
        Format.asprintf
          "%s must be a top-level type, but has been overridden with %a" name
          pp_definition_node node
      in
      raise (SpecError msg)

(** [get_constant id_to_defining_node name] retrieves the [Constant.t]
    associated with [name] in [id_to_defining_node].
    @raise [SpecError]
      If [name] is not associated with a [Constant.t] in [id_to_defining_node].
*)
let get_constant id_to_defining_node name =
  match StringMap.find name id_to_defining_node with
  | Node_Constant def -> def
  | node ->
      let msg =
        Format.asprintf "%s must be a constant, but has been overridden with %a"
          name pp_definition_node node
      in
      raise (SpecError msg)

(** [get_relation id_to_defining_node name] retrieves the [Relation.t]
    associated with [name] in [id_to_defining_node].
    @raise [SpecError]
      If [name] is not associated with a [Relation.t] in [id_to_defining_node].
*)
let get_relation id_to_defining_node name =
  match StringMap.find name id_to_defining_node with
  | Node_Relation def when def.is_operator -> def
  | node ->
      let msg =
        Format.asprintf
          "%s must be an operator, but has been overridden with %a" name
          pp_definition_node node
      in
      raise (SpecError msg)

(** [extend_ast_with_builtins ast id_to_defining_node] prepends to [ast] the AST
    elements from the built-in specification that are not already defined in
    [ast], as determined by [id_to_defining_node]. *)
let prepend_ast_with_builtins ast id_to_defining_node =
  (* builtins.ml is auto-generated via dune and contains a single
     string value builtin_spec_str. *)
  let builtin_ast =
    Parsing.parse_spec_from_string ~spec:Builtins.builtin_spec_str
      ~filename:"builtins.ml"
  in
  List.fold_right
    (fun elem acc_elems ->
      let elem_name = ASTUtils.elem_name elem in
      if StringMap.mem elem_name id_to_defining_node then acc_elems
      else elem :: acc_elems)
    builtin_ast ast

(** [make_spec_with_builtins ast] constructs a specification containing the
    builtin definitions. *)
let make_spec_with_builtins ast =
  let id_to_defining_node = make_symbol_table ast in
  let ast = prepend_ast_with_builtins ast id_to_defining_node in
  let id_to_defining_node = make_symbol_table ast in
  let get_constant = get_constant id_to_defining_node in
  let get_type = get_type id_to_defining_node in
  let get_relation = get_relation id_to_defining_node in
  {
    ast;
    id_to_defining_node;
    (* The following fields are not currently used, but they will be used in a future
    PR that implements type inference. *)
    bottom_constant = get_constant "bot";
    none_constant = get_constant "None";
    empty_set = get_constant "empty_set";
    empty_list = get_constant "empty_list";
    _bool_type = get_type "Bool";
    n_type = get_type "N";
    z_type = get_type "Z";
    _q = get_type "Q";
    assign = get_relation "assign";
    reverse_assign = get_relation "reverse_assign";
    _equal = get_relation "equal";
  }

let from_ast ast =
  let spec = make_spec_with_builtins ast in
  let () = Check.check_no_undefined_ids spec in
  let () = Check.check_relations_outputs spec in
  let () = Check.CheckTypeInstantiations.check spec in
  let () = Check.check_math_layout spec in
  let () = Check.CheckProseTemplates.check spec in
  let () = Check.relation_named_arguments_if_exists_rule ast in
  let spec = ResolveApplicationExpr.resolve spec in
  let spec = ResolveRules.resolve spec in
  let spec = ExtendNames.extend spec in
  let () = Check.CheckRules.check spec in
  let spec = add_default_rule_renders spec in
  spec
