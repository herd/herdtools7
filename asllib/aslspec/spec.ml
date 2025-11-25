open AST
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

module Error = struct
  let spec_error msg = raise (SpecError msg)

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

  let invalid_application_of_expression_to_arguments expr =
    spec_error
    @@ Format.asprintf "Invalid application of expression to arguments in %a"
         PP.pp_expr expr

  let type_subsumption_failure sub super =
    spec_error
    @@ Format.asprintf "Unable to determine that `%a` is subsumed by `%a`"
         PP.pp_type_term sub PP.pp_type_term super

  let non_constant_used_as_constant_set id =
    spec_error
    @@ Format.asprintf
         "%s is used as a constant even though it is not defined as one" id

  let tuple_instantiation_length_failure term components label def_components =
    spec_error
    @@ Format.asprintf
         "The type term `%a` cannot be instantiated since it has %i type terms \
          and `%s` requires %i type terms"
         PP.pp_type_term term (List.length components) label
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

  let missing_output_judgment relation_name =
    spec_error
    @@ Format.sprintf
         "The rule for relation %s must end with an output judgment"
         relation_name

  let bad_layout term layout ~consistent_layout =
    spec_error
    @@ Format.asprintf
         "layout %a is inconsistent with %a. Here's a consistent layout: %a"
         PP.pp_math_shape layout PP.pp_type_term term PP.pp_math_shape
         consistent_layout
end

let rec vars_of_type_term term =
  let listed_vars =
    match term with
    | Label _ -> []
    | TypeOperator { term } -> opt_named_term_to_var_list term
    | LabelledTuple { components } -> vars_of_opt_named_type_terms components
    | LabelledRecord { fields } ->
        Utils.list_concat_map
          (fun { name_and_type = name, field_term; _ } ->
            name :: vars_of_type_term field_term)
          fields
    | ConstantsSet _ -> []
    | Function { from_type; to_type } ->
        opt_named_term_to_var_list from_type
        @ opt_named_term_to_var_list to_type
  in
  List.sort_uniq String.compare listed_vars

(** [opt_named_term_to_var_list (var, t)] returns the list of term-naming
    variables that occur at any depth inside [t], plus [var] if it is [Some x].
*)
and opt_named_term_to_var_list (var, t) =
  Option.to_list var @ vars_of_type_term t

and vars_of_opt_named_type_terms opt_named_terms =
  Utils.list_concat_map opt_named_term_to_var_list opt_named_terms

let variant_to_label_opt { TypeVariant.term } =
  match term with
  | Label label -> Some label
  | LabelledTuple { label_opt } | LabelledRecord { label_opt } -> label_opt
  | _ -> None

type definition_node =
  | Node_Type of Type.t
  | Node_Relation of Relation.t
  | Node_TypeVariant of TypeVariant.t
  | Node_Constant of Constant.t
  | Node_RecordField of record_field

let definition_node_name = function
  | Node_Type { Type.name }
  | Node_Relation { Relation.name }
  | Node_Constant { Constant.name } ->
      name
  | Node_TypeVariant def -> Option.get (variant_to_label_opt def)
  | Node_RecordField { name_and_type = name, _; _ } -> name

let math_macro_opt_for_node = function
  | Node_Type def -> Type.math_macro def
  | Node_Relation def -> Relation.math_macro def
  | Node_TypeVariant def -> TypeVariant.math_macro def
  | Node_Constant def -> Constant.math_macro def
  | Node_RecordField def -> record_field_math_macro def

let prose_description_for_node = function
  | Node_Type def -> Type.prose_description def
  | Node_Relation def -> Relation.prose_description def
  | Node_TypeVariant def -> TypeVariant.prose_description def
  | Node_Constant def -> Constant.prose_description def
  | Node_RecordField def -> record_field_prose_description def

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
  | Node_RecordField { name_and_type = field_name, field_type; _ } ->
      field_name :: vars_of_type_term field_type

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
    make_tuple
      [
        (None, make_labelled_tuple name input);
        (None, make_tuple opt_named_output_terms);
      ]

  let rec for_type_term term =
    match term with
    | Label _ -> Unspecified
    | TypeOperator { term = _, t } -> for_type_term t
    | LabelledTuple { components } ->
        if List.length components > 1 then
          Horizontal (List.map (fun (_, t) -> for_type_term t) components)
        else Unspecified
    | LabelledRecord { fields; _ } ->
        if List.length fields > 1 then
          let layout_per_field =
            List.map
              (fun { name_and_type = _, field_type; _ } ->
                for_type_term field_type)
              fields
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
    | Node_RecordField { name_and_type = _, field_type; _ } ->
        for_type_term field_type
end

let elem_name = function
  | Elem_Type { Type.name }
  | Elem_Relation { Relation.name }
  | Elem_Constant { Constant.name }
  | Elem_RenderTypes { TypesRender.name }
  | Elem_RenderRule { RuleRender.name } ->
      name

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
                | Label _ | LabelledTuple { label_opt = Some _ } ->
                    Node_TypeVariant variant :: acc_nodes
                | LabelledRecord { label_opt; fields } ->
                    let field_nodes =
                      List.map (fun field -> Node_RecordField field) fields
                    in
                    let variant_if_labelled =
                      Option.map (fun _ -> Node_TypeVariant variant) label_opt
                      |> Option.to_list
                    in
                    variant_if_labelled @ field_nodes @ acc_nodes
                | LabelledTuple { label_opt = None }
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
  ast : AST.t;  (** The original AST as parsed *)
  id_to_defining_node : definition_node StringMap.t;
      (** Associates identifiers with the AST nodes where they are defined. *)
  defined_ids : string list;
      (** The list of identifiers defined in the spec in order of appearance. *)
}

let ast spec = spec.ast

(** [update_symbol_table ast] creates a symbol table from [ast]. *)
let make_symbol_table ast =
  let definition_nodes = list_definition_nodes ast in
  make_id_to_definition_node definition_nodes

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

  (** [resolve_application_expr id_to_defining_node expr] resolves application
      expressions appearing in [expr] by using [id_to_defining_node] to lookup
      type variants and relations. *)
  let rec resolve_application_expr id_to_defining_node expr =
    match expr with
    | Var _ | FieldAccess _ | ListIndex _ -> expr
    | Record { label; fields } ->
        let resolved_fields =
          List.map
            (fun (field_name, field_expr) ->
              ( field_name,
                resolve_application_expr id_to_defining_node field_expr ))
            fields
        in
        Record { label; fields = resolved_fields }
    | Application ({ applicator; args } as app) -> (
        let resolved_args =
          List.map (resolve_application_expr id_to_defining_node) args
        in
        match applicator with
        | EmptyApplicator | Relation _ | ExprOperator _ | Fields _ ->
            Application { app with args = resolved_args }
        | TupleLabel _ -> assert false
        (* Tuple labels appear only post resolution so not expected here. *)
        | Unresolved (Var id) -> (
            match StringMap.find_opt id id_to_defining_node with
            | Some (Node_Relation _) ->
                Application { applicator = Relation id; args = resolved_args }
            | Some (Node_TypeVariant _) ->
                Application { applicator = TupleLabel id; args = resolved_args }
            | Some (Node_Constant { Constant.name })
            | Some (Node_Type { Type.name }) ->
                Error.invalid_application_of_symbol_in_expr name expr
            | _ ->
                Application { applicator = Fields [ id ]; args = resolved_args }
            )
        | Unresolved (FieldAccess path) ->
            Application { applicator = Fields path; args = resolved_args }
        | Unresolved _ ->
            Error.invalid_application_of_expression_to_arguments expr)
    | Transition { lhs; rhs; short_circuit } ->
        let resolved_lhs = resolve_application_expr id_to_defining_node lhs in
        let resolved_rhs = resolve_application_expr id_to_defining_node rhs in
        let resolved_short_circuit =
          Option.map
            (List.map (resolve_application_expr id_to_defining_node))
            short_circuit
        in
        Transition
          {
            lhs = resolved_lhs;
            rhs = resolved_rhs;
            short_circuit = resolved_short_circuit;
          }
    | Indexed ({ body : expr } as indexed_expr) ->
        let resolved_body = resolve_application_expr id_to_defining_node body in
        Indexed { indexed_expr with body = resolved_body }
    | NamedExpr (sub_expr, name) ->
        let resolved_sub_expr =
          resolve_application_expr id_to_defining_node sub_expr
        in
        NamedExpr (resolved_sub_expr, name)

  (** [resolve_rule_element id_to_defining_node rule_element] performs
      resolution transformations on [rule_element] using [id_to_defining_node]
      to lookup type variants and relations. *)
  let rec resolve_rule_element id_to_defining_node conclusion_lhs rule_element =
    let open Rule in
    match rule_element with
    | Judgment ({ expr; is_output } as judgment) ->
        let resolved_expr = resolve_application_expr id_to_defining_node expr in
        let resolved_expr =
          if is_output then
            Transition
              {
                lhs = conclusion_lhs;
                rhs = resolved_expr;
                short_circuit = Some [];
                (* Output transitions never have alternatives.
                   This ensures alternatives are not inserted. *)
              }
          else resolved_expr
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

  (** [lhs_of_conclusion] returns an expression representing the LHS of the
      conclusion judgment for relation definition with the given [name] and
      [input] arguments. This function assumes
      [relation_names_arguments_if_exists_rule] has been called on the AST to
      ensure that all input arguments have names. *)
  let lhs_of_conclusion { Relation.name; input } =
    let input_vars =
      List.map (fun (name_opt, _) -> Var (Option.get name_opt)) input
    in
    Application { applicator = Relation name; args = input_vars }

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

  let resolve ast id_to_defining_node =
    let open Rule in
    let ast =
      List.map
        (fun elem ->
          match elem with
          | Elem_Type _ | Elem_Constant _ | Elem_RenderTypes _
          | Elem_RenderRule _ ->
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
    (* Since we generated new relation nodes, we need to re-generate the symbol table. *)
    (ast, make_symbol_table ast)
end

(** A module for extending expressions in output configurations of
    conclusionjudgments with names derived from type terms. *)
module ExtendNames = struct
  open Rule

  (** [opt_extend] Wraps [expr] with a name if [opt_name] is [Some], but avoids
      naming a variable expression with its own name. *)
  let opt_extend expr opt_name =
    match (expr, opt_name) with
    | _, None -> expr
    | Var v, Some name when String.equal v name ->
        expr (* An optimization to avoid naming a variable with its own name. *)
    | _, Some name -> NamedExpr (expr, name)

  (** [applicator_matches_label] Checks whether the applicator is a tuple with
      the matching optional expression label. *)
  let applicator_matches_label applicator expr_label =
    match (applicator, expr_label) with
    | EmptyApplicator, None -> true
    | TupleLabel label1, Some label2 -> String.equal label1 label2
    | _ -> false

  (** [extend_with_names type_term expr ] recursively transforms [expr] by
      adding names from [type_term] to sub-expressions of [expr]. Currently,
      only tuples (labelled or unlabelled) are supported, which is sufficient
      for most output configurations. *)
  let rec extend_with_names type_term expr =
    match (type_term, expr) with
    | LabelledTuple { label_opt = None; components = [ (opt_name, _) ] }, _ ->
        (* An unlabelled tuple with a single component serves as a named reference
           to any type.*)
        opt_extend expr opt_name
    | LabelledTuple { label_opt; components }, Application { applicator; args }
      when applicator_matches_label applicator label_opt ->
        let () =
          if List.compare_lengths components args <> 0 then
            let msg =
              Format.asprintf
                "The expression %a cannot be extended with names from the type \
                 term %a since they have different number of components."
                PP.pp_expr expr PP.pp_type_term type_term
            in
            raise (SpecError msg)
          else ()
        in
        let extended_args =
          List.map2
            (fun arg (opt_name, arg_type) ->
              opt_extend (extend_with_names arg_type arg) opt_name)
            args components
        in
        Application { applicator; args = extended_args }
    | _ -> expr

  (** [extend_rule_element output_type rule_element] extends output judgments in
      [rule_element] with names from [output_type]. *)
  let rec extend_rule_element output_type rule_element =
    match rule_element with
    | Judgment
        ({ expr = Transition { lhs; rhs; short_circuit }; is_output = true } as
         judgment) ->
        let extended_rhs = extend_with_names output_type rhs in
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
  let extend ast =
    let ast =
      List.map
        (fun elem ->
          match elem with
          | ( Elem_Type _ | Elem_Constant _ | Elem_RenderTypes _
            | Elem_RenderRule _
            | Elem_Relation { rule_opt = None } ) as elem ->
              elem
          | Elem_Relation ({ rule_opt = Some elements; output } as def) ->
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
    (* Since we generated new relation nodes, we need to re-generate the symbol table. *)
    (ast, make_symbol_table ast)
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

  (** [concat_expanded_rules prefix suffix] concatenates two expanded rules,
      [prefix] and [suffix], combining their optional names and categories as
      needed. *)
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
    match (term, layout) with
    | Label _, Unspecified -> ()
    | Label _, _ -> Error.bad_layout term layout ~consistent_layout
    | TypeOperator { term = _, t }, _ -> check_layout t layout
    | LabelledTuple { components }, Horizontal cells
    | LabelledTuple { components }, Vertical cells ->
        if List.compare_lengths components cells <> 0 then
          Error.bad_layout term layout ~consistent_layout
        else
          List.iter2
            (fun (_, term) cell -> check_layout term cell)
            components cells
    | LabelledRecord { fields }, Horizontal cells
    | LabelledRecord { fields }, Vertical cells ->
        if List.compare_lengths fields cells <> 0 then
          Error.bad_layout term layout ~consistent_layout
        else
          List.iter2
            (fun { name_and_type = _, term; _ } cell -> check_layout term cell)
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

  (** [check_math_layout definition_nodes] checks that the math layouts for all
      [definition_nodes] are structurally consistent with their type terms. *)
  let check_math_layout definition_nodes =
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
      | Node_RecordField { name_and_type = _, field_type; _ } ->
          check_layout field_type (math_layout_for_node node)
    in
    List.iter check_math_layout_for_definition_node definition_nodes

  (** Returns all the identifiers referencing nodes that define identifiers. *)
  let rec referenced_ids = function
    | Label id -> [ id ]
    | TypeOperator { term = _, t } -> referenced_ids t
    | LabelledTuple { label_opt; components } -> (
        let component_ids =
          List.map snd components |> Utils.list_concat_map referenced_ids
        in
        match label_opt with
        | None -> component_ids
        | Some label -> label :: component_ids)
    | LabelledRecord { label_opt; fields } -> (
        let fields_ids =
          List.map
            (fun { name_and_type = _, field_type; _ } -> field_type)
            fields
          |> Utils.list_concat_map referenced_ids
        in
        match label_opt with
        | None -> fields_ids
        | Some label -> label :: fields_ids)
    | ConstantsSet constant_names -> constant_names
    | Function { from_type = _, from_term; to_type = _, to_term } ->
        referenced_ids from_term @ referenced_ids to_term

  (** [check_no_undefined_ids elements id_to_defining_node] checks that all
      identifiers referenced in [elements] are keys in [id_to_defining_node]. *)
  let check_no_undefined_ids elements id_to_defining_node =
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
      List.iter
        (fun id ->
          if not (StringMap.mem id id_to_defining_node) then
            Error.undefined_reference id (elem_name elem))
        ids_referenced_by_elem
    in
    List.iter (check_no_undefined_ids_in_elem id_to_defining_node) elements

  (** [check_relations_outputs elems id_to_defining_node] checks that, for each
      relation in [elems], the first output type term is arbitrary, and that all
      type terms following it are either type names or sets of constants.
      Furthermore, it checks that all type names used as alternative output type
      terms reference types with the [short_circuit_macro] attribute defined. If
      not, raises a [SpecError] describing the issue. *)
  let check_relations_outputs elems id_to_defining_node =
    let relations_defs =
      List.filter_map
        (function Elem_Relation def -> Some def | _ -> None)
        elems
    in
    let check_alternative_outputs { Relation.name; output } =
      let alternative_outputs = ListLabels.tl output in
      List.iter
        (fun term ->
          match term with
          | Label id -> (
              match StringMap.find id id_to_defining_node with
              | Node_Type typedef -> (
                  match Type.short_circuit_macro typedef with
                  | None -> Error.missing_short_circuit_attribute name term id
                  | Some _ -> ())
              | _ -> Error.not_type_name_error name term)
          | ConstantsSet _ -> ()
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
    val check : definition_node list -> unit
    (** [check definition_nodes] checks all prose templates in
        [definition_nodes] *)
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

    let check defining_nodes =
      List.iter check_prose_template_for_definition_node defining_nodes
  end

  (** A module for checking that all rules are well-formed. That is, that they
      end with an output judgment and that all preceding judgments are
      non-output judgments. *)
  module CheckRules = struct
    let check_well_formed_expanded relation_name expanded_rule =
      let open ExpandRules in
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
      | _ -> Error.missing_output_judgment (Option.get expanded_rule.name_opt)

    let check_rule_for_relation { Relation.name } elements =
      let expanded_rules = ExpandRules.expand elements in
      List.iter (check_well_formed_expanded name) expanded_rules

    (** Checks the rules in all relations. *)
    let check ast =
      let open Rule in
      List.iter
        (fun elem ->
          match elem with
          | Elem_Type _ | Elem_Constant _ | Elem_RenderTypes _
          | Elem_RenderRule _
          | Elem_Relation { rule_opt = None } ->
              ()
          | Elem_Relation ({ rule_opt = Some elements } as def) ->
              check_rule_for_relation def elements)
        ast
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
      it parenthesized. The check needs to ensure that [NV_Literal(L_Int(v: Z))]
      is valid.

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
    val check : definition_node StringMap.t -> elem list -> unit
    (** [check id_to_defining_node elems] conservatively checks that all type
        terms referenced in [elems] that instantiate type terms in the range of
        [id_to_defining_node] are also subsumed by them. The check assumes that
        [check_no_undefined_ids] has already been run. *)
  end = struct
    (** [subsumed sub_op super_op] is true if all values in the domain of
        [TypeOperator { op = sub_op; term }] are also in the domain of
        [TypeOperator { op = super_op; term }]. *)
    let operator_subsumed sub_op super_op =
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

        In the example, [M(B, Num)] is not subsumed by [M(A, Num)]. The
        algorithm checks whether [B] is subsumed by [A], which requires checking
        whether [B] is subsumed by [L] and whether [B] is subsumed by
        [M(A, Num)]. The first check fails, but the second check requires
        expanding [B] to check whether all of its variants are subsumed by
        [M(A, Num)], namely whether [M(B, Num)] is subsumed by [M(A, Num)],
        which is the original subsumption test.

        To avoid infinite recursion, the algorithm tracks which types have
        already been expanded, and do not expand it again when checking
        subsumption for [B]. Thus, [B] is not expanded again, and the
        subsumption test returns [false].
        {[
          typedef;
          typedef A = L | M(A, Num);
          typedef B = ( M(B, Num) );
        ]} *)
    let rec subsumed id_to_defining_node expanded_types sub super =
      (* The domain of an unlabelled singleton tuple is the domain
         of its single type term. *)
      let equiv_singleton_tuple term =
        match term with
        | LabelledTuple
            { label_opt = None; components = [ (_, referenced_term) ] } ->
            referenced_term
        | _ -> term
      in
      (* In the example above [( M(B, Num) )] is equivalent to [M(B, Num)]. *)
      let sub = equiv_singleton_tuple sub in
      let super = equiv_singleton_tuple super in
      match (sub, super) with
      | _, Label super_label ->
          let sub_is_label_case =
            match sub with
            | Label sub_label ->
                String.equal sub_label super_label
                ||
                (* The case where [sub_label] is a type name,
                  like [B] of [M(B, Num)] in the example. *)
                subsumed_typename_term_type id_to_defining_node expanded_types
                  sub_label super
            | _ -> false
          in
          sub_is_label_case
          (* The case where [super_label] is a type name,
              like [A] of [M(A, Num)] in the example.
          *)
          || subsumed_term_type_typename id_to_defining_node expanded_types sub
               super_label
      (* From here on the test operates via structural induction. *)
      | ( TypeOperator { op = sub_op; term = _, sub_term },
          TypeOperator { op = super_op; term = _, super_term } ) ->
          operator_subsumed sub_op super_op
          && subsumed id_to_defining_node expanded_types sub_term super_term
      | ( LabelledTuple
            { label_opt = sub_label_opt; components = sub_components },
          LabelledTuple
            { label_opt = super_label_opt; components = super_components } ) ->
          Option.equal String.equal sub_label_opt super_label_opt
          && List.for_all2
               (fun (_, sub_term) (_, super_term) ->
                 subsumed id_to_defining_node expanded_types sub_term super_term)
               sub_components super_components
      | ( LabelledRecord { label_opt = sub_label_opt; fields = sub_fields },
          LabelledRecord { label_opt = super_label_opt; fields = super_fields }
        ) ->
          Option.equal String.equal sub_label_opt super_label_opt
          && List.for_all2
               (fun { name_and_type = _, sub_term; _ }
                    { name_and_type = _, super_term; _ } ->
                 subsumed id_to_defining_node expanded_types sub_term super_term)
               sub_fields super_fields
      | ( Function { from_type = _, sub_from_term; to_type = _, sub_to_term },
          Function
            { from_type = _, super_from_term; to_type = _, super_to_term } ) ->
          (* Functions can be partial or total, which require different subsumption tests.
             To make this simple, we require equivalence of the from-terms and to-terms,
             which is sufficient for our needs.
          *)
          let equivalence_test term term' =
            subsumed id_to_defining_node expanded_types term term'
            && subsumed id_to_defining_node expanded_types term' term
          in
          equivalence_test sub_from_term super_from_term
          && equivalence_test sub_to_term super_to_term
      | ConstantsSet sub_names, ConstantsSet super_names ->
          List.for_all (fun name -> List.mem name super_names) sub_names
      | _ ->
          (* false is safely conservative. *)
          false

    (** [subsumed_term_type_typename id_to_defining_node sub_term typename]
        checks if [sub_term] is subsumed by any of the type variants defined by
        [typename] in [id_to_defining_node]. If [typename] is not a type with
        type variants, returns false. *)
    and subsumed_term_type_typename id_to_defining_node expanded_types sub_term
        typename =
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
                   subsumed id_to_defining_node expanded_types sub_term
                     super_term)
                 variants
        | _ -> false

    (** [subsumed_typename_term_type id_to_defining_node typename super_term]
        checks if all type variants defined by [typename] are subsumed by
        [super_term]. If [typename] is not a type with type variants, returns
        false. *)
    and subsumed_typename_term_type id_to_defining_node expanded_types typename
        super_term =
      if StringSet.mem typename expanded_types then false
      else
        (* Prevent infinite recursion on recursive types by expanding a type name at most once. *)
        let expanded_types = StringSet.add typename expanded_types in
        match StringMap.find_opt typename id_to_defining_node with
        | Some (Node_Type { Type.variants; _ }) ->
            (not (Utils.list_is_empty variants))
            && List.for_all
                 (fun { TypeVariant.term = sub_term } ->
                   subsumed id_to_defining_node expanded_types sub_term
                     super_term)
                 variants
        | _ -> false

    (** [check_subsumed_terms_lists id_to_defining_node term label sub_terms
         super_terms] checks that each term in [sub_terms] is subsumed by the
        corresponding term in [super_terms]. Both lists of terms comprise the
        labelled tuple/labelled record [term] with label [label]. If the lists
        have different lengths, or if any term in [sub_terms] is not subsumed by
        the corresponding term in [super_terms], a [SpecError] is raised. *)
    let check_subsumed_terms_lists id_to_defining_node sub_terms super_terms =
      let check_subsumed id_to_defining_node sub super =
        if subsumed id_to_defining_node StringSet.empty sub super then ()
        else Error.type_subsumption_failure sub super
      in
      List.iter2 (check_subsumed id_to_defining_node) sub_terms super_terms

    (** [is_constant id_to_defining_node id] checks if [id] is either defined as
        a constant directly or as a type variant with a label.

        For example:
        {[
          typedef A = L;
          constant C
        ]}
        defines the constant [L] and the constant [C]. *)
    let check_is_constant id_to_defining_node id =
      match StringMap.find_opt id id_to_defining_node with
      | Some (Node_Constant _) | Some (Node_TypeVariant { term = Label _ }) ->
          ()
      | _ -> Error.non_constant_used_as_constant_set id

    (** [check_well_typed id_to_defining_node term] checks that every type
        referenced by [term] correctly instantiates its defining type with
        respect to the type definitions in the range of [id_to_defining_node].
        The check assumes that both [check_no_undefined_ids] and that
        [check_well_formed] have already been run.

        The check operates by structural induction on [term], except when a
        label term references another type in which case the type variants of
        the referenced type are considered. *)
    let rec check_well_instantiated id_to_defining_node term =
      match term with
      | TypeOperator { term = _, operator_term } ->
          check_well_instantiated id_to_defining_node operator_term
      | LabelledTuple { label_opt; components } -> (
          let terms = List.map snd components in
          let () =
            List.iter (check_well_instantiated id_to_defining_node) terms
          in
          match label_opt with
          | None -> ()
          | Some label -> (
              let variant_def = StringMap.find label id_to_defining_node in
              match variant_def with
              | Node_TypeVariant
                  {
                    TypeVariant.term =
                      LabelledTuple { components = def_opt_named_components };
                  } ->
                  let def_terms = List.map snd def_opt_named_components in
                  check_subsumed_terms_lists id_to_defining_node terms def_terms
              | _ -> assert false))
      | LabelledRecord { label_opt; fields } -> (
          let terms =
            List.map (fun { name_and_type = _, term; _ } -> term) fields
          in
          let () =
            List.iter (check_well_instantiated id_to_defining_node) terms
          in
          match label_opt with
          | None -> ()
          | Some label -> (
              let variant_def = StringMap.find label id_to_defining_node in
              match variant_def with
              | Node_TypeVariant
                  {
                    TypeVariant.term = LabelledRecord { fields = def_fields; _ };
                  } ->
                  let def_terms = List.map field_type def_fields in
                  check_subsumed_terms_lists id_to_defining_node terms def_terms
              | _ -> assert false))
      | Function { from_type = _, from_term; to_type = _, to_term } ->
          check_well_instantiated id_to_defining_node from_term;
          check_well_instantiated id_to_defining_node to_term
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
      | LabelledTuple { label_opt; components } -> (
          let terms = List.map snd components in
          let () = List.iter (check_well_formed id_to_defining_node) terms in
          match label_opt with
          | None -> ()
          | Some label -> (
              let variant_def = StringMap.find label id_to_defining_node in
              match variant_def with
              | Node_TypeVariant
                  {
                    TypeVariant.term =
                      LabelledTuple { components = def_components };
                  } ->
                  if List.compare_lengths components def_components <> 0 then
                    Error.tuple_instantiation_length_failure term components
                      label def_components
                  else ()
              | _ ->
                  Error.tuple_instantiation_failure_not_labelled_tuple term
                    label))
      | LabelledRecord { label_opt; fields } -> (
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
                      LabelledRecord { fields = def_fields; _ } as def_term;
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

    let check_well_typed id_to_defining_node term =
      check_well_formed id_to_defining_node term;
      (* Not for check whether [term] instantiates a type
        and if so, check that it is subsumed by it.
      *)
      check_well_instantiated id_to_defining_node term

    let check id_to_defining_node elems =
      List.iter
        (fun elem ->
          try
            match elem with
            | Elem_Constant _ | Elem_RenderTypes _ | Elem_RenderRule _ -> ()
            | Elem_Relation { input; output } ->
                List.iter
                  (fun (_, term) -> check_well_typed id_to_defining_node term)
                  input;
                List.iter (check_well_typed id_to_defining_node) output
            | Elem_Type { Type.variants; _ } ->
                List.iter
                  (fun { TypeVariant.term } ->
                    match term with
                    | Label _ ->
                        () (* A constant label definition is well-formed. *)
                    | _ -> check_well_typed id_to_defining_node term)
                  variants
          with SpecError e ->
            stack_spec_error e
              (Format.asprintf "While checking: %s" (elem_name elem)))
        elems
  end

  (** [relation_names_arguments_if_exists_rule ast] checks that for each
      relation in [ast] that has a rule, all its input arguments have names. *)
  let relation_names_arguments_if_exists_rule ast =
    let open Rule in
    let check_relation { Relation.name; input; rule_opt } =
      match rule_opt with
      | None -> ()
      | Some _ ->
          List.iter
            (fun (opt_name, _) ->
              if Option.is_none opt_name then
                Error.missing_relation_argument_name name)
            input
    in
    List.iter (function Elem_Relation def -> check_relation def | _ -> ()) ast
end

let from_ast ast =
  let definition_nodes = list_definition_nodes ast in
  let defined_ids = List.map definition_node_name definition_nodes in
  let id_to_defining_node = make_id_to_definition_node definition_nodes in
  let () = Check.check_no_undefined_ids ast id_to_defining_node in
  let () = Check.check_relations_outputs ast id_to_defining_node in
  let () = Check.CheckTypeInstantiations.check id_to_defining_node ast in
  let () = Check.check_math_layout definition_nodes in
  let () = Check.CheckProseTemplates.check definition_nodes in
  let () = Check.relation_names_arguments_if_exists_rule ast in
  let ast, _ = ResolveRules.resolve ast id_to_defining_node in
  let ast, id_to_defining_node = ExtendNames.extend ast in
  let () = Check.CheckRules.check ast in
  { ast; id_to_defining_node; defined_ids }

let defined_ids self = self.defined_ids

let defining_node_opt_for_id self id =
  StringMap.find_opt id self.id_to_defining_node

let defining_node_for_id self id =
  match defining_node_opt_for_id self id with
  | Some def -> def
  | None -> Error.undefined_element id

let relation_for_id self id =
  match defining_node_for_id self id with
  | Node_Relation def -> def
  | _ -> assert false

let is_defined_id self id = StringMap.mem id self.id_to_defining_node
