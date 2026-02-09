open AST
open ASTUtils
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type type_env = Term.t StringMap.t
(** A type environment mapping variable names to their inferred types. *)

type term_at_case = string * Term.t
(** A term associated with a given case element. *)

type type_case_table = term_at_case list StringMap.t

(* A variable for discarding of values. *)
let ignore_var = "_"
let is_ignore_var id = String.equal id ignore_var

(** The identifier that must be used to mark bound variables in the spec
    definition of the quantifying operator. *)
let bound_variable = "bound_variable"

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

(** [is_type_name id_to_defining_node id] is true if and only if [id] is defined
    as a top-level type. *)
let is_type_name id_to_defining_node id =
  match StringMap.find_opt id id_to_defining_node with
  | Some (Node_Type _) -> true
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

let make_variant_id_to_containing_type ast =
  let typedefs =
    List.filter_map (function Elem_Type def -> Some def | _ -> None) ast
  in
  List.fold_left
    (fun acc_map { Type.name; variants } ->
      List.fold_left
        (fun acc_map { TypeVariant.term } ->
          match term with
          | Term.Label label
          | Term.Tuple { label_opt = Some label }
          | Term.Record { label_opt = Some label }
          (* Only labels, labelled tuples, and labelled records have
             an id that uniquely identifies them.*)
            ->
              StringMap.add label name acc_map
          | _ -> acc_map)
        acc_map variants)
    StringMap.empty typedefs

type t = {
  ast : AST.t;  (** The AST, added with builtin definitions, transformed. *)
  id_to_defining_node : definition_node StringMap.t;
      (** Associates identifiers with the AST nodes where they are defined. *)
  variant_id_to_containing_type : string StringMap.t;
      (** Associates variant labels with the name of the type that contains
          them. *)
  assign : Relation.t;
  reverse_assign : Relation.t;
  bottom_constant : Constant.t;
  bottom_term : Term.t;
  none_constant : Constant.t;
  empty_set : Constant.t;
  empty_list : Constant.t;
  bool : Type.t;
  n_type : Type.t;
  z_type : Type.t;
  some_operator : Relation.t;
  cond_operator : Relation.t;
}

type spec_type = t

let is_builtin_type id (t : Type.t) = String.equal id t.name
let is_builtin_constant id (c : Constant.t) = String.equal id c.name
let is_builtin_relation id (r : Relation.t) = String.equal id r.name

let is_cond_operator_name (spec : t) id =
  is_builtin_relation id spec.cond_operator

(** [make_symbol_table ast] creates a symbol table from [ast]. *)
let make_symbol_table ast =
  let definition_nodes = list_definition_nodes ast in
  make_id_to_definition_node definition_nodes

(** [update_spec_ast spec ast] updates the specification [spec] with a new AST
    [ast], returning a new specification. *)
let update_spec_ast spec ast =
  (* With new AST elements, we need to re-generate the symbol table. *)
  {
    spec with
    ast;
    id_to_defining_node = make_symbol_table ast;
    variant_id_to_containing_type = make_variant_id_to_containing_type ast;
  }

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

(** [is_quantifying_operator spec op_name] tests whether [op_name] is a
    quantifying operator (like "forall" or "exists") based on whether its first
    argument is named as [bound_variable]. *)
let is_quantifying_operator spec op_name =
  let operator = relation_for_id spec op_name in
  match operator.input with
  | (Some arg_name, _) :: _ -> String.equal arg_name bound_variable
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
    | RecordUpdate { record_expr; updates } ->
        let resolved_record_expr = resolve_in_context record_expr in
        let resolved_updates =
          List.map
            (fun (field_name, field_expr) ->
              (field_name, resolve_in_context field_expr))
            updates
        in
        RecordUpdate
          { record_expr = resolved_record_expr; updates = resolved_updates }
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
    (* Converts an optionally-named type term into a relation argument expression. *)
    let rec arg_of opt_named_term =
      match opt_named_term with
      | Some name, _ -> Expr.Var name
      | None, Term.Tuple { label_opt; args } ->
          let args = List.map arg_of args in
          Expr.make_opt_labelled_tuple label_opt args
      | ( None,
          ( Term.Label _ | Term.Record _ | Term.TypeOperator _ | Term.Function _
          | Term.ConstantsSet _ ) ) ->
          let msg =
            Format.asprintf "Unexpected un-named argument term: %a"
              PP.pp_type_term (snd opt_named_term)
          in
          failwith msg
    in
    let named_args = List.map arg_of input in
    Expr.Relation { name; is_operator; args = named_args }

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

  (** [rule_exprs expanded_rule] retrieves from [expanded_rule] the expressions
      making the premises and the output expression of the conclusion judgment.
  *)
  let rule_exprs expanded_rule =
    let premises, conclusion = Utils.split_last expanded_rule.judgments in
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
    (premises_exprs, output_expr)

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

let filter_rule_for_path { Relation.name; rule_opt } path_str =
  let open Rule in
  let path = Str.split (Str.regexp_string ".") path_str in
  let rec filter_rule_elements rule_elements path =
    match (rule_elements, path) with
    | _, [] -> rule_elements
    | [], _ ->
        let msg =
          Format.asprintf
            "Case '%s' does not exist in rule for relation '%s' for path '%s'."
            path_str name path_str
        in
        failwith msg
    | Rule.Judgment judgment :: rest, _ ->
        Rule.Judgment judgment :: filter_rule_elements rest path
    | Rule.Cases cases :: rest, case_name :: path_tail -> (
        match
          List.find_opt (fun { name } -> String.equal name case_name) cases
        with
        | Some { elements = case_elements; _ } ->
            let filtered_case_elements =
              filter_rule_elements case_elements path_tail
            in
            filtered_case_elements @ filter_rule_elements rest []
        | None ->
            let msg =
              Format.asprintf
                "Case name '%s' not found in rule for relation '%s' for path \
                 '%s'."
                case_name name path_str
            in
            failwith msg)
  in
  filter_rule_elements (Option.get rule_opt) path

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

    val subsumed : t -> Term.t -> Term.t -> bool
    (** [subsumed id_to_defining_node sub super] conservatively tests whether
        all values in the domain of [sub] are also in the domain of [super].
        [id_to_definition_node] is used to look up types by their names. *)

    val operator_subsumed : Term.type_operator -> Term.type_operator -> bool
    (** [operator_subsumed sub_op super_op] is true if any [term], all values in
        the domain of [TypeOperator { op = sub_op; term }] are also in the
        domain of [TypeOperator { op = super_op; term }]. *)

    val reduce_term : t -> Term.t -> Term.t
    (** [reduce_term id_to_defining_node term] reduces [term] by applying two
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
              reduce_term spec variant_term
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
          reduce_term spec referenced_term
      | _ -> term

    and reduce_term spec term =
      reduce_type_reference spec term |> reduce_single_variant_type spec

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
      let sub = reduce_term spec sub in
      let super = reduce_term spec super in
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
            (* The empty list is a subset of every list, since
               empty_list is universally quantified over all element types.
            *)
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
        | Label sub_label, ConstantsSet super_names ->
            List.exists (String.equal sub_label) super_names
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
            operator_subsumed sub_op super_op
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
            let sub_fields = ASTUtils.sort_record_fields sub_fields in
            let super_fields = ASTUtils.sort_record_fields super_fields in
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

    let subsumed id_to_defining_node sub super =
      subsumed_rec id_to_defining_node StringSet.empty sub super

    (** [check_subsumed_terms_lists id_to_defining_node term label sub_terms
         super_terms] checks that each term in [sub_terms] is subsumed by the
        corresponding term in [super_terms]. Both lists of terms comprise the
        labelled tuple/labelled record [term] with label [label]. If the lists
        have different lengths, or if any term in [sub_terms] is not subsumed by
        the corresponding term in [super_terms], a [SpecError] is raised. *)
    let check_subsumed_terms_lists id_to_defining_node sub_terms super_terms =
      let check_subsumed id_to_defining_node sub super =
        if subsumed id_to_defining_node sub super then ()
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
          let variant_def = StringMap.find label id_to_defining_node in
          match variant_def with
          | Node_Type _ | Node_TypeVariant { TypeVariant.term = Label _ } -> ()
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

  (** A module for checking variable usage in rulea. The functions in this
      module assume that a rule exists and that
      [Check.relation_named_arguments_if_exists_rule] has been run. *)
  module UseDef = struct
    open Expr

    (** [vars_of_identifiers id_to_defining_node identifiers] returns the
        sublist of [identifiers] that represent variables. The map
        [id_to_defining_node] is used to filter out constants and labels. *)
    let vars_of_identifiers id_to_defining_node identifiers =
      (* Filters out identifiers that are definitely not variables. *)
      let is_non_var id =
        match StringMap.find_opt id id_to_defining_node with
        | Some (Node_Constant _) | Some (Node_TypeVariant _) ->
            (* Constants and variant labels are not variables. *)
            true
        | Some (Node_Type _)
        | Some (Node_RecordField _)
        | Some (Node_Relation _)
        (* Type names, field names, and relation names can be used as variables. *)
        | None ->
            false
      in
      identifiers
      |> List.filter (fun id -> not (is_ignore_var id || is_non_var id))

    type use_def = {
      used : StringSet.t;
      defined : StringSet.t;
      list_vars : StringSet.t;
          (** lists can be updated at various indices so we need to keep track
              of them to avoid false positives. *)
    }
    (** An environment data type for tracking used and defined variables. *)

    let empty_use_def =
      {
        used = StringSet.empty;
        defined = StringSet.empty;
        list_vars = StringSet.empty;
      }

    (** Pretty-prints a [use_def] environment for debugging. *)
    let pp_use_def fmt { used; defined; list_vars } =
      Format.fprintf fmt "{ used: [%s]; defined: [%s]; list_vars: [%s]}"
        (String.concat ", " (StringSet.elements used))
        (String.concat ", " (StringSet.elements defined))
        (String.concat ", " (StringSet.elements list_vars))

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
      let redefined_vars =
        StringSet.diff
          (StringSet.inter var_set use_def.defined)
          (* Ignore list variables. *)
          use_def.list_vars
      in
      let () =
        if not (StringSet.is_empty redefined_vars) then
          Error.redefined_variable_in_rule ~context_expr
            (StringSet.choose redefined_vars)
      in
      let new_defined =
        StringSet.union use_def.defined (StringSet.of_list vars)
      in
      { use_def with defined = new_defined }

    (** [add_as_list_vars use_def vars] adds all variables in [vars] to the list
        variable set of [use_def]. *)
    let add_as_list_vars use_def vars =
      List.fold_left
        (fun acc_use_def id ->
          { acc_use_def with list_vars = StringSet.add id use_def.list_vars })
        use_def vars

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
      let () =
        if false then
          Format.eprintf "Updating use-def for expr: %a with mode: %s@."
            PP.pp_expr expr
            (match mode with Use -> "Use" | Def -> "Def")
      in
      let use_def =
        match expr with
        | Var id ->
            vars_of_identifiers spec.id_to_defining_node [ id ]
            |> check_and_add_for_expr mode use_def
        | FieldAccess { base } -> (
            match mode with
            | Use -> update_use_def_for_expr Use spec use_def base
            | Def ->
                failwith
                  "A field access is not expected to be used as a \
                   variable-defining expression")
        | ListIndex { list_var; index } ->
            let list_vars =
              vars_of_identifiers spec.id_to_defining_node [ list_var ]
            in
            (* We have to first add the list variable to properly qualify it as a list variable and
               only then add it as used/defined to avoid
               the possibility of having it flagged as a re-defined variable. *)
            let use_def = add_as_list_vars use_def list_vars in
            let use_def = list_vars |> check_and_add_for_expr mode use_def in
            (* The index sub-expression never defines variables. *)
            update_use_def_for_expr Use spec use_def index
        | Record { fields } ->
            let initializing_exprs = List.map snd fields in
            update_use_def_for_expr_list mode spec use_def initializing_exprs
        | RecordUpdate { record_expr; updates } ->
            let () =
              match mode with
              | Def -> Error.record_update_expression_not_assignable expr
              | Use -> ()
            in
            let use_def =
              update_use_def_for_expr Use spec use_def record_expr
            in
            let update_exprs = List.map snd updates in
            update_use_def_for_expr_list Use spec use_def update_exprs
        | NamedExpr (sub_expr, _) ->
            update_use_def_for_expr mode spec use_def sub_expr
        | Tuple { args } -> update_use_def_for_expr_list mode spec use_def args
        | Map { lhs; args } ->
            update_use_def_for_expr_list mode spec use_def (lhs :: args)
        | Relation { is_operator = true; name; args = [ lhs; rhs ] }
          when is_builtin_relation name spec.assign ->
            let use_def = update_use_def_for_expr Use spec use_def rhs in
            update_use_def_for_expr Def spec use_def lhs
        | Relation { is_operator = true; name; args = [ lhs; rhs ] }
          when is_builtin_relation name spec.reverse_assign ->
            let use_def = update_use_def_for_expr Use spec use_def lhs in
            update_use_def_for_expr Def spec use_def rhs
        | Relation
            { is_operator = true; name; args = Var bound_var :: tail_args }
          when is_quantifying_operator spec name ->
            update_use_def_with_bound_variable mode spec use_def tail_args
              ~context_expr:expr ~bound_var
        | Relation { args } ->
            update_use_def_for_expr_list mode spec use_def args
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
      in
      let () =
        if false then Format.eprintf "Updated use-def: %a@." pp_use_def use_def
      in
      use_def

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
      let premise_exprs, output_expr = ExpandRules.rule_exprs expanded_rule in
      let rule_exprs = premise_exprs @ [ output_expr ] in
      (* The input variables are implicitly defined at the start of the rule. *)
      let defined_args =
        vars_of_opt_named_type_terms relation.Relation.input
        |> StringSet.of_list
      in
      let use_def =
        {
          used = StringSet.empty;
          defined = defined_args;
          list_vars = StringSet.empty;
        }
      in
      let () =
        if false then
          Format.eprintf "@.--- Checking use-def for relation %s case %s ---@."
            relation.Relation.name
            (Option.value ~default:"top level"
               expanded_rule.ExpandRules.name_opt)
      in
      let use_def = update_use_def_for_expr_list Use spec use_def rule_exprs in
      let () =
        if false then Format.eprintf "After premises: %a@." pp_use_def use_def
      in
      ()
  end

  (** A module for inferring and checking the types of expression for a given
      rule. The functions in this module assume that use-def correctness was
      already checked. *)
  module TypeInference : sig
    val check_and_infer :
      Relation.t -> spec_type -> ExpandRules.expanded_rule -> type_env
    (** [check_and_infer relation spec expanded_rule] checks that all
        expressions in [expanded_rule] are type-correct according to [spec],
        using [relation] for error messages, and returns the inferred type
        environment.

        @raise [SpecError] if a type error is found. *)

    val infer : spec_type -> Expr.t -> Term.t
    (** [infer spec expr] infers the type of [expr] according to [spec].

        @raise [SpecError] if the type of [expr] cannot be inferred. *)

    val infer_for_relation : spec_type -> Relation.t -> type_case_table
  end = struct
    open Expr
    open UseDef

    (** [type_term_for_typedef def] returns the type term corresponding to the
        given type definition. *)
    let type_term_for_typedef def = Term.Label def.Type.name

    (** [type_of_id spec type_env id] returns the type of [id] using [spec] to
        lookup the defining node for [id] if needed, and using [type_env] as a
        fallback. This function assumes that the type of [id] has already been
        inferred if it is not directly available from its defining node.

        Notice that [id] must not be a record field, since it cannot be as a
        free variable, and must be used only within a [FieldAccess] expression.

        This function assumes that ExtendConstantsWithTypes.extend has been run.
    *)
    let type_of_id ~context_expr spec type_env id =
      match defining_node_opt_for_id spec id with
      | Some (Node_TypeVariant { TypeVariant.term }) -> term
      | Some (Node_Constant { Constant.opt_type = Some type_term }) -> type_term
      | Some (Node_Constant { Constant.opt_type = None }) ->
          Error.missing_type_for_constant id
      | Some (Node_Relation _)
      | Some (Node_RecordField _)
      | Some (Node_Type _)
      (* A variable that happens to share a name with a relation, a record field, and a type. *)
      | None -> (
          match StringMap.find_opt id type_env with
          | Some id_type -> id_type
          | None ->
              let msg =
                Format.asprintf "Encountered an untyped variable '%s' in %a." id
                  PP.pp_expr context_expr
              in
              failwith msg)

    (** [type_term_for_field spec id] returns the type term associated with the
        record field with identifier [id]. *)
    let type_term_for_field spec id =
      match defining_node_opt_for_id spec id with
      | Some (Node_RecordField { term }) -> term
      | _ -> failwith "Expected a record field definition node."

    (** Pretty-prints a type environment for debugging. *)
    let pp_type_env fmt type_env =
      let var_to_type = StringMap.bindings type_env in
      let pp_var_to_type fmt (var, type_term) =
        Format.fprintf fmt "%s: %a" var PP.pp_type_term type_term
      in
      Format.fprintf fmt "{ %a }"
        (PP.pp_sep_list ~sep:", " pp_var_to_type)
        var_to_type

    (** [unify_terms] attempts to find the most precise type that subsumes both
        [term1] and [term2]. A type term [A] is more precise than a type term
        [B] if [B] subsumes [A] and [A] does not subsume [B]. *)
    let rec unify_terms spec term1 term2 =
      let () =
        if false then
          Format.eprintf "Unifying terms: %a and %a.@." PP.pp_type_term term1
            PP.pp_type_term term2
      in
      let open Term in
      let unified_term =
        if CheckTypeInstantiations.subsumed spec term1 term2 then Some term2
        else if CheckTypeInstantiations.subsumed spec term2 term1 then
          Some term1
        else
          (* First, attempt to match terms that have (the same) internal structure. *)
          let structural_unification_result_opt =
            unify_structural_terms spec term1 term2
          in
          let get_variant_label = function
            | Label id when StringMap.mem id spec.variant_id_to_containing_type
              ->
                Some id
            | Tuple { label_opt = Some id; _ }
            | Record { label_opt = Some id; _ } ->
                Some id
            | _ -> None
          in
          match structural_unification_result_opt with
          | Some unified_term -> Some unified_term
          | None -> (
              match (get_variant_label term1, get_variant_label term2) with
              | Some label1, Some label2 ->
                  let container1 =
                    StringMap.find label1 spec.variant_id_to_containing_type
                  in
                  let container2 =
                    StringMap.find label2 spec.variant_id_to_containing_type
                  in
                  if String.equal container1 container2 then
                    Some (Label container1)
                  else None
              | _ -> None)
      in
      let () =
        if false then
          match unified_term with
          | Some term ->
              Format.eprintf "Unified term: %a.@." PP.pp_type_term term
          | None -> Format.eprintf "Failed to unify terms.@."
      in
      unified_term

    (** [unify_structural_terms] attempts to unify two type terms that have the
        same internal structure. If successful, returns the unified term.
        Otherwise, returns [None]. *)
    and unify_structural_terms spec term1 term2 =
      if term1 == term2 then Some term1
      else
        let open Term in
        match (term1, term2) with
        | Label id1, Label id2 when String.equal id1 id2 ->
            (* Here, we assume that the labels are not type names. *)
            Some term1
        | ( TypeOperator { op = op1; term = _, arg_term1 },
            TypeOperator { op = op2; term = _, arg_term2 } )
          when type_operator_equal op1 op2 -> (
            match unify_terms spec arg_term1 arg_term2 with
            | Some unified_arg_term ->
                Some
                  (TypeOperator { op = op1; term = (None, unified_arg_term) })
            | None -> None)
        | ( Tuple { label_opt = label1_opt; args = args1 },
            Tuple { label_opt = label2_opt; args = args2 } )
          when Option.equal String.equal label1_opt label2_opt
               && List.compare_lengths args1 args2 = 0 -> (
            match unify_term_lists spec args1 args2 with
            | Some unified_args ->
                Some
                  (Tuple
                     {
                       label_opt = label1_opt;
                       args =
                         List.map2
                           (fun (name, _) unified_term -> (name, unified_term))
                           args1 unified_args;
                     })
            | None -> None)
        | ( Record { label_opt = label1_opt; fields = fields1 },
            Record { label_opt = label2_opt; fields = fields2 } )
          when Option.equal String.equal label1_opt label2_opt
               && List.compare_lengths fields1 fields2 = 0 -> (
            let fields1 = ASTUtils.sort_record_fields fields1 in
            let fields2 = ASTUtils.sort_record_fields fields2 in
            let field_terms1 = List.map (fun { term; _ } -> term) fields1 in
            let field_terms2 = List.map (fun { term; _ } -> term) fields2 in
            let unified_field_terms =
              unify_term_lists spec
                (List.map (fun term -> (None, term)) field_terms1)
                (List.map (fun term -> (None, term)) field_terms2)
            in
            match unified_field_terms with
            | Some unified_field_terms ->
                Some
                  (Record
                     {
                       label_opt = label1_opt;
                       fields =
                         List.map2
                           (fun field unified_term ->
                             { field with term = unified_term })
                           fields1 unified_field_terms;
                     })
            | None -> None)
        | _ ->
            (* ConstantsSet and Function terms are not supported. *)
            None

    and unify_term_lists spec args1 args2 =
      let unified_args_opt =
        List.map2
          (fun (_, term1) (_, term2) -> unify_terms spec term1 term2)
          args1 args2
      in
      Utils.list_get_all_option unified_args_opt

    module InstantiateOperator : sig
      val instantiate_operator_types_from_inferred_types :
        spec_type ->
        string ->
        int ->
        use_def_mode * Term.t list ->
        context_expr:Expr.t ->
        Term.t list * Term.t
      (** [instantiate_operator_types_from_inferred_types spec relation_name
           num_actual_args (mode, inferred_types) ~context_expr] instantiates
          the types of the arguments and output of the operator [relation_name]
          given the number of actual arguments [num_actual_args] and the
          [inferred_types] for either its arguments or its output depending on
          [mode]. The [context_expr] is used for error reporting. When mode is
          [Use], the [inferred_types] correspond to the argument types. When
          mode is [Def], the [inferred_types] correspond to the output type. By
          instantiating we mean substituting type parameters in the operator
          definition with concrete types inferred for a given operator
          invocation expression. *)
    end = struct
      (** [unify_parameter_type spec ~relation_name parameter_name
           parameter_type type_env] attempts to unify [parameter_type] with any
          existing type for [parameter_name] in [type_env]. If unification is
          successful, returns an updated [type_env] with the unified type.
          @raise [SpecError]
            if [parameter_type] cannot be unified with an existing type for
            [parameter_name]. *)
      let unify_parameter_type spec ~relation_name parameter_name parameter_type
          type_env =
        StringMap.update parameter_name
          (function
            | None -> Some parameter_type
            | Some existing_type -> (
                match unify_terms spec parameter_type existing_type with
                | Some unified_type -> Some unified_type
                | None ->
                    Error.parameter_type_unification_failure ~relation_name
                      parameter_name existing_type parameter_type))
          type_env

      (** [infer_parameter_type parameters formal_type arg_type type_env] infers
          the types of parameters in [parameters] given [formal_type] and the
          corresponding [arg_type], updating the [type_env] accordingly.

          This is achieved by recursing into [formal_type] and [arg_type] in
          lockstep, and when a sub-term of [formal_type] is a parameter, mapping
          it to the corresponding concrete sub-term of [arg_type]. Since the
          same parameter may appear multiple times in [formal_type], unification
          is used to find a single term consistent with all concrete terms
          associated with the parameter. *)
      let rec infer_parameter_type spec ~relation_name parameters formal_type
          arg_type type_env =
        let is_parameter id = List.exists (String.equal id) parameters in
        let open CheckTypeInstantiations in
        let formal_type = reduce_term spec formal_type in
        let arg_type = reduce_term spec arg_type in
        let open Term in
        match (formal_type, arg_type) with
        | Label formal_id, _ ->
            if is_parameter formal_id then
              unify_parameter_type spec ~relation_name formal_id arg_type
                type_env
            else type_env
        | Tuple { args = formal_args }, Tuple { args = actual_args } ->
            let () =
              if not (List.compare_lengths formal_args actual_args = 0) then
                Error.type_instantiation_length_failure formal_type arg_type
                  ~expected_length:(List.length formal_args)
                  ~actual_length:(List.length actual_args)
            in
            List.fold_left2
              (fun curr_type_env (_, formal_term) (_, arg_term) ->
                infer_parameter_type spec ~relation_name parameters formal_term
                  arg_term curr_type_env)
              type_env formal_args actual_args
        | Record { fields = formal_fields }, Record { fields = arg_fields } ->
            let formal_field_names =
              List.map field_name formal_fields |> List.sort String.compare
            in
            let arg_field_names =
              List.map field_name arg_fields |> List.sort String.compare
            in
            let () =
              if
                not
                  (Utils.list_is_equal String.equal formal_field_names
                     arg_field_names)
              then
                Error.type_instantiation_length_failure formal_type arg_type
                  ~expected_length:(List.length formal_fields)
                  ~actual_length:(List.length arg_fields)
            in
            type_env
        | ( Function
              { from_type = _, formal_from_term; to_type = _, formal_to_term },
            Function { from_type = _, arg_from_term; to_type = _, arg_to_term }
          ) ->
            let type_env =
              infer_parameter_type spec ~relation_name parameters
                formal_from_term arg_from_term type_env
            in
            infer_parameter_type spec ~relation_name parameters formal_to_term
              arg_to_term type_env
        | ( TypeOperator { op = formal_op; term = _, formal_inner_term },
            TypeOperator { op = arg_op; term = _, arg_inner_term } ) ->
            let () =
              if
                not (CheckTypeInstantiations.operator_subsumed arg_op formal_op)
              then
                Error.type_operator_instantiation_failure ~relation_name
                  formal_type arg_type
            in
            infer_parameter_type spec ~relation_name parameters
              formal_inner_term arg_inner_term type_env
        | _ -> type_env

      (** [substitute_type_parameters term parameter_env] substitutes type
          parameters appearing as sub-terms in [term] by their corresponding
          terms in [parameter_env]. *)
      let rec substitute_type_parameters term parameter_env =
        let open Term in
        match term with
        | Label id -> (
            match StringMap.find_opt id parameter_env with
            | Some substituted_type -> substituted_type
            | None -> term)
        | Tuple { label_opt; args } ->
            let substituted_args =
              List.map
                (fun (name, sub_term) ->
                  (name, substitute_type_parameters sub_term parameter_env))
                args
            in
            Tuple { label_opt; args = substituted_args }
        | Record { label_opt; fields } ->
            let substituted_fields =
              List.map
                (fun ({ term = field_term } as field) ->
                  {
                    field with
                    term = substitute_type_parameters field_term parameter_env;
                  })
                fields
            in
            Record { label_opt; fields = substituted_fields }
        | Function
            {
              from_type = from_name, from_term;
              to_type = to_name, to_term;
              total;
            } ->
            let substituted_from_term =
              substitute_type_parameters from_term parameter_env
            in
            let substituted_to_term =
              substitute_type_parameters to_term parameter_env
            in
            Function
              {
                from_type = (from_name, substituted_from_term);
                to_type = (to_name, substituted_to_term);
                total;
              }
        | TypeOperator { op; term = term_name, inner_term } ->
            let substituted_inner_term =
              substitute_type_parameters inner_term parameter_env
            in
            TypeOperator { op; term = (term_name, substituted_inner_term) }
        | ConstantsSet _ -> term

      (** [make_operator_formals_for_actual_num_of_args spec operator_name
           actual_num_of_args] returns the list of formal argument types for the
          operator [operator_name] given that it is being invoked with
          [actual_num_of_args] arguments. If the operator is not variadic, it is
          just the list of its defined formal argument types. If the operator is
          variadic, the list contains [actual_num_of_args] copies of its element
          type. *)
      let make_operator_formals_for_actual_num_of_args spec operator_name
          actual_num_of_args =
        let { Relation.input; is_variadic } =
          relation_for_id spec operator_name
        in
        let () =
          if
            (not is_variadic)
            && List.compare_length_with input actual_num_of_args <> 0
          then
            let msg =
              Format.asprintf "operator %s expects %d arguments but received %d"
                operator_name (List.length input) actual_num_of_args
            in
            failwith msg
        in
        (* For a variadic operator we want n copies of its element type,
         where n is the number of actual arguments. *)
        if is_variadic_operator spec operator_name then
          let arg_type =
            match List.hd input with
            | _, Term.TypeOperator { op = List0 | List1; term = _, arg_term } ->
                arg_term
            | _ -> failwith "Unexpected variadic operator shape"
          in
          List.init actual_num_of_args (fun _ -> arg_type)
        else List.map snd input

      (** [infer_parameter_types spec relation_name parameters formal_terms
           actual_terms ~context_expr] infers the types of parameters in
          [parameters] given the [formal_terms] and the corresponding
          [actual_terms], returning a parameter environment mapping parameter
          names to their inferred types. The [context_expr] is used for error
          reporting. *)
      let infer_parameter_types spec relation_name parameters formal_terms
          actual_terms ~context_expr =
        let parameter_env =
          List.fold_left2
            (fun curr_env formal_type arg_type ->
              infer_parameter_type spec ~relation_name parameters formal_type
                arg_type curr_env)
            StringMap.empty formal_terms actual_terms
        in
        let () =
          List.iter
            (fun param ->
              if not (StringMap.mem param parameter_env) then
                Error.uninstantiated_parameter_in_relation param relation_name
                  ~context_expr)
            parameters
        in
        parameter_env

      (** [instantiate_operator_types_from_env parameter_env formal_arg_types
           formal_output_type] instantiates the types of the arguments,
          [formal_arg_types], and output type, [formal_output_type], of an
          operator given a [parameter_env] mapping type parameter names to their
          inferred types. The result is the list of instantiated argument types
          and the instantiated output type. *)
      let instantiate_operator_types_from_env parameter_env formal_arg_types
          formal_output_type =
        if StringMap.is_empty parameter_env then
          (formal_arg_types, formal_output_type)
        else
          let instantiated_arg_types =
            List.map
              (fun arg_type ->
                let instantiated_arg_type =
                  substitute_type_parameters arg_type parameter_env
                in
                instantiated_arg_type)
              formal_arg_types
          in
          let instantiated_output_type =
            substitute_type_parameters formal_output_type parameter_env
          in
          (instantiated_arg_types, instantiated_output_type)

      let instantiate_operator_types_from_inferred_types spec relation_name
          num_actual_args (mode, inferred_types) ~context_expr =
        let formal_arg_types =
          make_operator_formals_for_actual_num_of_args spec relation_name
            num_actual_args
        in
        let { Relation.parameters; output } =
          relation_for_id spec relation_name
        in
        let formal_output_type =
          match output with [ t ] -> t | _ -> assert false
        in
        let formal_types =
          match mode with
          | Use -> formal_arg_types
          | Def -> [ formal_output_type ]
        in
        let parameter_env =
          infer_parameter_types spec relation_name parameters formal_types
            inferred_types ~context_expr
        in
        instantiate_operator_types_from_env parameter_env formal_arg_types
          formal_output_type
    end

    (** A module for matching assignable expressions to candidate type terms. *)
    module MatchAssignableExprToTerms : sig
      val match_output_type : spec_type -> Expr.t -> Term.t list -> Term.t
      (** [match_output_type spec e terms] returns the type term from [terms],
          which corresponds to [e] among the output terms of some relation. By
          default, for a transition [r(intput)->e] and relation
          [r(formals)-> t1 |...| tn], the output type is [t1]. However, if [e]
          has structure that definitely matches one of the type terms in
          [t2,...,tn], then we return that type term instead.
          @raise [SpecError] if there are multiple matching terms. *)

      val match_refined_type : spec_type -> Expr.t -> Term.t -> Term.t
      (** [match_refined_type spec e t] attempts to return a variant type term
          of [t] that matches [e]. If no such variant exists, the result is [t].
      *)
    end = struct
      (** [list_structured_terms spec terms] returns the list of structured
          terms that each term in [terms] references. A structured term is any
          term that is not a type name. A structured term references itself,
          whereas a type name references the structured terms defined by its
          variants. *)
      let list_structured_terms spec terms =
        let rec list_structured_terms_for_one_term spec term =
          let open Term in
          match term with
          | Label id
          | Term.Tuple { label_opt = None; args = [ (_, Term.Label id) ] }
            when is_type_name spec.id_to_defining_node id -> (
              match defining_node_for_id spec id with
              | Node_Type { Type.variants } ->
                  Utils.list_concat_map
                    (fun { TypeVariant.term = variant } ->
                      list_structured_terms_for_one_term spec variant)
                    variants
              | _ -> [])
          | Tuple { label_opt = None; args = [ (_, sub_term) ] } -> [ sub_term ]
          | Label _ ->
              (* A constant label. *)
              [ term ]
          | _ -> [ term ]
        in
        Utils.list_concat_map (list_structured_terms_for_one_term spec) terms

      (** [is_structured_assignable_expr expr] checks if [expr] is an assignable
          expression that has structure that can be used to narrow down the set
          of candidate type terms. For example, a tuple has structure, while a
          variable does not. This function assumes that [expr] is an assignable
          expression, unlike, for example, a field access or a function
          application. *)
      let rec is_structured_assignable_expr expr =
        match expr with
        | Var _ | ListIndex _
        (* Although a list index has structure, it does not constrain the
        set of type terms that can be applied to it when appearing as an
        assignable expression. *)
          ->
            false
        | Tuple _ | Record _ | RecordUpdate _ | Relation _ -> true
        | NamedExpr (sub_expr, _) -> is_structured_assignable_expr sub_expr
        | FieldAccess _ | Map _ | Transition _ | Indexed _
        | UnresolvedApplication _ ->
            let msg =
              Format.asprintf
                "Unexpected expression when checking for structured assignable \
                 expression: %a."
                PP.pp_expr expr
            in
            failwith msg

      (** [match_structured_assignable_expr spec expr terms] attempts to find a
          type term in [terms] that matches [expr]. That is, a term that is a
          candidate type for [expr]. This function is specialized to expressions
          that have structure and are assignable (see
          [is_structured_assignable_expr] for details).
          @raise [SpecError]
            if there is no matching term or there are multiple matching terms.
      *)
      let match_structured_assignable_expr spec expr terms =
        assert (not (Utils.list_is_empty terms));
        assert (is_structured_assignable_expr expr);
        let expr_matches_term term =
          match (expr, term) with
          | ( Expr.Tuple { label_opt = Some expr_label; _ },
              Term.Tuple { label_opt = Some term_label; _ } ) ->
              String.equal expr_label term_label
          | ( Expr.Tuple { label_opt = None; args = term_args },
              Term.Tuple { label_opt = None; args = type_args } ) ->
              List.compare_lengths term_args type_args = 0
          | ( Expr.Record { label_opt = Some expr_label; _ },
              Term.Record { label_opt = Some term_label; _ } ) ->
              String.equal expr_label term_label
          | Expr.Record { label_opt = None }, Term.Record { label_opt = None }
            ->
              true
          | ( Expr.Relation { is_operator = true; name = rel_name; args = _ },
              Term.TypeOperator { op = Option } ) ->
              is_builtin_relation rel_name spec.some_operator
          | _ -> false
        in
        let structured_terms = list_structured_terms spec terms in
        let () =
          if false then
            Format.eprintf "Structured terms: %a.@."
              (PP.pp_sep_list ~sep:", " PP.pp_type_term)
              structured_terms
        in
        match List.filter expr_matches_term structured_terms with
        | [] -> Error.no_matching_output_type expr ~context_expr:expr
        | [ matching_term ] -> matching_term
        | candidates ->
            Error.ambiguous_output_type expr candidates ~context_expr:expr

      let find_match spec expr terms =
        if is_structured_assignable_expr expr then
          Some (match_structured_assignable_expr spec expr terms)
        else
          (* If expr is not structured, we cannot hope to find a matching type in terms. *)
          None

      let match_output_type spec expr terms =
        match find_match spec expr terms with
        | Some matching_term -> matching_term
        | None -> List.hd terms

      let match_refined_type spec expr term =
        match find_match spec expr [ term ] with
        | Some matching_term -> matching_term
        | None -> term
    end

    (** [is_field_accessible spec base_type field_name] checks if an expression
        [x.field_name] is valid given that [x] has type [base_type]. *)
    let rec is_field_accessible spec base_type field_name =
      match base_type with
      | Term.Record { fields; _ } ->
          List.exists (fun { Term.name } -> String.equal name field_name) fields
      | Term.Label id -> (
          match defining_node_opt_for_id spec id with
          | Some (Node_Type { Type.variants; _ }) ->
              List.for_all
                (fun { TypeVariant.term } ->
                  is_field_accessible spec term field_name)
                variants
          | _ -> false)
      | _ -> false

    (** [infer_type_in_env spec type_env expr] infers the type of [expr] using
        [spec] and [type_env] the types of variables. The type environment is
        also updated in cases where sub-expressions of [expr] define variables.
        The result is a pair consisting of the type inferred for [expr] and the
        updated type environment. *)
    let rec infer_type_in_env spec type_env expr : Term.t * type_env =
      let open Expr in
      let t, tenv =
        match expr with
        | Var id when is_ignore_var id ->
            (* If we type '_' with the bottom type, it will be subsumed by any other type. *)
            (spec.bottom_term, type_env)
        | Var id ->
            let id_type = type_of_id ~context_expr:expr spec type_env id in
            (id_type, type_env)
        | FieldAccess { base; field } ->
            let base_type, type_env = infer_type_in_env spec type_env base in
            let () =
              if not (is_field_accessible spec base_type field) then
                Error.undefined_field_in_record ~context_expr:expr base_type
                  field
            in
            let field_type = type_term_for_field spec field in
            (field_type, type_env)
        | ListIndex { list_var; index } ->
            (* list_var should be a list-typed variable and index must be the
               natural number type. The result type is the type of the list elements.
            *)
            let list_base_type =
              type_of_id ~context_expr:expr spec type_env list_var
            in
            let elem_type =
              match list_base_type with
              | TypeOperator { op = List0 | List1; term = _, elem_type } ->
                  elem_type
              | _ ->
                  Error.invalid_list_base_type list_base_type ~context_expr:expr
            in
            let index_type, type_env = infer_type_in_env spec type_env index in
            if
              CheckTypeInstantiations.subsumed spec index_type
                (type_term_for_typedef spec.n_type)
            then (elem_type, type_env)
            else Error.invalid_list_index_type index_type ~context_expr:expr
        | Record { label_opt; fields } ->
            (* All arguments must typecheck, and if the record is labelled,
               the inferred record type must be subsumed by the labelled record type. *)
            let fields =
              List.sort
                (fun (name1, _) (name2, _) -> String.compare name1 name2)
                fields
            in
            let field_names, field_initializers = List.split fields in
            let field_types, type_env =
              infer_type_list spec type_env field_initializers
            in
            let inferred_type =
              let record_fields =
                List.map2
                  (fun field_name field_type ->
                    Term.make_record_field (field_name, field_type) [])
                  field_names field_types
              in
              Term.Record { label_opt; fields = record_fields }
            in
            let () =
              check_subsumed_by_opt_labelled_type spec inferred_type label_opt
                ~context_expr:expr
            in
            (inferred_type, type_env)
        | RecordUpdate { record_expr; updates } -> (
            (* check that record_expr typechecks and that each field update expression typechecks
               and matches the record field type *)
            let record_type, type_env =
              infer_type_in_env spec type_env record_expr
            in
            let open Term in
            match CheckTypeInstantiations.reduce_term spec record_type with
            | Record { fields } ->
                let field_map =
                  List.map (fun field -> (field.name, field.term)) fields
                  |> List.to_seq |> StringMap.of_seq
                in
                let updated_field_types, type_env =
                  infer_type_list spec type_env (List.map snd updates)
                in
                let formal_field_types =
                  List.map
                    (fun (field_name, _) ->
                      match StringMap.find_opt field_name field_map with
                      | Some field_type -> field_type
                      | None ->
                          Error.undefined_field_in_record ~context_expr:expr
                            record_type field_name)
                    updates
                in
                let field_exprs = List.map snd updates in
                let () =
                  check_arg_types spec field_exprs updated_field_types
                    formal_field_types ~context_expr:expr
                in
                (record_type, type_env)
            | _ ->
                Error.invalid_record_update_base_type record_type
                  ~context_expr:expr)
        | NamedExpr (sub_expr, _) -> infer_type_in_env spec type_env sub_expr
        | Tuple { label_opt; args } ->
            (* All arguments must typecheck, and if the tuple is labelled,
               the inferred tuple type must be subsumed by the labelled tuple type. *)
            let arg_types, type_env = infer_type_list spec type_env args in
            let inferred_type =
              let anonymous_typed_args =
                List.map (fun t -> (None, t)) arg_types
              in
              Term.Tuple { label_opt; args = anonymous_typed_args }
            in
            let () =
              check_subsumed_by_opt_labelled_type spec inferred_type label_opt
                ~context_expr:expr
            in
            (inferred_type, type_env)
        | Relation { is_operator = true; name; args = [ lhs; rhs ] }
          when is_builtin_relation name spec.assign ->
            let rhs_type, type_env = infer_type_in_env spec type_env rhs in
            let type_env = apply_type spec type_env lhs rhs_type in
            (* Mathematically, assignment expressions are just equalities so the
             resulting type is Boolean. *)
            (type_term_for_typedef spec.bool, type_env)
        | Relation { is_operator = true; name; args = [ lhs; rhs ] }
          when is_builtin_relation name spec.reverse_assign ->
            (* Reduce to normal assignment by switching lhs and rhs. *)
            infer_type_in_env spec type_env
              (Relation
                 {
                   is_operator = true;
                   name = spec.assign.name;
                   args = [ rhs; lhs ];
                 })
        | Relation { name; args; is_operator = true } ->
            (* If the operator is quantifying, the type of the bound variable
               (first argument) cannot be inferred from the expression alone,
               since it's a newly introduced variable. Therefore, we first
               infer its type from the domain (second) argument. *)
            let opt_bound_var, type_env =
              update_type_env_for_bound_variable spec name type_env args
            in
            let arg_types, type_env = infer_type_list spec type_env args in
            (* Instantiate the operator's input/output types based on the types inferred
               for its arguments. *)
            let instantiated_arg_types, instantiated_output_type =
              InstantiateOperator.instantiate_operator_types_from_inferred_types
                spec name (List.length args) (Use, arg_types) ~context_expr:expr
            in
            let () =
              if false then
                let instantiated_operator =
                  Relation.make name RelationProperty_Function None
                    (List.map (fun t -> (None, t)) instantiated_arg_types)
                    [ instantiated_output_type ]
                    [] None
                in
                Format.eprintf "Instantiated %s with operator %a@." name
                  PP.pp_relation_definition instantiated_operator
            in
            let () =
              check_arg_types spec args arg_types instantiated_arg_types
                ~context_expr:expr
            in
            (* Bound variable goes out of scope. *)
            let type_env =
              match opt_bound_var with
              | Some bound_var -> StringMap.remove bound_var type_env
              | None -> type_env
            in
            (instantiated_output_type, type_env)
        | Relation { name; args; is_operator = false } ->
            let { Relation.input; output } = relation_for_id spec name in
            let output =
              match output with
              | [ output ] -> output
              | _ ->
                  (* We allow relations with multiple outputs to be used in transitions,
                   but only single output relations outside of transitions.
                   Otherwise, the type of the expression cannot be deterministically determined. *)
                  Error.only_single_output_relations_supported name
                    ~context_expr:expr
            in
            let arg_types, type_env = infer_type_list spec type_env args in
            let formal_arg_types = List.map snd input in
            let () =
              check_arg_types spec args arg_types formal_arg_types
                ~context_expr:expr
            in
            (output, type_env)
        | Transition { lhs = Relation { name; args; is_operator = false }; rhs }
          ->
            let { Relation.input; output } = relation_for_id spec name in
            let arg_types, type_env = infer_type_list spec type_env args in
            let formal_arg_types = List.map snd input in
            let () =
              check_arg_types spec args arg_types formal_arg_types
                ~context_expr:expr
            in
            let () =
              if false then
                Format.eprintf
                  "Inferring type for transition output %a among %a@."
                  PP.pp_expr rhs
                  (PP.pp_sep_list ~sep:" | " PP.pp_type_term)
                  output
            in
            let output_term =
              MatchAssignableExprToTerms.match_output_type spec rhs output
            in
            let type_env = apply_type spec type_env rhs output_term in
            (* A transition r(input)->output an assertion that (input,output) belong
               to the relation r. Therefore, its type is Boolean. *)
            (type_term_for_typedef spec.bool, type_env)
        | Transition _ ->
            Error.only_relation_transitions_supported ~context_expr:expr
        | Indexed { index; list_var; body } ->
            (* The index is a bound variable that always has the natural number type. *)
            let type_env =
              StringMap.add index (type_term_for_typedef spec.n_type) type_env
            in
            let list_var_type =
              type_of_id ~context_expr:expr spec type_env list_var
            in
            let () =
              match list_var_type with
              | TypeOperator { op = List0 | List1 } -> ()
              | _ ->
                  Error.invalid_list_base_type list_var_type ~context_expr:expr
            in
            let body_type, type_env = infer_type_in_env spec type_env body in
            let () =
              (* The body is a transition and should thus have Boolean type. *)
              if
                not
                  (CheckTypeInstantiations.subsumed spec body_type
                     (type_term_for_typedef spec.bool))
              then Error.invalid_indexed_body_type body_type ~context_expr:expr
            in
            (* The bound variable goes out of scope. *)
            let type_env = StringMap.remove index type_env in
            (type_term_for_typedef spec.bool, type_env)
        | Map { lhs; args } ->
            (* Check that the single argument type matches the function's input type
               and return the function's output type. *)
            let lhs_type, type_env = infer_type_in_env spec type_env lhs in
            let from_term, to_term =
              match CheckTypeInstantiations.reduce_term spec lhs_type with
              | Function { from_type = _, from_term; to_type = _, to_term } ->
                  (from_term, to_term)
              | _ -> Error.invalid_map_lhs_type lhs_type ~context_expr:expr
            in
            let arg_types, type_env = infer_type_list spec type_env args in
            let arg_type =
              match arg_types with
              | [ t ] ->
                  t
                  (* Currently, only single argument functions are supported. *)
              | _ ->
                  Error.invalid_number_of_arguments_for_map ~expected:1
                    ~actual:(List.length arg_types) ~context_expr:expr
            in
            if not (CheckTypeInstantiations.subsumed spec arg_type from_term)
            then
              Error.invalid_argument_type ~arg_expr:(List.hd args)
                ~actual_type:arg_type ~formal_type:from_term ~context_expr:expr
            else (to_term, type_env)
        | UnresolvedApplication _ ->
            let msg =
              Format.asprintf
                "Unresolved application found when inferring type for \
                 expression: %a."
                PP.pp_expr expr
            in
            failwith msg
      in
      let () =
        if false then
          Format.eprintf "Expression: %a, Inferred type: %a@." PP.pp_expr expr
            PP.pp_type_term t
      in
      (t, tenv)

    (** [infer_type_list spec type_env exprs] infers the types of the list of
        expressions [exprs] using [spec] and [type_env]. The type environment is
        also updated in cases where sub-expressions of [exprs] define variables.
        The result is a pair consisting of the list of types inferred for
        [exprs] and the updated type environment. *)
    and infer_type_list spec type_env exprs : Term.t list * type_env =
      let types, type_env =
        List.fold_left
          (fun (curr_types, curr_env) expr ->
            let expr_type, updated_env = infer_type_in_env spec curr_env expr in
            (expr_type :: curr_types, updated_env))
          ([], type_env) exprs
      in
      (List.rev types, type_env)

    (** [check_subsumed_by_opt_labelled_type spec actual_type label_opt
         ~context_expr] checks that [actual_type] is subsumed by the labelled
        type indicated by [label_opt], if any. The [context_expr] is used for
        error reporting. *)
    and check_subsumed_by_opt_labelled_type spec actual_type label_opt
        ~context_expr =
      match label_opt with
      | Some label -> (
          match StringMap.find label spec.id_to_defining_node with
          | Node_TypeVariant { TypeVariant.term = declared_type } ->
              if
                not
                  (CheckTypeInstantiations.subsumed spec actual_type
                     declared_type)
              then Error.type_subsumption_failure actual_type declared_type
          | _ -> Error.invalid_labelled_type label ~context_expr)
      | None -> ()

    and check_arg_types spec arg_exprs arg_types arg_formal_types ~context_expr
        =
      Utils.list_iter3
        (fun arg_expr actual_type formal_type ->
          if not (CheckTypeInstantiations.subsumed spec actual_type formal_type)
          then
            Error.invalid_argument_type ~arg_expr ~actual_type ~formal_type
              ~context_expr)
        arg_exprs arg_types arg_formal_types

    (** [update_type_env_for_bound_variable spec relation_name type_env args]
        updates [type_env] if [relation_name] is a quantifying operator by
        inferring the type of the variable from the second argument in [args].
        If [relation_name] is not a quantifying operator, [type_env] is returned
        and [None] for the optional bound variable name. *)
    and update_type_env_for_bound_variable spec relation_name type_env args =
      if is_quantifying_operator spec relation_name then
        let () =
          if false then
            Format.eprintf
              "Updating type env for bound variable in relation %s(%a)@."
              relation_name
              (PP.pp_sep_list ~sep:", " PP.pp_expr)
              args
        in
        let bound_variable_name, domain_term, type_env =
          match args with
          | Var id :: domain_expr :: _ ->
              let domain_term, type_env =
                infer_type_in_env spec type_env domain_expr
              in
              (id, domain_term, type_env)
          | _ ->
              (* TODO: add a quantifier qualifier to operators and check
                 well-formedness of quantifying operator definitions. *)
              failwith
                "A quantifying operator should start with a variable and \
                 follow with a domain expression"
        in
        match CheckTypeInstantiations.reduce_term spec domain_term with
        | Term.TypeOperator
            {
              op = Powerset | Powerset_Finite | List0 | List1 | Option;
              term = _, elem_type;
            } ->
            let () =
              if false then
                Format.eprintf "Quantifying over variable of type %a@."
                  PP.pp_type_term elem_type
            in
            let new_env =
              StringMap.add bound_variable_name elem_type type_env
            in
            (Some bound_variable_name, new_env)
        | _ ->
            let msg =
              Format.asprintf
                "Unexpected domain type for quantifying operator: %a."
                PP.pp_type_term domain_term
            in
            failwith msg
      else (None, type_env)

    (** [apply_type spec type_env expr target_type] deconstructs [expr] and
        [target_type] in lockstep to assign the sub-expressions of [expr] the
        corresponding sub-types in [target_type], updating [type_env]
        accordingly. *)
    and apply_type spec type_env expr target_type : type_env =
      let () =
        if false then
          Format.eprintf "Applying type %a to expr %a@." PP.pp_type_term
            target_type PP.pp_expr expr
      in
      let target_type = CheckTypeInstantiations.reduce_term spec target_type in
      let expr =
        match expr with NamedExpr (sub_expr, _) -> sub_expr | _ -> expr
      in
      match (expr, target_type) with
      | Var id, _ when is_ignore_var id -> type_env
      | Var id, _ -> StringMap.add id target_type type_env
      | ( Relation { name; args = [ arg ]; is_operator = true },
          Term.TypeOperator { op; term = _, op_arg_type } )
        when is_builtin_relation name spec.some_operator
             && Term.type_operator_equal op Term.Option ->
          (* Special treatment for the some/Option. *)
          apply_type spec type_env arg op_arg_type
      | Expr.Relation { name; args; is_operator = true }, _ ->
          let instantiated_arg_types, instantiated_output_type =
            InstantiateOperator.instantiate_operator_types_from_inferred_types
              spec name (List.length args) (Def, [ target_type ])
              ~context_expr:expr
          in
          let () =
            if
              not
                (CheckTypeInstantiations.subsumed spec instantiated_output_type
                   target_type)
            then
              Error.argument_subsumption_failure instantiated_output_type
                target_type ~context_expr:expr
          in
          let () =
            (* TODO: use a more dedicated error for this case. *)
            if not (List.compare_lengths args instantiated_arg_types = 0) then
              Error.invalid_number_of_arguments name expr
                ~expected:(List.length instantiated_arg_types)
                ~actual:(List.length args)
          in
          List.fold_left2
            (fun curr_env formal_type arg ->
              apply_type spec curr_env arg formal_type)
            type_env instantiated_arg_types args
      | Expr.Tuple { args; _ }, Term.Tuple { args = target_args; _ } ->
          assert (List.compare_lengths args target_args = 0);
          List.fold_left2
            (fun curr_env arg (_, target_arg_type) ->
              apply_type spec curr_env arg target_arg_type)
            type_env args target_args
      | ListIndex { list_var; index }, _ ->
          let index_type, _ = infer_type_in_env spec type_env index in
          let () =
            if
              CheckTypeInstantiations.subsumed spec index_type
                (type_term_for_typedef spec.n_type)
            then ()
            else Error.invalid_list_index_type index_type ~context_expr:expr
          in
          let list_var_type =
            Term.TypeOperator { op = List0; term = (None, target_type) }
          in
          StringMap.add list_var list_var_type type_env
      | ( Expr.Record { fields = expr_fields; _ },
          Term.Record { fields = target_fields; _ } ) ->
          let target_field_map =
            List.fold_left
              (fun acc { Term.name; term = field_type } ->
                StringMap.add name field_type acc)
              StringMap.empty target_fields
          in
          List.fold_left
            (fun curr_env (field_name, init_expr) ->
              match StringMap.find_opt field_name target_field_map with
              | Some target_field_type ->
                  apply_type spec curr_env init_expr target_field_type
              | None ->
                  Error.undefined_field_in_record ~context_expr:expr target_type
                    field_name)
            type_env expr_fields
      | Expr.RecordUpdate _, _
      | Expr.Relation _, _
      | FieldAccess _, _
      | UnresolvedApplication _, _
      | Transition _, _
      | Indexed _, _
      | NamedExpr _, _ ->
          let msg =
            Format.asprintf "unexpected expression in apply_type: %a."
              PP.pp_expr expr
          in
          failwith msg
      | (Expr.Tuple _ | Expr.Record _), _ ->
          let matched_term =
            MatchAssignableExprToTerms.match_refined_type spec expr target_type
          in
          apply_type spec type_env expr matched_term
      | Expr.Map _, _ -> Error.cannot_apply_type_to_expr expr target_type

    (** [update_env_for_term type_env (opt_name, term)] updates [type_env] by
        recursing into [term] and treating each term name as a variable, and
        binding it to the term it names. *)
    let rec update_env_for_term type_env (opt_name, term) =
      let type_env =
        match opt_name with
        | Some id -> StringMap.add id term type_env
        | None -> type_env
      in
      match term with
      | Term.Tuple { args } ->
          List.fold_left
            (fun curr_env arg -> update_env_for_term curr_env arg)
            type_env args
      | Term.Label _ | Term.TypeOperator _ | Term.Record _ | Term.Function _
      | Term.ConstantsSet _ ->
          type_env

    (** [generate_type_env_from_relation_args relation] generates a type
        environment from the arguments of [relation].

        Example: given the relation signature
        {[
          relation annotate_get_array(
              tenv: static_envs,
              (size: array_index, t_elem: ty),
              (e_base: expr, ses_base: powerset(TSideEffect),
              e_index: expr)) ->
                (t: ty, new_e: expr, ses: powerset(TSideEffect))
        ]}
        the resulting type environment is
        {[
          "tenv" -> static_envs,
          "size" -> array_index,
          "t_elem" -> ty,
          "e_base" -> expr,
          "ses_base" -> powerset(TSideEffect),
          "e_index" -> expr
        ]} *)
    let generate_type_env_from_relation_args relation =
      List.fold_left
        (fun curr_env arg -> update_env_for_term curr_env arg)
        StringMap.empty relation.Relation.input

    let infer spec expr =
      let empty_env = StringMap.empty in
      infer_type_in_env spec empty_env expr |> fst

    (** [infer_type_for_judgment spec type_env expr] infers the type of the
        judgment expression [expr] using [spec] and [type_env]. The type
        environment is also updated in cases where sub-expressions of [expr]
        define variables. The result is a pair consisting of the type inferred
        for [expr] and the updated type environment.

        This function is mainly a wrapper around [infer_type_in_env] that adds
        error context information. TODO: remove once location information is
        added to expressions.

        @raise [SpecError] if a type error is found during inference. *)
    let infer_type_for_judgment spec type_env expr =
      try
        let () =
          if false then
            Format.eprintf "--- Inferring types for judgment %a ---@."
              PP.pp_expr expr
          else ()
        in
        let judgment_type, updated_env = infer_type_in_env spec type_env expr in
        (judgment_type, updated_env)
      with SpecError err | Failure err ->
        stack_spec_error err (Format.asprintf "In judgment %a" PP.pp_expr expr)

    let check_and_infer relation spec expanded_rule =
      let () =
        if false then
          Format.eprintf "@.=== Checking types for relation %s case %s ===@."
            relation.Relation.name
            (Option.value ~default:"top level"
               expanded_rule.ExpandRules.name_opt)
      in
      let input_type_env = generate_type_env_from_relation_args relation in
      let premise_exprs, output_expr = ExpandRules.rule_exprs expanded_rule in
      let type_env =
        List.fold_left
          (fun curr_env expr ->
            let judgment_type, updated_env =
              infer_type_for_judgment spec curr_env expr
            in
            let _check_judgment_types_are_boolean =
              if
                not
                  (CheckTypeInstantiations.subsumed spec judgment_type
                     (type_term_for_typedef spec.bool))
              then Error.judgment_not_boolean judgment_type expr
            in
            updated_env)
          input_type_env premise_exprs
      in
      (* Now check that the type of the output expression matches any of the output formal types. *)
      let output_judgment_type, type_env =
        infer_type_for_judgment spec type_env output_expr
      in
      let { Relation.output } = relation in
      let output_type_matched =
        List.exists
          (fun output_type ->
            CheckTypeInstantiations.subsumed spec output_judgment_type
              output_type)
          output
      in
      let () =
        if not output_type_matched then
          Error.output_type_mismatch output_judgment_type output output_expr
        else ()
      in
      let () =
        if false then
          Format.eprintf "Inferred variable types: %a@." pp_type_env type_env
      in
      type_env

    let merge_type_case_tables table1 table2 =
      StringMap.merge
        (fun _var entry1_opt entry2_opt ->
          match (entry1_opt, entry2_opt) with
          | Some entry1, Some entry2 -> Some (entry1 @ entry2)
          | Some entry, None | None, Some entry -> Some entry
          | None, None -> None)
        table1 table2

    let infer_for_relation spec ({ Relation.name; rule_opt } as relation) :
        type_case_table =
      let rule =
        match rule_opt with
        | Some rule -> rule
        | None ->
            let msg = Format.asprintf "Relation %s has no rules." name in
            failwith msg
      in
      let expanded_rules = ExpandRules.expand rule in
      let type_case_tables =
        List.map
          (fun expanded_rule ->
            let name =
              Option.value ~default:"" expanded_rule.ExpandRules.name_opt
            in
            let type_env = check_and_infer relation spec expanded_rule in
            let var_to_term = StringMap.bindings type_env in
            let var_to_case_and_term =
              List.map (fun (var, term) -> (var, [ (name, term) ])) var_to_term
            in
            StringMap.of_list var_to_case_and_term)
          expanded_rules
      in
      try List.fold_left merge_type_case_tables StringMap.empty type_case_tables
      with Failure err ->
        let msg =
          Format.asprintf
            "Type error when merging type environments for relation %s: %s" name
            err
        in
        failwith msg
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
      | RecordUpdate { record_expr; updates } ->
          let () = check_expr_in_context record_expr in
          let update_field_names, update_field_inits = List.split updates in
          let () = check_expr_list_in_context update_field_inits in
          List.iter
            (fun field_name ->
              if not (is_field field_name) then Error.non_field field_name expr)
            update_field_names
      | Transition { lhs; rhs; short_circuit } ->
          check_expr_in_context lhs;
          check_expr_in_context rhs;
          Option.iter check_expr_list_in_context short_circuit
      | Indexed { body } -> check_expr_in_context body
      | ListIndex { index } -> check_expr_in_context index
      | Var _ -> ()
      | FieldAccess { field } ->
          if not (is_field field) then Error.non_field field expr
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
              let open ExpandRules in
              let () =
                List.iter
                  (fun { Rule.expr } -> check_expr_well_formed spec expr)
                  expanded_rule.judgments
              in
              let () = UseDef.check_use_def relation spec expanded_rule in
              let _discarded_type_env =
                TypeInference.check_and_infer relation spec expanded_rule
              in
              ()
            with SpecError err | Failure err ->
              stack_spec_error err
                (Format.asprintf "In rule for relation %s, case %s"
                   relation.Relation.name
                   (Option.value ~default:"top-level" expanded_rule.name_opt)))
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
    let rec is_correctly_named_argument =
      let open Term in
      function
      | Some _, sub_term -> not (is_correctly_named_argument (None, sub_term))
      | None, Tuple { args } -> List.for_all is_correctly_named_argument args
      | None, _ -> false
    in
    let check_relation { Relation.name; input; rule_opt } =
      if Option.is_some rule_opt then
        List.iter
          (fun arg ->
            if not (is_correctly_named_argument arg) then
              Error.relation_argument_incorrect_naming name arg)
          input
    in
    List.iter (function Elem_Relation def -> check_relation def | _ -> ()) ast
end

(** [match_output_expr_to_term spec expr terms] performs a best-effort matching
    of an output expression [expr] to any term in [terms], returning the first
    matching term if any. This function assumes [expr] and all the terms in
    [terms] are well-formed and well-typed. *)
let match_output_expr_to_term spec expr terms =
  let rec expr_matches_term spec expr term =
    let open Expr in
    let open Term in
    let term = Check.CheckTypeInstantiations.reduce_term spec term in
    match (expr, term) with
    | NamedExpr (sub_expr, _), _ -> expr_matches_term spec sub_expr term
    | Var _, _ ->
        (* A variable does not have any structure and so can match any term. *)
        true
    | ( Expr.Tuple { label_opt = expr_label; args },
        Term.Tuple { label_opt = term_label; args = term_args } ) ->
        Option.equal String.equal expr_label term_label
        && List.length args = List.length term_args
        && List.for_all2 (expr_matches_term spec) args (List.map snd term_args)
    | Expr.Tuple _, _ -> false
    | ( Expr.Record { label_opt = expr_label; fields = expr_fields },
        Term.Record { label_opt = term_label; fields = term_fields } ) ->
        let expr_fields =
          List.sort
            (fun (field_name1, _) (field_name2, _) ->
              String.compare field_name1 field_name2)
            expr_fields
        in
        let term_fields = sort_record_fields term_fields in
        Option.equal String.equal expr_label term_label
        && List.length expr_fields = List.length term_fields
        && List.for_all2
             (fun (_, field_expr) { Term.term = field_term } ->
               expr_matches_term spec field_expr field_term)
             expr_fields term_fields
    | Expr.Record _, _ -> false
    | RecordUpdate { updates }, Term.Record { fields = term_fields } ->
        let expr_fields = List.map fst updates in
        let term_fields = List.map (fun { Term.name; _ } -> name) term_fields in
        List.for_all
          (fun field -> List.exists (String.equal field) term_fields)
          expr_fields
    | Relation _, _
    | Map _, _
    | RecordUpdate _, _
    | ListIndex _, _
    | FieldAccess _, _ ->
        (* Give up. *)
        false
    | Transition _, _ | Indexed _, _ | UnresolvedApplication _, _ ->
        Format.eprintf "Unsupported expression for matching output type: %a@."
          PP.pp_expr expr;
        assert false
  in
  List.find_opt (expr_matches_term spec expr) terms

(** A module for extending expressions in output configurations of conclusion
    judgments with names derived from type terms. *)
module ExtendNames = struct
  open Expr
  open Rule

  (** [opt_extend] Wraps [expr] with a name if [opt_param_name] is [Some].
      Avoids naming a variable expression with its own name. *)
  let opt_extend spec expr opt_param_name =
    match (expr, opt_param_name) with
    | _, None -> expr
    | Var v, Some name when String.equal v name ->
        expr (* Avoid naming a variable with its own name. *)
    | ( Expr.Relation
          { name = operator_name; is_operator = true; args = [ Var v ] },
        Some name ) ->
        let relation = relation_for_id spec operator_name in
        if Relation.is_typecast_operator relation && String.equal v name then
          (* Typecasts render the input variable. If the input variable has the same name
             as the parameter, avoid naming it. *)
          expr
        else NamedExpr (expr, name)
    | _, Some name -> NamedExpr (expr, name)

  (** [extend_with_names type_term expr ] recursively transforms [expr] by
      adding names from [type_term] to sub-expressions of [expr]. Currently,
      only tuples (labelled or unlabelled) are supported, which is sufficient
      for most output configurations. *)
  let rec extend_with_names spec type_term expr =
    match (type_term, expr) with
    | Term.Tuple { label_opt = None; args = [ (opt_name, _) ] }, _ ->
        (* An unlabelled tuple with a single component serves as a named reference
           to any type.*)
        opt_extend spec expr opt_name
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
              opt_extend spec (extend_with_names spec arg_type arg) opt_name)
            term_components expr_components
        in
        Expr.Tuple { label_opt = expr_label_opt; args = extended_args }
    | _ -> expr

  (** [extend_rule_element output_type rule_element] extends output judgments in
      [rule_element] with names from [output_type]. *)
  let rec extend_rule_element spec output_types rule_element =
    match rule_element with
    | Judgment
        ({ expr = Transition { lhs; rhs; short_circuit }; is_output = true } as
         judgment) ->
        let output_type =
          match match_output_expr_to_term spec rhs output_types with
          | Some output_type -> output_type
          | _ ->
              (* Fallback to the main output type. *)
              List.hd output_types
        in
        let extended_rhs = extend_with_names spec output_type rhs in
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
                List.map (extend_rule_element spec output_types) elements
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
          | Elem_Relation { output = [] } ->
              assert false (* The parser ensures that [output] is non-empty. *)
          | Elem_Relation ({ rule_opt = Some elements; output } as def) ->
              let extended_elements =
                List.map (extend_rule_element spec output) elements
              in
              Elem_Relation { def with rule_opt = Some extended_elements })
        ast
    in
    update_spec_ast spec ast
end

(** A module to ensure that each constant in the specification has an associated
    type. If a constant does not have a type, it infers it from its
    initialization expression, if any, or assigns it a type labeled by its name
    otherwise. *)
module ExtendConstantsWithTypes = struct
  let extend_constant spec
      ({ Constant.name; opt_type; opt_value_and_attributes } as def) =
    let constant_type =
      match (opt_type, opt_value_and_attributes) with
      | Some term, _ -> term (* Already has a type, nothing to do. *)
      | None, Some (init_expr, _) -> (
          try
            let _check_init_expr =
              Check.UseDef.(
                update_use_def_for_expr Use spec empty_use_def init_expr)
            in
            let init_type = Check.TypeInference.infer spec init_expr in
            init_type
          with SpecError err ->
            stack_spec_error err
              (Format.asprintf
                 "When inferring type for constant %s from its initialization \
                  expression %a. Hint: you can explicitly specify the type of \
                  the constant or reorder the constants."
                 name PP.pp_expr init_expr))
      | None, None ->
          (* A constant without a specified type has a type labeled by its name. *)
          Label name
    in
    { def with Constant.opt_type = Some constant_type }

  let extend ({ ast } as spec) =
    let ast, _ =
      List.fold_left
        (fun (curr_ast, spec) elem ->
          match elem with
          | Elem_Constant def ->
              let new_def = extend_constant spec def in
              (* We need to update id_to_defining_node since the type may be needed for later constants. *)
              let id_to_defining_node =
                StringMap.add def.Constant.name (Node_Constant new_def)
                  spec.id_to_defining_node
              in
              let new_elem = Elem_Constant new_def in
              let spec = { spec with id_to_defining_node } in
              (new_elem :: curr_ast, spec)
          | _ -> (elem :: curr_ast, spec))
        ([], spec) ast
    in
    update_spec_ast spec (List.rev ast)
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
  match StringMap.find_opt name id_to_defining_node with
  | Some (Node_Relation def) when def.is_operator -> def
  | Some node ->
      let msg =
        Format.asprintf
          "%s must be an operator, but has been overridden with %a" name
          pp_definition_node node
      in
      raise (SpecError msg)
  | None ->
      let msg = Format.asprintf "Relation/operator %s is undefined." name in
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
    bottom_constant = get_constant "bot";
    bottom_term = Label "bot";
    none_constant = get_constant "None";
    empty_set = get_constant "empty_set";
    empty_list = get_constant "empty_list";
    bool = get_type "Bool";
    n_type = get_type "N";
    z_type = get_type "Z";
    assign = get_relation "assign";
    reverse_assign = get_relation "reverse_assign";
    some_operator = get_relation "some";
    cond_operator = get_relation "cond_op";
    variant_id_to_containing_type = make_variant_id_to_containing_type ast;
  }

let infer_type_case_table spec relation =
  Check.TypeInference.infer_for_relation spec relation

let case_table_vars type_case_table =
  List.map fst (StringMap.bindings type_case_table)

let from_ast ast =
  let spec = make_spec_with_builtins ast in
  let () = Check.check_no_undefined_ids spec in
  let () = Check.check_relations_outputs spec in
  let () = Check.CheckTypeInstantiations.check spec in
  let () = Check.check_math_layout spec in
  let () = Check.CheckProseTemplates.check spec in
  let () = Check.relation_named_arguments_if_exists_rule ast in
  let spec = ResolveApplicationExpr.resolve spec in
  let spec = ExtendConstantsWithTypes.extend spec in
  let spec = ResolveRules.resolve spec in
  let spec = ExtendNames.extend spec in
  let () = Check.CheckRules.check spec in
  let spec = add_default_rule_renders spec in
  spec
