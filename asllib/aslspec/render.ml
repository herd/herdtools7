(** A module for rendering an ASL semantics-specification for inclusion in a
    LaTeX document. *)

open AST
open Format
open Latex
open LayoutUtils
open Macros
open Spec

(** * A signature for modules that provide a specification to the Make functor.
*)
module type SPEC_VALUE = sig
  val spec : Spec.t
end

(** A functor module for rendering an ASL semantics-specification for a given
    specification. *)
module Make (S : SPEC_VALUE) = struct
  (** Returns the math macro given for the element defined by [id], unless none
      is given in which case it generates a math macro name and returns it. *)
  let get_or_gen_math_macro id =
    assert (not (String.equal id ""));
    (* We add a prefix to the macro name to avoid potential clashes. *)
    let macro_prefix = function
      | Node_Relation _ | Node_Type _ | Node_TypeVariant _ | Node_Constant _ ->
          ""
      | Node_RecordField _ -> "FIELD"
    in
    let node = Spec.defining_node_for_id S.spec id in
    let math_macro_opt = Spec.math_macro_opt_for_node node in
    match math_macro_opt with
    | Some str -> str
    | None -> Latex.elem_name_to_math_macro (macro_prefix node ^ id)

  (** [get_short_circuit_macro id] returns the short-circuit macro for the
      element defined by [id], if one exists, and [None] otherwise. *)
  let get_short_circuit_macro id =
    assert (not (String.equal id ""));
    let node = Spec.defining_node_for_id S.spec id in
    match node with Node_Type def -> Type.short_circuit_macro def | _ -> None

  let pp_id_as_macro fmt id = pp_print_string fmt (get_or_gen_math_macro id)

  let pp_id_opt_as_macro fmt id_opt =
    match id_opt with
    | Some id -> pp_print_string fmt (get_or_gen_math_macro id)
    | None -> ()

  (** [hypertarget_for_id id] returns a string [target] that can be used for the
      LaTeX [\hypertarget{target}{}] for [id]. *)
  let hypertarget_for_id id =
    let open Spec in
    let name_id = StringOps.remove_underscores id in
    let category =
      match defining_node_for_id S.spec id with
      | Node_Relation _ -> "relation"
      | Node_Type { Type.type_kind } -> (
          match type_kind with
          | TypeKind_Generic -> "type"
          | TypeKind_AST -> "ast")
      | Node_TypeVariant { TypeVariant.type_kind } -> (
          match type_kind with
          | TypeKind_Generic -> "type"
          | TypeKind_AST -> "ast")
      | Node_Constant _ -> "constant"
      | Node_RecordField _ -> "recordfield"
    in
    sprintf "%s-%s" category name_id

  (** [pp_constant_definition fmt def] renders the definition of the constant
      given by [def] with the formatter [fmt]. *)
  let pp_constant_definition fmt { Constant.name } =
    fprintf fmt "$%a$" pp_id_as_macro name

  (** [pp_constant_definition_macro fmt def] renders the LaTeX wrapper macro
      [\DefineConstant{name}{...}] around the rendering of a constant definition
      [def] with the formatter [fmt].*)
  let pp_constant_definition_macro fmt ({ Constant.name } as def) =
    let hyperlink_target = hypertarget_for_id name in
    fprintf fmt {|\DefineConstant{%s}{%a%a} %% EndDefineConstant|} name
      pp_texthypertarget hyperlink_target pp_constant_definition def

  (** Renders the application of the type operator [op] to [arg] with [fmt].
      [arg] is rendered by [pp_arg fmt arg]. *)
  let pp_type_operator fmt op pp_arg arg =
    let operator_to_macro_name = function
      | Powerset -> pow_macro_name
      | Powerset_Finite -> powfin_macro_name
      | List0 -> kleene_star_macro_name
      | List1 -> kleene_plus_macro_name
      | Option -> option_macro_name
    in
    pp_one_arg_macro (operator_to_macro_name op) pp_arg fmt arg

  let pp_field_name fmt field_name = pp_id_as_macro fmt field_name

  (** [pp_type_term fmt (type_term, layout)] renders [type_term] with [fmt],
      laid out according to [layout]. *)
  let rec pp_type_term fmt (type_term, layout) =
    match type_term with
    | Label name -> (
        match get_short_circuit_macro name with
        | Some short_circuit_macro ->
            pp_overtext fmt pp_id_as_macro name pp_print_string
              short_circuit_macro
        | None -> pp_id_as_macro fmt name)
    | TypeOperator { op; term = sub_term } ->
        pp_type_operator fmt op pp_opt_named_type_term (sub_term, layout)
    | LabelledTuple { label_opt; components } ->
        let is_type_reference =
          (* Singleton unlabelled tuples are a special case -
           they are used to reference type terms, rather than defining them. *)
          Option.is_none label_opt && Utils.is_singleton_list components
        in
        if is_type_reference then
          pp_opt_named_type_term fmt (List.hd components, layout)
        else
          fprintf fmt "%a%a" pp_id_opt_as_macro label_opt
            (pp_parenthesized Parens true pp_opt_named_type_terms)
            (components, layout)
    | LabelledRecord { label_opt; fields } ->
        let pp_record_fields_as_pairs =
          List.map
            (fun ({ name_and_type = field_name, _; _ } as field) ->
              (field_name, field))
            fields
        in
        fprintf fmt "%a%a" pp_id_opt_as_macro label_opt
          (pp_fields pp_field_name pp_record_field_as_pair)
          (pp_record_fields_as_pairs, layout)
    | ConstantsSet constant_names ->
        let layout_contains_vertical = LayoutUtils.contains_vertical layout in
        pp_parenthesized Braces layout_contains_vertical
          (PP.pp_sep_list ~sep:", " pp_id_as_macro)
          fmt constant_names
    | Function { from_type; to_type; total } ->
        let arrow_macro_name =
          if total then rightarrow_macro_name else partialto_macro_name
        in
        pp_connect_pair ~alignment:"r" fmt pp_opt_named_type_term from_type
          arrow_macro_name pp_opt_named_type_term to_type layout

  (** [pp_opt_named_type_term fmt ((name_opt, term), layout)] formats the type
      term [term] with the optional name [name_opt] above it using [fmt] and
      laid out according to [layout]. *)
  and pp_opt_named_type_term fmt ((name_opt, term), layout) =
    match name_opt with
    | None -> pp_type_term fmt (term, layout)
    | Some name -> pp_overtext fmt pp_type_term (term, layout) pp_var name

  (** [pp_opt_named_type_terms fmt (opt_type_terms, layout)] formats a list of
      optionally-named type terms [opt_type_terms] using [fmt] and laid out
      according to [layout]. *)
  and pp_opt_named_type_terms fmt (opt_type_terms, layout) =
    pp_aligned_elements ~pp_sep:pp_comma ~alignment:"c" pp_opt_named_type_term
      layout fmt opt_type_terms

  and pp_record_field_as_pair fmt ({ name_and_type = _, field_type; _ }, layout)
      =
    pp_type_term fmt (field_type, layout)

  (** [pp_output_types fmt (terms, layout)] renders the relation output [terms]
      in with the given [layout]. *)
  let pp_output_types fmt (terms, layout) =
    if Utils.is_singleton_list terms then
      pp_type_term fmt (List.hd terms, layout)
    else
      let pp_multiple_terms fmt (terms, layout) =
        pp_aligned_elements_and_operators
          ~pp_sep:(fun fmt () -> pp_macro fmt union_macro_name)
          ~alignment:"c" pp_type_term layout fmt terms
      in
      pp_parenthesized Parens true pp_multiple_terms fmt (terms, layout)

  (** Renders the mathematical formula for the relation signature [def] using
      [layout] and referencing elements in [S.spec]. *)
  let pp_relation_math layout fmt { Relation.name; property; input; output } =
    (* Reuse the rendering for type terms. *)
    let input_as_labelled_tuple =
      LabelledTuple { label_opt = Some name; components = input }
    in
    let property_macro_name =
      match property with
      | RelationProperty_Relation -> bigtimes_macro_name
      | RelationProperty_Function -> longrightarrow_macro_name
    in
    pp_connect_pair ~alignment:"c" fmt pp_type_term input_as_labelled_tuple
      property_macro_name pp_output_types output layout

  let pp_relation_definition fmt
      ({ Relation.name; property; input; output } as def) =
    let input_vars = vars_of_opt_named_type_terms input in
    let output_vars = Utils.list_concat_map vars_of_type_term output in
    let vars = input_vars @ output_vars in
    let instantiated_prose_description =
      substitute_spec_vars_by_latex_vars ~math_mode:true
        (Relation.prose_description def)
        vars
      (* necessary to avoid spurious line breaks. *)
      |> StringOps.shrink_whitespace
    in
    let layout = Layout.math_layout_for_node (Node_Relation def) in
    let hyperlink_target = hypertarget_for_id name in
    let relation_property_description =
      match property with
      | RelationProperty_Relation -> "relation"
      | RelationProperty_Function -> "function"
    in
    fprintf fmt {|The %s
\[@.%a%a@.\]
%a|} relation_property_description
      pp_mathhypertarget hyperlink_target (pp_relation_math layout) def
      pp_print_text instantiated_prose_description

  let pp_relation_definition_macro fmt def =
    fprintf fmt {|\DefineRelation{%s}{@.%a} %% EndDefineRelation|}
      def.Relation.name pp_relation_definition def

  let pp_variant fmt ({ TypeVariant.term } as variant) =
    let layout =
      match TypeVariant.math_layout variant with
      | Some layout -> layout
      | None -> Layout.for_type_term term
    in
    pp_type_term fmt (term, layout)

  (** [pp_typename_with_hypertarget fmt type_name] renders the type name
      [type_name] along with its hypertarget. *)
  let pp_typename_with_hypertarget fmt type_name =
    let hyperlink_target = hypertarget_for_id type_name in
    (* The hypertarget macro must come after the type name macro.
       Otherwise LaTeX compilation fails. *)
    fprintf fmt "%a%a" pp_id_as_macro type_name pp_mathhypertarget
      hyperlink_target

  (** [pp_variant_with_hypertarget fmt variant] renders the variant [variant]
      along with its hypertarget. *)
  let pp_variant_with_hypertarget fmt ({ TypeVariant.term } as variant) =
    let variant_name_opt = Spec.variant_to_label_opt variant in
    let hyperlink_target_opt = Option.map hypertarget_for_id variant_name_opt in
    match term with
    | LabelledRecord { fields } ->
        (* Records are a special case, since each field has its own hypertarget. *)
        let field_hyperlink_targets =
          List.map
            (fun { name_and_type = field_name, _; _ } ->
              hypertarget_for_id field_name)
            fields
        in
        fprintf fmt "%a%a%a"
          (Format.pp_print_option pp_mathhypertarget)
          hyperlink_target_opt
          (PP.pp_sep_list ~sep:"" pp_mathhypertarget)
          field_hyperlink_targets pp_variant variant
    | _ ->
        fprintf fmt "%a%a" pp_variant variant
          (Format.pp_print_option pp_mathhypertarget)
          hyperlink_target_opt

  (** [pp_type_and_variants ~is_first ~is_last fmt (type_def, variants)] renders
      the type definition [type_def] along with its list of variants [variants]
      using the formatter [fmt]. The boolean flags [is_first] and [is_last]
      indicate whether this is the first or last type in a list of types being
      rendered. *)
  let pp_type_and_variants ?(lhs_hypertargets = true) ?(is_first = true)
      ?(is_last = true) fmt ({ Type.type_kind; Type.name }, variants) =
    let equality_symbol, join_symbol =
      match type_kind with
      | TypeKind_AST -> (derivation_macro_name, pipe_macro_name)
      | TypeKind_Generic -> (triangleq_macro_name, union_macro_name)
    in
    let first_variant, variants_tail =
      match variants with
      | [] -> (* Expected to be called with non-empty list *) assert false
      | first_variant :: variants_tail -> (first_variant, variants_tail)
    in
    let _render_begin_flalign =
      if is_first then fprintf fmt {|@.\begin{flalign*}|} else ()
    in
    let _render_newline = if not is_first then fprintf fmt {|\\|} else () in
    let pp_typename =
      if lhs_hypertargets then pp_typename_with_hypertarget else pp_id_as_macro
    in
    let _first_line =
      fprintf fmt {|@.%a %a\ & %a|} pp_typename name pp_macro equality_symbol
        pp_variant_with_hypertarget first_variant
    in
    let _add_latex_line_break_only_if_more_variants =
      if List.length variants > 1 then fprintf fmt {|\\@.|}
      else fprintf fmt "@."
    in
    let num_variants_tail = List.length variants_tail in
    let _render_variants_tail =
      List.iteri
        (fun counter variant ->
          let () =
            fprintf fmt {|@[<h>%a\ & %a@]|} pp_macro join_symbol
              pp_variant_with_hypertarget variant
          in
          let _add_latex_line_break_except_on_last_line =
            if counter < num_variants_tail - 1 then fprintf fmt {|\\@.|}
          in
          ())
        variants_tail
    in
    let _end_flalign = if is_last then fprintf fmt {|\end{flalign*}|} else () in
    ()

  (** [pp_basic_type fmt basic_type] renders the basic type (a type with no
      variants) [basic_type] with the formatter [fmt]. *)
  let pp_basic_type fmt { Type.name } =
    let hyperlink_target = hypertarget_for_id name in
    fprintf fmt "%a$%a$" pp_texthypertarget hyperlink_target pp_id_as_macro name

  (** [pp_type_definition fmt def] renders the type definition [def] with the
      formatter [fmt]. *)
  let pp_type_definition fmt ({ Type.name; variants } as def) =
    match variants with
    | [] ->
        (* A basic type like `typedef A` *)
        pp_basic_type fmt def
    | _ :: _ -> (
        (* A complex type like `typedef A = V1 | ... | Vk` *)
        try pp_type_and_variants fmt (def, variants)
        with SpecError e ->
          stack_spec_error e (Format.sprintf "While checking: %s" name))

  (** [pp_type_definition_macro name fmt pp_value value] renders a wrapper
      around the rendering of a type definition for the type. The wrapper uses
      the LaTeX macro [\DefineType{name}{...}] to define the type [name] with
      the content rendered by [pp_value fmt value]. *)
  let pp_type_definition_macro fmt def =
    fprintf fmt {|\DefineType{%s}{%a} %% EndDefineType|} def.Type.name
      pp_type_definition def

  (** [pp_pointer ~is_first ~is_last fmt pointer] renders a subset of type
      definitions [pointer] with the formatter [fmt]. The boolean flags
      [is_first] and [is_last] indicate whether this is the first or last
      pointer in a list of pointers being rendered. *)
  let pp_pointer ~lhs_hypertargets ~is_first ~is_last fmt
      { TypesRender.type_name; variant_names } =
    let ({ Type.variants } as def) =
      match Spec.defining_node_for_id S.spec type_name with
      | Node_Type def -> def
      | _ -> assert false
    in
    let selected_variants =
      (* If [variant_names] is empty, we use all the variants from the defining type.
       Otherwise, list the labelelled tuples and records whose label are in [variant_names].
    *)
      if Utils.list_is_empty variant_names then variants
      else
        List.map
          (fun variant_name ->
            match Spec.defining_node_for_id S.spec variant_name with
            | Node_TypeVariant def -> def
            | _ -> assert false)
          variant_names
    in
    pp_type_and_variants ~lhs_hypertargets ~is_first ~is_last fmt
      (def, selected_variants)

  (** [pp_pointers fmt pointers] renders [pointers] - a list of subsets of type
      definitions - with the formatter [fmt]. *)
  let pp_pointers ~lhs_hypertargets fmt pointers =
    let num_pointers = List.length pointers in
    List.iteri
      (fun i pointer ->
        pp_pointer ~lhs_hypertargets ~is_first:(i = 0)
          ~is_last:(i = num_pointers - 1)
          fmt pointer)
      pointers

  (** [pp_render_types fmt render_types] renders the named list of subsets of
      type definitions [render_types] with the formatter [fmt]. *)
  let pp_render_types fmt ({ TypesRender.pointers } as def) =
    let lhs_hypertargets = TypesRender.lhs_hypertargets def in
    pp_pointers ~lhs_hypertargets fmt pointers

  (** [pp_render_types_macro fmt def] renders the LaTeX wrapper macro
      [\DefineRenderTypes{name}{...}] around the rendering of a list of subsets
      of type definitions [def] with the formatter [fmt]. *)
  let pp_render_types_macro fmt def =
    fprintf fmt {|\DefineRenderTypes{%s}{%a
} %% EndDefineRenderTypes|}
      def.TypesRender.name pp_render_types def

  (** A module for rendering rules that comprise relation definitions.*)
  module RenderRule = struct
    open Rule

    (** Renders the field path [path] with [fmt]. *)
    let pp_field_path fmt = function
      | [] -> assert false
      | var :: [] -> pp_var fmt var
      | var :: path ->
          fprintf fmt "%a.%a" pp_var var
            (PP.pp_sep_list ~sep:"." pp_field_name)
            path

    (** Returns the macro for a given relation category and a default long right
        arrow for [None]. *)
    let arrow_macro_name_for_category_opt =
      let open Relation in
      function
      | Some RelationCategory_Typing -> typearrow_macro_name
      | Some RelationCategory_Semantics -> evalarrow_macro_name
      | None -> longrightarrow_macro_name

    (** [short_circuit_macros_for_type_term type_term] returns the list of
        short-circuit macros for the given [type_term]. Currently only called
        for type terms that are either type names or sets of constants. *)
    let short_circuit_macros_for_type_term type_term =
      match type_term with
      | Label id -> (
          match Spec.defining_node_for_id S.spec id with
          | Node_Type typedef ->
              [ Type.short_circuit_macro typedef |> Option.get ]
          | Node_TypeVariant { term = Label id } -> [ get_or_gen_math_macro id ]
          | _ -> assert false)
      | ConstantsSet constant_names ->
          List.map get_or_gen_math_macro constant_names
      | _ -> assert false (* Checked in spec.ml *)

    (** [pp_expr fmt (expr, layout)] renders the expression [expr] with the
        formatter [fmt] and laid out according to [layout]. *)
    let rec pp_expr fmt (expr, layout) =
      match expr with
      | NamedExpr (sub_expr, name) ->
          pp_overtext fmt pp_expr (sub_expr, layout) pp_var name
      | Var name -> (
          (* Constants/labels should render via their macros.
          So do tuples and records with labels, and type names, since their label
          itself may be used as a constant in the spec (configuration domain).
          Plain variables should be rendered as text. *)
          match Spec.defining_node_opt_for_id S.spec name with
          | Some (Node_Constant _)
          | Some
              (Node_TypeVariant
                 {
                   term =
                     ( Label _
                     | LabelledTuple { label_opt = Some _ }
                     | LabelledRecord { label_opt = Some _ } );
                 })
          | Some (Node_Type _) ->
              pp_id_as_macro fmt name
          | _ -> pp_var fmt name)
      | Application { applicator = ExprOperator op; args } ->
          pp_operator op layout fmt args
      | Application { applicator; args } ->
          let pp_lhs fmt lhs =
            match lhs with
            | EmptyApplicator ->
                () (* No left-hand side so nothing to render. *)
            | Relation name | TupleLabel name -> pp_id_as_macro fmt name
            | Fields path -> pp_field_path fmt path
            | ExprOperator _ | Unresolved _ -> assert false
          in
          fprintf fmt "%a%a" pp_lhs applicator
            (pp_parenthesized Parens (contains_vertical layout)
               (pp_aligned_elements ~pp_sep:pp_comma ~alignment:"l" pp_expr
                  layout))
            args
      | Record { label; fields } ->
          fprintf fmt "%a%a" pp_id_as_macro label
            (pp_fields pp_field_name pp_expr)
            (fields, layout)
      | ListIndex { var; index } -> fprintf fmt "%a[%a]" pp_var var pp_var index
      | FieldAccess path -> pp_field_path fmt path
      | Indexed { index; list; body } ->
          let pp_indexed_lhs fmt ((index, list_var), _layout) =
            fprintf fmt "%a \\in %a(%a)" pp_var index pp_macro
              listrange_macro_name pp_var list_var
          in
          pp_connect_pair ~alignment:"ll" fmt pp_indexed_lhs (index, list)
            colon_macro_name pp_expr body layout
      | Transition { lhs; rhs; short_circuit } ->
          let relation_name =
            match lhs with
            | Application { applicator = Relation relation_name } ->
                relation_name
            | _ ->
                let msg =
                  Format.asprintf "Unexpected LHS in transition judgment: %a"
                    PP.pp_expr expr
                in
                raise (SpecError msg)
          in
          let { Relation.category } =
            Spec.relation_for_id S.spec relation_name
          in
          let arrow_macro_name = arrow_macro_name_for_category_opt category in
          let pp_rhs_with_short_circuit fmt rhs_with_layout =
            fprintf fmt "%a%a" pp_expr rhs_with_layout
              (pp_short_circuit relation_name)
              short_circuit
          in
          pp_connect_pair ~alignment:"r" fmt pp_expr lhs arrow_macro_name
            pp_rhs_with_short_circuit rhs layout

    (** [pp_operator op_name layout fmt args] renders an operator named
        [op_name] applied to [args] with [fmt] and laid out according to
        [layout]. *)
    and pp_operator op_name layout fmt args =
      let is_prefix_list_operator def =
        match def.Relation.input with
        | [ (_, TypeOperator { op = List0 | List1 }) ] ->
            not (Relation.is_associative_operator def)
        | _ -> false
      in
      let op_macro = get_or_gen_math_macro op_name in
      let operator = Spec.relation_for_id S.spec op_name in
      match operator.Relation.input with
      | [ _ ] when is_prefix_list_operator operator ->
          (* A variadic operator over a list of arguments. *)
          fprintf fmt "%a{%a}" pp_macro op_macro
            (pp_aligned_elements ~pp_sep:pp_comma ~alignment:"l" pp_expr layout)
            args
      | [ _ ] when Relation.is_associative_operator operator ->
          (* A variadic operator over a list of arguments rendered by separating arguments
             with the operator macro. *)
          pp_aligned_elements_and_operators
            ~pp_sep:(fun fmt () -> pp_macro fmt op_macro)
            ~alignment:"c" pp_expr layout fmt args
      | [ _ ] ->
          (* A simple unary operator. *)
          let arg = List.hd args in
          fprintf fmt "%a{%a}" pp_macro op_macro pp_expr (arg, layout)
      | [ _; _ ] when not (Relation.is_custom_operator operator) ->
          (* A simple binary operator. *)
          let lhs_arg, rhs_arg =
            match args with [ lhs; rhs ] -> (lhs, rhs) | _ -> assert false
          in
          pp_connect_pair ~alignment:"c" fmt pp_expr lhs_arg op_macro pp_expr
            rhs_arg layout
      | _ ->
          let pp_arg fmt (arg, layout) =
            fprintf fmt "{%a}" pp_expr (arg, layout)
          in
          let layout = horizontal_if_unspecified layout args in
          let args_with_layouts = apply_layout_to_list layout args in
          let vertical = LayoutUtils.contains_vertical layout in
          let layout_arg = if vertical then "{V}" else "{H}" in
          fprintf fmt "%a%s%a" pp_macro op_macro layout_arg
            (PP.pp_sep_list ~sep:"" pp_arg)
            args_with_layouts

    (** [pp_short_circuit relation_name fmt short_circuit] renders the optional
        short-circuit override expressions [short_circuit] for the relation
        [relation_name] with the formatter [fmt]. If no short-circuit
        expressions are provided, then they are generated from the definition of
        the relation. *)
    and pp_short_circuit relation_name fmt short_circuit =
      match short_circuit with
      | None -> (
          let { Relation.output } = Spec.relation_for_id S.spec relation_name in
          let alternative_output_types = Utils.list_tail output in
          let alternative_macros =
            Utils.list_concat_map short_circuit_macros_for_type_term
              alternative_output_types
          in
          match alternative_macros with
          | [] -> ()
          | _ ->
              fprintf fmt {|\;%a\;%a|} pp_macro terminateas_macro_name
                (PP.pp_sep_list ~sep:", " pp_print_string)
                alternative_macros)
      | Some [] -> ()
      | Some alternatives ->
          let layout =
            Horizontal (List.map (fun _ -> Unspecified) alternatives)
          in
          let terms_with_layouts = apply_layout_to_list layout alternatives in
          fprintf fmt {|\;%a\;%a|} pp_macro terminateas_macro_name
            (PP.pp_sep_list ~sep:", " pp_expr)
            terms_with_layouts

    (** [pp_judgmentfmt judgment] renders the judgment [judgment] with the
        formatter [fmt]. *)
    let pp_judgment fmt ({ Rule.expr } as judgment) =
      let layout = Rule.judgment_layout judgment in
      fprintf fmt "{%a}" pp_expr (expr, layout)

    (** [pp_case_name_opt fmt name_opt] renders the optional case name
        [name_opt] with the formatter [fmt]. *)
    let pp_case_name_opt fmt name_opt =
      match name_opt with
      | None -> ()
      | Some name -> fprintf fmt "[%s]" (StringOps.escape_underscores name)

    let pp_math_expanded_rule fmt { ExpandRules.name_opt; judgments } =
      let premises, conclusion = Utils.split_last judgments in
      let pp_premise fmt premise = pp_judgment fmt premise in
      let pp_conclusion fmt conclusion = pp_judgment fmt conclusion in
      fprintf fmt {|\begin{mathpar}@.\inferrule%a{%a}{@.%a@.}@.\end{mathpar}|}
        pp_case_name_opt name_opt
        (pp_print_list
         (* The quadruple backslash means the next premise definitely starts on a new line. *)
           ~pp_sep:(fun fmt () -> fprintf fmt {|\\\\@.|})
           pp_premise)
        premises pp_conclusion conclusion

    (** [pp_math_expanded_rules fmt expanded_rules] renders the list of expanded
        rules [expanded_rules] with the formatter [fmt]. *)
    let pp_math_expanded_rules fmt expanded_rules =
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt "@.@.")
        pp_math_expanded_rule fmt expanded_rules

    (** [pp_render_rule fmt rule_render] renders the mathematical inference
        rules referenced by [rule_render] with the formatter [fmt]. *)
    let pp_render_rule fmt { RuleRender.relation_name; path } =
      let { Relation.rule_opt } = Spec.relation_for_id S.spec relation_name in
      let rule = Option.get rule_opt in
      let expanded_rules = ExpandRules.expand rule in
      let expanded_subset =
        List.filter
          (fun { ExpandRules.name_opt; _ } ->
            match name_opt with
            | None -> true
            (* This corresponds to relations defined by a single rule, not containing any cases. *)
            | Some expanded_path ->
                Utils.string_starts_with ~prefix:path expanded_path)
          expanded_rules
      in
      pp_math_expanded_rules fmt expanded_subset

    (** [pp_render_rule_macro fmt def] renders the LaTeX wrapper macro
        [\DefineRule{name}{...}] around the rendering of the mathematical
        inference rules referenced by [def] with the formatter [fmt]. *)
    let pp_render_rule_macro fmt def =
      fprintf fmt {|\DefineRule{%s}{%a} %% EndDefineRule|} def.RuleRender.name
        pp_render_rule def
  end

  (** [pp_elem fmt elem] renders an element of the specification. *)
  let pp_elem fmt = function
    | Elem_Constant def -> pp_constant_definition fmt def
    | Elem_Type def -> pp_type_definition fmt def
    | Elem_Relation def -> pp_relation_definition fmt def
    | Elem_RenderTypes def -> pp_render_types fmt def
    | Elem_RenderRule def -> RenderRule.pp_render_rule fmt def

  (** [pp_elem_definition_macro fmt elem] renders a macro definition for an
      element of the specification. *)
  let pp_elem_definition_macro fmt = function
    | Elem_Constant def -> pp_constant_definition_macro fmt def
    | Elem_Type def -> pp_type_definition_macro fmt def
    | Elem_Relation def -> pp_relation_definition_macro fmt def
    | Elem_RenderTypes def -> pp_render_types_macro fmt def
    | Elem_RenderRule def -> RenderRule.pp_render_rule_macro fmt def

  (** Renders a LaTeX document containing all of the elements in [S.spec]. A
      header and footer are added to enable compiling the generated file
      separately, for testing and debugging. *)
  let render_latex_debug_elements fmt =
    let _print_header =
      fprintf fmt
        {|\documentclass{book}
\input{ASLmacros.tex}
\input{rendering_macros.tex}
\input{generated_macros.tex}
\begin{document}@.|}
    in
    let _print_elements =
      List.iter
        (fun elem ->
          let name = elem_name elem in
          let latex_name =
            Latex.spec_var_to_latex_var ~font_type:Latex.Text name
          in
          match elem with
          | Elem_Constant _ ->
              fprintf fmt {|
\section*{%s}
%a
|} latex_name pp_elem elem
          | Elem_Relation _ ->
              if Spec.is_operator elem then ()
              else fprintf fmt {|
\section*{%s}
%a
|} latex_name pp_elem elem
          | Elem_Type _ ->
              fprintf fmt {|
\section*{%s}
%a
|} latex_name pp_elem elem
          | Elem_RenderTypes _ ->
              fprintf fmt {|
\section*{%s}
%a
|} latex_name pp_elem elem
          | Elem_RenderRule _ ->
              fprintf fmt {|
\section*{%s}
%a
|} latex_name pp_elem elem)
        (Spec.elements S.spec)
    in
    let _print_footer = fprintf fmt {|@.\end{document}@.|} in
    ()

  (** [pp_id_macro fmt id] renders the LaTeX macro corresponding to the element
      defined for [id] with the formatter [fmt]. *)
  let pp_id_macro fmt id =
    let hyperlink_target = hypertarget_for_id id in
    let node = Spec.defining_node_for_id S.spec id in
    let font_for_type_kind = function
      | TypeKind_Generic -> Latex.TextSF
      | TypeKind_AST -> Latex.TextSC
    in
    let font_type =
      match node with
      | Node_Relation _ -> Latex.TextIT
      | Node_Type _ -> Latex.TextSF
      | Node_TypeVariant { TypeVariant.type_kind } ->
          font_for_type_kind type_kind
      | Node_Constant _ -> Latex.TextSF
      | Node_RecordField _ -> Latex.TextSF
    in
    if Option.is_some (Spec.math_macro_opt_for_node node) then ()
    else
      let typeset_macro = Latex.spec_var_to_latex_var ~font_type id in
      fprintf fmt
        {|\newcommand%a[0]{ \hyperlink{%s}{%s} } %% Generated from %s|}
        pp_id_as_macro id hyperlink_target typeset_macro id

  (** [generate_latex_macros fmt] generates LaTeX macros for all of the elements
      defined in [S.spec] using the formatter [fmt]. *)
  let generate_latex_macros fmt =
    let open AST in
    let _header =
      fprintf fmt
        {|%% ==================================================
%% AUTO-GENERATED - DO NOT EDIT
%% ==================================================

%% ------------------
%% Macros for symbols
%% ------------------

|}
    in
    let _generate_symbol_macros =
      List.iter
        (fun id ->
          pp_id_macro fmt id;
          fprintf fmt "\n")
        (Spec.defined_ids S.spec)
    in
    let _elements_header =
      fprintf fmt
        {|
%% -------------------
%% Macros for elements
%% -------------------

|}
    in
    let _element_macros =
      List.iter
        (fun elem ->
          if Spec.is_operator elem then ()
          else pp_elem_definition_macro fmt elem;
          fprintf fmt "\n\n")
        (Spec.elements S.spec)
    in
    let _footer = fprintf fmt "@." in
    ()

  let render fmt =
    let () = generate_latex_macros fmt in
    ()

  let render_debug fmt =
    let () = render_latex_debug_elements fmt in
    ()
end

(** Renders the macros for [spec] by using the Make functor internally. *)
let render spec fmt =
  let module R = Make (struct
    let spec = spec
  end) in
  R.render fmt

(** Renders macro invocations for [spec] by using the Make functor internally.
*)
let render_debug spec fmt =
  let module R = Make (struct
    let spec = spec
  end) in
  R.render_debug fmt
