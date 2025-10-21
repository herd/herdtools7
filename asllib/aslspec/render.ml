(** A module for rendering an ASL semantics-specification for inclusion in a
    LaTeX document. *)

open AST
open Format
open Text
open Spec

let extend_args_with_layouts args layout =
  let layout = Layout.horizontal_if_unspecified layout args in
  let args_layout =
    match layout with
    | Horizontal l | Vertical l -> l
    | Unspecified -> assert false
  in
  assert (List.compare_lengths args args_layout = 0);
  List.combine args args_layout

let field_name_to_latex field_name =
  Text.spec_var_to_latex_var ~font_type:Text field_name

(** [pp_latex_array alignment fmt pp_fun_rows] renders a table of elements using
    a LaTeX array environment. The [alignment] string specifies the alignment of
    each column and is copied directly to the array environment. The
    [pp_fun_rows] is a list of lists, where the outer list represents the array
    rows and the inner lists represent the columns. *)
let pp_latex_array alignment fmt pp_fun_rows =
  let () = assert (Str.string_match (Str.regexp "[lcr]+$") alignment 0) in
  let num_columns = String.length alignment in
  let pp_elt = fun fmt pp_fun -> pp_fun fmt in
  let pp_one_row fmt pp_funs =
    let () = assert (List.compare_length_with pp_funs num_columns = 0) in
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " & ") pp_elt fmt pp_funs
  in
  fprintf fmt "@[<v>\\begin{array}{%s}@ %a@ \\end{array}@]" alignment
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt {|\\@ |}) pp_one_row)
    pp_fun_rows

let pp_sep_elements ~sep pp_elem layout fmt elements =
  let layout = Layout.horizontal_if_unspecified layout elements in
  let elements_with_layouts = extend_args_with_layouts elements layout in
  match layout with
  | Horizontal _ ->
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt sep)
        pp_elem fmt elements_with_layouts
  | Vertical _ ->
      let elem_pp_funs =
        List.map
          (fun elem_with_layout ->
            [
              (fun fmt ->
                pp_elem fmt elem_with_layout;
                pp_print_string fmt ",");
            ])
          elements_with_layouts
      in
      pp_latex_array "l" fmt elem_pp_funs
  | Unspecified -> assert false

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
    let node = Spec.defining_node_for_id S.spec id in
    let math_macro_opt = Spec.math_macro_opt_for_node node in
    match math_macro_opt with
    | Some str -> str
    | None -> Text.elem_name_to_math_macro id

  let pp_texthypertarget fmt target_str =
    fprintf fmt {|\texthypertarget{%s}|} target_str

  let pp_mathhypertarget fmt target_str =
    fprintf fmt {|\mathhypertarget{%s}|} target_str

  (** [hyperlink_target_for_id id] returns a string [target] that can be used
      for the LaTeX [\hypertarget{target}{}] for [id]. *)
  let hyperlink_target_for_id id =
    let open Spec in
    let name_id = Text.remove_underscores id in
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
    in
    sprintf "%s-%s" category name_id

  (** [substitute_spec_vars_by_latex_vars math_mode s vars] returns a string [s]
      with every variable like [my_var] is substituted into [\texttt{my\_var}],
      which makes the returned string suitable to typsetting with LaTeX. If
      [math_mode] is true then the result is surrounded by [$$]. *)
  let substitute_spec_vars_by_latex_vars ~math_mode str vars =
    let open Text in
    let substitutions_map =
      List.fold_left
        (fun acc_map var_str ->
          let latex_var = spec_var_to_latex_var ~font_type:TextTT var_str in
          let latex_var =
            if math_mode then to_math_mode latex_var else latex_var
          in
          let template_var = spec_var_to_template_var var_str in
          StringMap.add template_var latex_var acc_map)
        StringMap.empty vars
    in
    let template_var_regexp = Str.regexp "{[a-zA-Z0-9_']+}" in
    let blocks = Str.full_split template_var_regexp str in
    List.fold_left
      (fun acc block ->
        match block with
        | Str.Text txt -> acc ^ txt
        | Str.Delim var -> (
            match StringMap.find_opt var substitutions_map with
            | Some latex_var -> acc ^ latex_var
            | None -> acc ^ var))
      "" blocks

  type parenthesis = Parens | Braces | Brackets [@@warning "-37"]

  let pp_parenthesized parenthesis large pp_elem fmt elem =
    let left = if large then {|\left|} else "" in
    let right = if large then {|\right|} else "" in
    match parenthesis with
    | Parens -> fprintf fmt "%s(%a%s)" left pp_elem elem right
    | Braces -> fprintf fmt "%s\\{%a%s\\}" left pp_elem elem right
    | Brackets -> fprintf fmt "%s[%a%s]" left pp_elem elem right

  (** Renders an instance of the constant [name] in a term. *)
  let pp_constant_instance fmt name =
    let macro = get_or_gen_math_macro name in
    pp_print_string fmt macro

  (** Renders the definition of the constant given by [def]. *)
  let pp_constant_definition fmt { Constant.name } =
    let macro = get_or_gen_math_macro name in
    let hyperlink_target = hyperlink_target_for_id name in
    fprintf fmt
      {|\DefineConstant{%s}{\hypertarget{%s}{} $%s$} %% EndDefineConstant|} name
      hyperlink_target macro

  (** Renders the operator [op] applied to the argument rendered by
      [pp_arg fmt arg]. *)
  let pp_operator op fmt pp_arg =
    let operator_to_macro = function
      | Powerset -> "pow"
      | Powerset_Finite -> "powfin"
      | List0 -> "KleeneStar"
      | List1 -> "KleenePlus"
      | Option -> "Option"
    in
    fprintf fmt {|\%s{%a}|} (operator_to_macro op) pp_arg

  (** [pp_type_term mode term] formats [term] into a string suitable for LaTeX
      math mode. *)
  let rec pp_type_term fmt (type_term, layout) =
    let () =
      if false then
        fprintf Format.std_formatter "%a with original layout %a@."
          PP.pp_type_term type_term PP.pp_math_shape layout
    in
    let layout_contains_vertical = Layout.contains_vertical layout in
    match type_term with
    | Label name -> pp_print_string fmt (get_or_gen_math_macro name)
    | Operator { op; term = sub_term } ->
        pp_operator op fmt pp_opt_named_type_term (sub_term, layout)
    | LabelledTuple { label_opt; components } ->
        let is_type_reference =
          (* Singleton unlabelled tuples are a special case -
           they are used to reference type terms, rather than defining them. *)
          Option.is_none label_opt && Utils.is_singleton_list components
        in
        if is_type_reference then
          pp_opt_named_type_term fmt (List.hd components, layout)
        else
          let label =
            match label_opt with
            | Some label -> get_or_gen_math_macro label
            | None -> ""
          in
          fprintf fmt "%s%a" label
            (pp_parenthesized Parens layout_contains_vertical
               pp_opt_named_type_terms)
            (components, layout)
    | LabelledRecord { label_opt; fields } ->
        let label =
          match label_opt with
          | Some label -> get_or_gen_math_macro label
          | None -> ""
        in
        fprintf fmt {|%s%a|} label pp_record_fields (fields, layout)
    | ConstantsSet constant_names ->
        fprintf fmt {|%a|}
          (pp_parenthesized Braces layout_contains_vertical
             (PP.pp_sep_list ~sep:", " pp_constant_instance))
          constant_names
    | Function { from_type; to_type; total } -> (
        let arrow_symbol = if total then {|\rightarrow|} else {|\partialto|} in
        let layout =
          Layout.horizontal_if_unspecified layout [ from_type; to_type ]
        in
        let from_type_with_layout, to_type_with_layout =
          Utils.list_match_two_elements
            (extend_args_with_layouts [ from_type; to_type ] layout)
        in
        match layout with
        | Horizontal _ ->
            fprintf fmt {|%a %s %a|} pp_opt_named_type_term
              from_type_with_layout arrow_symbol pp_opt_named_type_term
              to_type_with_layout
        | Vertical _ ->
            pp_latex_array "c" fmt
              [
                [
                  (fun fmt -> pp_opt_named_type_term fmt from_type_with_layout);
                ];
                [ (fun fmt -> pp_print_string fmt arrow_symbol) ];
                [ (fun fmt -> pp_opt_named_type_term fmt to_type_with_layout) ];
              ]
        | Unspecified -> assert false)

  and pp_named_type_term fmt ((name, term), layout) =
    fprintf fmt {|\overtext{%a}{%s}|} pp_type_term (term, layout)
      (Text.spec_var_to_latex_var ~font_type:TextTT name)

  and pp_opt_named_type_term fmt ((name_opt, term), layout) =
    match name_opt with
    | None -> fprintf fmt {|%a|} pp_type_term (term, layout)
    | Some name -> pp_named_type_term fmt ((name, term), layout)

  and pp_opt_named_type_terms fmt (opt_type_terms, layout) =
    pp_sep_elements ~sep:", " pp_opt_named_type_term layout fmt opt_type_terms

  and pp_record_fields fmt (fields, layout) =
    let layout = Layout.horizontal_if_unspecified layout fields in
    let field_layouts =
      match layout with
      | Horizontal l | Vertical l -> l
      | Unspecified -> assert false
    in
    let fields_with_layouts =
      List.map2
        (fun (field_name, field_term) layout ->
          (field_name, (field_term, layout)))
        fields field_layouts
    in
    match layout with
    | Vertical _ ->
        let field_pp_funs =
          List.map
            (fun (field_name, (field_term, layout)) ->
              [
                (fun fmt ->
                  pp_print_string fmt (field_name_to_latex field_name));
                (fun fmt -> pp_print_string fmt ":");
                (fun fmt -> pp_type_term fmt (field_term, layout));
              ])
            fields_with_layouts
        in
        fprintf fmt {|%a|}
          (pp_parenthesized Braces true (pp_latex_array "lcl"))
          field_pp_funs
    | Horizontal _ ->
        let pp_field fmt (field_name, (field_term, layout)) =
          fprintf fmt {|%s : %a|}
            (field_name_to_latex field_name)
            pp_type_term (field_term, layout)
        in
        fprintf fmt {|%a|}
          (pp_parenthesized Braces true (PP.pp_sep_list ~sep:", " pp_field))
          fields_with_layouts
    | Unspecified -> assert false

  let pp_type_term_union fmt (terms, layout) =
    if Utils.is_singleton_list terms then
      fprintf fmt {|%a|} pp_type_term (List.hd terms, layout)
    else
      let layout = Layout.horizontal_if_unspecified layout terms in
      let terms_with_layouts = extend_args_with_layouts terms layout in
      match layout with
      | Vertical _ ->
          let term_pp_funs =
            List.mapi
              (fun term_counter (term, layout) ->
                [
                  (fun fmt -> pp_type_term fmt (term, layout));
                  (if term_counter < List.length terms - 1 then fun fmt ->
                     pp_print_string fmt {|\cup|}
                   else fun _fmt -> ());
                ])
              terms_with_layouts
          in
          pp_latex_array "ll" fmt term_pp_funs
      | Horizontal _ ->
          fprintf fmt {|\left(%a\right)|}
            (PP.pp_sep_list ~sep:{| \cup |} pp_type_term)
            terms_with_layouts
      | Unspecified -> assert false

  (** Renders the mathematical formula for the relation signature [def] using
      [layout] and referencing elements in [S.spec]. *)
  let pp_relation_math layout fmt { Relation.name; property; input; output } =
    (* Reuse the rendering for type terms. *)
    let input_as_labelled_tuple =
      LabelledTuple { label_opt = Some name; components = input }
    in
    (* If a layout is unspecified, expand one level to a 2-element horizontal layout. *)
    let layout = Layout.horizontal_if_unspecified layout [ (); () ] in
    let property_symbol =
      match property with
      | RelationProperty_Relation -> {|\bigtimes|}
      | RelationProperty_Function -> {|\longrightarrow|}
    in
    match layout with
    | Horizontal [ input_layout; output_layout ] ->
        fprintf fmt {|%a \;%s\; %a|} pp_type_term
          (input_as_labelled_tuple, input_layout)
          property_symbol pp_type_term_union (output, output_layout)
    | Vertical [ input_layout; output_layout ] ->
        pp_latex_array "c" fmt
          [
            [
              (fun fmt ->
                pp_type_term fmt (input_as_labelled_tuple, input_layout));
            ];
            [ (fun fmt -> pp_print_string fmt property_symbol) ];
            [ (fun fmt -> pp_type_term_union fmt (output, output_layout)) ];
          ]
    | _ -> assert false

  let pp_relation fmt ({ Relation.name; property; input; output } as def) =
    let input_vars = vars_of_opt_named_type_terms input in
    let output_vars = List.map vars_of_type_term output |> List.concat in
    let vars = input_vars @ output_vars in
    let instantiated_prose_description =
      substitute_spec_vars_by_latex_vars ~math_mode:true
        (Relation.prose_description def)
        vars
      (* necessary to avoid spurious line breaks. *)
      |> Text.shrink_whitespace
    in
    let layout = Layout.math_layout_for_node (Node_Relation def) in
    let hyperlink_target = hyperlink_target_for_id name in
    let relation_property_description =
      match property with
      | RelationProperty_Relation -> "relation"
      | RelationProperty_Function -> "function"
    in
    fprintf fmt
      {|\DefineRelation{%s}{@.
The %s
\[@.%a%a@.\]
%a@.} %% EndDefineRelation|}
      name relation_property_description pp_mathhypertarget hyperlink_target
      (pp_relation_math layout) def pp_print_text instantiated_prose_description

  let pp_variant fmt ({ TypeVariant.term } as variant) =
    let layout =
      match TypeVariant.math_layout variant with
      | Some layout -> layout
      | None -> Layout.default_for_type_term term
    in
    fprintf fmt "%a" pp_type_term (term, layout)

  (** [pp_define_type_wrapper name fmt pp_value value] renders a wrapper around
      the rendering of a type definition for the type. The wrapper uses the
      LaTeX macro [\DefineType{name}{...}] to define the type [name] with the
      content rendered by [pp_value fmt value]. *)
  let pp_define_type_wrapper name fmt pp_value value =
    fprintf fmt {|\DefineType{%s}{|} name;
    pp_value fmt value;
    fprintf fmt {|} %% EndDefineType|}

  let pp_type_and_variants ?(is_first = true) ?(is_last = true) fmt
      ({ Type.type_kind; Type.name }, variants) =
    let macro = get_or_gen_math_macro name in
    let hyperlink_target = hyperlink_target_for_id name in
    let equality_symbol, join_symbol =
      match type_kind with
      | TypeKind_AST -> ({|\derives|}, "|")
      | TypeKind_Generic -> ({|\triangleq|}, {|\cup|})
    in
    let first_variant, variants_tail =
      match variants with
      | [] -> (* Expected to be called with non-empty list *) assert false
      | first_variant :: variants_tail -> (first_variant, variants_tail)
    in
    let variant_hyperlink_targets =
      List.map
        (fun variant ->
          let variant_name_opt = Spec.variant_to_label_opt variant in
          Option.map
            (fun variant_name -> hyperlink_target_for_id variant_name)
            variant_name_opt)
        variants
    in
    let hd_hypertarget_opt = List.hd variant_hyperlink_targets in
    let tl_variant_hypertarget_opts =
      Utils.list_tl_or_empty variant_hyperlink_targets
    in
    let _render_begin_flalign =
      if is_first then fprintf fmt {|@.\begin{flalign*}|} else ()
    in
    let _render_newline = if not is_first then fprintf fmt {|\\|} else () in
    let _first_line =
      fprintf fmt {|@.%s%a %s\ & %a%a|} macro pp_texthypertarget
        hyperlink_target equality_symbol
        (Format.pp_print_option pp_mathhypertarget)
        hd_hypertarget_opt pp_variant first_variant
    in
    let _add_latex_line_break_only_if_more_variants =
      if List.length variants > 1 then fprintf fmt {|\\@.|}
      else fprintf fmt {|@.|}
    in
    let variant_and_hypertargets =
      List.combine variants_tail tl_variant_hypertarget_opts
    in
    let num_variants_tail = List.length variant_and_hypertargets in
    let _render_variants_tail =
      List.iteri
        (fun counter (variant_opt, hyptarget_opt) ->
          let () =
            fprintf fmt {|@[<h>%s\ & %a@,%a@]|} join_symbol pp_variant
              variant_opt
              (Format.pp_print_option pp_mathhypertarget)
              hyptarget_opt
          in
          let _add_latex_line_break_except_on_last_line =
            if counter < num_variants_tail - 1 then fprintf fmt {|\\@.|}
            else fprintf fmt {||}
          in
          ())
        variant_and_hypertargets
    in
    let _end_flalign = if is_last then fprintf fmt {|\end{flalign*}|} else () in
    ()

  let pp_basic_type fmt { Type.name } =
    let macro = get_or_gen_math_macro name in
    let hyperlink_target = hyperlink_target_for_id name in
    fprintf fmt {|%a$%s$|} pp_texthypertarget hyperlink_target macro

  let pp_type fmt ({ Type.name; variants } as def) =
    match variants with
    | [] ->
        (* A basic type like `typedef A` *)
        pp_define_type_wrapper name fmt pp_basic_type def
    | _ :: _ ->
        (* A complex type like `typedef A = V1 | ... | Vk` *)
        pp_define_type_wrapper name fmt pp_type_and_variants (def, variants)

  let pp_pointer ~is_first ~is_last fmt { TypesRender.type_name; variant_names }
      =
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
    pp_type_and_variants ~is_first ~is_last fmt (def, selected_variants)

  let pp_pointers fmt pointers =
    let num_pointers = List.length pointers in
    List.iteri
      (fun i pointer ->
        pp_pointer ~is_first:(i = 0) ~is_last:(i = num_pointers - 1) fmt pointer)
      pointers

  let pp_render_types fmt { TypesRender.name; pointers } =
    fprintf fmt {|\DefineRenderTypes{%s}{%a
} %% EndDefineRenderTypes|} name
      pp_pointers pointers

  module RenderRule = struct
    open Rule

    let infix_operator_to_string = function
      | Operator_Assign -> "\\eqdef"
      | Operator_Equal -> "="
      | Operator_Iff -> "\\leftrightarrow"
      | _ -> assert false

    let prefix_operator_to_string = function
      | Operator_List -> "\\LIST"
      | Operator_Size -> "\\listlen"
      | Operator_Union -> "\\cup"
      | Operator_Some -> "\\some"
      | _ -> ""

    let pp_field_path fmt path =
      let latex_path =
        List.map (fun field_name -> field_name_to_latex field_name) path
      in
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ".")
        pp_print_string fmt latex_path

    let rec pp_expr fmt (expr, layout) =
      match expr with
      | Var name ->
          pp_print_string fmt
            (Text.spec_var_to_latex_var ~font_type:Text.TextTT name)
      | Application { lhs = Operator op; args = [ lhs; rhs ] }
        when is_infix_operator op ->
          let layout = Layout.horizontal_if_unspecified layout [ lhs; rhs ] in
          let operands_with_layouts =
            extend_args_with_layouts [ lhs; rhs ] layout
          in
          let lhs_with_layout, rhs_with_layout =
            Utils.list_match_two_elements operands_with_layouts
          in
          fprintf fmt {|%a %s %a|} pp_expr lhs_with_layout
            (infix_operator_to_string op)
            pp_expr rhs_with_layout
      | Application { lhs = Operator op; args } when is_prefix_operator op ->
          let args_with_layouts = extend_args_with_layouts args layout in
          fprintf fmt {|%s{%a}|}
            (prefix_operator_to_string op)
            (PP.pp_sep_list ~sep:", " pp_expr)
            args_with_layouts
      | Application { lhs; args } ->
          let pp_lhs fmt lhs =
            match lhs with
            | Tuple ->
                () (* Effectively, no left-hand-side so nothing to render. *)
            | Relation name -> pp_print_string fmt (get_or_gen_math_macro name)
            | TupleLabel name ->
                pp_print_string fmt (get_or_gen_math_macro name)
            | Fields path -> pp_field_path fmt path
            | Operator _ | Unresolved _ -> assert false
          in
          fprintf fmt {|%a%a|} pp_lhs lhs
            (pp_parenthesized Parens true
               (pp_sep_elements ~sep:"," pp_expr layout))
            args
      | Record { label; fields } -> (
          let layout = Layout.vertical_if_unspecified layout fields in
          let field_exprs = List.map snd fields in
          let field_exprs_with_layouts =
            extend_args_with_layouts field_exprs layout
          in
          let field_names_and_exprs_with_layouts =
            List.combine (List.map fst fields) field_exprs_with_layouts
          in
          match layout with
          | Horizontal _ ->
              fprintf fmt {|%s%a|}
                (get_or_gen_math_macro label)
                (pp_parenthesized Brackets true
                   (PP.pp_sep_list ~sep:", "
                      (fun fmt (field, expr_with_layout) ->
                        fprintf fmt {|%s : %a|}
                          (field_name_to_latex field)
                          pp_expr expr_with_layout)))
                field_names_and_exprs_with_layouts
          | Vertical _ ->
              let field_pp_funs =
                List.map
                  (fun (field_name, (field_expr, layout)) ->
                    [
                      (fun fmt ->
                        pp_print_string fmt (field_name_to_latex field_name));
                      (fun fmt -> pp_print_string fmt ":");
                      (fun fmt -> pp_expr fmt (field_expr, layout));
                    ])
                  field_names_and_exprs_with_layouts
              in
              fprintf fmt {|%s%a|}
                (get_or_gen_math_macro label)
                (pp_parenthesized Braces true (pp_latex_array "lcl"))
                field_pp_funs
          | Unspecified -> assert false)
      | ListIndex { var; index } ->
          fprintf fmt {|%s[%s]|}
            (Text.spec_var_to_latex_var ~font_type:TextTT var)
            (Text.spec_var_to_latex_var ~font_type:TextTT index)
      | FieldPath path -> pp_field_path fmt path

    let arrow_macro_for_category_opt category_opt =
      let open Relation in
      let macro_name =
        match category_opt with
        | Some RelationCategory_Typing -> "typearrow"
        | Some RelationCategory_Semantics -> "semanticsarrow"
        | None -> "rightarrow"
      in
      "\\" ^ macro_name

    let rec pp_judgment_form fmt (form, layout) =
      match form with
      | Expr expr -> pp_expr fmt (expr, layout)
      | Transition { lhs; rhs } -> (
          let arrow_macro =
            match lhs with
            | Application { lhs = Relation name; _ } ->
                let { Relation.category } = Spec.relation_for_id S.spec name in
                arrow_macro_for_category_opt category
            | _ ->
                (* TODO: shouldn't happen once all relations are defined, but for now
                the following code is a safe fallback. Once all relations have been
                defined: assert false *)
                arrow_macro_for_category_opt None
          in
          let layout = Layout.horizontal_if_unspecified layout [ lhs; rhs ] in
          let lhs_with_layout, rhs_with_layout =
            Utils.list_match_two_elements
              (extend_args_with_layouts [ lhs; rhs ] layout)
          in
          match layout with
          | Horizontal _ ->
              fprintf fmt {|%a \;%s\; %a|} pp_expr lhs_with_layout arrow_macro
                pp_expr rhs_with_layout
          | Vertical _ ->
              pp_latex_array "r" fmt
                [
                  [
                    (fun fmt ->
                      pp_expr fmt lhs_with_layout;
                      pp_print_string fmt arrow_macro);
                  ];
                  [ (fun fmt -> pp_expr fmt rhs_with_layout) ];
                ]
          | _ -> assert false)
      | Indexed { index; list; body } ->
          fprintf fmt {|%s \in %s \;:\; %a|} index list pp_judgment_form
            (body, layout)
      | Output _ ->
          assert false (* Was supposed to be resolved into a transition. *)

    let pp_judgment fmt ({ Rule.form } as judgment) =
      let layout = Rule.judgment_layout judgment in
      fprintf fmt "{%a}" pp_judgment_form (form, layout)

    let pp_case_name_opt fmt name_opt =
      match name_opt with
      | None -> ()
      | Some name -> fprintf fmt "[%s]" (Text.escape_underscores name)

    let pp_math_expanded_rule fmt { ExpandRules.name_opt; judgments } =
      let premises, conclusion = Utils.split_last judgments in
      fprintf fmt
        {|\begin{mathpar}@.\inferrule%a{@.%a@.}{@.%a@.}@.\end{mathpar}|}
        pp_case_name_opt name_opt
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt "\\\\\\\\@.")
           pp_judgment)
        premises pp_judgment conclusion

    let pp_math_expanded_rules fmt expanded_rules =
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt "@.@.")
        pp_math_expanded_rule fmt expanded_rules

    let pp_render_rule fmt { RuleRender.name; relation_name; path } =
      let { Relation.rule_opt; category } =
        Spec.relation_for_id S.spec relation_name
      in
      let rule = Option.get rule_opt in
      let expanded_rules = ExpandRules.expand category rule in
      let expanded_subset =
        List.filter
          (fun { ExpandRules.name_opt; _ } ->
            match name_opt with
            | None -> true
            | Some expanded_path ->
                Utils.string_starts_with ~prefix:path expanded_path)
          expanded_rules
      in
      fprintf fmt
        {|\DefineRule{%s}{
\ProseParagraph

\FormallyParagraph
%a
} %% EndDefineRule|}
        name pp_math_expanded_rules expanded_subset
  end

  let pp_elem fmt = function
    | Elem_Constant def -> pp_constant_definition fmt def
    | Elem_Type def -> pp_type fmt def
    | Elem_Relation def -> pp_relation fmt def
    | Elem_RenderTypes def -> pp_render_types fmt def
    | Elem_RenderRule def -> RenderRule.pp_render_rule fmt def

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
            Text.spec_var_to_latex_var ~font_type:Text.Text name
          in
          match elem with
          | Elem_Constant _ ->
              fprintf fmt
                {|
\section*{%s}
\begin{lstlisting}
%a
\end{lstlisting}
\RenderConstant{%s}
|}
                latex_name PP.pp_elem elem name
          | Elem_Relation _ ->
              fprintf fmt
                {|
\section*{%s}
\begin{lstlisting}
%a
\end{lstlisting}
\RenderRelation{%s}
|}
                latex_name PP.pp_elem elem name
          | Elem_Type _ ->
              fprintf fmt
                {|
\section*{%s}
\begin{lstlisting}
%a
\end{lstlisting}
\RenderType{%s}
|}
                latex_name PP.pp_elem elem name
          | Elem_RenderTypes _ ->
              fprintf fmt
                {|
\section*{%s}
\begin{lstlisting}
%a
\end{lstlisting}
\RenderTypes{%s}
|}
                latex_name PP.pp_elem elem name
          | Elem_RenderRule _ ->
              fprintf fmt
                {|
\section*{%s}
\begin{lstlisting}
%a
\end{lstlisting}
\RenderRule{%s}
|}
                latex_name PP.pp_elem elem name)
        (Spec.ast S.spec)
    in
    let _print_footer = fprintf fmt {|@.\end{document}@.|} in
    ()

  (* Renders macro to render the symbol defined by [name]. *)
  let pp_id_macro fmt name =
    let hyperlink_target = hyperlink_target_for_id name in
    let node = Spec.defining_node_for_id S.spec name in
    let font_for_type_kind = function
      | TypeKind_Generic -> Text.TextSF
      | TypeKind_AST -> Text.TextSC
    in
    let font_type =
      match node with
      | Node_Relation _ -> Text.TextIT
      | Node_Type _ -> Text.TextSF
      | Node_TypeVariant { TypeVariant.type_kind } ->
          font_for_type_kind type_kind
      | Node_Constant _ -> Text.TextSF
    in
    if Option.is_some (Spec.math_macro_opt_for_node node) then ()
    else
      let typeset_macro = Text.spec_var_to_latex_var ~font_type name in
      fprintf fmt
        {|\newcommand%s[0]{ \hyperlink{%s}{%s} } %% Generated from %s|}
        (get_or_gen_math_macro name)
        hyperlink_target typeset_macro name

  let generate_latex_macros fmt =
    let open AST in
    let defined_ids = Spec.defined_ids S.spec in
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
        defined_ids
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
          pp_elem fmt elem;
          fprintf fmt "\n\n")
        (Spec.ast S.spec)
    in
    let _footer = fprintf fmt {|@.|} in
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
