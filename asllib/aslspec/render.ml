(** A module for rendering an ASL semantics-specification for inclusion in a
    LaTeX document. *)

open AST
open Format
open Latex
open LayoutUtils
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
    let node = Spec.defining_node_for_id S.spec id in
    let math_macro_opt = Spec.math_macro_opt_for_node node in
    match math_macro_opt with
    | Some str -> str
    | None -> Latex.elem_name_to_math_macro id

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

  (** [hyperlink_target_for_id id] returns a string [target] that can be used
      for the LaTeX [\hypertarget{target}{}] for [id]. *)
  let hyperlink_target_for_id id =
    let open Spec in
    let name_id = Latex.remove_underscores id in
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

  (** [pp_constant_definition fmt def] renders the definition of the constant
      given by [def] with the formatter [fmt]. *)
  let pp_constant_definition fmt { Constant.name } =
    fprintf fmt {|$%a$|} pp_id_as_macro name

  (** [pp_constant_definition_macro fmt def] renders the LaTeX wrapper macro
      [\DefineConstant{name}{...}] around the rendering of a constant definition
      [def] with the formatter [fmt].*)
  let pp_constant_definition_macro fmt ({ Constant.name } as def) =
    let hyperlink_target = hyperlink_target_for_id name in
    fprintf fmt {|\DefineConstant{%s}{%a%a} %% EndDefineConstant|} name
      pp_texthypertarget hyperlink_target pp_constant_definition def

  (** Renders the application of the type operator [op] to [arg] with [fmt].
      [arg] is rendered by [pp_arg fmt arg]. *)
  let pp_type_operator fmt op pp_arg arg =
    let operator_to_macro_name = function
      | Powerset -> "pow"
      | Powerset_Finite -> "powfin"
      | List0 -> "KleeneStar"
      | List1 -> "KleenePlus"
      | Option -> "Option"
    in
    pp_one_arg_macro fmt (operator_to_macro_name op) pp_arg arg

  (** [pp_type_term fmt (type_term, layout)] renders [type_term] with [fmt],
      laid out according to [layout]. *)
  let rec pp_type_term fmt (type_term, layout) =
    match type_term with
    | Label name -> (
        match get_short_circuit_macro name with
        | Some short_circuit_macro ->
            over_text fmt pp_id_as_macro name pp_print_string
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
        fprintf fmt {|%a%a|} pp_id_opt_as_macro label_opt
          (pp_fields pp_type_term) (fields, layout)
    | ConstantsSet constant_names ->
        let layout_contains_vertical = LayoutUtils.contains_vertical layout in
        fprintf fmt {|%a|}
          (pp_parenthesized Braces layout_contains_vertical
             (PP.pp_sep_list ~sep:", " pp_id_as_macro))
          constant_names
    | Function { from_type; to_type; total } ->
        let arrow_macro_name =
          if total then {|rightarrow|} else {|partialto|}
        in
        pp_connect_pair ~alignment:"r" fmt pp_opt_named_type_term from_type
          arrow_macro_name pp_opt_named_type_term to_type layout

  (** [pp_opt_named_type_term fmt ((name_opt, term), layout)] formats the type
      term [term] with the optional name [name_opt] above it using [fmt] and
      laid out according to [layout]. *)
  and pp_opt_named_type_term fmt ((name_opt, term), layout) =
    match name_opt with
    | None -> fprintf fmt {|%a|} pp_type_term (term, layout)
    | Some name -> over_text fmt pp_type_term (term, layout) pp_var name

  (** [pp_opt_named_type_terms fmt (opt_type_terms, layout)] formats a list of
      optionally-named type terms [opt_type_terms] using [fmt] and laid out
      according to [layout]. *)
  and pp_opt_named_type_terms fmt (opt_type_terms, layout) =
    pp_sep_elements ~pp_sep:pp_comma ~alignment:"c" pp_opt_named_type_term
      layout fmt opt_type_terms

  (** [pp_output_types fmt (terms, layout)] renders the relation output [terms]
      in with the given [layout]. *)
  let pp_output_types fmt (terms, layout) =
    if Utils.is_singleton_list terms then
      fprintf fmt {|%a|} pp_type_term (List.hd terms, layout)
    else
      let pp_multiple_terms fmt (terms, layout) =
        pp_connect_elements
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
      | RelationProperty_Relation -> "bigtimes"
      | RelationProperty_Function -> "longrightarrow"
    in
    pp_connect_pair ~alignment:"c" fmt pp_type_term input_as_labelled_tuple
      property_macro_name pp_output_types output layout

  let pp_relation_definition fmt
      ({ Relation.name; property; input; output } as def) =
    let input_vars = vars_of_opt_named_type_terms input in
    let output_vars = List.map vars_of_type_term output |> List.concat in
    let vars = input_vars @ output_vars in
    let instantiated_prose_description =
      substitute_spec_vars_by_latex_vars ~math_mode:true
        (Relation.prose_description def)
        vars
      (* necessary to avoid spurious line breaks. *)
      |> Latex.shrink_whitespace
    in
    let layout = Layout.math_layout_for_node (Node_Relation def) in
    let hyperlink_target = hyperlink_target_for_id name in
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
      | None -> Layout.default_for_type_term term
    in
    fprintf fmt "%a" pp_type_term (term, layout)

  let pp_type_and_variants ?(is_first = true) ?(is_last = true) fmt
      ({ Type.type_kind; Type.name }, variants) =
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
      fprintf fmt {|@.%a%a %s\ & %a%a|} pp_id_as_macro name pp_texthypertarget
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

  (** [pp_basic_type fmt basic_type] renders the basic type (a type with no
      variants) [basic_type] with the formatter [fmt]. *)
  let pp_basic_type fmt { Type.name } =
    let hyperlink_target = hyperlink_target_for_id name in
    fprintf fmt {|%a$%a$|} pp_texthypertarget hyperlink_target pp_id_as_macro
      name

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

  (** [pp_pointers fmt pointers] renders [pointers] - a list of subsets of type
      definitions - with the formatter [fmt]. *)
  let pp_pointers fmt pointers =
    let num_pointers = List.length pointers in
    List.iteri
      (fun i pointer ->
        pp_pointer ~is_first:(i = 0) ~is_last:(i = num_pointers - 1) fmt pointer)
      pointers

  (** [pp_render_types fmt render_types] renders the named list of subsets of
      type definitions [render_types] with the formatter [fmt]. *)
  let pp_render_types fmt { TypesRender.pointers } = pp_pointers fmt pointers

  (** [pp_render_types_macro fmt def] renders the LaTeX wrapper macro
      [\DefineRenderTypes{name}{...}] around the rendering of a list of subsets
      of type definitions [def] with the formatter [fmt]. *)
  let pp_render_types_macro fmt def =
    fprintf fmt {|\DefineRenderTypes{%s}{%a
} %% EndDefineRenderTypes|}
      def.TypesRender.name pp_render_types def

  (** [pp_elem fmt elem] renders an element of the specification. *)
  let pp_elem fmt = function
    | Elem_Constant def -> pp_constant_definition fmt def
    | Elem_Type def -> pp_type_definition fmt def
    | Elem_Relation def -> pp_relation_definition fmt def
    | Elem_RenderTypes def -> pp_render_types fmt def

  (** [pp_elem_definition_macro fmt elem] renders a macro definition for an
      element of the specification. *)
  let pp_elem_definition_macro fmt = function
    | Elem_Constant def -> pp_constant_definition_macro fmt def
    | Elem_Type def -> pp_type_definition_macro fmt def
    | Elem_Relation def -> pp_relation_definition_macro fmt def
    | Elem_RenderTypes def -> pp_render_types_macro fmt def

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
              fprintf fmt {|
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
|} latex_name pp_elem elem)
        (Spec.ast S.spec)
    in
    let _print_footer = fprintf fmt {|@.\end{document}@.|} in
    ()

  (** [pp_id_macro fmt id] renders the LaTeX macro corresponding to the element
      defined for [id] with the formatter [fmt]. *)
  let pp_id_macro fmt id =
    let hyperlink_target = hyperlink_target_for_id id in
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
          pp_elem_definition_macro fmt elem;
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
