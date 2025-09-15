(** A module for rendering an ASL semantics-specification
    for inclusion in a LaTeX document.
*)

open AST
open Format
open Text
open Spec

(** Returns the math macro given for the element defined by [id],
    unless none is given in which case it generates a math macro name
    and returns it.
*)
let get_or_gen_math_macro spec id =
  let node = Spec.defining_node_for_id spec id in
  let math_macro_opt = Spec.math_macro_opt_for_node node in
  match math_macro_opt with
  | Some str -> str
  | None -> Text.elem_name_to_math_macro id

let pp_hypertarget fmt target_str =
  fprintf fmt {|\hypertarget{%s}{}|} target_str

(** [hyperlink_target_for_id spec id] returns a string [target] that can be used for the LaTeX
    [\hypertarget{target}{}] for [id].
    This is not needed when the [id] is a spec element with a [math_macro] attribute.
*)
let hyperlink_target_for_id spec id =
  let open Spec in
  let name_id = Text.remove_underscores id in
  let category =
    match defining_node_for_id spec id with
    | Node_Relation _ -> "relation"
    | Node_Type def -> (
        match Type.kind def with
        | TypeKind_Generic -> "type"
        | TypeKind_AST -> "ast")
    | Node_TypeVariant def -> (
        match TypeVariant.kind def with
        | TypeKind_Generic -> "type"
        | TypeKind_AST -> "ast")
    | Node_Constant _ -> "constant"
  in
  sprintf "%s-%s" category name_id

(** [vars_of_type_term term] returns the list of term-naming variables
    that occur at any depth inside [term]. *)
let rec vars_of_type_term term =
  let listed_vars =
    match term with
    | Label var -> [ var ]
    | Powerset sub_term -> vars_of_type_term sub_term
    | Option sub_term -> vars_of_type_term sub_term
    | Tuple components | LabelledTuple { components } ->
        vars_of_opt_named_type_terms components
    | Record fields | LabelledRecord { fields } ->
        vars_of_named_type_terms fields
    | List { member_type } -> vars_of_type_term member_type
    | ConstantsSet _ -> []
    | Function { from_type; to_type } ->
        vars_of_type_term from_type @ vars_of_type_term to_type
  in
  List.sort_uniq String.compare listed_vars

(** [vars_of_opt_named_type_terms named_terms] returns the list of term-naming variables
    that occur at any depth inside [opt_named_terms]. *)
and vars_of_opt_named_type_terms opt_named_terms =
  List.map
    (fun (name_opt, term) ->
      let term_vars = vars_of_type_term term in
      match name_opt with Some name -> name :: term_vars | None -> term_vars)
    opt_named_terms
  |> List.concat

and vars_of_named_type_terms named_terms =
  List.map (fun (name, term) -> (Some name, term)) named_terms
  |> vars_of_opt_named_type_terms

(** [substitute_spec_vars_by_latex_vars math_mode s vars] returns a string [s]
    with every variable like [my_var] is substituted into [\texttt{my\_var}],
    which makes the returned string suitable to typsetting with LaTeX.
    If [math_mode] is true then the result is surrounded by [$$].
*)
let substitute_spec_vars_by_latex_vars ~math_mode str vars =
  let open Text in
  let substitutions =
    List.map
      (fun var_str ->
        let latex_var = spec_var_to_latex_var ~font_type:TextTT var_str in
        let latex_var =
          if math_mode then to_math_mode latex_var else latex_var
        in
        (spec_var_to_template_var var_str, latex_var))
      vars
  in
  apply_substitutions str substitutions

type parenthesis = Parens | Braces | Brackets

let pp_parenthesized parenthesis large pp_elem fmt elem =
  if large then
    match parenthesis with
    | Parens -> fprintf fmt {|\left(%a\right)|} pp_elem elem
    | Braces -> fprintf fmt {|\left\{%a\right\}|} pp_elem elem
    | Brackets -> fprintf fmt {|\left[%a\right]|} pp_elem elem
  else
    match parenthesis with
    | Parens -> fprintf fmt {|(%a)|} pp_elem elem
    | Braces -> fprintf fmt {|\{%a\}|} pp_elem elem
    | Brackets -> fprintf fmt {|[%a]|} pp_elem elem

(** Renders an instance of the constant [name] in a term. *)
let pp_constant_instance spec fmt name =
  let macro = get_or_gen_math_macro spec name in
  pp_print_string fmt macro

(** Renders the definition of the constant given by [def]. *)
let pp_constant_definition spec fmt def =
  let name = Constant.name def in
  let macro = get_or_gen_math_macro spec name in
  let hyperlink_target = hyperlink_target_for_id spec name in
  fprintf fmt {|\hypertarget{%s}{} $%s$|} hyperlink_target macro

(** [pp_latex_array fmt rows]
    renders a table of elements using a LaTeX array environment.
    The elements are assumed to be organized by the (top-level) list
    [rows] representing the table rows.
    Each (second-level) list represents a column where the elements
    are formatting functions invoked with [fmt].
*)
let pp_latex_array fmt rows =
  let () = assert (not (Utils.list_is_empty rows)) in
  let num_columns = List.length (List.hd rows) in
  let rows_argument = String.init num_columns (fun _ -> 'l') in
  let () = fprintf fmt {|\begin{array}{%s}@.|} rows_argument in
  let () =
    let num_rows = List.length rows in
    List.iteri
      (fun row_index row ->
        List.iter (fun cell_fun -> cell_fun fmt) row;
        (* emit a LaTeX line break, except for the last line. *)
        if row_index < num_rows - 1 then fprintf fmt {|\\@.|}
        else fprintf fmt {|@.|})
      rows
  in
  let () = fprintf fmt {|\end{array}|} in
  ()

(** [pp_type_term mode spec term] formats [term] into a string
    suitable for LaTeX math mode.
*)
let rec pp_type_term spec fmt (type_term, layout) =
  let () =
    if false then
      fprintf Format.std_formatter "%a with original layout %a@."
        PP.pp_type_term type_term PP.pp_math_shape layout
  in
  let layout_contains_vertical = Layout.contains_vertical layout in
  match type_term with
  | Label name -> pp_print_string fmt (get_or_gen_math_macro spec name)
  | Powerset sub_term ->
      let powerset_macro =
        if layout_contains_vertical then {|\Pow|} else {|\pow|}
      in
      fprintf fmt {|%s{%a}|} powerset_macro (pp_type_term spec)
        (sub_term, layout)
  | Option sub_term ->
      let optional_macro =
        if layout_contains_vertical then {|\Some|} else {|\some|}
      in
      fprintf fmt {|%s{%a}|} optional_macro (pp_type_term spec)
        (sub_term, layout)
  | Tuple components -> (
      match components with
      | [] -> assert false
      | [ term ] -> fprintf fmt "%a" (pp_opt_named_type_term spec) (term, layout)
      | _ :: _ ->
          fprintf fmt "%a"
            (pp_parenthesized Parens layout_contains_vertical
               (pp_opt_named_type_terms spec))
            (components, layout))
  | LabelledTuple { label; components } ->
      fprintf fmt "%s%a"
        (get_or_gen_math_macro spec label)
        (pp_parenthesized Parens layout_contains_vertical
           (pp_opt_named_type_terms spec))
        (components, layout)
  | Record fields -> pp_record_fields spec fmt (fields, layout)
  | LabelledRecord { label; fields } ->
      fprintf fmt {|%s%a|}
        (get_or_gen_math_macro spec label)
        (pp_record_fields spec) (fields, layout)
  | List { maybe_empty; member_type } ->
      let iteration_macro =
        if maybe_empty then {|\KleeneStar|} else {|\KleenePlus|}
      in
      fprintf fmt {|%s{%a}|} iteration_macro (pp_type_term spec)
        (member_type, layout)
  | ConstantsSet constant_names ->
      fprintf fmt {|%a|}
        (pp_parenthesized Braces layout_contains_vertical
           (PP.pp_sep_list ~sep:", " (pp_constant_instance spec)))
        constant_names
  | Function { from_type; to_type; total } -> (
      let layout = Layout.horizontal_for_list layout [ from_type; to_type ] in
      let arrow_symbol = if total then {|\rightarrow|} else {|\partialto|} in
      let layout_list =
        match layout with
        | Horizontal l | Vertical l -> l
        | Unspecified -> assert false
      in
      let input_layout = List.nth layout_list 0 in
      let output_layout = List.nth layout_list 1 in
      match layout with
      | Horizontal _ ->
          fprintf fmt {|%a %s %a|} (pp_type_term spec) (from_type, input_layout)
            arrow_symbol (pp_type_term spec) (to_type, output_layout)
      | Vertical _ ->
          fprintf fmt {|\begin{array}{c}@.%a %s\\@.%a@.\end{array}@.|}
            (pp_type_term spec) (from_type, input_layout) arrow_symbol
            (pp_type_term spec) (to_type, output_layout)
      | Unspecified -> assert false)

and pp_named_type_term spec fmt ((name, term), layout) =
  fprintf fmt {|\overtext{%a}{%s}|} (pp_type_term spec) (term, layout)
    (Text.spec_var_to_prose name)

and pp_opt_named_type_term spec fmt ((name_opt, term), layout) =
  match name_opt with
  | None -> fprintf fmt {|%a|} (pp_type_term spec) (term, layout)
  | Some name -> pp_named_type_term spec fmt ((name, term), layout)

and pp_opt_named_type_terms spec fmt (opt_type_terms, layout) =
  let layout = Layout.horizontal_for_list layout opt_type_terms in
  let term_layouts =
    match layout with
    | Horizontal l | Vertical l -> l
    | Unspecified -> assert false
  in
  let opt_terms_with_layouts = List.combine opt_type_terms term_layouts in
  let num_terms = List.length opt_type_terms in
  match layout with
  | Horizontal _ ->
      List.iteri
        (fun i (opt_named_term, layout) ->
          (pp_opt_named_type_term spec) fmt (opt_named_term, layout);
          if i < num_terms - 1 then fprintf fmt ", " else ())
        opt_terms_with_layouts
  | Vertical _ ->
      fprintf fmt {|\begin{array}{c}@.|};
      List.iteri
        (fun i (opt_named_term, layout) ->
          (pp_opt_named_type_term spec) fmt (opt_named_term, layout);
          if i < num_terms - 1 then fprintf fmt {|,\\@.|}
          else fprintf fmt {|,@.|})
        opt_terms_with_layouts;
      fprintf fmt {|\end{array}@.|}
  | Unspecified -> assert false

and pp_record_fields spec fmt (fields, layout) =
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
      let row_funs =
        List.map
          (fun (field_name, (field_term, layout)) ->
            [
              (fun fmt ->
                pp_print_string fmt
                  (Text.spec_var_to_latex_var ~font_type:Text field_name));
              (fun fmt -> pp_print_string fmt " &:& ");
              (fun fmt -> pp_type_term spec fmt (field_term, layout));
            ])
          fields_with_layouts
      in
      fprintf fmt {|\left\{%a\right\}|} pp_latex_array row_funs
  | Horizontal _ ->
      let pp_field fmt (field_name, (field_term, layout)) =
        fprintf fmt {|%s : %a|}
          (Text.spec_var_to_latex_var ~font_type:Text field_name)
          (pp_type_term spec) (field_term, layout)
      in
      fprintf fmt {|\left\{%a\right\}|}
        (PP.pp_sep_list ~sep:", " pp_field)
        fields_with_layouts
  | Unspecified -> assert false

let pp_type_term_union spec fmt (terms, layout) =
  if Utils.is_singleton_list terms then
    fprintf fmt {|%a|} (pp_type_term spec) (List.hd terms, layout)
  else
    let layout = Layout.horizontal_for_list layout terms in
    match layout with
    | Vertical layouts ->
        let terms_with_layouts = List.combine terms layouts in
        fprintf fmt
          {|\left(\begin{array}{ll}@[<hv>%a@] & \\@.\end{array}\right)|}
          (PP.pp_sep_list ~sep:{| & \cup \\@.|} (pp_type_term spec))
          terms_with_layouts
    | Horizontal layouts ->
        let terms_with_layouts = List.combine terms layouts in
        fprintf fmt {|\left(%a\right)|}
          (PP.pp_sep_list ~sep:{| \cup |} (pp_type_term spec))
          terms_with_layouts
    | Unspecified -> assert false

(** Renders the mathematical formula for the relation signature [def]
    using [layout] and referencing elements in [spec]. *)
let pp_relation_math spec layout fmt def =
  (* Reuse the rendering for type terms. *)
  let input_as_labelled_tuple =
    LabelledTuple { label = Relation.name def; components = Relation.input def }
  in
  (* If a layout is unspecified, expand one level to a 2-element horizontal layout. *)
  let layout = Layout.horizontal_for_list layout [ (); () ] in
  let output = Relation.output def in
  match layout with
  | Horizontal [ input_layout; output_layout ] ->
      fprintf fmt {|%a \;\bigtimes\; %a|} (pp_type_term spec)
        (input_as_labelled_tuple, input_layout)
        (pp_type_term_union spec) (output, output_layout)
  | Vertical [ input_layout; output_layout ] ->
      fprintf fmt {|\begin{array}{c}@.%a\\@.\bigtimes\\@.%a@.\end{array}|}
        (pp_type_term spec)
        (input_as_labelled_tuple, input_layout)
        (pp_type_term_union spec) (output, output_layout)
  | _ -> assert false

let pp_relation spec fmt def =
  let input_terms = Relation.input def in
  let input_vars = vars_of_opt_named_type_terms input_terms in
  let output_vars =
    List.map vars_of_type_term (Relation.output def) |> List.concat
  in
  let vars = input_vars @ output_vars in
  let instantiated_prose_description =
    substitute_spec_vars_by_latex_vars ~math_mode:true
      (Relation.prose_description def)
      vars
    (* not necessary but nice to have: *)
    |> Text.shrink_space_segments
  in
  let layout = Layout.math_layout_for_node (Node_Relation def) in
  let hyperlink_target = hyperlink_target_for_id spec (Relation.name def) in
  fprintf fmt {|%a
The relation
\[@.%a@.\]
%a|} pp_hypertarget hyperlink_target
    (pp_relation_math spec layout)
    def pp_print_text instantiated_prose_description

let vars_of_type_variant variant = vars_of_type_term (TypeVariant.term variant)

let vars_of_type def =
  List.map vars_of_type_variant (Type.variants def) |> List.concat

let pp_variant spec fmt variant =
  let term = TypeVariant.term variant in
  fprintf fmt "%a" (pp_type_term spec) (term, Layout.default_for_type_term term)

(**
  A type like `ast t = v1 | v2 | ... | vn` is roughly rendered as
\hypertarget{ast-t}{}\hypertarget{ast-t}{}
\begin{flalign*}
\t \derives\ & <v1-rendering> \hypertarget{ast-v2}{}\\
|\ & <v2-rendering> \hypertarget{ast-v3}{}\\
...
|\ & <vn-rendering>\\
\end{flalign*}
  *)
let pp_type spec fmt def =
  let name = Type.name def in
  let macro = get_or_gen_math_macro spec name in
  let hyperlink_target = hyperlink_target_for_id spec name in
  let variants = Type.variants def in
  let equality_symbol, join_symbol =
    match Type.kind def with
    | TypeKind_AST -> ({|\derives|}, "|")
    | TypeKind_Generic -> ({|\triangleq|}, {|\cup|})
  in
  match variants with
  | [] ->
      (* A basic type as in `typedef N` *)
      fprintf fmt {|%a
$%s$|} pp_hypertarget hyperlink_target macro
  | first_variant :: variants_tail ->
      (* A type defined by a list of variants *)
      let variant_hyperlink_targets =
        List.map
          (fun variant ->
            let variant_name_opt = Spec.variant_to_label_opt variant in
            Option.map
              (fun variant_name -> hyperlink_target_for_id spec variant_name)
              variant_name_opt)
          variants
      in
      let hd_hypertarget_opt = List.hd variant_hyperlink_targets in
      (* Every line containing a variant, includes the hypertarget
         of the next variant (to make LaTeX hyperlinks point to the right place).
         Since the hypertarget for the first variant appears above the table,
         we add one dummy optional to make the lists of
         hypertargets equal in length to the list of variants second-to-last.
      *)
      let tl_variant_hypertarget_opts =
        Utils.list_tl_or_empty variant_hyperlink_targets @ [ None ]
      in
      let second_hypertarget, rest_of_variants_hypertargets =
        match tl_variant_hypertarget_opts with
        | [] -> (None, [])
        | head :: tail -> (head, tail)
      in
      let _render_hypertarget_for_type = pp_hypertarget fmt hyperlink_target in
      let _render_hypertarget_for_first_variant =
        (Format.pp_print_option pp_hypertarget) fmt hd_hypertarget_opt
      in
      let _open_flalign_and_first_line =
        fprintf fmt {|
\begin{flalign*}
%s %s\ & %a%a|} macro equality_symbol
          (pp_variant spec) first_variant
          (Format.pp_print_option pp_hypertarget)
          second_hypertarget
      in
      let _add_latex_line_break_only_if_more_variants =
        if List.length variants > 1 then fprintf fmt {|\\@.|}
        else fprintf fmt {|@.|}
      in
      let variant_and_hypertargets =
        List.combine variants_tail rest_of_variants_hypertargets
      in
      let num_variants_tail = List.length variant_and_hypertargets in
      let _render_variants_tail =
        List.iteri
          (fun counter (variant_opt, hyptarget_opt) ->
            let () =
              fprintf fmt {|@[<h>%s\ & %a@,%a@]|} join_symbol (pp_variant spec)
                variant_opt
                (Format.pp_print_option pp_hypertarget)
                hyptarget_opt
            in
            let _add_latex_line_break_except_on_last_line =
              if counter < num_variants_tail - 1 then fprintf fmt {|\\@.|}
              else fprintf fmt {|@.|}
            in
            ())
          variant_and_hypertargets
      in
      let _close_flalign = fprintf fmt {|\end{flalign*}|} in
      ()

let pp_elem spec fmt = function
  | Elem_Type def -> pp_type spec fmt def
  | Elem_Relation def -> pp_relation spec fmt def
  | Elem_Constant def -> pp_constant_definition spec fmt def

let pp_elem_block spec fmt elem =
  let elem_name = Spec.elem_name elem in
  fprintf fmt
    {|
\section*{%s}
\subsection*{Specification}
\begin{lstlisting}
%a
\end{lstlisting}
\subsection*{Rendering}
%% BEGIN_GENERATED_ELEMENT(%s)
%a
%% END_GENERATED_ELEMENT@.|}
    (Text.spec_var_to_latex_var ~font_type:Text elem_name)
    PP.pp_elem elem elem_name (pp_elem spec) elem

(** Renders a LaTeX document containing all of the elements in [spec].
    A header and footer are added to enable compiling the generated file
    separately, for testing and debugging.
*)
let render_latex_elements spec =
  let generated_elements_filename = "generated_elements.tex" in
  let open AST in
  let file_channel = open_out_bin generated_elements_filename in
  let () =
    Fun.protect
      ~finally:(fun () -> close_out_noerr file_channel)
      (fun () ->
        let file_formatter = Format.formatter_of_out_channel file_channel in
        let _print_header =
          fprintf file_formatter
            {|\documentclass{book}
\input{ASLmacros.tex}
\input{generated_macros.tex}
\begin{document}@.|}
        in
        let _print_elements =
          List.iter
            (fun elem -> pp_elem_block spec file_formatter elem)
            (Spec.ast spec)
        in
        let _print_footer = fprintf file_formatter {|@.\end{document}@.|} in
        ())
  in
  Format.fprintf std_formatter
    "%sWrote LaTeX blocks for aslspec elements into %s\n%s" green
    generated_elements_filename reset_color

let pp_id_macro spec fmt name =
  let hyperlink_target = hyperlink_target_for_id spec name in
  let node = Spec.defining_node_for_id spec name in
  let font_for_type_kind = function
    | TypeKind_Generic -> Text.TextSF
    | TypeKind_AST -> Text.TextSC
  in
  let font_type =
    match node with
    | Node_Relation _ -> Text.TextIT
    | Node_Type _ -> Text.TextSF
    | Node_TypeVariant def ->
        let kind = TypeVariant.kind def in
        font_for_type_kind kind
    | Node_Constant _ -> Text.TextSF
  in
  if Option.is_some (Spec.math_macro_opt_for_node node) then ()
  else
    let typeset_macro = Text.spec_var_to_latex_var ~font_type name in
    fprintf fmt {|%% Generated from %s
\newcommand%s[0]{ \hyperlink{%s}{%s} }
|}
      name
      (get_or_gen_math_macro spec name)
      hyperlink_target typeset_macro

let generate_latex_macros spec =
  let generated_macros_filename = "generated_macros.tex" in
  let open AST in
  let file_channel = open_out_bin generated_macros_filename in
  let () =
    Fun.protect
      ~finally:(fun () -> close_out_noerr file_channel)
      (fun () ->
        let file_formatter = Format.formatter_of_out_channel file_channel in
        let defined_ids = Spec.defined_ids spec in
        List.iter (fun id -> pp_id_macro spec file_formatter id) defined_ids)
  in
  Format.fprintf std_formatter "%sGenerated LaTeX macros into %s\n%s" green
    generated_macros_filename reset_color

let render spec =
  let () = render_latex_elements spec in
  let () = generate_latex_macros spec in
  ()
