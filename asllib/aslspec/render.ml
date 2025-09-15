(** A module for rendering an ASL semantics-specification
    for inclusion in a LaTeX document.
*)

open AST
open Format
open Text

let is_singleton_list list = 1 == List.length list
let list_tl_or_empty list = match list with [] -> [] | _ :: t -> t

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
let rec pp_type_term ~type_def spec fmt = function
  | Label name -> pp_print_string fmt (get_or_gen_math_macro spec name)
  | Powerset sub_term ->
      fprintf fmt {|\pow{%a}|} (pp_type_term ~type_def spec) sub_term
  | Option sub_term ->
      fprintf fmt {|\Some{%a}|} (pp_type_term ~type_def spec) sub_term
  | Tuple components -> (
      match components with
      | [] -> assert false
      | [ term ] ->
          fprintf fmt "%a" (pp_opt_named_type_term ~type_def spec) term
      | _ :: _ ->
          fprintf fmt "(%a)"
            (PP.pp_sep_list ~sep:", " (pp_opt_named_type_term ~type_def spec))
            components)
  | LabelledTuple { label; components } ->
      fprintf fmt "%s(%a)"
        (get_or_gen_math_macro spec label)
        (PP.pp_sep_list ~sep:", " (pp_opt_named_type_term ~type_def spec))
        components
  | Record fields -> pp_record_fields spec fmt fields
  | LabelledRecord { label; fields } ->
      fprintf fmt {|%s%a|}
        (get_or_gen_math_macro spec label)
        (pp_record_fields spec) fields
  | List { maybe_empty; member_type } ->
      let iteration_macro =
        if maybe_empty then {|\KleeneStar|} else {|\KleenePlus|}
      in
      fprintf fmt {|%s{%a}|} iteration_macro
        (pp_type_term ~type_def spec)
        member_type
  | ConstantsSet constant_names ->
      fprintf fmt "\\{%a\\}"
        (PP.pp_sep_list ~sep:", " (pp_constant_instance spec))
        constant_names
  | Function { from_type; to_type; total } ->
      let arrow_symbol = if total then {|\rightarrow|} else {|\partialto|} in
      fprintf fmt "%a %s %a"
        (pp_type_term ~type_def spec)
        from_type arrow_symbol
        (pp_type_term ~type_def spec)
        to_type

and pp_named_type_term ~type_def spec fmt (name, term) =
  if type_def then
    fprintf fmt {|\overtext{%a}{%s}|}
      (pp_type_term ~type_def spec)
      term
      (Text.spec_var_to_prose name)
  else
    fprintf fmt {|\overname{%a}{%s}|}
      (pp_type_term ~type_def spec)
      term
      (Text.spec_var_to_latex_var ~font_type:Text.TextTT name)

and pp_opt_named_type_term ~type_def spec fmt (name_opt, term) =
  match name_opt with
  | None -> fprintf fmt {|%a|} (pp_type_term ~type_def spec) term
  | Some name -> pp_named_type_term ~type_def spec fmt (name, term)

and pp_record_fields spec fmt fields =
  let row_funs =
    List.map
      (fun (field_name, field_term) ->
        [
          (fun fmt ->
            pp_print_string fmt
              (Text.spec_var_to_latex_var ~font_type:Text field_name));
          (fun fmt -> pp_print_string fmt " &:& ");
          (fun fmt -> pp_type_term spec ~type_def:true fmt field_term);
        ])
      fields
  in
  fprintf fmt {|\left\{%a\right\}|} pp_latex_array row_funs

let pp_type_term_union ~stacked spec fmt terms =
  let stacked = if is_singleton_list terms then false else stacked in
  if stacked then
    fprintf fmt {|\left(\begin{array}{ll}@[<hv>%a@] & \\@.\end{array}\right)|}
      (PP.pp_sep_list ~sep:{| & \cup \\@.|} (pp_type_term ~type_def:false spec))
      terms
  else
    PP.pp_sep_list ~sep:{| \cup |} (pp_type_term ~type_def:false spec) fmt terms

let pp_relation spec fmt def =
  let input_terms = Relation.input def in
  let input_vars = vars_of_named_type_terms input_terms in
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
  let hyperlink_target = hyperlink_target_for_id spec (Relation.name def) in
  fprintf fmt {|%a
The relation
\[
%s(%a)
\;\bigtimes\;
%a
\]
%a|}
    pp_hypertarget hyperlink_target
    (get_or_gen_math_macro spec (Relation.name def))
    (PP.pp_sep_list ~sep:", " (pp_named_type_term ~type_def:false spec))
    (Relation.input def)
    (pp_type_term_union ~stacked:true spec)
    (Relation.output def) pp_print_text instantiated_prose_description

let vars_of_type_variant variant = vars_of_type_term (TypeVariant.term variant)

let vars_of_type def =
  List.map vars_of_type_variant (Type.variants def) |> List.concat

let pp_variant spec fmt variant =
  let term = TypeVariant.term variant in
  fprintf fmt "%a" (pp_type_term ~type_def:true spec) term

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
        list_tl_or_empty variant_hyperlink_targets @ [ None ]
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
  fprintf fmt {|
%% BEGIN_GENERATED_ELEMENT(%s)
%a
%% END_GENERATED_ELEMENT@.|}
    elem_name (pp_elem spec) elem

let render_latex_elements spec =
  let generated_elements_filename = "generated_elements.tex" in
  let open AST in
  let file_channel = open_out_bin generated_elements_filename in
  let () =
    Fun.protect
      ~finally:(fun () -> close_out_noerr file_channel)
      (fun () ->
        let file_formatter = Format.formatter_of_out_channel file_channel in
        List.iter
          (fun elem -> pp_elem_block spec file_formatter elem)
          (Spec.ast spec))
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
