(** A module for LaTeX formatting utilities. *)

open Format
open AST
open LayoutUtils

let regex_underscore = Str.regexp_string "_"
let regexp_spaces_segment = Str.regexp "[ \t]+"
let union_macro_name = "cup"

(** A LaTeX underscore escape sequence. *)
let escaped_underscore = {|\_|}

let shrink_whitespace str =
  let regexp_newline_segment = Str.regexp "[ \n\r]+" in
  Str.global_replace regexp_newline_segment " " str

let escape_underscores str =
  Str.global_replace regex_underscore escaped_underscore str

let underscore_to_space str = Str.global_replace regex_underscore " " str

type font_type = Text | TextTT | TextSF | TextSC | TextIT

(** [pp_one_arg_macro fmt macro_name pp_arg arg] renders a LaTeX macro named
    [macro_name] with one argument, applied to the argument is [arg], which is
    formatted using [pp_arg]. *)
let pp_one_arg_macro fmt macro_name pp_arg arg =
  fprintf fmt "\\%s{%a}" macro_name pp_arg arg

(** [pp_macro fmt macro_name] renders a LaTeX macro by prefixing it with a
    backslash. *)
let pp_macro fmt macro_name = fprintf fmt "\\%s" macro_name

(** [spec_var_to_latex_var ~font_type var_str] returns a version of the
    specification variable [var_str] suitable for LaTeX math mode using the
    specified [font_type]. *)
let spec_var_to_latex_var ~font_type var_str =
  if String.equal var_str "_" then "\\Ignore"
  else
    let font_macro_name =
      match font_type with
      | Text -> "text"
      | TextTT -> "texttt"
      | TextSF -> "textsf"
      | TextSC -> "textsc"
      | TextIT -> "textit"
    in
    Format.asprintf {|%a{%s}|} pp_macro font_macro_name
      (escape_underscores var_str)

let spec_var_to_prose var_str = underscore_to_space var_str
let to_math_mode str = "$" ^ str ^ "$"
let spec_var_to_template_var var_str = "{" ^ var_str ^ "}"
let remove_underscores str = Str.global_replace (Str.regexp_string "_") "" str
let elem_name_to_math_macro elem_name = "\\" ^ remove_underscores elem_name
let pp_comma = fun fmt () -> fprintf fmt ", "

(** [substitute_spec_vars_by_latex_vars math_mode s vars] returns a string [s]
    with every variable like [my_var] is substituted into [\texttt{my\_var}],
    which makes the returned string suitable to typsetting with LaTeX. If
    [math_mode] is true then the result is surrounded by [$$]. *)
let substitute_spec_vars_by_latex_vars ~math_mode str vars =
  let module StringMap = Map.Make (String) in
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

(** [pp_texthypertarget fmt target_str] renders a hypertarget for [target_str]
    suitable for inclusion in LaTeX text mode, using the formatter [fmt]. *)
let pp_texthypertarget fmt target_str =
  fprintf fmt {|\texthypertarget{%s}|} target_str

(** [pp_mathhypertarget fmt target_str] renders a hypertarget for [target_str]
    suitable for inclusion in LaTeX math mode, using the formatter [fmt]. *)
let pp_mathhypertarget fmt target_str =
  fprintf fmt {|\mathhypertarget{%s}|} target_str

(** A type enumerating different kinds of parentheses. *)
type parenthesis = Parens | Braces | Brackets

(** [pp_parenthesized parenthesis large pp_elem fmt elem] formats [elem] with
    the formatter [fmt] using [pp_elem] and surrounds it with the specified
    [parenthesis]. If [large] is true, then large versions of the parentheses
    are used. *)
let pp_parenthesized parenthesis large pp_elem fmt elem =
  let left = if large then {|\left|} else "" in
  let right = if large then {|\right|} else "" in
  match parenthesis with
  | Parens -> fprintf fmt "%s(%a%s)" left pp_elem elem right
  | Braces -> fprintf fmt "%s\\{%a%s\\}" left pp_elem elem right
  | Brackets -> fprintf fmt "%s[%a%s]" left pp_elem elem right

(** [over_text fmt fmt_top top fmt_bottom bottom] renders
    [\overtext{top}{bottom}] where [top] and [bottom] are formatted using
    [fmt_top] and [fmt_bottom] respectively. *)
let over_text fmt fmt_top top fmt_bottom bottom =
  fprintf fmt {|\overtext{%a}{%a}|} fmt_top top fmt_bottom bottom

(** [pp_field_name_to_latex field_name] formats [field_name] - a field name
    appearing in the specification text - in a form suitable for LaTeX. *)
let field_name_to_latex field_name =
  spec_var_to_latex_var ~font_type:Text field_name

(** [pp_var fmt var_str] formats [var_str] - a variable appearing in the
    specification text - in a form suitable for LaTeX. *)
let pp_var fmt var_str =
  pp_print_string fmt (spec_var_to_latex_var ~font_type:TextTT var_str)

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

(** [pp_sep_elements ~pp_sep ~alignment pp_elem layout fmt elements] formats a
    list of [elements] separated by [pp_sep] according to [layout]. Each element
    is formatted using [pp_elem]. The [alignment] string specifies the alignment
    of each column and is copied directly to the array environment when the
    [layout] is vertical. *)
let pp_sep_elements ~pp_sep ~alignment pp_elem layout fmt elements =
  let layout = horizontal_if_unspecified layout elements in
  let elements_with_layouts = apply_layout_to_list layout elements in
  match layout with
  | Horizontal _ -> pp_print_list ~pp_sep pp_elem fmt elements_with_layouts
  | Vertical _ ->
      let elem_pp_funs =
        List.mapi
          (fun elem_counter elem_with_layout ->
            [
              (fun fmt ->
                pp_elem fmt elem_with_layout;
                if elem_counter < List.length elements_with_layouts - 1 then
                  pp_sep fmt ()
                else ());
            ])
          elements_with_layouts
      in
      pp_latex_array alignment fmt elem_pp_funs
  | Unspecified -> assert false

(** [pp_connect_elements ~pp_sep ~alignment pp_elem layout fmt elements] formats
    a list of [elements] connected by the separator [pp_sep] according to
    [layout]. Each element is formatted using [pp_elem]. The [alignment] string
    specifies the alignment of each column and is copied directly to the array
    environment when the [layout] is vertical. *)
let pp_connect_elements ~pp_sep ~alignment pp_elem layout fmt elements =
  let layout = horizontal_if_unspecified layout elements in
  let elements_with_layouts = apply_layout_to_list layout elements in
  match layout with
  | Horizontal _ -> pp_print_list ~pp_sep pp_elem fmt elements_with_layouts
  | Vertical _ ->
      let elem_pp_funs =
        List.mapi
          (fun elem_counter elem_with_layout ->
            [
              (fun fmt -> pp_elem fmt elem_with_layout);
              (fun fmt ->
                if elem_counter < List.length elements_with_layouts - 1 then
                  pp_sep fmt ()
                else ());
            ])
          elements_with_layouts
      in
      let array_alignment = alignment ^ "l" in
      pp_latex_array array_alignment fmt elem_pp_funs
  | Unspecified -> assert false

(** [pp_connect_pair ~alignment fmt pp_lhs_with_layout lhs connector_macro_name
     pp_rhs_with_layout rhs layout] formats a left-hand side [lhs] and a
    right-hand side [rhs] connected by a LaTeX macro [connector_macro_name]
    according to [layout]. The [alignment] string specifies the alignment of
    each column and is copied directly to the array environment when the
    [layout] is vertical. *)
let pp_connect_pair ~alignment fmt pp_lhs_with_layout lhs connector_macro_name
    pp_rhs_with_layout rhs layout =
  let layout = horizontal_if_unspecified layout [ (); () ] in
  let lhs_with_layout, rhs_with_layout =
    match layout with
    | Horizontal [ lhs_layout; rhs_layout ]
    | Vertical [ lhs_layout; rhs_layout ] ->
        ((lhs, lhs_layout), (rhs, rhs_layout))
    | Unspecified -> ((lhs, Unspecified), (rhs, Unspecified))
    | _ -> assert false
  in
  match layout with
  | Horizontal _ ->
      fprintf fmt {|%a %a %a|} pp_lhs_with_layout lhs_with_layout pp_macro
        connector_macro_name pp_rhs_with_layout rhs_with_layout
  | Vertical _ ->
      fprintf fmt {|\begin{array}{%s}@.  %a %a\\@.  %a@.\end{array}|} alignment
        pp_lhs_with_layout lhs_with_layout pp_macro connector_macro_name
        pp_rhs_with_layout rhs_with_layout
  | Unspecified -> assert false

(** [pp_fields pp_field_value fmt (fields, layout)] formats [fields] with [fmt]
    using [pp_field_value] and laid out according to [layout]. Each field is a
    pair of a field name and a field value. *)
let pp_fields pp_field_value fmt (fields, layout) =
  let layout = LayoutUtils.vertical_if_unspecified layout fields in
  let field_layouts =
    match layout with
    | Horizontal l | Vertical l -> l
    | Unspecified -> assert false
  in
  (* Distribute the list of layouts to the field values. *)
  let fields_values_with_layouts =
    List.map2
      (fun (field_name, fields_value) layout ->
        (field_name, (fields_value, layout)))
      fields field_layouts
  in
  match layout with
  | Vertical _ ->
      let field_pp_funs =
        List.map
          (fun (field_name, (fields_value, layout)) ->
            [
              (fun fmt -> pp_print_string fmt (field_name_to_latex field_name));
              (fun fmt -> pp_print_string fmt ":");
              (fun fmt -> pp_field_value fmt (fields_value, layout));
            ])
          fields_values_with_layouts
      in
      fprintf fmt {|%a|}
        (pp_parenthesized Braces true (pp_latex_array "lcl"))
        field_pp_funs
  | Horizontal _ ->
      let pp_field fmt (field_name, (fields_value, layout)) =
        fprintf fmt {|%s : %a|}
          (field_name_to_latex field_name)
          pp_field_value (fields_value, layout)
      in
      fprintf fmt {|%a|}
        (pp_parenthesized Braces true (PP.pp_sep_list ~sep:", " pp_field))
        fields_values_with_layouts
  | Unspecified -> assert false
