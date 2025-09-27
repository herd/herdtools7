(** A module for pretty-printing the AST. *)

open Format
open AST
open SpecParser

let tok_str = TokenStrings.string_of_token
let pp_comma fmt () = fprintf fmt ",@ "
let pp_double_newline fmt () = fprintf fmt "\n@ "
let pp_comma_list pp_elt fmt = pp_print_list ~pp_sep:pp_comma pp_elt fmt

let pp_sep_list ~sep pp_elem elements =
  Format.pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt sep) pp_elem elements

let rec pp_math_shape fmt layout =
  match layout with
  | Unspecified -> pp_print_string fmt "_"
  | Horizontal l -> fprintf fmt "(%a)" (pp_sep_list ~sep:"," pp_math_shape) l
  | Vertical l -> fprintf fmt "[%a]" (pp_sep_list ~sep:"," pp_math_shape) l

let pp_attribute_key fmt key = pp_print_string fmt (AttributeKey.to_str key)

let pp_attribute fmt = function
  | StringAttribute s -> fprintf fmt {|"%s"|} s
  | MathLayoutAttribute layout -> pp_math_shape fmt layout

let pp_attribute_key_value fmt (key, value) =
  fprintf fmt {|%a = %a|} pp_attribute_key key pp_attribute value

let pp_attribute_key_values fmt attributes =
  if Utils.list_is_empty attributes then ()
  else
    fprintf fmt "{@[<v>@.%a@.@]}"
      (fun fmt attrs ->
        pp_sep_list ~sep:",@,"
          (fun fmt attr -> fprintf fmt "  %a" pp_attribute_key_value attr)
          fmt attrs)
      attributes

let rec pp_type_term fmt = function
  | Label name -> pp_print_string fmt name
  | Powerset { term; finite } ->
      let powerset_token = if finite then POWERSET_FINITE else POWERSET in
      fprintf fmt "%s(%a)" (tok_str powerset_token) pp_type_term term
  | Option elt_term ->
      fprintf fmt "%s(%a)" (tok_str OPTION) pp_type_term elt_term
  | LabelledTuple { label_opt; components } ->
      fprintf fmt "%s(%a)"
        (Option.value label_opt ~default:"")
        pp_opt_named_type_terms components
  | LabelledRecord { label_opt; fields } ->
      let label = Option.value label_opt ~default:"" in
      fprintf fmt "%s[%a]" label pp_named_type_terms fields
  | List { maybe_empty; member_type } ->
      let list_label = if maybe_empty then tok_str LIST0 else tok_str LIST1 in
      fprintf fmt "%s(%a)" list_label pp_type_term member_type
  | ConstantsSet constants ->
      fprintf fmt "%s(%a)" (tok_str CONSTANTS_SET)
        (pp_sep_list ~sep:"," pp_print_string)
        constants
  | Function { from_type; to_type; total } ->
      let keyword = if total then tok_str FUN else tok_str PARTIAL in
      fprintf fmt "%s %a -> %a" keyword pp_type_term from_type pp_type_term
        to_type

and pp_named_type_term fmt (name, term) =
  fprintf fmt "%s: %a" name pp_type_term term

and pp_opt_named_type_term fmt (name_opt, term) =
  match name_opt with
  | Some name -> pp_named_type_term fmt (name, term)
  | None -> fprintf fmt "%a" pp_type_term term

and pp_named_type_terms fmt named_terms =
  (pp_sep_list ~sep:", " pp_named_type_term) fmt named_terms

and pp_opt_named_type_terms fmt opt_named_terms =
  (pp_sep_list ~sep:", " pp_opt_named_type_term) fmt opt_named_terms

let pp_type_term_with_attributes fmt ({ TypeVariant.term } as variant) =
  fprintf fmt "%a@.%a" pp_type_term term pp_attribute_key_values
    (TypeVariant.attributes_to_list variant)

let pp_variants_with_attributes fmt variants =
  (pp_sep_list ~sep:" @,| " pp_type_term_with_attributes) fmt variants

let pp_variants fmt variants =
  (pp_sep_list ~sep:" | " pp_type_term) fmt variants

let pp_relation_definition fmt ({ Relation.name; input; output } as relation) =
  let module Rel = Relation in
  fprintf fmt "%s %s(%a) -> %a@.%a;" (tok_str RELATION) name
    (pp_sep_list ~sep:", " pp_opt_named_type_term)
    input pp_variants output pp_attribute_key_values
    (Rel.attributes_to_list relation)

let type_kind_to_string = function
  | TypeKind_Generic -> tok_str TYPEDEF
  | TypeKind_AST -> tok_str AST

let pp_type_definition fmt ({ Type.name; type_kind; variants } as def) =
  let eq_str = if Utils.list_is_empty variants then "" else " = " in
  fprintf fmt "%s %s@,%a@,%s@,@[<v 2>  %a@];"
    (type_kind_to_string type_kind)
    name pp_attribute_key_values
    (Type.attributes_to_list def)
    eq_str pp_variants_with_attributes variants

let pp_constant_definition fmt ({ Constant.name } as def) =
  fprintf fmt "%s %s %a;" (tok_str CONSTANT) name pp_attribute_key_values
    (Constant.attributes_to_list def)

let type_subset_pointer fmt { TypesRender.type_name; variant_names } =
  if Utils.list_is_empty variant_names then fprintf fmt {|%s(-)|} type_name
  else
    fprintf fmt {|%s(%a)|} type_name
      (pp_sep_list ~sep:", " pp_print_string)
      variant_names

let pp_render_definition fmt { TypesRender.name; pointers } =
  fprintf fmt "%s %s = %a;" (tok_str RENDER) name
    (pp_sep_list ~sep:", " type_subset_pointer)
    pointers

let pp_elem fmt = function
  | Elem_Type def -> pp_type_definition fmt def
  | Elem_Relation def -> pp_relation_definition fmt def
  | Elem_Constant def -> pp_constant_definition fmt def
  | Elem_Render def -> pp_render_definition fmt def

let pp_spec fmt spec =
  fprintf fmt "@[<v>%a@]" (pp_sep_list ~sep:"\n\n" pp_elem) spec
