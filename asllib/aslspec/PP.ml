(** A module for pretty-printing the AST. *)

open Format
open AST
open SpecParser

let tok_str = TokenStrings.string_of_token

let pp_comma_list pp_elt fmt =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_elt fmt

let pp_sep_list ~sep pp_elem elements =
  Format.pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt sep) pp_elem elements

let rec pp_math_shape fmt layout =
  match layout with
  | Unspecified -> pp_print_string fmt "_"
  | Horizontal l -> fprintf fmt "(%a)" (pp_sep_list ~sep:"," pp_math_shape) l
  | Vertical l -> fprintf fmt "[%a]" (pp_sep_list ~sep:"," pp_math_shape) l

let pp_attribute_key fmt key = pp_print_string fmt (AttributeKey.to_str key)

let pp_attribute fmt = function
  | StringAttribute s ->
      let s = Str.global_replace (Str.regexp "\n") " " s in
      fprintf fmt "\"@[<hov>%a@]\"" pp_print_text s
  | MathLayoutAttribute layout -> pp_math_shape fmt layout
  | MathMacroAttribute macro -> pp_print_string fmt macro

let pp_attribute_key_value fmt (key, value) =
  fprintf fmt {|%a = %a|} pp_attribute_key key pp_attribute value

let pp_attribute_key_values fmt attributes =
  if Utils.list_is_empty attributes then ()
  else
    fprintf fmt "{@[<v>@;<0 2>%a@,@]}"
      (fun fmt attrs ->
        pp_sep_list ~sep:",@,"
          (fun fmt attr -> pp_attribute_key_value fmt attr)
          fmt attrs)
      attributes

let operator_to_token = function
  | Powerset -> POWERSET
  | Powerset_Finite -> POWERSET_FINITE
  | List0 -> LIST0
  | List1 -> LIST1
  | Option -> OPTION

let rec pp_type_term fmt = function
  | Label name -> pp_print_string fmt name
  | Operator { op; term } ->
      fprintf fmt "%s(%a)"
        (operator_to_token op |> tok_str)
        pp_opt_named_type_term term
  | LabelledTuple { label_opt; components } ->
      fprintf fmt "%s(%a)"
        (Option.value label_opt ~default:"")
        pp_opt_named_type_terms components
  | LabelledRecord { label_opt; fields } ->
      let label = Option.value label_opt ~default:"" in
      fprintf fmt "%s[%a]" label pp_named_type_terms fields
  | ConstantsSet constants ->
      fprintf fmt "%s(%a)" (tok_str CONSTANTS_SET)
        (pp_sep_list ~sep:"," pp_print_string)
        constants
  | Function { from_type; to_type; total } ->
      let keyword = if total then tok_str FUN else tok_str PARTIAL in
      fprintf fmt "%s %a -> %a" keyword pp_opt_named_type_term from_type
        pp_opt_named_type_term to_type

and pp_named_type_term fmt (name, term) =
  fprintf fmt "%s: %a" name pp_type_term term

and pp_opt_named_type_term fmt (name_opt, term) =
  match name_opt with
  | Some name -> pp_named_type_term fmt (name, term)
  | None -> pp_type_term fmt term

and pp_named_type_terms fmt named_terms =
  pp_sep_list ~sep:", " pp_named_type_term fmt named_terms

and pp_opt_named_type_terms fmt opt_named_terms =
  pp_sep_list ~sep:", " pp_opt_named_type_term fmt opt_named_terms

let pp_type_term_with_attributes fmt ({ TypeVariant.term } as variant) =
  fprintf fmt "%a@.%a" pp_type_term term pp_attribute_key_values
    (TypeVariant.attributes_to_list variant)

let pp_variants_with_attributes fmt variants =
  pp_sep_list ~sep:" @,| " pp_type_term_with_attributes fmt variants

let pp_variants fmt variants = pp_sep_list ~sep:" | " pp_type_term fmt variants

let pp_relation_category fmt = function
  | None -> ()
  | Some Relation.RelationCategory_Typing -> fprintf fmt "%s " (tok_str TYPING)
  | Some Relation.RelationCategory_Semantics ->
      fprintf fmt "%s " (tok_str SEMANTICS)

let pp_relation_definition fmt
    ({ Relation.name; property; category; input; output } as relation) =
  let module Rel = Relation in
  fprintf fmt "@[<v>%a%s %s(%a) -> %a@,%a@];" pp_relation_category category
    (match property with
    | RelationProperty_Relation -> tok_str RELATION
    | RelationProperty_Function -> tok_str FUNCTION)
    name
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
  fprintf fmt "@[<v>%a@]" (pp_sep_list ~sep:"@,@," pp_elem) spec
