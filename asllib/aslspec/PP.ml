(** A module for pretty-printing the AST. *)

open Format
open AST
open SpecParser

let tok_str = TokenStrings.string_of_token

let pp_comma_list pp_elt fmt =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_elt fmt

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
  fprintf fmt {|%a = %a,|} pp_attribute_key key pp_attribute value

let pp_attribute_key_values fmt attributes =
  if Utils.list_is_empty attributes then ()
  else
    fprintf fmt "{@[<v>@;<0 2>%a@]@,}"
      (fun fmt attrs ->
        pp_sep_list ~sep:"@;<0 2>"
          (fun fmt attr -> pp_attribute_key_value fmt attr)
          fmt attrs)
      attributes

let type_operator_to_token = function
  | Powerset -> POWERSET
  | Powerset_Finite -> POWERSET_FINITE
  | List0 -> LIST0
  | List1 -> LIST1
  | Option -> OPTION

let rec pp_type_term fmt = function
  | Label name -> pp_print_string fmt name
  | Operator { op; term } ->
      fprintf fmt "%s(%a)"
        (type_operator_to_token op |> tok_str)
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
  fprintf fmt "@[<v>%a@,%a@]" pp_type_term term pp_attribute_key_values
    (TypeVariant.attributes_to_list variant)

let pp_variants_with_attributes fmt variants =
  pp_sep_list ~sep:"@,  | " pp_type_term_with_attributes fmt variants

let pp_variants fmt variants = pp_sep_list ~sep:" | " pp_type_term fmt variants

let expr_operator_to_string =
  let open Rule in
  function
  | Operator_Assign -> tok_str COLON_EQ
  | Operator_Equal -> tok_str EQ
  | Operator_Iff -> tok_str IFF
  | Operator_List -> tok_str LIST
  | Operator_Size -> tok_str SIZE
  | Operator_Union -> tok_str UNION
  | Operator_Some -> tok_str SOME

let rec pp_expr fmt =
  let open Rule in
  function
  | Var name -> pp_print_string fmt name
  | Application { lhs = Operator op; args = [ lhs; rhs ] }
    when is_infix_operator op ->
      fprintf fmt "%a %s %a" pp_expr lhs
        (expr_operator_to_string op)
        pp_expr rhs
  | Application { lhs; args } ->
      fprintf fmt "%a(%a)" pp_application_lhs lhs (pp_comma_list pp_expr) args
  | FieldPath path -> fprintf fmt "%s" (String.concat "." path)
  | ListIndex { var; index } -> fprintf fmt "%s[%s]" var index
  | Record { label; fields } ->
      fprintf fmt "%s[%a]" label
        (pp_sep_list ~sep:", " (fun fmt (field, expr) ->
             fprintf fmt "%s : %a" field pp_expr expr))
        fields

and pp_application_lhs fmt =
  let open Rule in
  function
  | Tuple -> ()
  | Relation name -> pp_print_string fmt name
  | TupleLabel name -> pp_print_string fmt name
  | Operator op -> pp_print_string fmt (expr_operator_to_string op)
  | Fields path -> fprintf fmt "%s" (String.concat "." path)
  | Unresolved expr -> pp_expr fmt expr

let rec pp_judgment_form fmt =
  let open Rule in
  function
  | Expr e -> pp_expr fmt e
  | Output e -> fprintf fmt "--@,%a" pp_expr e
  | Transition { lhs; rhs } -> fprintf fmt "%a -> %a" pp_expr lhs pp_expr rhs
  | Indexed { index; list; body } ->
      fprintf fmt "%s(%s, %s): %a" (tok_str INDEX) index list pp_judgment_form
        body

let pp_judgment fmt { Rule.form; att } =
  fprintf fmt "%a%a;" pp_judgment_form form pp_attribute_key_values
    (Attributes.bindings att)

let rec pp_case fmt { Rule.name; elements } =
  fprintf fmt "case %s {@.@[<v 2>  %a@]@.}@." name
    (pp_sep_list ~sep:"@," pp_rule_element)
    elements

and pp_rule_element fmt elem =
  match elem with
  | Rule.Judgement judgement -> pp_judgment fmt judgement
  | Rule.Cases cases ->
      fprintf fmt "@.@[<v>%a@]@." (pp_sep_list ~sep:"" pp_case) cases

let pp_rule_opt fmt rule_opt =
  match rule_opt with
  | None -> ()
  | Some elements ->
      fprintf fmt "@ =@[<v>  %a@]@."
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@.") pp_rule_element)
        elements

let pp_relation_category fmt = function
  | None -> ()
  | Some Relation.RelationCategory_Typing -> fprintf fmt "%s " (tok_str TYPING)
  | Some Relation.RelationCategory_Semantics ->
      fprintf fmt "%s " (tok_str SEMANTICS)

let relation_keyword_to_string =
  let open Relation in
  function
  | RelationProperty_Relation -> tok_str RELATION
  | RelationProperty_Function -> tok_str FUNCTION

let pp_relation_definition fmt
    ({ Relation.name; property; category; input; output; rule_opt } as relation)
    =
  fprintf fmt "@[<b>%a%s %s(%a) -> %a@,%a%a@];" pp_relation_category category
    (relation_keyword_to_string property)
    name
    (pp_sep_list ~sep:", " pp_opt_named_type_term)
    input pp_variants output pp_attribute_key_values
    (Relation.attributes_to_list relation)
    pp_rule_opt rule_opt

let type_kind_to_string = function
  | TypeKind_Generic -> tok_str TYPEDEF
  | TypeKind_AST -> tok_str AST

let pp_type_definition fmt ({ Type.name; type_kind; variants } as def) =
  let eq_str = if Utils.list_is_empty variants then "" else " = " in
  fprintf fmt "%s %s@.%a@.%s@,@[<v>  %a@]@,;"
    (type_kind_to_string type_kind)
    name pp_attribute_key_values
    (Type.attributes_to_list def)
    eq_str pp_variants_with_attributes variants

let pp_constant_definition fmt ({ Constant.name } as def) =
  fprintf fmt "%s %s@.%a;" (tok_str CONSTANT) name pp_attribute_key_values
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
  | Elem_RenderTypes def -> pp_render_definition fmt def
  | Elem_RenderRule _def -> ()

let pp_spec fmt spec =
  pp_set_margin fmt 120;
  (* Set margin to 120 characters *)
  fprintf fmt "@[<b>%a@]" (pp_sep_list ~sep:"@.@." pp_elem) spec
