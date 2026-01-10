(** A module for pretty-printing the AST. *)

open Format
open AST
open SpecParser

let tok_str = TokenStrings.string_of_token

let pp_comma_list pp_elt fmt =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_elt fmt

let pp_sep_list ~sep pp_elem elements =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt sep) pp_elem elements

let rec pp_layout fmt layout =
  match layout with
  | Unspecified -> pp_print_string fmt "_"
  | Horizontal l -> fprintf fmt "(%a)" (pp_sep_list ~sep:"," pp_layout) l
  | Vertical l -> fprintf fmt "[%a]" (pp_sep_list ~sep:"," pp_layout) l

let pp_attribute_key fmt key = pp_print_string fmt (AttributeKey.to_str key)

let pp_attribute fmt = function
  | StringAttribute s ->
      let s = Str.global_replace (Str.regexp "\n") " " s in
      fprintf fmt "\"@[<hov>%a@]\"" pp_print_text s
  | MathLayoutAttribute layout -> pp_layout fmt layout
  | MathMacroAttribute macro -> pp_print_string fmt macro
  | BoolAttribute b -> pp_print_bool fmt b

let pp_attribute_key_value fmt (key, value) =
  fprintf fmt "%a = %a," pp_attribute_key key pp_attribute value

let pp_attribute_key_values fmt attributes =
  if Utils.list_is_empty attributes then ()
  else
    fprintf fmt "{@[<v>@;<0 2>%a@]@,}"
      (fun fmt attrs ->
        pp_sep_list ~sep:"@;<0 2>"
          (fun fmt attr -> pp_attribute_key_value fmt attr)
          fmt attrs)
      attributes

let type_operator_to_token =
  let open Term in
  function
  | Powerset -> POWERSET
  | Powerset_Finite -> POWERSET_FINITE
  | List0 -> LIST0
  | List1 -> LIST1
  | Option -> OPTION

let rec pp_type_term fmt =
  let open Term in
  function
  | Label name -> pp_print_string fmt name
  | TypeOperator { op; term } ->
      fprintf fmt "%s(%a)"
        (type_operator_to_token op |> tok_str)
        pp_opt_named_type_term term
  | Tuple { label_opt; args } ->
      fprintf fmt "%s(%a)"
        (Option.value label_opt ~default:"")
        pp_opt_named_type_terms args
  | Record { label_opt; fields } ->
      let label = Option.value label_opt ~default:"" in
      fprintf fmt "%s[%a]" label pp_record_fields fields
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

and pp_record_fields fmt fields =
  pp_sep_list ~sep:", " pp_record_field fmt fields

and pp_record_field fmt { name_and_type; att } =
  fprintf fmt "%a%a" pp_named_type_term name_and_type pp_attribute_key_values
    (Attributes.bindings att)

let pp_type_term_with_attributes fmt ({ TypeVariant.term } as variant) =
  fprintf fmt "@[<v>%a@,%a@]" pp_type_term term pp_attribute_key_values
    (TypeVariant.attributes_to_list variant)

let pp_variants_with_attributes fmt variants =
  pp_sep_list ~sep:"@,  | " pp_type_term_with_attributes fmt variants

let pp_variants fmt variants = pp_sep_list ~sep:" | " pp_type_term fmt variants
let pp_output_opt fmt is_output = if is_output then fprintf fmt "--@," else ()

let rec pp_expr fmt =
  let open Expr in
  function
  | Var name -> pp_print_string fmt name
  | UnresolvedApplication { lhs; args } ->
      fprintf fmt "%a(%a)" pp_expr lhs (pp_comma_list pp_expr) args
  | Tuple { label_opt; args } ->
      fprintf fmt "%s(%a)"
        (Option.value label_opt ~default:"")
        (pp_comma_list pp_expr) args
  | Relation { name; args } ->
      fprintf fmt "%s(%a)" name (pp_comma_list pp_expr) args
  | Map { lhs; args } ->
      fprintf fmt "%a(%a)" pp_expr lhs (pp_comma_list pp_expr) args
  | FieldAccess { var; fields } ->
      pp_print_string fmt (String.concat "." (var :: fields))
  | ListIndex { list_var; index } -> fprintf fmt "%s[%a]" list_var pp_expr index
  | Record { label_opt; fields } ->
      fprintf fmt "%a[%a]"
        (pp_print_option pp_print_string)
        label_opt
        (pp_sep_list ~sep:", " (fun fmt (field, expr) ->
             fprintf fmt "%s : %a" field pp_expr expr))
        fields
  | Transition { lhs; rhs; short_circuit } ->
      fprintf fmt "%a -> %a%a" pp_expr lhs pp_expr rhs pp_short_circuit
        short_circuit
  | Indexed { index; list_var; body } ->
      fprintf fmt "%s(%s, %s: %a)" (tok_str INDEX) index list_var pp_expr body
  | NamedExpr (e, name) -> fprintf fmt "%a /* %s */" pp_expr e name

and pp_short_circuit fmt short_circuit =
  match short_circuit with
  | None -> ()
  | Some alternatives ->
      if Utils.list_is_empty alternatives then ()
      else fprintf fmt " | %a" (pp_sep_list ~sep:", " pp_expr) alternatives

let pp_judgment fmt { Rule.expr; is_output; att } =
  let open Rule in
  fprintf fmt "%a%a%a;" pp_output_opt is_output pp_expr expr
    pp_attribute_key_values (Attributes.bindings att)

let rec pp_case fmt { Rule.name; elements } =
  fprintf fmt "case %s {@.@[<v 2>  %a@]@.}@." name
    (pp_sep_list ~sep:"@," pp_rule_element)
    elements

and pp_rule_element fmt elem =
  match elem with
  | Rule.Judgment judgment -> pp_judgment fmt judgment
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

(** [pp_parameters fmt parameters] pretty-prints the list of parameters
    [parameters] with the formatter [fmt]. If the list is empty, nothing is
    printed. *)
let pp_parameters fmt parameters =
  if not (Utils.list_is_empty parameters) then
    fprintf fmt "[%a]" (pp_sep_list ~sep:", " pp_print_string) parameters
  else ()

let pp_relation_definition fmt
    ({
       Relation.name;
       parameters;
       is_operator;
       property;
       category;
       input;
       output;
       rule_opt;
     } as relation) =
  let pp_relation_prefix fmt () =
    if is_operator then pp_print_string fmt (tok_str OPERATOR)
    else
      fprintf fmt "%a%s" pp_relation_category category
        (relation_keyword_to_string property)
  in
  fprintf fmt "@[<b>%a %s%a(%a) -> %a@,%a%a@];" pp_relation_prefix () name
    pp_parameters parameters
    (pp_sep_list ~sep:", " pp_opt_named_type_term)
    input pp_variants output pp_attribute_key_values
    (Relation.attributes_to_list relation)
    pp_rule_opt rule_opt

let type_kind_to_string =
  let open Term in
  function TypeKind_Generic -> tok_str TYPEDEF | TypeKind_AST -> tok_str AST

let pp_type_definition fmt ({ Type.name; type_kind; variants } as def) =
  let eq_str = if Utils.list_is_empty variants then "" else " = " in
  fprintf fmt "%s %s@.%a@.%s@,@[<v>  %a@]@,;"
    (type_kind_to_string type_kind)
    name pp_attribute_key_values
    (Type.attributes_to_list def)
    eq_str pp_variants_with_attributes variants

let pp_constant_definition fmt
    ({ Constant.name; opt_type; opt_value_and_attributes } as def) =
  let pp_opt_type fmt opt_type =
    pp_print_option
      (fun fmt type_term -> fprintf fmt ": %a" pp_type_term type_term)
      fmt opt_type
  in
  let pp_opt_initial_value fmt opt_value =
    pp_print_option
      (fun fmt (value, value_attributes) ->
        fprintf fmt " = %a %a" pp_expr value pp_attribute_key_values
          (Attributes.bindings value_attributes))
      fmt opt_value
  in
  fprintf fmt "%s %s%a@.%a%a;" (tok_str CONSTANT) name pp_opt_type opt_type
    pp_attribute_key_values
    (Constant.attributes_to_list def)
    pp_opt_initial_value opt_value_and_attributes

let pp_type_subset_pointer fmt { TypesRender.type_name; variant_names } =
  if Utils.list_is_empty variant_names then fprintf fmt "%s(-)" type_name
  else
    fprintf fmt "%s(%a)" type_name
      (pp_sep_list ~sep:", " pp_print_string)
      variant_names

let pp_render_definition fmt ({ TypesRender.name; pointers } as def) =
  fprintf fmt "%s %s%a = %a;" (tok_str RENDER) name pp_attribute_key_values
    (TypesRender.attributes_to_list def)
    (pp_sep_list ~sep:", " pp_type_subset_pointer)
    pointers

let pp_elem fmt = function
  | Elem_Type def -> pp_type_definition fmt def
  | Elem_Relation def -> pp_relation_definition fmt def
  | Elem_Constant def -> pp_constant_definition fmt def
  | Elem_RenderTypes def -> pp_render_definition fmt def
  | Elem_RenderRule _def -> ()

let pp_spec fmt spec =
  pp_set_margin fmt 120;
  (* Set margin in terms of number of characters. *)
  fprintf fmt "@[<b>%a@]" (pp_sep_list ~sep:"@.@." pp_elem) spec
