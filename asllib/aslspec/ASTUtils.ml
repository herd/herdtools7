open AST

let elem_name = function
  | Elem_Type { Type.name }
  | Elem_Relation { Relation.name }
  | Elem_Constant { Constant.name }
  | Elem_RenderTypes { TypesRender.name }
  | Elem_RenderRule { RuleRender.name } ->
      name

let rec vars_of_type_term term =
  let open Term in
  let listed_vars =
    match term with
    | Label _ -> []
    | TypeOperator { term } -> opt_named_term_to_var_list term
    | LabelledTuple { args } -> vars_of_opt_named_type_terms args
    | LabelledRecord { fields } ->
        Utils.list_concat_map
          (fun { name_and_type = name, field_term; _ } ->
            name :: vars_of_type_term field_term)
          fields
    | ConstantsSet _ -> []
    | Function { from_type; to_type } ->
        opt_named_term_to_var_list from_type
        @ opt_named_term_to_var_list to_type
  in
  List.sort_uniq String.compare listed_vars

(** [opt_named_term_to_var_list (var, t)] returns the list of term-naming
    variables that occur at any depth inside [t], plus [var] if it is [Some x].
*)
and opt_named_term_to_var_list (var, t) =
  Option.to_list var @ vars_of_type_term t

and vars_of_opt_named_type_terms opt_named_terms =
  Utils.list_concat_map opt_named_term_to_var_list opt_named_terms

let variant_to_label_opt { TypeVariant.term } =
  match term with
  | Label label -> Some label
  | LabelledTuple { label_opt } | LabelledRecord { label_opt } -> label_opt
  | _ -> None

let is_operator elem =
  match elem with
  | Elem_Relation { Relation.is_operator } -> is_operator
  | _ -> false
