(** A module for querying the AST (of the semantics-specification for ASL) and
    for checking its correctness. *)

open AST
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

(** [vars_of_type_term term] returns the list of term-naming variables that
    occur at any depth inside [term]. *)
let rec vars_of_type_term term =
  let listed_vars =
    match term with
    | Label _ -> []
    | Operator { term } -> opt_named_term_to_var_list term
    | LabelledTuple { components } -> vars_of_opt_named_type_terms components
    | LabelledRecord { fields } ->
        Utils.list_concat_map
          (fun (field_name, t) -> field_name :: vars_of_type_term t)
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

(** [vars_of_opt_named_type_terms named_terms] returns the list of term-naming
    variables that occur at any depth inside [opt_named_terms]. *)
and vars_of_opt_named_type_terms opt_named_terms =
  List.map opt_named_term_to_var_list opt_named_terms |> List.concat

(** Returns the label of a type variant, if it consists of a label. *)
let variant_to_label_opt { TypeVariant.term } =
  match term with
  | Label label -> Some label
  | LabelledTuple { label_opt } | LabelledRecord { label_opt } -> label_opt
  | _ -> None

(** Wraps AST nodes that define identifiers that may appear in type terms and
    expression terms: types, constants, relations, [Label]s, labelled tuples,
    and labelled records. *)
type definition_node =
  | Node_Type of Type.t
  | Node_Relation of Relation.t
  | Node_TypeVariant of TypeVariant.t
  | Node_Constant of Constant.t

let definition_node_name = function
  | Node_Type { Type.name }
  | Node_Relation { Relation.name }
  | Node_Constant { Constant.name } ->
      name
  | Node_TypeVariant def -> Option.get (variant_to_label_opt def)

let math_macro_opt_for_node = function
  | Node_Type def -> Type.math_macro def
  | Node_Relation def -> Relation.math_macro def
  | Node_TypeVariant def -> TypeVariant.math_macro def
  | Node_Constant def -> Constant.math_macro def

let prose_description_for_node = function
  | Node_Type def -> Type.prose_description def
  | Node_Relation def -> Relation.prose_description def
  | Node_TypeVariant def -> TypeVariant.prose_description def
  | Node_Constant def -> Constant.prose_description def

(** [vars_of_node node] returns the list of term-naming variables that occur at
    any depth inside the definition node [node]. *)
let vars_of_node = function
  | Node_Type { Type.variants; _ } ->
      Utils.list_concat_map
        (fun { TypeVariant.term } -> vars_of_type_term term)
        variants
  | Node_TypeVariant { TypeVariant.term } -> vars_of_type_term term
  | Node_Constant _ -> []
  | Node_Relation { Relation.input; output; _ } ->
      vars_of_opt_named_type_terms input
      @ Utils.list_concat_map vars_of_type_term output

(** Utility functions for handling layouts. *)
module Layout = struct
  (** For a relation like [ r(a,b,c) -> A|B|C ] generates a term
      [ (r(a,b,c), (A,B,C)) ]. That is, a pair where the first component is a
      labelled tuple with the relation name and arguments and the second
      component is a tuple with all output terms. *)
  let relation_to_tuple { Relation.name; input; output } =
    let opt_named_output_terms = List.map (fun term -> (None, term)) output in
    make_tuple
      [
        (None, make_labelled_tuple name input);
        (None, make_tuple opt_named_output_terms);
      ]

  (** Creates a horizontal layout for [term]. *)
  let rec horizontal_for_type_term term =
    match term with
    | Label _ -> Unspecified
    | Operator { term = _, t } -> horizontal_for_type_term t
    | LabelledTuple { components } ->
        if List.length components > 1 then
          Horizontal
            (List.map (fun (_, t) -> horizontal_for_type_term t) components)
        else Unspecified
    | LabelledRecord { fields; _ } ->
        if List.length fields > 1 then
          Horizontal
            (List.map (fun (_, t) -> horizontal_for_type_term t) fields)
        else Unspecified
    | ConstantsSet names ->
        Horizontal ((List.init (List.length names)) (fun _ -> Unspecified))
    | Function { from_type = _, from_term; to_type = _, to_term; _ } ->
        Horizontal
          [
            horizontal_for_type_term from_term; horizontal_for_type_term to_term;
          ]

  let rec default_for_type_term term =
    match term with
    | Label _ -> Unspecified
    | Operator { term = _, t } -> default_for_type_term t
    | LabelledTuple { components } ->
        if List.length components > 1 then
          Horizontal
            (List.map (fun (_, t) -> default_for_type_term t) components)
        else Unspecified
    | LabelledRecord { fields; _ } ->
        if List.length fields > 1 then
          Vertical (List.map (fun (_, t) -> default_for_type_term t) fields)
        else Unspecified
    | ConstantsSet names ->
        Horizontal ((List.init (List.length names)) (fun _ -> Unspecified))
    | Function { from_type = _, from_term; to_type = _, to_term; _ } ->
        Horizontal
          [ default_for_type_term from_term; default_for_type_term to_term ]

  let horizontal_if_unspecified layout terms =
    match layout with
    | Horizontal _ | Vertical _ -> layout
    | _ -> Horizontal (List.map (fun _ -> Unspecified) terms)

  let rec contains_vertical = function
    | Unspecified -> false
    | Horizontal shapes -> List.exists contains_vertical shapes
    | Vertical _ -> true

  let math_layout_for_node = function
    | Node_Type def -> (
        match Type.math_layout def with
        | Some layout -> layout
        | None -> Unspecified)
    | Node_Constant _ -> Unspecified
    | Node_Relation def -> (
        match Relation.math_layout def with
        | Some layout -> layout
        | None -> default_for_type_term (relation_to_tuple def))
    | Node_TypeVariant ({ TypeVariant.term } as def) -> (
        match TypeVariant.math_layout def with
        | Some layout -> layout
        | None -> default_for_type_term term)
end

let definition_node_attributes = function
  | Node_Type { Type.att }
  | Node_TypeVariant { TypeVariant.att }
  | Node_Relation { Relation.att }
  | Node_Constant { Constant.att } ->
      att

let elem_name = function
  | Elem_Type { Type.name }
  | Elem_Relation { Relation.name }
  | Elem_Constant { Constant.name } ->
      name
  | Elem_Render { name } -> name

(** Lists nodes that define identifiers and can be referenced. *)
let list_definition_nodes ast =
  List.fold_left
    (fun acc_nodes elem ->
      match elem with
      | Elem_Relation def -> Node_Relation def :: acc_nodes
      | Elem_Type ({ Type.variants } as def) ->
          let acc_nodes = Node_Type def :: acc_nodes in
          (* Labelled types directly under this type are considered defined here,
             but labelled types nested further in are considered as type references.
          *)
          List.fold_left
            (fun acc_nodes ({ TypeVariant.term } as variant) ->
              match term with
              | Label _
              | LabelledTuple { label_opt = Some _ }
              | LabelledRecord { label_opt = Some _ } ->
                  Node_TypeVariant variant :: acc_nodes
              | LabelledTuple { label_opt = None }
              | LabelledRecord { label_opt = None }
              | _ ->
                  acc_nodes)
            acc_nodes variants
      | Elem_Constant def -> Node_Constant def :: acc_nodes
      (* Although a render defines an identifier, it does not carry semantic
         meaning, and cannot be referenced elsewhere. *)
      | Elem_Render _ -> acc_nodes)
    [] ast

(** Creates a map from identifiers to the nodes where they are defined. If two
    nodes with the same identifier exist, a [SpecError] is raised. *)
let make_id_to_definition_node definition_nodes =
  List.fold_left
    (fun acc_map node ->
      let name = definition_node_name node in
      if StringMap.mem name acc_map then
        let msg = Format.sprintf "Duplicate definitions found for '%s'" name in
        raise (SpecError msg)
      else StringMap.add name node acc_map)
    StringMap.empty definition_nodes

type t = {
  ast : AST.t;  (** The original AST as parsed *)
  id_to_defining_node : definition_node StringMap.t;
      (** Associates identifiers with the AST nodes where they are defined. *)
  defined_ids : string list;
      (** The list of identifiers defined in the spec in order of appearance. *)
}

let ast spec = spec.ast

(** [defining_node_for_id self id] returns the defining node for the element
    with identifier [id] that is given in the specification. If no such element
    exists, a [SpecError] is raised. *)
let defining_node_for_id self id =
  try StringMap.find id self.id_to_defining_node
  with Not_found ->
    let msg = Format.sprintf "Encountered undefined element: %s" id in
    raise (SpecError msg)

module Check = struct
  let list_find_duplicate strs =
    let module StringSet = Set.Make (String) in
    let rec find_with_set set = function
      | [] -> None
      | str :: tail ->
          if StringSet.mem str set then Some str
          else find_with_set (StringSet.add str set) tail
    in
    find_with_set StringSet.empty strs

  (** Checks, for each definition node, that all mandatory attributes are
      present. The parser ensures only allowed attributes are added. *)
  let check_mandatory_attributes definition_nodes =
    let check_mandatory_attributes_for_definition_node defining_node =
      let attributes = definition_node_attributes defining_node in
      let mandatory_attrs =
        let open AttributeKey in
        match defining_node with
        | Node_Type _ | Node_TypeVariant _ | Node_Constant _ ->
            [ Prose_Description ]
        | Node_Relation _ -> [ Prose_Description; Prose_Application ]
      in
      List.iter
        (fun attr ->
          if not (Attributes.mem attr attributes) then
            let name = definition_node_name defining_node in
            let msg =
              Format.sprintf "element '%s' is missing mandatory attribute: '%s'"
                name (AttributeKey.to_str attr)
            in
            raise (SpecError msg))
        mandatory_attrs
    in
    List.iter check_mandatory_attributes_for_definition_node definition_nodes

  let rec check_layout term layout =
    let msg =
      Format.asprintf
        "layout %a is inconsistent with %a. Here's a consistent layout: %a"
        PP.pp_math_shape layout PP.pp_type_term term PP.pp_math_shape
        (Layout.default_for_type_term term)
    in
    let open Layout in
    match (term, layout) with
    | Label _, Unspecified -> ()
    | Label _, _ -> raise (SpecError msg)
    | Operator { term = _, t }, _ -> check_layout t layout
    | LabelledTuple { components }, Horizontal cells
    | LabelledTuple { components }, Vertical cells ->
        if List.length components <> List.length cells then
          raise (SpecError msg)
        else
          List.iter2
            (fun (_, term) cell -> check_layout term cell)
            components cells
    | LabelledRecord { fields }, Horizontal cells
    | LabelledRecord { fields }, Vertical cells ->
        if List.length fields <> List.length cells then raise (SpecError msg)
        else
          List.iter2 (fun (_, term) cell -> check_layout term cell) fields cells
    | ConstantsSet names, Horizontal cells ->
        if List.length names <> List.length cells then raise (SpecError msg)
        else List.iter2 (fun _ cell -> check_layout (Label "") cell) names cells
    | ( Function { from_type = _, from_term; to_type = _, to_term; _ },
        Horizontal cells ) ->
        if List.length cells <> 2 then raise (SpecError msg)
        else check_layout from_term (List.nth cells 0);
        check_layout to_term (List.nth cells 1)
    | _ -> ()

  let check_math_layout definition_nodes =
    let check_math_layout_for_definition_node node =
      let open Layout in
      match node with
      | Node_Type { Type.name } ->
          check_layout (Label name) (math_layout_for_node node)
      | Node_Constant { Constant.name } ->
          check_layout (Label name) (math_layout_for_node node)
      | Node_TypeVariant { TypeVariant.term } ->
          check_layout term (math_layout_for_node node)
      | Node_Relation def ->
          check_layout (relation_to_tuple def) (math_layout_for_node node)
    in
    List.iter check_math_layout_for_definition_node definition_nodes

  (** Returns all the identifiers referencing nodes that define identifiers. *)
  let rec referenced_ids = function
    | Label id -> [ id ]
    | Operator { term = _, t } -> referenced_ids t
    | LabelledTuple { label_opt; components } -> (
        let component_ids =
          List.map snd components |> Utils.list_concat_map referenced_ids
        in
        match label_opt with
        | None -> component_ids
        | Some label -> label :: component_ids)
    | LabelledRecord { label_opt; fields } -> (
        match label_opt with
        | None -> List.map snd fields |> Utils.list_concat_map referenced_ids
        | Some label ->
            label
            :: (List.map snd fields |> Utils.list_concat_map referenced_ids))
    | ConstantsSet constant_names -> constant_names
    | Function { from_type = _, from_term; to_type = _, to_term } ->
        referenced_ids from_term @ referenced_ids to_term

  (** [check_no_undefined_ids elements id_to_defining_node] checks that all
      identifiers referenced in [elements] are keys in [id_to_defining_node]. *)
  let check_no_undefined_ids elements id_to_defining_node =
    let check_no_undefined_ids_in_elem id_to_defining_node elem =
      let referenced_ids_for_list = Utils.list_concat_map referenced_ids in
      let ids_referenced_by_elem =
        match elem with
        | Elem_Constant _ -> []
        | Elem_Type { Type.variants } ->
            referenced_ids_for_list
              (List.map (fun { TypeVariant.term } -> term) variants)
        | Elem_Relation { Relation.input; output } ->
            let input_terms = List.map snd input in
            referenced_ids_for_list (input_terms @ output)
        | Elem_Render { pointers } ->
            Utils.list_concat_map
              (fun { TypesRender.type_name; variant_names } ->
                type_name :: variant_names)
              pointers
      in
      List.iter
        (fun id ->
          if not (StringMap.mem id id_to_defining_node) then
            let msg =
              Format.sprintf "Undefined reference to '%s' in '%s'" id
                (elem_name elem)
            in
            raise (SpecError msg))
        ids_referenced_by_elem
    in
    List.iter (check_no_undefined_ids_in_elem id_to_defining_node) elements

  (** A module for checking that each prose template string ([prose_description]
      and [prose_application] attributes) to ensure it does not contain a
      [{var}] where [var] does not name any type term. If it does, LaTeX will
      fail on [{var}], which would require debugging the generated code. This
      check catches such cases and generates an easy to understand explanation.
  *)
  module CheckProseTemplates : sig
    val check : definition_node list -> unit
    (** [check definition_nodes] checks all prose templates in
        [definition_nodes] *)
  end = struct
    (** [check_prose_template_for_vars template vars] checks that [template]
        does not contain a [{var}] where [var] is not in [vars]. Otherwise,
        raises a [SpecError] detailing the unmatched variables. *)
    let check_prose_template_for_vars template vars =
      let open Text in
      (* Populate with [{var}] for each [var]. *)
      let template_vars =
        List.fold_left
          (fun acc_map var_str ->
            let template_var = spec_var_to_template_var var_str in
            StringSet.add template_var acc_map)
          StringSet.empty vars
      in
      let template_var_regexp = Str.regexp "{[a-zA-Z0-9_']+}" in
      (* Remove things like [\texttt{a}], which do not (should not) reference variables. *)
      let reduce_template =
        Str.global_replace
          (Str.regexp
             {|\\\([a-zA-Z]+\){[a-zA-Z0-9_']+}\|\(\\hyperlink{[a-zA-Z_\-]*}{[a-zA-Z_\-]*}\)|})
          "" template
      in
      let blocks = Str.full_split template_var_regexp reduce_template in
      let unmatched_vars =
        List.fold_left
          (fun acc block ->
            match block with
            | Str.Text _ -> acc
            | Str.Delim var -> (
                match StringSet.find_opt var template_vars with
                | Some _ -> acc
                | None -> var :: acc))
          [] blocks
      in
      if Utils.list_is_empty unmatched_vars then ()
      else
        let msg =
          Format.sprintf
            "The prose template '%s' contains the following unmatched \
             variables: %s"
            template
            (String.concat ", " unmatched_vars)
        in
        raise (SpecError msg)

    let check_prose_template_for_definition_node defining_node =
      let prose_description = prose_description_for_node defining_node in
      let vars = vars_of_node defining_node in
      let () = check_prose_template_for_vars prose_description vars in
      match defining_node with
      | Node_Type _ | Node_TypeVariant _ | Node_Constant _ -> ()
      | Node_Relation def ->
          let prose_application = Relation.prose_application def in
          let () = check_prose_template_for_vars prose_application vars in
          ()

    let check defining_nodes =
      List.iter check_prose_template_for_definition_node defining_nodes
  end

  (** TODO:
      - Check that instantiated type terms for labelled type terms and labelled
        records match their definition (see [tests/type_instance.bad]). *)
end

let from_ast ast =
  let definition_nodes = list_definition_nodes ast in
  let defined_ids = List.map definition_node_name definition_nodes in
  let id_to_defining_node = make_id_to_definition_node definition_nodes in
  let () = Check.check_no_undefined_ids ast id_to_defining_node in
  let () = Check.check_mandatory_attributes definition_nodes in
  let () = Check.check_math_layout definition_nodes in
  let () = Check.CheckProseTemplates.check definition_nodes in
  let defined_ids = List.rev defined_ids in
  { ast; id_to_defining_node; defined_ids }

let defined_ids self = self.defined_ids
