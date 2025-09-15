(** A module for querying the AST (of the semantics-specification for ASL)
    and for checking its correctness.
*)

open AST
module StringMap = Map.Make (String)

let variant_to_label_opt variant =
  match TypeVariant.term variant with
  | Label label | LabelledTuple { label } | LabelledRecord { label } ->
      Some label
  | _ -> None

(** Wraps AST nodes that define identifiers that may appear in type terms
  and expression terms:
  types, constants, relations, [Label]s, labelled tuples, and labelled records.
*)
type definition_node =
  | Node_Type of Type.t
  | Node_Relation of Relation.t
  | Node_TypeVariant of TypeVariant.t
  | Node_Constant of Constant.t

let definition_node_name = function
  | Node_Type def -> Type.name def
  | Node_Relation def -> Relation.name def
  | Node_TypeVariant def -> Option.get (variant_to_label_opt def)
  | Node_Constant def -> Constant.name def

let definition_node_attributes = function
  | Node_Type def -> Type.attributes def
  | Node_TypeVariant def -> TypeVariant.attributes def
  | Node_Relation def -> Relation.attributes def
  | Node_Constant def -> Constant.attributes def

type string_map = String.t StringMap.t

let elem_name = function
  | Elem_Type def -> Type.name def
  | Elem_Relation def -> Relation.name def
  | Elem_Constant def -> Constant.name def

(* let elem_attributes = function
   | Elem_Type def -> Type.attributes def
   | Elem_Relation def -> Relation.attributes def
   | Elem_Constant def -> Constant.attributes def *)

let get_type_defs ast =
  List.filter_map
    (function Elem_Type type_def -> Some type_def | _ -> None)
    ast

let get_relations ast =
  List.filter_map
    (function Elem_Relation rel_def -> Some rel_def | _ -> None)
    ast

let list_definition_nodes ast =
  List.fold_left
    (fun acc_nodes elem ->
      match elem with
      | Elem_Relation def -> Node_Relation def :: acc_nodes
      | Elem_Type def ->
          let acc_nodes = Node_Type def :: acc_nodes in
          (* Labelled types directly under this type are considered defined here,
             but labelled types nested further in are considered as type references.
          *)
          List.fold_left
            (fun acc_nodes variant ->
              match TypeVariant.term variant with
              | Label _ | LabelledTuple _ | LabelledRecord _ ->
                  Node_TypeVariant variant :: acc_nodes
              | _ -> acc_nodes)
            acc_nodes (Type.variants def)
      | Elem_Constant def -> Node_Constant def :: acc_nodes)
    [] ast

(** Generates a map from identifiers to the nodes where they are defined.
    If two nodes with the same identifier exist, a [SpecError] is raised.
*)
let gen_id_to_definition_node definition_nodes =
  List.fold_left
    (fun acc_map node ->
      let name = definition_node_name node in
      if StringMap.mem name acc_map then
        let msg = Format.sprintf "Duplicate definitions found for '%s'" name in
        raise (SpecError msg)
      else StringMap.add name node acc_map)
    StringMap.empty definition_nodes

type t = {
  ast : AST.t;  (** The original AST as parsed. *)
  id_to_defining_node : definition_node StringMap.t;
      (** Associate identifiers with the AST nodes where they are defined. *)
  defined_ids : string list;
      (** The list of identifiers defined in the spec in the order they appear. *)
}

let ast spec = spec.ast

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

  let check_definition_name name =
    let () = assert (String.length name > 0) in
    if Utils.string_exists (fun c -> '0' <= c && c <= '9') name then
      let msg =
        Format.sprintf
          "element-defining identifiers must not contain digits: %s" name
      in
      raise (SpecError msg)

  let check_defining_names defined_ids =
    List.iter check_definition_name defined_ids

  let check_mandatory_attributes_for_definition_node defining_node =
    let attributes = definition_node_attributes defining_node in
    let mandatory_attrs =
      match defining_node with
      | Node_Type _ | Node_TypeVariant _ | Node_Constant _ ->
          [ Attributes.prose_description ]
      | Node_Relation _ ->
          [ Attributes.prose_description; Attributes.prose_application ]
    in
    List.iter
      (fun attr ->
        if not (Attributes.mem attr attributes) then
          let name = definition_node_name defining_node in
          let msg =
            Format.sprintf "element '%s' is missing mandatory attribute: '%s'"
              name attr
          in
          raise (SpecError msg))
      mandatory_attrs

  let check_mandatory_attributes definition_nodes =
    List.iter check_mandatory_attributes_for_definition_node definition_nodes

  (** Returns all the identifiers referencing nodes that define identifiers. *)
  let rec referenced_ids = function
    | Label id -> [ id ]
    | Powerset term -> referenced_ids term
    | Option term -> referenced_ids term
    | Tuple components ->
        List.map snd components |> Utils.list_concat_map referenced_ids
    | LabelledTuple { label; components } ->
        label
        :: (List.map snd components |> Utils.list_concat_map referenced_ids)
    | Record fields ->
        List.map snd fields |> Utils.list_concat_map referenced_ids
    | LabelledRecord { label; fields } ->
        label :: (List.map snd fields |> Utils.list_concat_map referenced_ids)
    | List { member_type } -> referenced_ids member_type
    | ConstantsSet constant_names -> constant_names
    | Function { from_type; to_type } ->
        referenced_ids from_type @ referenced_ids to_type

  let check_no_undefined_ids_in_elem id_to_defining_node elem =
    let terms =
      match elem with
      | Elem_Constant _ -> []
      | Elem_Type def ->
          let variants = Type.variants def in
          List.map TypeVariant.term variants
      | Elem_Relation def ->
          let input_terms = List.map snd (Relation.input def) in
          input_terms @ Relation.output def
    in
    let referenced_ids = Utils.list_concat_map referenced_ids terms in
    List.iter
      (fun id ->
        if not (StringMap.mem id id_to_defining_node) then
          let msg =
            Format.sprintf "Undefined reference to '%s' in '%s'" id
              (elem_name elem)
          in
          raise (SpecError msg))
      referenced_ids

  let check_no_undefined_ids elements id_to_defining_node =
    List.iter (check_no_undefined_ids_in_elem id_to_defining_node) elements

  (** Checks that [type_term] references only top-level types.
    That is, not constants or type variants.
    This check assumes that all identifiers in [type_term] are mapped to their defining nodes
    in [id_to_defining_node]. *)
  let check_only_top_level_types id_to_defining_node type_term =
    let identifiers_in_type_term = referenced_ids type_term in
    List.iter
      (fun id ->
        match StringMap.find id id_to_defining_node with
        | Node_Type _ -> ()
        | _ ->
            let msg =
              Format.asprintf
                "Identifier '%s' in '%a' does not refer to a top-level type" id
                PP.pp_type_term type_term
            in
            raise (SpecError msg))
      identifiers_in_type_term

  let check_only_top_level_types_in_relations id_to_defining_node elements =
    List.iter
      (function
        | Elem_Relation rel ->
            List.iter
              (fun (_, type_term) ->
                check_only_top_level_types id_to_defining_node type_term)
              (Relation.input rel)
        | _ -> ())
      elements

  (* TODO:
     - Check that math macros contain only letters, except for the initial backslash. Perhaps define an appropriate token type?
  *)
end

let from_ast ast =
  let definition_nodes = list_definition_nodes ast in
  let defined_ids = List.map definition_node_name definition_nodes in
  let () = Check.check_defining_names defined_ids in
  let id_to_defining_node = gen_id_to_definition_node definition_nodes in
  let () = Check.check_no_undefined_ids ast id_to_defining_node in
  let () = Check.check_mandatory_attributes definition_nodes in
  let () =
    Check.check_only_top_level_types_in_relations id_to_defining_node ast
  in
  let defined_ids = List.rev defined_ids in
  { ast; id_to_defining_node; defined_ids }

let defining_node_for_id self id =
  try StringMap.find id self.id_to_defining_node
  with Not_found ->
    let msg = Format.sprintf "Encountered undefined element: %s" id in
    raise (SpecError msg)

let defined_ids self = self.defined_ids

let math_macro_opt_for_node = function
  | Node_Type def -> Type.math_macro def
  | Node_Relation def -> Relation.math_macro def
  | Node_TypeVariant def -> TypeVariant.math_macro def
  | Node_Constant def -> Constant.math_macro def
