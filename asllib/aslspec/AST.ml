(** A module for representing the abstract syntax trees of aslspec --- the
    domain-specific language that defines the semantics-specification for ASL.
*)

exception SpecError of string

(** Extends an error message and re-raises an exception. This is a temporary
    hack until the AST supports source locations. *)
let stack_spec_error msg extra_msg =
  let full_msg = Printf.sprintf "%s\n%s" msg extra_msg in
  raise (SpecError full_msg)

(** The kind of a type, either generic or AST-specific. *)

type type_kind = TypeKind_Generic | TypeKind_AST

(** A unary operator that transforms one type into another. *)
type operator =
  | Powerset  (** All subsets (finite and infinite) of the given type. *)
  | Powerset_Finite  (** All finite subsets of the given type. *)
  | List0  (** All (empty and non-empty) sequences of the given member type. *)
  | List1  (** All non-empty sequences of the given member type. *)
  | Option  (** A set containing at most a single value of the given type. *)

(** Terms for constructing types out of other types, with [Label t] being the
    leaf case.

    In the context of a type definition, a [Label] variant defines a new label -
    a type representing just this single label. In other contexts, for example a
    type variant appearing in the signature of a relation, this can refer to a
    type defined elsewhere. *)
type type_term =
  | Label of string
      (** Either a set containing the single value named by the given string or
          a reference to a type with the given name. *)
  | Operator of { op : operator; term : opt_named_type_term }
      (** A set containing all types formed by applying the operator [op] to the
          type given by [term]. *)
  | LabelledTuple of {
      label_opt : string option;
      components : opt_named_type_term list;
    }
      (** A set containing all optionally-labelled tuples formed by the given
          components. An unlabelled tuple containing a single term is a special
          case - its domain is the domain of that term; this serves to reference
          types defined elsewhere. *)
  | LabelledRecord of {
      label_opt : string option;
      fields : named_type_term list;
    }
      (** A set containing all optionally-labelled records formed by the given
          fields. *)
  | ConstantsSet of string list
      (** A set containing all constants formed by the given names. *)
  | Function of {
      from_type : opt_named_type_term;
      to_type : opt_named_type_term;
      total : bool;
    }
      (** A set containing all functions formed by the given types. If [total]
          is true, the function is total, otherwise it is partial. *)

and named_type_term = string * type_term
(** A term associated with a variable name. *)

and opt_named_type_term = string option * type_term
(** A term optionally associated with a variable name. *)

(** [make_operator op term] Constructs an operator term with the given operator
    and term. *)
let make_operator op term = Operator { op; term }

(** [make_tuple components] Constructs an unlabelled tuple for the tuple
    components [components]. *)
let make_tuple components = LabelledTuple { label_opt = None; components }

(** [make_labelled_tuple label components] Constructs a tuple labelled [label]
    and tuple components [components]. *)
let make_labelled_tuple label components =
  LabelledTuple { label_opt = Some label; components }

(** [make_record fields] Constructs an unlabelled record with fields [fields].
*)
let make_record fields = LabelledRecord { label_opt = None; fields }

(** [make_labelled_record label fields] Constructs a record labelled [label] and
    fields [fields]. *)
let make_labelled_record label fields =
  LabelledRecord { label_opt = Some label; fields }

(** Specifies a visual layout for a compound term. *)
type layout =
  | Unspecified
      (** No specific layout, appropriate for atomic terms and terms with
          singleton lists. *)
  | Horizontal of layout list
      (** Specifies a horizontal layout for terms where the layout for each term
          is specified individually. *)
  | Vertical of layout list
      (** Specifies a vertical layout for terms where the layout for each term
          is specified individually. *)

(** A module for totally ordered attribute keys. *)
module AttributeKey = struct
  type t =
    | Prose_Description
        (** A description of the element in prose with template variables in the
            format [{var}]. *)
    | Prose_Application
        (** A description of the element in prose, describing its application in
            an inference rule premise. Can contain template variables in the
            format [{var}]. *)
    | Math_Macro  (** A LaTeX macro name for the element. *)
    | Math_Layout  (** The visual layout of the element. *)
    | Short_Circuit_Macro
        (** A LaTeX macro name to succinctly denote any value of a type [T].
            This is used to denote the short-circuit result of a relation
            application yielding a value of type [T]. *)

  (* A total ordering on attribute keys. *)
  let compare a b =
    let key_to_int = function
      | Prose_Description -> 0
      | Prose_Application -> 1
      | Math_Macro -> 2
      | Math_Layout -> 3
      | Short_Circuit_Macro -> 4
    in
    let a_int = key_to_int a in
    let b_int = key_to_int b in
    Int.compare a_int b_int

  (** Converts an attribute key with the same string as the corresponding token.
  *)
  let to_str = function
    | Prose_Description -> "prose_description"
    | Prose_Application -> "prose_application"
    | Math_Macro -> "math_macro"
    | Math_Layout -> "math_layout"
    | Short_Circuit_Macro -> "short_circuit_macro"
end

(** A value associated with an attribute key. *)
type attribute =
  | StringAttribute of string
  | MathMacroAttribute of string
  | MathLayoutAttribute of layout

(** A module for associating attributes with attribute keys. *)
module Attributes = struct
  include Map.Make (AttributeKey)

  type 'a map = 'a t
  (** Shadows [Map.t] with a string-to-string map type. *)

  type nonrec t = attribute t

  let check_definition_name name =
    let () = assert (String.length name > 0) in
    let id_regexp = Str.regexp "^[A-Za-z_']+$" in
    if not (Str.string_match id_regexp name 0) then
      let msg = Format.sprintf "illegal element-defining identifier: %s" name in
      raise (SpecError msg)

  (** Shadows [of_list] by raising a [SpecError] exception on pairs containing
      the same key. *)
  let of_list pairs =
    List.fold_left
      (fun acc_map (k, v) ->
        if mem k acc_map then
          let msg =
            Format.sprintf "Duplicate attribute: '%s'" (AttributeKey.to_str k)
          in
          raise (SpecError msg)
        else add k v acc_map)
      empty pairs
end

(** A datatype for top-level type terms used in the definition of a type. *)
module TypeVariant : sig
  type t = { type_kind : type_kind; term : type_term; att : Attributes.t }

  val make : type_kind -> type_term -> (AttributeKey.t * attribute) list -> t
  val attributes_to_list : t -> (AttributeKey.t * attribute) list
  val prose_description : t -> string
  val math_macro : t -> string option

  val math_layout : t -> layout option
  (** The layout of the type term when rendered in the context of its containing
      type. *)
end = struct
  type t = { type_kind : type_kind; term : type_term; att : Attributes.t }

  let make type_kind term attribute_pairs =
    { type_kind; term; att = Attributes.of_list attribute_pairs }

  open Attributes

  let attributes_to_list self = bindings self.att

  let prose_description self =
    match Attributes.find_opt AttributeKey.Prose_Description self.att with
    | Some (StringAttribute s) -> s
    | _ -> ""

  let math_macro self =
    match find_opt AttributeKey.Math_Macro self.att with
    | Some (MathMacroAttribute s) -> Some s
    | _ -> None

  let math_layout self =
    match find_opt AttributeKey.Math_Layout self.att with
    | Some (MathLayoutAttribute layout) -> Some layout
    | _ -> None
end

(** A datatype for a type definition. *)
module Type : sig
  type t = {
    name : string;
    type_kind : type_kind;
    variants : TypeVariant.t list;
    att : Attributes.t;
  }

  val make :
    type_kind ->
    string ->
    TypeVariant.t list ->
    (AttributeKey.t * attribute) list ->
    t

  val attributes_to_list : t -> (AttributeKey.t * attribute) list
  val prose_description : t -> string
  val math_macro : t -> string option
  val short_circuit_macro : t -> string option

  val math_layout : t -> layout option
  (** The layout used when rendered as a stand-alone type definition. *)
end = struct
  type t = {
    name : string;
    type_kind : type_kind;
    variants : TypeVariant.t list;
    att : Attributes.t;
  }

  let make type_kind name variants attribute_pairs =
    (* The [type_kind] of the variants is the [type_kind] given above. *)
    let variants_with_parent_type_kind =
      List.map
        (fun ({ TypeVariant.term } as variant) ->
          let attribute_pairs = TypeVariant.attributes_to_list variant in
          TypeVariant.make type_kind term attribute_pairs)
        variants
    in
    {
      type_kind;
      name;
      variants = variants_with_parent_type_kind;
      att = Attributes.of_list attribute_pairs;
    }

  open Attributes

  let attributes_to_list self = Attributes.bindings self.att

  let prose_description self =
    match Attributes.find_opt AttributeKey.Prose_Description self.att with
    | Some (StringAttribute s) -> s
    | _ -> ""

  let math_macro self =
    match find_opt AttributeKey.Math_Macro self.att with
    | Some (MathMacroAttribute s) -> Some s
    | _ -> None

  let short_circuit_macro self =
    match find_opt AttributeKey.Short_Circuit_Macro self.att with
    | Some (MathMacroAttribute s) -> Some s
    | _ -> None

  let math_layout self =
    match find_opt AttributeKey.Math_Layout self.att with
    | Some (MathLayoutAttribute layout) -> Some layout
    | _ -> None
end

(** A datatype for a relation definition. *)
module Relation : sig
  type relation_property =
    | RelationProperty_Relation
    | RelationProperty_Function

  type relation_category =
    | RelationCategory_Typing
    | RelationCategory_Semantics

  type t = {
    name : string;
    property : relation_property;
    category : relation_category option;
    input : opt_named_type_term list;
    output : type_term list;
    att : Attributes.t;
  }

  val make :
    string ->
    relation_property ->
    relation_category option ->
    opt_named_type_term list ->
    type_term list ->
    (AttributeKey.t * attribute) list ->
    t

  val attributes_to_list : t -> (AttributeKey.t * attribute) list
  val prose_description : t -> string
  val math_macro : t -> string option
  val prose_application : t -> string

  val math_layout : t -> layout option
  (** The layout used when rendered as a stand-alone relation definition. *)
end = struct
  type relation_property =
    | RelationProperty_Relation
    | RelationProperty_Function

  type relation_category =
    | RelationCategory_Typing
    | RelationCategory_Semantics

  type t = {
    name : string;
    property : relation_property;
    category : relation_category option;
    input : opt_named_type_term list;
    output : type_term list;
    att : Attributes.t;
  }

  let make name property category input output attributes =
    {
      name;
      property;
      category;
      input;
      output;
      att = Attributes.of_list attributes;
    }

  let attributes_to_list self = Attributes.bindings self.att

  open Attributes

  let prose_description self =
    match Attributes.find_opt AttributeKey.Prose_Description self.att with
    | Some (StringAttribute s) -> s
    | _ -> ""

  let math_macro self =
    match find_opt AttributeKey.Math_Macro self.att with
    | Some (MathMacroAttribute s) -> Some s
    | _ -> None

  let prose_application self =
    match find_opt AttributeKey.Prose_Application self.att with
    | Some (StringAttribute s) -> s
    | _ -> ""

  let math_layout self =
    match find_opt AttributeKey.Math_Layout self.att with
    | Some (MathLayoutAttribute layout) -> Some layout
    | _ -> None
end

(** A datatype for a constant definition. *)
module Constant : sig
  type t = { name : string; att : Attributes.t }

  val make : string -> (AttributeKey.t * attribute) list -> t
  val attributes_to_list : t -> (AttributeKey.t * attribute) list
  val prose_description : t -> string
  val math_macro : t -> string option
end = struct
  type t = { name : string; att : Attributes.t }

  let attributes_to_list self = Attributes.bindings self.att

  open Attributes

  let make name attributes = { name; att = Attributes.of_list attributes }

  let prose_description self =
    match Attributes.find_opt AttributeKey.Prose_Description self.att with
    | Some (StringAttribute s) -> s
    | _ -> assert false

  let math_macro self =
    match find_opt AttributeKey.Math_Macro self.att with
    | Some (MathMacroAttribute s) -> Some s
    | _ -> None
end

(** A datatype for grouping (subsets of) type definitions. *)
module TypesRender : sig
  type type_subset_pointer = { type_name : string; variant_names : string list }
  type render = { name : string; pointers : type_subset_pointer list }

  val make : string -> (string * string list) list -> render
end = struct
  type type_subset_pointer = { type_name : string; variant_names : string list }
  type render = { name : string; pointers : type_subset_pointer list }

  let make name pointer_pairs =
    {
      name;
      pointers =
        List.map
          (fun (type_name, variant_names) -> { type_name; variant_names })
          pointer_pairs;
    }
end

(** The top-level elements of a specification. *)
type elem =
  | Elem_Type of Type.t
  | Elem_Relation of Relation.t
  | Elem_Constant of Constant.t
  | Elem_Render of TypesRender.render

type t = elem list
(** A specification is a list of top-level elements. *)
