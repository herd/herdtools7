(** A module for representing the abstract syntax trees of the domain-specific
    language, which defines the semantics-specification for ASL. *)

exception SpecError of string

type type_kind = TypeKind_Generic | TypeKind_AST

(** Terms for constructing types out of other types, with [Label t] being the
    leaf case.

    In the context of a type definition, a [Label] variant defines a new label -
    a type representing just this single label. In other contexts, for example a
    type variant appearing in the signature of a relation, this can either refer
    to a type name of a label defined as a type variant. *)
type type_term =
  | Label of string
      (** Either a set containing the single value named by the given string or
          a reference to a type with the given name. *)
  | Powerset of { term : type_term; finite : bool }
      (** A set containing all subsets of the given type. If [finite] is true
          then only the finite subsets are included. *)
  | Option of type_term
      (** Either the empty set of a set containing a single value of the given
          type. *)
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
  | List of { maybe_empty : bool; member_type : type_term }
      (** A set containing all sequences of the given member type. If
          [maybe_empty] is true, the list may also be empty. *)
  | ConstantsSet of string list
      (** A set containing all constants formed by the given names. *)
  | Function of { from_type : type_term; to_type : type_term; total : bool }
      (** A set containing all functions formed by the given types. If [total]
          is true, the function is total, otherwise it is partial. *)

and named_type_term = string * type_term
(** A term associated with a variable name. *)

and opt_named_type_term = string option * type_term
(** A term optionally associated with a variable name. *)

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

(** Specifies how to layout a compound term. *)
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
    | Prose_Description  (** A description of the element in prose. *)
    | Prose_Application
        (** A description of the element in prose describing its application in
            an inference rule premise. *)
    | Math_Macro  (** A LaTeX macro for the element. *)
    | Math_Layout  (** The layout of the element in a mathematical context. *)

  (* A total ordering on attribute keys. *)
  let compare a b =
    let key_to_int = function
      | Prose_Description -> 0
      | Prose_Application -> 1
      | Math_Macro -> 2
      | Math_Layout -> 3
    in
    let a_int = key_to_int a in
    let b_int = key_to_int b in
    Stdlib.compare a_int b_int

  (** Converts an attribute key with the same string as the corresponding token.
  *)
  let to_str = function
    | Prose_Description -> "prose_description"
    | Prose_Application -> "prose_application"
    | Math_Macro -> "math_macro"
    | Math_Layout -> "math_layout"
end

(** A value associated with an attribute key. *)
type attribute = StringAttribute of string | MathLayoutAttribute of layout

(** A module for associating attributes with attribute keys. *)
module Attributes = struct
  include Map.Make (AttributeKey)

  type 'a map = 'a t
  (** Shadows [Map.t] with a string-to-string map type. *)

  type t = attribute map

  (** Shadows [of_list] by raising a [SpecError] exception on pairs containing
      the same key. *)
  let of_list pairs =
    List.fold_left
      (fun acc_map (k, v) ->
        if mem k acc_map then
          let msg =
            Format.sprintf {| encountered second occurrence of attribute '%s'|}
              (AttributeKey.to_str k)
          in
          raise (SpecError msg)
        else add k v acc_map)
      empty pairs
end

(** A module signature for a datatype with attributes. *)
module type HasAttributes = sig
  type t

  val attributes : t -> Attributes.t
  val prose_description : t -> string
  val math_macro : t -> string option
  val attributes_to_list : t -> (AttributeKey.t * attribute) list
end

(** A datatype for type terms used in the definition of a type, with associated
    attributes. *)
module TypeVariant : sig
  type t = { type_kind : type_kind; term : type_term; att : Attributes.t }

  val make : type_kind -> type_term -> (AttributeKey.t * attribute) list -> t
  val attributes_to_list : t -> (AttributeKey.t * attribute) list
  val prose_description : t -> string
  val math_macro : t -> string option
  val math_layout : t -> layout option
end = struct
  type t = { type_kind : type_kind; term : type_term; att : Attributes.t }

  let make type_kind term attribute_pairs =
    { type_kind; term; att = Attributes.of_list attribute_pairs }

  open Attributes

  let attributes_to_list self = bindings self.att

  let prose_description self =
    match Attributes.find_opt AttributeKey.Prose_Description self.att with
    | Some (StringAttribute s) -> s
    | _ -> assert false

  let math_macro self =
    match find_opt AttributeKey.Math_Macro self.att with
    | Some (StringAttribute s) -> Some s
    | _ -> None

  let math_layout self =
    match find_opt AttributeKey.Math_Layout self.att with
    | Some (MathLayoutAttribute layout) -> Some layout
    | _ -> None
end

(** A datatype for a type definition. *)
module Type : sig
  include HasAttributes

  val kind : t -> type_kind
  val name : t -> string
  val variants : t -> TypeVariant.t list

  val make :
    type_kind ->
    string ->
    TypeVariant.t list ->
    (AttributeKey.t * attribute) list ->
    t

  val math_layout : t -> layout option
end = struct
  type t = {
    type_kind : type_kind;
    name : string;
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

  let kind self = self.type_kind
  let name self = self.name
  let variants self = self.variants

  open Attributes

  let prose_description self =
    match Attributes.find_opt AttributeKey.Prose_Description self.att with
    | Some (StringAttribute s) -> s
    | _ -> assert false

  let math_macro self =
    match find_opt AttributeKey.Math_Macro self.att with
    | Some (StringAttribute s) -> Some s
    | _ -> None

  let math_layout self =
    match find_opt AttributeKey.Math_Layout self.att with
    | Some (MathLayoutAttribute layout) -> Some layout
    | _ -> None

  let attributes_to_list self = Attributes.bindings self.att
  let attributes self = self.att
end

(** A datatype for a relation definition. *)
module Relation : sig
  include HasAttributes

  val name : t -> string
  val input : t -> opt_named_type_term list
  val output : t -> type_term list

  val make :
    string ->
    opt_named_type_term list ->
    type_term list ->
    (AttributeKey.t * attribute) list ->
    t

  val prose_description : t -> string
  val math_macro : t -> string option
  val prose_application : t -> string
  val math_layout : t -> layout option
  val attributes_to_list : t -> (AttributeKey.t * attribute) list
end = struct
  type t = {
    name : string;
    input : opt_named_type_term list;
    output : type_term list;
    att : Attributes.t;
  }

  let name self = self.name
  let input self = self.input
  let output self = self.output

  let make name input output attributes =
    { name; input; output; att = Attributes.of_list attributes }

  open Attributes

  let prose_description self =
    match Attributes.find_opt AttributeKey.Prose_Description self.att with
    | Some (StringAttribute s) -> s
    | _ -> assert false

  let math_macro self =
    match find_opt AttributeKey.Math_Macro self.att with
    | Some (StringAttribute s) -> Some s
    | _ -> None

  let prose_application self =
    match find_opt AttributeKey.Prose_Application self.att with
    | Some (StringAttribute s) -> s
    | _ -> assert false

  let math_layout self =
    match find_opt AttributeKey.Math_Layout self.att with
    | Some (MathLayoutAttribute layout) -> Some layout
    | _ -> None

  let attributes_to_list self = Attributes.bindings self.att
  let attributes self = self.att
end

module Constant : sig
  include HasAttributes

  val name : t -> string
  val prose_description : t -> string
  val math_macro : t -> string option
  val attributes_to_list : t -> (AttributeKey.t * attribute) list
  val make : string -> (AttributeKey.t * attribute) list -> t
end = struct
  type t = { name : string; att : Attributes.t }

  let name self = self.name

  open Attributes

  let make name attributes = { name; att = Attributes.of_list attributes }

  let prose_description self =
    match Attributes.find_opt AttributeKey.Prose_Description self.att with
    | Some (StringAttribute s) -> s
    | _ -> assert false

  let math_macro self =
    match find_opt AttributeKey.Math_Macro self.att with
    | Some (StringAttribute s) -> Some s
    | _ -> None

  let attributes_to_list self = Attributes.bindings self.att
  let attributes self = self.att
end

type type_subset_pointer = { type_name : string; variant_names : string list }
type render = { render_name : string; pointers : type_subset_pointer list }

let make_render render_name pointer_pairs =
  {
    render_name;
    pointers =
      List.map
        (fun (type_name, variant_names) -> { type_name; variant_names })
        pointer_pairs;
  }

type elem =
  | Elem_Type of Type.t
  | Elem_Relation of Relation.t
  | Elem_Constant of Constant.t
  | Elem_Render of render

type t = elem list
