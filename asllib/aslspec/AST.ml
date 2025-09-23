(** A module for representing the abstract syntax trees
    of the domain-specific language, which defines the
    semantics-specification for ASL.
*)

exception SpecError of string

type type_kind = TypeKind_Generic | TypeKind_AST

(** Terms for constructing types out of other types,
  with [Label t] being the leaf case.

  In the context of a type definition, a [Label] variant
  defines a new label - a type representing just this
  single label. In other contexts, for example a type
  variant appearing in the signature of a relation,
  this can either refer to a type name of a label
  defined as a type variant.
*)
type type_term =
  | Label of string
      (** Either a set containing the single value named by the given string
       or a reference to a type with the given name. *)
  | Powerset of type_term
      (** A set containing all subsets of the given type. *)
  | Option of type_term
      (** Either the empty set of a set containing a single value of the given type. *)
  | Tuple of opt_named_type_term list
      (** A set containing all tuples formed by the given components.
        A tuple containing a single term is a special case - its domain is the domain
        of that term.
    *)
  | LabelledTuple of { label : string; components : opt_named_type_term list }
      (** A set containing all labelled tuples formed by the given components. *)
  | Record of named_type_term list
      (** A set containing all records formed by the given fields. *)
  | LabelledRecord of { label : string; fields : named_type_term list }
      (** A set containing all labelled records formed by the given fields. *)
  | List of { maybe_empty : bool; member_type : type_term }
      (** A set containing all sequences of the given member type. If [maybe_empty] is true, the list may also be empty. *)
  | ConstantsSet of string list
      (** A set containing all constants formed by the given names. *)
  | Function of { from_type : type_term; to_type : type_term; total : bool }
      (** A set containing all functions formed by the given types. If [total] is true, the function is total, otherwise it is partial. *)

and named_type_term = string * type_term
(** A term associated with a variable name. *)

and opt_named_type_term = string option * type_term
(** A term optionally associated with a variable name. *)

(** Specifies how to layout a term. *)
type layout =
  | Unspecified
      (** No specific layout, appropriate for atomic terms and terms with singleton lists. *)
  | Horizontal of layout list  (** Layout terms horizontally. *)
  | Vertical of layout list  (** Layout terms vertically. *)

(** A module for totally ordered attribute keys. *)
module AttributeKey = struct
  type t = Prose_Description | Prose_Application | Math_Macro | Math_Layout

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

  let to_str = function
    | Prose_Description -> "prose_description"
    | Prose_Application -> "prose_application"
    | Math_Macro -> "math_macro"
    | Math_Layout -> "math_layout"
end

type attribute = StringAttribute of string | MathLayoutAttribute of layout

(** A module for key-value attributes. *)
module Attributes = struct
  include Map.Make (AttributeKey)

  (* Shadow [Map.t] with a string-to-string map type. *)
  type 'a map = 'a t
  type t = attribute map

  (** Shadow [of_list] by raising a [SpecError] exception on pairs
     containing the same key. *)
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

(** A datatype for type terms used in the definition of a type,
    with associated attributes.
*)
module TypeVariant : sig
  include HasAttributes

  val make : type_kind -> type_term -> (AttributeKey.t * attribute) list -> t
  val kind : t -> type_kind
  val term : t -> type_term
  val math_layout : t -> layout option
end = struct
  type t = { type_kind : type_kind; term : type_term; att : Attributes.t }

  let make type_kind term attribute_pairs =
    { type_kind; term; att = Attributes.of_list attribute_pairs }

  let kind self = self.type_kind
  let term self = self.term

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

  let attributes_to_list self = bindings self.att
  let attributes self = self.att
end

(** A datatype for a type definition.
*)
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
        (fun variant ->
          let term = TypeVariant.term variant in
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

(** A datatype for a relation definition.
*)
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

type elem =
  | Elem_Type of Type.t
  | Elem_Relation of Relation.t
  | Elem_Constant of Constant.t

type t = elem list
