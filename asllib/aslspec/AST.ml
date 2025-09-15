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
  | Powerset of type_term
  | Option of type_term
  | Tuple of opt_named_type_term list
  | LabelledTuple of { label : string; components : opt_named_type_term list }
  | Record of named_type_term list
  | LabelledRecord of { label : string; fields : named_type_term list }
  | List of { maybe_empty : bool; member_type : type_term }
  | ConstantsSet of string list
  | Function of { from_type : type_term; to_type : type_term; total : bool }

and named_type_term = string * type_term
(** A term associated with a variable name. *)

and opt_named_type_term = string option * type_term
(** A term optionally associated with a variable name. *)

(** A module for key-value attributes. *)
module Attributes = struct
  let prose_description = "prose_description"
  let prose_application = "prose_application"
  let math_macro = "math_macro"

  include Map.Make (String)

  (* Shadow [Map.t] with a string-to-string map type. *)
  type 'a map = 'a t
  type t = String.t map

  (** Shadow [of_list] by raising a [SpecError] exception on pairs
     containing the same key. *)
  let of_list pairs =
    List.fold_left
      (fun acc_map (k, v) ->
        if mem k acc_map then
          let msg =
            Format.sprintf
              {| encountered second occurrence of attribute '%s' with "%s" |} k
              v
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
  val attributes_to_list : t -> (string * string) list
end

(** A datatype for type terms used in the definition of a type,
    with associated attributes.
*)
module TypeVariant : sig
  include HasAttributes

  val make : type_kind -> type_term -> (string * string) list -> t
  val kind : t -> type_kind
  val term : t -> type_term
end = struct
  type t = { type_kind : type_kind; term : type_term; att : Attributes.t }

  let make type_kind term attribute_pairs =
    { type_kind; term; att = Attributes.of_list attribute_pairs }

  let kind self = self.type_kind
  let term self = self.term

  open Attributes

  let prose_description self = find prose_description self.att
  let math_macro self = find_opt math_macro self.att
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
    type_kind -> string -> TypeVariant.t list -> (string * string) list -> t
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

  module A = Attributes

  let prose_description self = A.find A.prose_description self.att
  let math_macro self = A.find_opt A.math_macro self.att
  let attributes_to_list self = A.bindings self.att
  let attributes self = self.att
end

(** A datatype for a relation definition.
*)
module Relation : sig
  include HasAttributes

  val name : t -> string
  val input : t -> named_type_term list
  val output : t -> type_term list

  val make :
    string ->
    named_type_term list ->
    type_term list ->
    (string * string) list ->
    t

  val prose_description : t -> string
  val math_macro : t -> string option
  val prose_application : t -> string
  val attributes_to_list : t -> (string * string) list
end = struct
  type t = {
    name : string;
    input : named_type_term list;
    output : type_term list;
    att : Attributes.t;
  }

  let name self = self.name
  let input self = self.input
  let output self = self.output

  module A = Attributes

  let make name input output attributes =
    { name; input; output; att = A.of_list attributes }

  let prose_description self = A.find A.prose_description self.att
  let math_macro self = A.find_opt A.math_macro self.att
  let prose_application self = A.find A.prose_application self.att
  let attributes_to_list self = A.bindings self.att
  let attributes self = self.att
end

module Constant : sig
  include HasAttributes

  val name : t -> string
  val prose_description : t -> string
  val math_macro : t -> string option
  val attributes_to_list : t -> (string * string) list
  val make : string -> (string * string) list -> t
end = struct
  type t = { name : string; att : Attributes.t }

  let name self = self.name

  module A = Attributes

  let make name attributes = { name; att = A.of_list attributes }
  let prose_description self = A.find A.prose_description self.att
  let math_macro self = A.find_opt A.math_macro self.att
  let attributes_to_list self = A.bindings self.att
  let attributes self = self.att
end

type elem =
  | Elem_Type of Type.t
  | Elem_Relation of Relation.t
  | Elem_Constant of Constant.t

type t = elem list
