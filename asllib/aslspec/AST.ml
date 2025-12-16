(** A module for representing the abstract syntax trees of aslspec --- the
    domain-specific language that defines the semantics-specification for ASL.
*)

exception SpecError of string

(** Extends an error message and re-raises an exception. This is a temporary
    hack until the AST supports source locations. *)
let stack_spec_error msg extra_msg =
  let full_msg = Printf.sprintf "%s\n%s" msg extra_msg in
  raise (SpecError full_msg)

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
    | Associative
        (** Whether an operator over a list of arguments should be rendered as a
            list of expressions separated by the operator macro. *)
    | Custom
        (** Whether an operator over a list of arguments should be rendered as a
            by a custom multi-argument macro. *)
    | Short_Circuit_Macro
        (** A LaTeX macro name to succinctly denote any value of a type [T].
            This is used to denote the short-circuit result of a relation
            application yielding a value of type [T]. *)
    | LHS_Hypertargets
        (** An attribute for [TypesRender] elements indicating whether
            hypertargets should be generated for the LHS of type definitions in
            the rendered output. *)
    | Auto_Name
        (** An attribute indicating whether automatic naming of sub-expressions
            in rule judgments should be enabled. *)

  (* A total ordering on attribute keys. *)
  let compare a b =
    let key_to_int = function
      | Prose_Description -> 0
      | Prose_Application -> 1
      | Math_Macro -> 2
      | Math_Layout -> 3
      | Short_Circuit_Macro -> 4
      | LHS_Hypertargets -> 5
      | Associative -> 6
      | Custom -> 7
      | Auto_Name -> 8
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
    | LHS_Hypertargets -> "lhs_hypertargets"
    | Associative -> "associative"
    | Custom -> "custom"
    | Auto_Name -> "auto_name"
end

(** A value associated with an attribute key. *)
type attribute =
  | StringAttribute of string
  | MathMacroAttribute of string
  | MathLayoutAttribute of layout
  | BoolAttribute of bool

type attribute_pairs = (AttributeKey.t * attribute) list

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

  (** Helper functions for typed attribute access *)

  (** [find_string key attrs] returns [Some s] if [key] maps to a
      [StringAttribute s] in [attrs], [None] otherwise. This assumes that [key]
      is indeed a string attribute. *)
  let find_string key attrs =
    match find_opt key attrs with
    | Some (StringAttribute s) -> Some s
    | Some _ -> assert false
    | _ -> None

  (** [get_string key attrs] returns the string value for [key] in [attrs], or
      [""] if not found. *)
  let get_string_or_empty key attrs =
    match find_opt key attrs with
    | Some (StringAttribute s) -> s
    | Some _ -> assert false
    | _ -> ""

  (** [get_string_exn key attrs] returns the string value for [key] in [attrs],
      or asserts false if not found or wrong type. *)
  let get_string_exn key attrs =
    match find_opt key attrs with
    | Some (StringAttribute s) -> s
    | _ -> assert false

  (** [find_math_macro key attrs] returns [Some s] if [key] is bound to the math
      macro [s], and [None] otherwise. This assumes that [key] is indeed a math
      macro attribute. *)
  let find_math_macro key attrs =
    match find_opt key attrs with
    | Some (MathMacroAttribute s) -> Some s
    | Some _ -> assert false
    | _ -> None

  (** [find_layout key attrs] returns [Some layout] if [key] is bound to
      [layout] in [attrs], and [None] otherwise. This assumes that [key] is
      indeed a math layout attribute. *)
  let find_layout key attrs =
    match find_opt key attrs with
    | Some (MathLayoutAttribute layout) -> Some layout
    | Some _ -> assert false
    | _ -> None

  (** [get_bool key ~default attrs] returns the bool value for [key] in [attrs],
      or [default] if not found. *)
  let get_bool key ~default attrs =
    match find_opt key attrs with
    | Some (BoolAttribute b) -> b
    | Some _ -> assert false
    | _ -> default
end

(** The kind of a type, either generic or AST-specific. *)
type type_kind = TypeKind_Generic | TypeKind_AST

(** A unary operator that transforms one type into another. *)
type type_operator =
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
  | TypeOperator of { op : type_operator; term : opt_named_type_term }
      (** A set containing all types formed by applying the type operator [op]
          to the type given by [term]. *)
  | LabelledTuple of {
      label_opt : string option;
      components : opt_named_type_term list;
    }
      (** A set containing all optionally-labelled tuples formed by the given
          components. An unlabelled tuple containing a single term is a special
          case - its domain is the domain of that term; this serves to reference
          types defined elsewhere. *)
  | LabelledRecord of { label_opt : string option; fields : record_field list }
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

and record_field = { name_and_type : named_type_term; att : Attributes.t }
(** A field of a record type. *)

(** [make_type_operation op term] Constructs a type term in which [op] is
    applied to [term]. *)
let make_type_operation op term = TypeOperator { op; term }

(** [make_tuple components] Constructs an unlabelled tuple for the tuple
    components [components]. *)
let make_tuple components = LabelledTuple { label_opt = None; components }

(** [make_labelled_tuple label components] Constructs a tuple labelled [label]
    and tuple components [components]. *)
let make_labelled_tuple label components =
  LabelledTuple { label_opt = Some label; components }

let make_record_field named_type_term attributes =
  let att = Attributes.of_list attributes in
  { name_and_type = named_type_term; att }

(** [make_record fields] Constructs an unlabelled record with fields [fields].
*)
let make_record fields = LabelledRecord { label_opt = None; fields }

let field_type { name_and_type = _, field_type; _ } = field_type
let field_name { name_and_type = name, _; _ } = name

let record_field_math_macro { att } =
  Attributes.find_math_macro AttributeKey.Math_Macro att

let record_field_prose_description { att } =
  Attributes.get_string_or_empty AttributeKey.Prose_Description att

(** [make_labelled_record label fields] Constructs a record labelled [label]
    with fields [fields]. *)
let make_labelled_record label fields =
  LabelledRecord { label_opt = Some label; fields }

module Expr = struct
  (** A term that can be used to form a rule judgment. *)
  type expr =
    | Var of string
    | FieldAccess of string list
        (** The first identifier is a variable and the rest are field names. *)
    | ListIndex of { list_var : string; index : expr }
        (** An expression indexing into the list variable [list_var] at position
            [index]. *)
    | Record of { label_opt : string option; fields : (string * expr) list }
        (** A record construction expression. *)
    | Application of { applicator : applicator; args : expr list }
        (** An application of [applicator] to the list of argument expressions
            [args]. For example, [annotate_literal(tenv, L_Int(one))], where
            [annotate_literal] is the applicator and [tenv] and [L_Int(one)] are
            the arguments. Here, [annotate_literal] is defined as a
            [typing function annotate_literal(tenv: static_envs, l: literal) ->
             (t: ty) ]. *)
    | Transition of {
        lhs : expr;
        rhs : expr;
        short_circuit : expr list option;
            (** The optional [short_circuit] contains short-circuiting
                alternatives. If [short_circuit] is [None], the alternatives are
                taken from the corresponding relation definition. Otherwise,
                they are overridden. *)
      }
        (** A transition from the [lhs] configuration to the [rhs] configuration
            with optional alternatives. *)
    | Indexed of { index : string; list : string; body : expr }
    | NamedExpr of expr * string
        (** An (internally-)named expression. Used for giving names to
            sub-expressions appearing in the output configuration of an output
            judgment. Initially, all expressions are unnamed, names are assigned
            during rule resolution. *)

  (** The left-hand side of an application expression. *)
  and applicator =
    | EmptyApplicator
        (** This is used for tuples like [(True, one)] where there is no
            applicator. *)
    | Relation of string
        (** Example: [annotate_literal] for an application like
            [annotate_literal(tenv, L_Int(one))] where [annotate_literal] is
            defined as
            [typing function annotate_literal(tenv: static_envs, l: literal) ->
             (t: ty)]. *)
    | TupleLabel of string
        (** Example: [S_Seq] for an AST node like [S_Seq(s1, s2)] where
            [ast stmt = S_Seq(first: stmt, second: stmt)]. *)
    | ExprOperator of string
        (** Example: [and] for an application like [and(True, False)] where
            [and] is defined as an operator. *)
    | Fields of string list
        (** Example: [tenv.static_envs_G.declared_types] in an expression
            [tenv.static_envs_G.declared_types(label)]. *)
    | Unresolved of expr

  (** [make_var id] constructs a variable expression with identifier [id]. *)
  let make_var id = Var id

  (** [make_tuple args] constructs a tuple expression with the given arguments.
  *)
  let make_tuple args = Application { applicator = EmptyApplicator; args }

  (** [make_application lhs exprs] constructs an application expression with
      left-hand side [lhs] and argument expressions [exprs]. During rule
      resolution, [lhs] is expected to resolve to either a relation name or a
      tuple label. *)
  let make_application lhs exprs =
    Application { applicator = Unresolved lhs; args = exprs }

  (** [make_operator_application op_name args] constructs an application
      expression representing the application of operator [op_name] to [args].
  *)
  let make_operator_application op_name args =
    Application { applicator = ExprOperator op_name; args }

  let make_record label_opt fields = Record { label_opt; fields }
  let make_list_index list_var index = ListIndex { list_var; index }
end

(** A datatype for a constant definition. *)
module Constant : sig
  type t = {
    name : string;
    opt_type : type_term option;
    opt_value_and_attributes : (Expr.expr * Attributes.t) option;
    att : Attributes.t;
  }

  val make :
    string ->
    type_term option ->
    (Expr.expr * attribute_pairs) option ->
    attribute_pairs ->
    t

  val attributes_to_list : t -> attribute_pairs
  val prose_description : t -> string
  val math_macro : t -> string option

  val value_math_layout : t -> layout option
  (** The layout for the value, if one exists. *)
end = struct
  type t = {
    name : string;
    opt_type : type_term option;
    opt_value_and_attributes : (Expr.expr * Attributes.t) option;
    att : Attributes.t;
  }

  let attributes_to_list self = Attributes.bindings self.att

  open Attributes

  let make name opt_type opt_value_and_attribute_pairs attributes =
    let opt_value_and_attributes =
      match opt_value_and_attribute_pairs with
      | Some (e, attr_pairs) -> Some (e, Attributes.of_list attr_pairs)
      | None -> None
    in
    {
      name;
      opt_type;
      opt_value_and_attributes;
      att = Attributes.of_list attributes;
    }

  let prose_description self =
    Attributes.get_string_or_empty AttributeKey.Prose_Description self.att

  let math_macro self =
    Attributes.find_math_macro AttributeKey.Math_Macro self.att

  let value_math_layout self =
    match self.opt_value_and_attributes with
    | Some (_, attrs) -> Attributes.find_layout AttributeKey.Math_Layout attrs
    | None -> None
end

(** A datatype for top-level type terms used in the definition of a type. *)
module TypeVariant : sig
  type t = { type_kind : type_kind; term : type_term; att : Attributes.t }

  val make : type_kind -> type_term -> attribute_pairs -> t
  val attributes_to_list : t -> attribute_pairs
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
    Attributes.get_string_or_empty AttributeKey.Prose_Description self.att

  let math_macro self =
    Attributes.find_math_macro AttributeKey.Math_Macro self.att

  let math_layout self =
    Attributes.find_layout AttributeKey.Math_Layout self.att
end

(** A datatype for a type definition. *)
module Type : sig
  type t = {
    name : string;
    type_kind : type_kind;
    variants : TypeVariant.t list;
    att : Attributes.t;
  }

  val make : type_kind -> string -> TypeVariant.t list -> attribute_pairs -> t
  val attributes_to_list : t -> attribute_pairs
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
    Attributes.get_string_or_empty AttributeKey.Prose_Description self.att

  let math_macro self =
    Attributes.find_math_macro AttributeKey.Math_Macro self.att

  let short_circuit_macro self =
    Attributes.find_math_macro AttributeKey.Short_Circuit_Macro self.att

  let math_layout self =
    Attributes.find_layout AttributeKey.Math_Layout self.att
end

(** A datatype for a set of inference rules for a given relation. *)
module Rule = struct
  open Expr

  type judgment = { expr : expr; is_output : bool; att : Attributes.t }
  (** A judgment represents either a premise or the the output configuration of
      the conclusion. If [is_output] is [true], the judgment represents the
      output configuration of the conclusion. *)

  (** [judgment_layout] returns the layout attribute of a judgment, if one was
      specified, and [Unspecified] otherwise. *)
  let judgment_layout { att } =
    match Attributes.find_opt AttributeKey.Math_Layout att with
    | Some (MathLayoutAttribute layout) -> layout
    | _ -> Unspecified

  (** [auto_name_judgment] returns [true] if automatic naming of sub-expressions
      in the judgment is enabled, [false] otherwise. By default, automatic
      naming is enabled unless the [auto_name] attribute is set to [false]. *)
  let auto_name_judgment { att } =
    Attributes.get_bool AttributeKey.Auto_Name ~default:true att

  (** A tree of elements. *)
  type rule_element =
    | Judgment of judgment  (** A leaf judgment. *)
    | Cases of case list

  and case = { name : string; elements : rule_element list }
  (** A sub-tree of judgments. *)

  type t = rule_element list

  (** The absolute path of a case name. *)
  let join_case_names names = String.concat "." names

  let make_judgement expr ~is_output attributes =
    { expr; is_output; att = Attributes.of_list attributes }

  let make_case name elements = { name; elements }
  let make_cases cases = Cases cases
  let make_root elements = elements
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
    parameters : string list;
        (** Type parameters. Currently, only available to operators. *)
    is_operator : bool;
    property : relation_property;
    category : relation_category option;
    input : opt_named_type_term list;
    output : type_term list;
    att : Attributes.t;
    rule_opt : Rule.t option;
  }

  val make :
    string ->
    relation_property ->
    relation_category option ->
    opt_named_type_term list ->
    type_term list ->
    attribute_pairs ->
    Rule.t option ->
    t

  val make_operator :
    string ->
    string list ->
    opt_named_type_term list ->
    type_term ->
    attribute_pairs ->
    t

  val attributes_to_list : t -> attribute_pairs
  val prose_description : t -> string
  val math_macro : t -> string option
  val prose_application : t -> string

  val is_associative_operator : t -> bool
  (** [is_associative_operator t] tests whether the operator represented by [t]
      has the [associative] attributes set to [true]. *)

  val is_custom_operator : t -> bool
  (** [is_custom_operator t] tests whether the operator represented by [t] has
      the [custom] attributes set to [true]. *)

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
    parameters : string list;
    is_operator : bool;
    property : relation_property;
    category : relation_category option;
    input : opt_named_type_term list;
    output : type_term list;
    att : Attributes.t;
    rule_opt : Rule.t option;
  }

  let make name property category input output attributes rule_opt =
    {
      name;
      parameters = [];
      is_operator = false;
      property;
      category;
      input;
      output;
      att = Attributes.of_list attributes;
      rule_opt;
    }

  let make_operator name parameters input output_type attributes =
    {
      name;
      parameters;
      is_operator = true;
      property = RelationProperty_Function;
      category = None;
      input;
      output = [ output_type ];
      att = Attributes.of_list attributes;
      rule_opt = None;
    }

  let attributes_to_list self = Attributes.bindings self.att

  open Attributes

  let prose_description self =
    Attributes.get_string_or_empty AttributeKey.Prose_Description self.att

  let math_macro self =
    Attributes.find_math_macro AttributeKey.Math_Macro self.att

  let prose_application self =
    Attributes.get_string_or_empty AttributeKey.Prose_Application self.att

  let math_layout self =
    Attributes.find_layout AttributeKey.Math_Layout self.att

  let is_associative_operator self =
    Attributes.get_bool AttributeKey.Associative ~default:false self.att

  let is_custom_operator self =
    Attributes.get_bool AttributeKey.Custom ~default:false self.att
end

(** A datatype for grouping (subsets of) type definitions. *)
module TypesRender : sig
  type type_subset_pointer = { type_name : string; variant_names : string list }

  type t = {
    name : string;
    pointers : type_subset_pointer list;
    att : Attributes.t;
  }

  val make : string -> (string * string list) list -> attribute_pairs -> t
  val attributes_to_list : t -> attribute_pairs

  val lhs_hypertargets : t -> bool
  (** Whether hypertargets should be generated for the LHS of type definitions
      in the rendered output. *)
end = struct
  type type_subset_pointer = { type_name : string; variant_names : string list }

  type t = {
    name : string;
    pointers : type_subset_pointer list;
    att : Attributes.t;
  }

  let make name pointer_pairs attributes =
    {
      name;
      pointers =
        List.map
          (fun (type_name, variant_names) -> { type_name; variant_names })
          pointer_pairs;
      att = Attributes.of_list attributes;
    }

  let attributes_to_list self = Attributes.bindings self.att

  let lhs_hypertargets self =
    Attributes.get_bool AttributeKey.LHS_Hypertargets ~default:true self.att
end

module RuleRender : sig
  type t = { name : string; relation_name : string; path : string }

  val make : name:string -> relation_name:string -> string list -> t
end = struct
  type t = { name : string; relation_name : string; path : string }

  (** [make ~name ~relation_name path] creates a rule render named [name], for
      the relation [relation_name] with [path] acting as a filter on the cases
      to render. Only cases whose predicates match the path are included (so an
      empty path selects all cases). *)
  let make ~name ~relation_name path =
    { name; relation_name; path = Rule.join_case_names path }
end

(** The top-level elements of a specification. *)
type elem =
  | Elem_Type of Type.t
  | Elem_Relation of Relation.t
  | Elem_Constant of Constant.t
  | Elem_RenderTypes of TypesRender.t
  | Elem_RenderRule of RuleRender.t

type t = elem list
(** A specification is a list of top-level elements. *)
