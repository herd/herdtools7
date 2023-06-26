(** Type Algebra *)

(**
   Types are defined as {!AST.ty}. This should map pretty-well with the current
   version of the Language Reference Manual.
*)

open AST

(** {1 Predicates on types} *)

val is_builtin : ty -> bool
val is_builtin_singular : ty -> bool
val is_builtin_aggregate : ty -> bool

(** Note that a builtin type is either builtin aggregate or builtin singular. *)

val is_singular : Env.Static.env -> ty -> bool
val is_aggregate : Env.Static.env -> ty -> bool

(** Note that a type is either singular or aggregate. *)

val is_named : ty -> bool
(** Types declared using the [type] syntax. *)

val is_anonymous : ty -> bool
(** Those not declared using †he [type] syntax. *)

(** Note that a type is either builtin, named or anonymous. *)

val is_primitive : ty -> bool
(** Types that only use the builtin types. *)

val is_non_primitive : ty -> bool
(** Types that are named types or which make use of named types.

    Usually for all [ty]: {[
      is_non_primitive ty = not (is_primitive ty)
    ]}
*)

(** {1 Relations on types} *)

(** {2 Type transformations} *)

val get_structure : Env.Static.env -> ty -> ty
(** The structure of a type is the primitive type that can hold the same
    values. *)

(** {2 Domains} *)

(** The domain of a type is the set of values which storagbe element of that type may hold. *)
module Domain : sig
  type t
  (** Abstract value set. *)

  val pp : Format.formatter -> t -> unit
  (** A printer for the domain type. *)

  val of_type : Env.Static.env -> ty -> t
  (** Construct the domain of a type. *)

  val mem : AST.value -> t -> bool
  (** [mem v d] is true if and only if [v] is in [d]. *)

  val equal : t -> t -> bool
  (** Wheather two domains are equal. *)

  val compare : t -> t -> int option
  (** The inclusion order on domains.

      It is a partial order. *)
end

(** {2 Orders on types} *)

val subtype_satisfies : Env.Static.env -> ty -> ty -> bool
(** Subtype-satisfation as per Definition TRVR. *)

val domain_subtype_satisfies : Env.Static.env -> ty -> ty -> bool
val structural_subtype_satisfies : Env.Static.env -> ty -> ty -> bool

val type_satisfies : Env.Static.env -> ty -> ty -> bool
(** Type-satisfation as per Rule FMXK. *)

val type_clashes : Env.Static.env -> ty -> ty -> bool
(** Type-clashing relation.

    Notes:
      - T subtype-satisfies S implies T and S type-clash
      - This is a equivalence relation

    As par Definition VPZZ.
*)

val subprogram_clashes :
  Env.Static.env -> 'a func_skeleton -> 'b func_skeleton -> bool
(** Subprogram clashing relation.

    As per Definition BTBR.
*)

val lowest_common_ancestor : Env.Static.env -> ty -> ty -> ty option
(** Lowest common ancestor.

    As per Rule YZHM.
*)
