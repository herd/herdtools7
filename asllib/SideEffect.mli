type identifier = string

module ISet = ASTUtils.ISet
module IMap = ASTUtils.IMap

module TimeFrame : sig
  type t = Constant | Config | Execution of bool

  val is_before : t -> t -> bool
  val max : t -> t -> t
  val maxs : t list -> t
  val of_ldk : AST.local_decl_keyword -> t
  val of_gdk : AST.global_decl_keyword -> t
end

type t =
  | ReadLocal of identifier * TimeFrame.t
  | WriteLocal of identifier
  | ReadGlobal of identifier * TimeFrame.t
  | WriteGlobal of identifier
  | ThrowException of identifier
  | RecursiveCall of identifier
  | PerformsAssertions
  | NonDeterministic

type side_effect = t

val equal : t -> t -> bool
val compare : t -> t -> int
val pp_print : Format.formatter -> t -> unit
val time_frame : t -> TimeFrame.t
val is_pure : t -> bool
val is_immutable : t -> bool
val is_statically_evaluable : t -> bool

(** The module [SES] provides an abstraction over a set of side-effects. *)
module SES : sig
  type t
  (** A side-effect set. *)

  (* Constructors *)
  val empty : t
  val read_local : identifier -> TimeFrame.t -> t
  val write_local : identifier -> t
  val read_global : identifier -> TimeFrame.t -> t
  val write_global : identifier -> t
  val throw_exception : identifier -> t
  val recursive_call : identifier -> t
  val performs_assertions : t
  val non_deterministic : t

  (* Properties *)
  val max_time_frame : t -> TimeFrame.t
  val is_pure : t -> bool
  val is_statically_evaluable : t -> bool
  val equal : t -> t -> bool
  val is_deterministic : t -> bool

  (* Setters *)
  val add_local_read : identifier -> TimeFrame.t -> t -> t
  val add_local_write : identifier -> t -> t
  val add_global_read : identifier -> TimeFrame.t -> t -> t
  val add_global_write : identifier -> t -> t
  val add_thrown_exception : identifier -> t -> t
  val add_recursive_call : identifier -> t -> t
  val add_side_effect : side_effect -> t -> t
  val set_assertions_performed : t -> t
  val set_non_determinism : t -> t
  val remove_pure : t -> t
  val remove_locals : t -> t
  val remove_thrown_exceptions : t -> t
  val remove_recursive_calls : t -> t
  val remove_assertions : t -> t
  val remove_non_determinism : t -> t
  val filter_thrown_exceptions : (identifier -> bool) -> t -> t
  val filter_recursive_calls : (identifier -> bool) -> t -> t

  (* Operations *)
  val union : t -> t -> t
  val unions : t list -> t
  val union3 : t -> t -> t -> t

  val non_conflicting_union :
    fail:(side_effect * side_effect -> t) -> t -> t -> t

  val non_conflicting_unions :
    fail:(side_effect * side_effect -> t) -> t list -> t

  (* Folders *)
  val fold_recursive_calls : (identifier -> 'a -> 'a) -> t -> 'a -> 'a

  (* Input & output *)
  val to_side_effect_list : t -> side_effect list
  val pp_print : Format.formatter -> t -> unit

  module SESet : Set.S with type elt = side_effect

  val to_side_effect_set : t -> SESet.t
end
