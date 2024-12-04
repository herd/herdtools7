type identifier = string

module TimeFrame : sig
  type t = Constant | Config | Execution

  val is_before : t -> t -> bool
  val max : t -> t -> t
  val of_ldk : AST.local_decl_keyword -> t
  val of_gdk : AST.global_decl_keyword -> t
end

type read = { name : identifier; time_frame : TimeFrame.t; immutable : bool }

(** Data type describing a potential side effect associated with an ASL piece of code. *)
type t =
  | ReadsLocal of read
      (** Reads the local storage element indicated by its argument. *)
  | WritesLocal of identifier
      (** Writes to the local variable indicated by its argument. *)
  | ReadsGlobal of read
      (** Reads the global storage element indicated by its argument. *)
  | WritesGlobal of identifier
      (** Writes to the global variable indicated by its argument. *)
  | ThrowsException of identifier
      (** Throws the exception indicated by its argument. *)
  | CallsRecursive of identifier
      (** Calls the function indicated by its argument. Can only happen in a
          strongly-connected component of mutually recursive functions. *)
  | PerformsAssertions  (** Performs an assertion. *)
  | NonDeterministic
      (** Uses a non-deterministic construct such as [ARBITRARY: ty]. *)

type side_effect = t

val equal : t -> t -> bool
val compare : t -> t -> int
val pp_print : Format.formatter -> t -> unit
val time_frame : t -> TimeFrame.t
val is_pure : t -> bool
val is_statically_evaluable : t -> bool

(** The module [SES] provides an abstraction over a set of side-effects. *)
module SES : sig
  type t
  (** A side-effect set. *)

  (* Constructors *)
  val empty : t
  val reads_local : identifier -> TimeFrame.t -> bool -> t
  val writes_local : identifier -> t
  val reads_global : identifier -> TimeFrame.t -> bool -> t
  val writes_global : identifier -> t
  val throws_exception : identifier -> t
  val calls_recursive : identifier -> t
  val performs_assertions : t
  val non_deterministic : t

  (* Properties *)
  val max_time_frame : t -> TimeFrame.t
  val is_pure : t -> bool
  val is_statically_evaluable : t -> bool
  val equal : t -> t -> bool
  val is_deterministic : t -> bool

  (* Updates *)
  val add_local_read : identifier -> TimeFrame.t -> bool -> t -> t
  val add_local_write : identifier -> t -> t
  val add_global_read : identifier -> TimeFrame.t -> bool -> t -> t
  val add_global_write : identifier -> t -> t
  val add_thrown_exception : identifier -> t -> t
  val add_calls_recursive : identifier -> t -> t
  val add_side_effect : side_effect -> t -> t
  val add_assertion : t -> t
  val add_non_determinism : t -> t
  val remove_pure : t -> t
  val remove_locals : t -> t
  val remove_thrown_exceptions : t -> t
  val remove_calls_recursives : t -> t
  val remove_assertions : t -> t
  val remove_non_determinism : t -> t
  val filter_thrown_exceptions : (identifier -> bool) -> t -> t
  val filter_calls_recursives : (identifier -> bool) -> t -> t

  (* Operations *)
  val union : t -> t -> t
  val unions : t list -> t
  val union3 : t -> t -> t -> t

  val non_conflicting_union :
    fail:(side_effect * side_effect -> t) -> t -> t -> t

  val non_conflicting_unions :
    fail:(side_effect * side_effect -> t) -> t list -> t

  val get_calls_recursives : t -> ASTUtils.ISet.t

  (* Input & output *)
  val to_side_effect_list : t -> side_effect list
  val pp_print : Format.formatter -> t -> unit

  module SESet : Set.S with type elt = side_effect

  val to_side_effect_set : t -> SESet.t
end
