(** Implements a transitive relation. *)

module type S = sig
  (** Output module. *)

  type elt
  (** Type of nodes in this relation. *)

  module Set : MySet.S with type elt = elt
  module Rel : InnerRel.S with type elt0 = elt and module Elts = Set

  type t = { input : Set.t; rel : Rel.t; output : Set.t }
  (** Represents a transitive relation.

      Note that users should not use this representation to build their
      relation, but they should rely on the constructor [from_nodes] and the
      [union] and [seq] operations.
  *)

  val empty : t
  (** The empty relation. *)

  val from_nodes : Set.t -> t
  (** Builds a transitive relation with those nodes and no edges. *)

  val map_nodes : (elt -> elt) -> t -> t
  (** maps on nodes, does not affect edges. *)

  val seq : t -> t -> t
  (** Sequential operation: every element of t1 is before every element of t2. *)

  val union : t -> t -> t
  (** Parallel composition, union of graphs. *)

  val union3 : t -> t -> t -> t
  val union4 : t -> t -> t -> t -> t
  val union5 : t -> t -> t -> t -> t -> t
  val union6 : t -> t -> t -> t -> t -> t -> t
  val unions : t list -> t

  val to_transitive_rel : t -> Rel.t
  (** [to_transitive_rel t] is the InnerRel representation of t. *)

  val to_implicitely_transitive_rel : t -> Rel.t
  (** [to_implicitely_transitive_rel t] is a non-transitive representation of t. *)
end

module Make : functor (O : MySet.OrderedType) ->
  S
    with type elt = O.t
     and module Set = MySet.Make(O)
     and module Rel = InnerRel.Make(O)
