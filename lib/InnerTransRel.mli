(** Implements an acyclic transitive relation. *)

module type S = sig
  (** Output module. *)

  type elt
  (** Type of nodes in this relation. *)

  module Set : MySet.S with type elt = elt
  module Rel : InnerRel.S with type elt0 = elt and module Elts = Set

  type t = { input : Set.t; rel : Rel.t; output : Set.t }
  (** Represents an (acyclic) transitive relation from input to output:
        + input is the set of minimal events (no relation to them);
        + output is the set of maximal events (no relation from them);
        + rel is a relation of which transitive closure is the relation
          represented.

      The input and output field are here for the efficiency, except
      for the case  of an empty relation where they are equal and describe
      the relation support.

      In the following we identify the rel components and the values of
      type t. Notice that, if r is this component, the value of type t
      represents r+.

      Users should not use this representation to build their
      relation, but they should rely on the constructor [from_nodes] and the
      [union] and [seq] operations.
  *)

  val empty : t
  (** The empty relation. *)

  val is_empty : t -> bool
  (** Check for emptiness *)

  val from_nodes : Set.t -> t
  (** Builds a transitive relation with those nodes and no edges. *)

  val map_nodes : (elt -> elt) -> t -> t
  (** maps on nodes, does not affect edges. *)

  val seq : t -> t -> t
  (** Sequential composition.
      [seq r1 r2] return a relation r, s. t. (a,b) is in r+, iff there
      exists  c and d, s.t, (a,c) is in r1+ and (d,b) is in r2+. *)

  val union : t -> t -> t
  (** Parallel composition, union of graphs.
      [union r1 r2] returns a relation r, s.t. r+ is (r1|r2)+. *)


  val union3 : t -> t -> t -> t
  val union4 : t -> t -> t -> t -> t
  val union5 : t -> t -> t -> t -> t -> t
  val union6 : t -> t -> t -> t -> t -> t -> t
  val unions : t list -> t

  val to_implicitely_transitive_rel : t -> Rel.t
  (** [to_implicitely_transitive_rel t] is a non-transitive
      representation of t, _i.e._ the function returns r
      such that [t] represent r+.
      In practice, r is the internal representation of [t].
  *)

end

module Make : functor (O : MySet.OrderedType) ->
  S
    with type elt = O.t
     and module Set = MySet.Make(O)
     and module Rel = InnerRel.Make(O)
