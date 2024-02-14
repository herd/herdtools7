(** This modules implement a strong component analysis. *)

(** {1 Topological sorting of ASTs.} *)

(** In the following, {i dependencies} are understood in the ASL declaration
    level. For example, in the following ASL code:
    {v
       constant x: integer = 4;
       constant y: integer = x + 1;
    v}
    the type-checker should first type-checks [x] and put it in the
    environment, before type-checking [y], which would otherwise complain about
    [x] not being defined.

    We also need strongly connected components analysis for mutually recursive
    functions, which are allowed in ASL. For example, the following example
    should be legal:
    {v
       func f (x: integer) => integer
       begin return 2 * g (x - 1); end

       func g (x: integer) => integer
       begin return if x <= 0 then -x else 1 + f(x); end
    v}
    Those two function should be handled by the type-checker at the same time:
    both need the declaration of the other to be correctly type-checked.
*)

(** Entry-point for dependency-ordered iterations on ASTs. *)
module ASTFold : sig
  (** A step in the strongly-connected folder. *)
  type step =
    | Single of AST.decl  (** A single declaration that is not recursive. *)
    | Recursive of AST.decl list
        (** A set of mutually recursive definitions. *)

  val fold : (step -> 'acc -> 'acc) -> AST.t -> 'acc -> 'acc
  (** [fold f ast] is the iterations of [f] on all mutually recursive
      declarations in [ast] ordered by their definitions. *)
end

(** {1 Abstract implementation of topological sorting} *)

(** Signature of the module argument of the [Make] functor. *)
module type OrderedHashedType = sig
  include Set.OrderedType
  include Hashtbl.HashedType with type t := t

  val to_string : t -> string
  (** For debugging purposes. *)
end

(** Abstract topological sorting module.

    This implements the Trajan algorithm.
*)
module Make (O : OrderedHashedType) : sig
  type succs = O.t -> O.t list
  (** Type of functions returning the successor of a node. *)

  val fold_strong_connected :
    ?size_hint:int -> (O.t list -> 'a -> 'a) -> O.t list -> succs -> 'a -> 'a
  (** fold the strongly connected components following the derived order given
      by [succs]. *)

  val sort_connected : O.t list -> succs -> O.t list list
  (** Sort the strongly connected components following the derived order given
      by [succs]. *)

  val index_connected : O.t list -> succs -> (O.t * int) list
  (** index all nodes in an order that respects the order given by [succs].
      Nodes have the same index if and only if they are in the same strongly
      connected component. *)

  module Properties : sig
    val order_respected : O.t list * succs -> bool
  end
end
