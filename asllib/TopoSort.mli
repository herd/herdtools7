module type OrderedHashedType = sig
  include Set.OrderedType
  include Hashtbl.HashedType with type t := t

  val to_string : t -> string
end

module Make (O : OrderedHashedType) : sig
  (** Type of functions returning the successor of a node. *)
  type succs = O.t -> O.t list

  (** fold the strongly connected components following the derived order given
      by [succs]. *)
  val fold_strong_connected :
    ?size_hint:int -> (O.t list -> 'a -> 'a) -> O.t list -> succs -> 'a -> 'a

  (** Sort the strongly connected components following the derived order given
      by [succs]. *)
  val sort_connected : O.t list -> succs -> O.t list list

  (** index all nodes in an order that respects the order given by [succs].
      Nodes have the same index if and only if they are in the same strongly
      connected component. *)
  val index_connected : O.t list -> succs -> (O.t * int) list

  module Properties : sig
    val order_respected : O.t list * succs -> bool
  end
end
