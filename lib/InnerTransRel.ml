module type S = sig
  type elt

  module Set : MySet.S with type elt = elt
  module Rel : InnerRel.S with type elt0 = elt and module Elts = Set

  type t = { input : Set.t; rel : Rel.t; output : Set.t }

  val empty : t
  val from_nodes : Set.t -> t
  val map_nodes : (elt -> elt) -> t -> t
  val seq : t -> t -> t
  val union : t -> t -> t
  val union3 : t -> t -> t -> t
  val union4 : t -> t -> t -> t -> t
  val union5 : t -> t -> t -> t -> t -> t
  val union6 : t -> t -> t -> t -> t -> t -> t
  val unions : t list -> t
  val to_transitive_rel : t -> Rel.t
end

module Make (O : MySet.OrderedType) :
  S
    with type elt = O.t
     and module Set = MySet.Make(O)
     and module Rel = InnerRel.Make(O) = struct
  module Set = MySet.Make (O)
  module Rel = InnerRel.Make (O)

  type elt = O.t
  type t = { input : Set.t; rel : Rel.t; output : Set.t }

  let empty = { input = Set.empty; rel = Rel.empty; output = Set.empty }
  let from_nodes events = { input = events; rel = Rel.empty; output = events }

  let map_nodes f { input; rel; output } =
    {
      input = Set.map f input;
      rel = Rel.map_nodes f rel;
      output = Set.map f output;
    }

  let seq t1 t2 =
    assert (not (Set.is_empty t1.output));
    assert (not (Set.is_empty t2.input));
    {
      input = t1.input;
      rel = Rel.union3 t1.rel t2.rel (Rel.cartesian t1.output t2.input);
      output = t2.output;
    }

  let union t1 t2 =
    {
      input = Set.union t1.input t2.input;
      rel = Rel.union t1.rel t2.rel;
      output = Set.union t1.output t2.output;
    }

  let union3 t1 t2 t3 =
    {
      input = Set.union3 t1.input t2.input t3.input;
      rel = Rel.union3 t1.rel t2.rel t3.rel;
      output = Set.union3 t1.output t2.output t3.output;
    }

  let union4 t1 t2 t3 t4 =
    {
      input = Set.union4 t1.input t2.input t3.input t4.input;
      rel = Rel.union4 t1.rel t2.rel t3.rel t4.rel;
      output = Set.union4 t1.output t2.output t3.output t4.output;
    }

  let union5 t1 t2 t3 t4 t5 =
    {
      input = Set.union5 t1.input t2.input t3.input t4.input t5.input;
      rel = Rel.union5 t1.rel t2.rel t3.rel t4.rel t5.rel;
      output = Set.union5 t1.output t2.output t3.output t4.output t5.output;
    }

  let union6 t1 t2 t3 t4 t5 t6 =
    {
      input = Set.union6 t1.input t2.input t3.input t4.input t5.input t6.input;
      rel = Rel.union6 t1.rel t2.rel t3.rel t4.rel t5.rel t6.rel;
      output =
        Set.union6 t1.output t2.output t3.output t4.output t5.output t6.output;
    }

  (* Dichotomic implementation of iterated union. *)

  (* [unions2 acc [l1; l2; ...; l2n]] is the list
     [[union l1 l2; union l3 l4; ... union l2n-1 l2n]]. *)
  let rec unions2 acc = function
    | [] -> acc
    | [ h ] -> h :: acc
    | h1 :: h2 :: t -> unions2 (union h1 h2 :: acc) t

  (* [unions li] calls [unions2] on [li] until it only has one element. *)
  let rec unions = function
    | [] -> empty
    | [ h ] -> h
    | li -> unions2 [] li |> unions

  let to_transitive_rel t = Rel.transitive_closure t.rel
end
