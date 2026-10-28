(* Reuse decisions for structurally identical symbolic computations.
   Definitions are local to an interpreter instance; decisions stay in its paths. *)

module V = ASLValue.V

module Make (VC : Valconstraint.S with type atom = V.v) = struct
  type t = { mutable definitions : VC.expr option V.ValueMap.t }

  let create () = { definitions = V.ValueMap.empty }

  (* Operation results are fresh symbols. Branch assumptions (Atom), initial
     reads and other constraints must never become shared definitions. *)
  let record t constraints =
    List.iter
      (function
        | VC.Assign ((V.Var _ as result),
                     ((VC.Unop _ | VC.Binop _ | VC.Terop _) as expression)) ->
            begin match V.ValueMap.find_opt result t.definitions with
            | None ->
                t.definitions <- V.ValueMap.add result (Some expression) t.definitions
            | Some (Some previous) when previous <> expression ->
                t.definitions <- V.ValueMap.add result None t.definitions
            | Some _ -> ()
            end
        | _ -> ())
      constraints

  module Pairs = Map.Make (struct
    type t = V.v * V.v
    let compare = Misc.pair_compare V.compare V.compare
  end)

  (* Compare definitions, retaining symbol identities at opaque leaves.
     Memoization avoids expanding shared subexpressions repeatedly and stops
     cycles conservatively. Equality also accepts reversed operands. *)
  let find t ~assumptions condition =
    let memo = ref Pairs.empty in
    let definition value =
      Option.join (V.ValueMap.find_opt value t.definitions)
    in
    let rec same left right =
      V.equal left right ||
      match Pairs.find_opt (left, right) !memo with
      | Some result -> result
      | None ->
          memo := Pairs.add (left, right) false !memo;
          let result =
            match definition left, definition right with
            | Some left, Some right -> same_expression left right
            | _ -> false
          in
          memo := Pairs.add (left, right) result !memo;
          result
    and same_expression left right =
      match left, right with
      | VC.Unop (op, value), VC.Unop (other_op, other_value) ->
          op = other_op && same value other_value
      | VC.Binop (op, left, right), VC.Binop (other_op, other_left, other_right) ->
          op = other_op &&
          ((same left other_left && same right other_right) ||
           (op = Op.Eq && same left other_right && same right other_left))
      | VC.Terop (op, a, b, c), VC.Terop (other_op, x, y, z) ->
          op = other_op && same a x && same b y && same c z
      | _ -> false
    in
    List.find_map
      (fun (previous, decision) ->
        if same condition previous then Some decision else None)
      assumptions
end
