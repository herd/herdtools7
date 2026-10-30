(* Recognize repeated symbolic conditions and reuse earlier branch decisions. *)

module V = ASLValue.V

module Make (VC : Valconstraint.S with type atom = V.v) = struct
  (* Map each symbol to its defining expression.
     None marks a symbol with conflicting definitions. *)
  type t = { mutable definitions : VC.expr option V.ValueMap.t }

  let create () = { definitions = V.ValueMap.empty }

  (* Record equations such as result = operation(inputs).
     Ignore branch constraints such as condition = true: they only hold
     on the path that selected that branch. *)
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

  (* Find an earlier condition with the same expression and return its decision.
     Symbols without a recorded definition only match themselves. *)
  let find t ~assumptions condition =
    (* Cache comparisons within this search to avoid comparing shared
       subexpressions repeatedly. *)
    let memo = ref Pairs.empty in
    let definition value =
      Option.join (V.ValueMap.find_opt value t.definitions)
    in
    let rec same left right =
      V.equal left right ||
      match Pairs.find_opt (left, right) !memo with
      | Some result -> result
      | None ->
          (* Assume no match while comparing this pair, to stop recursive cycles. *)
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
