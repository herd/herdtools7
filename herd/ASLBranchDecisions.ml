(* Reuse decisions for identical symbolic values or slice/literal comparisons.
   Provenance is local to an interpreter instance; decisions stay in its paths. *)

module V = ASLValue.V

type slice = V.v * int list
type comparison = slice * Asllib.Bitvector.t

type t = {
  mutable slices : slice V.ValueMap.t;
  mutable comparisons : comparison V.ValueMap.t;
}

let create () =
  { slices = V.ValueMap.empty; comparisons = V.ValueMap.empty }

(* Keep the symbolic source, not its register name or memory address. *)
let record_slice t ~result ~source ~positions =
  match result with
  | V.Var _ when not (V.equal result source)
                 && not (V.ValueMap.mem result t.slices) ->
      t.slices <- V.ValueMap.add result (source, positions) t.slices
  | _ -> ()

(* Only an exact slice/literal equality is recognized; no algebraic reasoning. *)
let record_equal t ~result ~left ~right =
  let comparison slice literal =
    match V.ValueMap.find_opt slice t.slices, literal with
    | Some source, V.Val (Constant.Concrete (ASLScalar.S_BitVector bits)) ->
        Some (source, bits)
    | _ -> None
  in
  match result with
  | V.Var _ when not (V.equal result left || V.equal result right)
                 && not (V.ValueMap.mem result t.comparisons) ->
      let key =
        match comparison left right with
        | Some _ as key -> key
        | None -> comparison right left
      in
      Option.iter
        (fun key ->
          t.comparisons <- V.ValueMap.add result key t.comparisons)
        key
  | _ -> ()

(* A missing match means that both branches must still be explored. *)
let find t ~assumptions condition =
  let same_comparison other =
    match V.ValueMap.find_opt condition t.comparisons,
          V.ValueMap.find_opt other t.comparisons with
    | Some ((source, positions), bits),
      Some ((other_source, other_positions), other_bits) ->
        V.equal source other_source
        && List.equal Int.equal positions other_positions
        && Asllib.Bitvector.equal bits other_bits
    | _ -> false
  in
  List.find_map
    (fun (other, decision) ->
      if V.equal condition other || same_comparison other then Some decision
      else None)
    assumptions
