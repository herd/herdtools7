open AST
module ISet = Set.Make (String)
module IMap = Map.Make (String)

let rec stmt_from_list = function
  | [] -> S_Pass
  | [ x ] -> x
  | h :: t -> S_Then (h, stmt_from_list t)

let mask_from_set_bits_positions size pos =
  let buf = Bytes.make size '0' in
  let set i = Bytes.set buf i '1' in
  let () = List.iter set pos in
  Bytes.to_string buf

let inv_mask =
  let one_char = function '0' -> '1' | '1' -> '0' | c -> c in
  String.map one_char

let use_expr include_funcs : expr -> ISet.t =
  let rec use_ acc = function
    | E_Literal _ -> acc
    | E_Var x -> ISet.add x acc
    | E_Binop (_op, e1, e2) -> use_ (use_ acc e2) e1
    | E_Unop (_op, e) -> use_ acc e
    | E_Call (x, args) ->
        let acc = if include_funcs then ISet.add x acc else acc in
        List.fold_left use_ acc args
    | E_Slice (e, args) ->
        let acc = use_ acc e in
        List.fold_left use_slice acc args
    | E_Cond (e1, e2, e3) -> use_ (use_ (use_ acc e1) e3) e2
    | E_GetField (e, _, _ta) -> use_ acc e
    | E_Record (_ty, li, _ta) -> List.fold_left use_field acc li
  and use_field acc (_, e) = use_ acc e
  and use_slice acc = function
    | Slice_Single e -> use_ acc e
    | Slice_Length (e1, e2) | Slice_Range (e1, e2) -> use_ (use_ acc e1) e2
  in
  use_ ISet.empty

let canonical_fields li =
  let compare (x, _) (y, _) = String.compare x y in
  List.sort compare li

let rec expr_of_lexpr = function
  | LE_Var x -> E_Var x
  | LE_Slice (le, args) -> E_Slice (expr_of_lexpr le, args)
  | LE_SetField (le, x, ta) -> E_GetField (expr_of_lexpr le, x, ta)
