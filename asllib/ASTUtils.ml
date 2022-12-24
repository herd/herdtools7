open AST
module ISet = Set.Make (String)
module IMap = Map.Make (String)

let rec stmt_from_list = function
  | [] -> S_Pass
  | [ x ] -> x
  | h :: t -> S_Then (h, stmt_from_list t)

let use_expr include_funcs : expr -> ISet.t =
  let rec use_ acc = function
    | E_Literal _ -> acc
    | E_Var x -> ISet.add x acc
    | E_Binop (_op, e1, e2) -> use_ (use_ acc e2) e1
    | E_Unop (_op, e) -> use_ acc e
    | E_Call (x, args) ->
        let acc = if include_funcs then ISet.add x acc else acc in
        List.fold_left use_ acc args
    | E_Getter (x, args) ->
        let acc = ISet.add x acc in
        List.fold_left use_ acc args
    | E_Cond (e1, e2, e3) -> use_ (use_ (use_ acc e1) e3) e2
    | E_GetField (e, _, _ta) -> use_ acc e
    | E_Record (_ty, li, _ta) -> List.fold_left use_field acc li
  and use_field acc (_, e) = use_ acc e in
  use_ ISet.empty

let canonical_fields li =
  let compare (x, _) (y, _) = String.compare x y in
  List.sort compare li

let rec expr_of_lexpr = function
  | LE_Var x -> E_Var x
  | LE_Setter (x, args) -> E_Getter (x, args)
  | LE_SetField (le, x, ta) -> E_GetField (expr_of_lexpr le, x, ta)
