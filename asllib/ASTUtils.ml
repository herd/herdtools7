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

let used_identifiers : AST.t -> ISet.t =
  let rec use_e acc = function
    | E_Literal _ -> acc
    | E_Var x -> ISet.add x acc
    | E_Binop (_op, e1, e2) -> use_e (use_e acc e2) e1
    | E_Unop (_op, e) -> use_e acc e
    | E_Call (x, args) ->
        let acc = ISet.add x acc in
        List.fold_left use_e acc args
    | E_Slice (e, args) ->
        let acc = use_e acc e in
        List.fold_left use_slice acc args
    | E_Cond (e1, e2, e3) -> use_e (use_e (use_e acc e1) e3) e2
    | E_GetField (e, _, _ta) -> use_e acc e
    | E_Record (_ty, li, _ta) -> List.fold_left use_field acc li
    | E_Concat es -> List.fold_left use_e acc es
    | E_Tuple es -> List.fold_left use_e acc es
  and use_field acc (_, e) = use_e acc e
  and use_slice acc = function
    | Slice_Single e -> use_e acc e
    | Slice_Length (e1, e2) | Slice_Range (e1, e2) -> use_e (use_e acc e1) e2
  and use_s acc = function
    | S_Pass | S_Return None -> acc
    | S_Then (s1, s2) -> use_s (use_s acc s1) s2
    | S_Assert e | S_Return (Some e) -> use_e acc e
    | S_Assign (le, e) -> use_le (use_e acc e) le
    | S_Call (x, args) -> List.fold_left use_e (ISet.add x acc) args
    | S_Cond (e, s1, s2) -> use_s (use_s (use_e acc e) s2) s1
    | S_Case (e, cases) -> List.fold_left use_case (use_e acc e) cases
  and use_case acc (es, stmt) = List.fold_left use_e (use_s acc stmt) es
  and use_le acc _le = acc
  and use_decl acc = function
    | D_Func { body; _ } -> use_s acc body
    | D_GlobalConst (_name, _ty, e) -> use_e acc e
    | _ -> acc
  in
  List.fold_left use_decl ISet.empty

let canonical_fields li =
  let compare (x, _) (y, _) = String.compare x y in
  List.sort compare li

let rec expr_of_lexpr = function
  | LE_Var x -> E_Var x
  | LE_Slice (le, args) -> E_Slice (expr_of_lexpr le, args)
  | LE_SetField (le, x, ta) -> E_GetField (expr_of_lexpr le, x, ta)
  | LE_Ignore -> E_Var "-"
  | LE_TupleUnpack les -> E_Tuple (List.map expr_of_lexpr les)

let fresh_var =
  let i = ref 0 in
  fun s ->
    let () = incr i in
    s ^ "-" ^ string_of_int !i

let rec big_union = function
  | [] -> E_Literal (V_Bool true)
  | [ e ] -> e
  | h :: t -> E_Binop (BOR, h, big_union t)

let case_to_conds =
  let rec cases_to_cond x = function
    | [] -> S_Pass
    | (es, s) :: t ->
        let conds = List.map (fun e -> E_Binop (EQ_OP, E_Var x, e)) es in
        S_Cond (big_union conds, s, cases_to_cond x t)
  in
  fun e cases ->
    match e with
    | E_Var y -> cases_to_cond y cases
    | _ ->
        let x = fresh_var "case" in
        S_Then (S_Assign (LE_Var x, e), cases_to_cond x cases)

let slice_as_single = function
  | Slice_Single e -> e
  | _ -> raise @@ Invalid_argument "slice_as_single"

let setter_name = ( ^ ) "setter-"
let getter_name = ( ^ ) "getter-"

let num_args = function
  | 0 -> Fun.id
  | n -> fun name -> name ^ "-" ^ string_of_int n

let default_t_bits = T_Bits (BitWidth_Constrained [], None)
