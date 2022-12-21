open AST
module ISet = Set.Make (String)
module IMap = Map.Make (String)

let rec stmt_from_list = function
  | [] -> S_Pass
  | [ x ] -> x
  | h :: t -> S_Then (h, stmt_from_list t)

let use_expr include_funcs : 'v expr -> ISet.t =
  let rec use_ acc = function
    | E_Literal _ -> acc
    | E_Var x -> ISet.add x acc
    | E_Binop (_op, e1, e2) -> use_ (use_ acc e2) e1
    | E_Unop (_op, e) -> use_ acc e
    | E_Call (x, args) ->
        let acc = if include_funcs then ISet.add x acc else acc in
        List.fold_left use_ acc args
    | E_Get (x, args) ->
        let acc = ISet.add x acc in
        List.fold_left use_ acc args
    | E_Cond (e1, e2, e3) -> use_ (use_ (use_ acc e1) e3) e2
  in
  use_ ISet.empty

let tr_values tr_int tr_bool tr_real tr_bitvector create_tuple create_record
    create_exception =
  let rec value_ = function
    | V_Int i -> tr_int i
    | V_Bool b -> tr_bool b
    | V_BitVector s -> tr_bitvector s
    | V_Real r -> tr_real r
    | V_Tuple li -> create_tuple value_ li
    | V_Record li -> create_record value_ li
    | V_Exception li -> create_exception value_ li
  and expr_ = function
    | E_Literal v -> E_Literal (value_ v)
    | E_Var x -> E_Var x
    | E_Unop (op, e) -> E_Unop (op, expr_ e)
    | E_Binop (op, e1, e2) -> E_Binop (op, expr_ e1, expr_ e2)
    | E_Cond (e1, e2, e3) -> E_Cond (expr_ e1, expr_ e2, expr_ e3)
    | E_Call (x, es) -> E_Call (x, List.map expr_ es)
    | E_Get (x, es) -> E_Get (x, List.map expr_ es)
  and lexpr_ = function
    | LEVar x -> LEVar x
    | LESet (x, args) -> LESet (x, List.map expr_ args)
  and stmt_ = function
    | S_Pass -> S_Pass
    | S_Assign (le, e) -> S_Assign (lexpr_ le, expr_ e)
    | S_Cond (e, s1, s2) -> S_Cond (expr_ e, stmt_ s1, stmt_ s2)
    | S_Call (x, es) -> S_Call (x, List.map expr_ es)
    | S_Return es -> S_Return (List.map expr_ es)
    | S_Then (s1, s2) -> S_Then (stmt_ s1, stmt_ s2)
  and decl_ = function
    | D_Func ({ body; _ } as func) -> D_Func { func with body = stmt_ body }
    | D_GlobalConst (x, e) -> D_GlobalConst (x, expr_ e)
    | D_TypeDecl (x, t) -> D_TypeDecl (x, t)
  in

  List.map decl_

type parsed_value = (int, bool, float, string) value
type parsed_t = parsed_value t
