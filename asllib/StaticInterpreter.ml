open AST

let fatal = Error.fatal

let value_as_int = function
  | V_Int i -> i
  | v -> fatal (Error.MismatchType (v, [ T_Int None ]))

let binop op v1 v2 =
  match (op, v1, v2) with
  (* int -> int -> int *)
  | PLUS, V_Int v1, V_Int v2 -> V_Int (v1 + v2)
  | MUL, V_Int v1, V_Int v2 -> V_Int (v1 * v2)
  | MINUS, V_Int v1, V_Int v2 -> V_Int (v1 - v2)
  | DIV, V_Int v1, V_Int v2 -> V_Int (v1 / v2)
  (* int -> int -> bool*)
  | EQ_OP, V_Int v1, V_Int v2 -> V_Bool (v1 == v2)
  | NEQ, V_Int v1, V_Int v2 -> V_Bool (v1 <> v2)
  | LEQ, V_Int v1, V_Int v2 -> V_Bool (v1 <= v2)
  | LT, V_Int v1, V_Int v2 -> V_Bool (v1 < v2)
  | GEQ, V_Int v1, V_Int v2 -> V_Bool (v1 >= v2)
  | GT, V_Int v1, V_Int v2 -> V_Bool (v1 > v2)
  (* bool -> bool -> bool *)
  | BAND, V_Bool b1, V_Bool b2 -> V_Bool (b1 && b2)
  | BOR, V_Bool b1, V_Bool b2 -> V_Bool (b1 || b2)
  | BEQ, V_Bool b1, V_Bool b2 -> V_Bool (b1 == b2)
  | IMPL, V_Bool b1, V_Bool b2 -> V_Bool ((not b1) || b2)
  | EQ_OP, V_Bool b1, V_Bool b2 -> V_Bool (b1 == b2)
  | NEQ, V_Bool b1, V_Bool b2 -> V_Bool (b1 <> b2)
  (* real -> real -> real *)
  | PLUS, V_Real v1, V_Real v2 -> V_Real (v1 +. v2)
  | MUL, V_Real v1, V_Real v2 -> V_Real (v1 *. v2)
  | MINUS, V_Real v1, V_Real v2 -> V_Real (v1 -. v2)
  | DIV, V_Real v1, V_Real v2 -> V_Real (v1 /. v2)
  (* real -> real -> bool *)
  | EQ_OP, V_Real v1, V_Real v2 -> V_Bool (v1 == v2)
  | NEQ, V_Real v1, V_Real v2 -> V_Bool (v1 <> v2)
  | LEQ, V_Real v1, V_Real v2 -> V_Bool (v1 <= v2)
  | LT, V_Real v1, V_Real v2 -> V_Bool (v1 < v2)
  | GEQ, V_Real v1, V_Real v2 -> V_Bool (v1 >= v2)
  | GT, V_Real v1, V_Real v2 -> V_Bool (v1 > v2)
  (* bits -> bits -> bits *)
  | EQ_OP, V_BitVector b1, V_BitVector b2 -> V_Bool (Bitvector.equal b1 b2)
  | NEQ, V_BitVector b1, V_BitVector b2 -> V_Bool (not @@ Bitvector.equal b1 b2)
  | _ -> fatal (Error.UnsupportedBinop (op, v1, v2))

let unop op v =
  match (op, v) with
  | NEG, V_Int i -> V_Int ~-i
  | NEG, V_Real r -> V_Real ~-.r
  | BNOT, V_Bool b -> V_Bool (not b)
  | _ -> fatal (Error.UnsupportedUnop (op, v))

let rec static_eval (env : string -> value) : expr -> value =
  let rec expr_ e =
    match e.desc with
    | E_Literal v -> v
    | E_Var x -> env x
    | E_Binop (op, e1, e2) ->
        let v1 = expr_ e1 and v2 = expr_ e2 in
        binop op v1 v2
    | E_Unop (op, e) ->
        let v = expr_ e in
        unop op v
    | E_Slice (e', slices) ->
        let bv =
          match expr_ e' with
          | V_Int i -> Bitvector.of_int i
          | V_BitVector bv -> bv
          | _ -> fatal (Error.UnsupportedExpr e)
        and positions = slices_to_positions env slices in
        V_BitVector (Bitvector.extract_slice bv positions)
    | _ -> fatal (Error.UnsupportedExpr e)
  in
  expr_

and slices_to_positions env =
  let eval_to_int e = static_eval env e |> value_as_int in
  let slice_to_positions =
    let interval top len = List.init len (( - ) top) in
    function
    | Slice_Single e -> [ eval_to_int e ]
    | Slice_Range (etop, ebot) ->
        let pbot = eval_to_int ebot and ptop = eval_to_int etop in
        interval ptop (ptop - pbot + 1)
    | Slice_Length (ebot, elength) ->
        let pbot = eval_to_int ebot and plength = eval_to_int elength in
        let ptop = pbot + plength - 1 in
        interval ptop plength
  in
  fun slices -> slices |> List.map slice_to_positions |> List.concat
