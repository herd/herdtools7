open AST

type static_interpreter_error =
  | UnsupportedBinop of binop * value * value
  | UnsupportedUnop of unop * value
  | UnsupportedExpr of expr
  | TypeError of value * string

exception StaticInterpreterError of static_interpreter_error

let error e = raise (StaticInterpreterError e)

let pp_static_interpreter_error f =
  let open Format in
  function
  | UnsupportedBinop (op, v1, v2) ->
      fprintf f "@[Unsupported binop %s for values@ %a@ and %a.@]"
        (PP.binop_to_string op) PP.pp_value v1 PP.pp_value v2
  | UnsupportedUnop (op, v) ->
      fprintf f "@[Unsupported unop %s for value@ %a.@]" (PP.unop_to_string op)
        PP.pp_value v
  | UnsupportedExpr e -> fprintf f "@[Unsupported expression %a.@]" PP.pp_expr e
  | TypeError (v, s) ->
      fprintf f "@[Expected type %s@ for@ value@ %a.@]" s PP.pp_value v

let static_interpreter_error_to_string =
  Format.asprintf "@[Static interpreter error:@ %a@]"
    pp_static_interpreter_error

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
  | _ -> error (UnsupportedBinop (op, v1, v2))

let unop op v =
  match (op, v) with
  | NEG, V_Int i -> V_Int ~-i
  | NEG, V_Real r -> V_Real ~-.r
  | BNOT, V_Bool b -> V_Bool (not b)
  | _ -> error (UnsupportedUnop (op, v))

let static_eval (env : string -> value) : expr -> value =
  let rec expr_ = function
    | E_Literal v -> v
    | E_Var x -> env x
    | E_Binop (op, e1, e2) ->
        let v1 = expr_ e1 and v2 = expr_ e2 in
        binop op v1 v2
    | E_Unop (op, e) ->
        let v = expr_ e in
        unop op v
    | e -> error (UnsupportedExpr e)
  in
  expr_
