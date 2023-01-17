open AST

let fatal = Error.fatal

let value_as_int = function
  | V_Int i -> i
  | v -> fatal (Error.MismatchType (v, [ T_Int None ]))

let int_to_bitvector =
  (* Inspired by https://discuss.ocaml.org/t/pretty-printing-binary-ints/9062/7 *)
  let int_size = Sys.int_size - 1 in
  (* Should be enough *)
  let buf = Bytes.create int_size in
  fun n ->
    for i = 0 to int_size - 1 do
      let pos = int_size - 1 - i in
      Bytes.set buf pos (if n land (1 lsl i) != 0 then '1' else '0')
    done;
    Bytes.to_string buf

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
  | _ -> fatal (Error.UnsupportedBinop (op, v1, v2))

let unop op v =
  match (op, v) with
  | NEG, V_Int i -> V_Int ~-i
  | NEG, V_Real r -> V_Real ~-.r
  | BNOT, V_Bool b -> V_Bool (not b)
  | _ -> fatal (Error.UnsupportedUnop (op, v))

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
    | E_Slice (e', slices) as e ->
        let v = expr_ e' in
        let bv =
          match v with
          | V_Int i -> int_to_bitvector i
          | V_BitVector bv -> bv
          | _ -> fatal (Error.UnsupportedExpr e)
        in
        let n = String.length bv in
        let slice_of = function
          | Slice_Single i ->
              let i = expr_ i |> value_as_int in
              String.sub bv i 1
          | Slice_Length (start, length) ->
              let start = expr_ start |> value_as_int |> ( - ) n
              and length = expr_ length |> value_as_int in
              String.sub bv start length
          | Slice_Range (endp, start) ->
              let start = expr_ start |> value_as_int |> ( - ) n
              and endp = expr_ endp |> value_as_int |> ( - ) n in
              let length = start - endp + 1 and real_start = endp - 1 in
              String.sub bv real_start length
        in
        slices |> List.map slice_of |> String.concat "" |> fun bv ->
        V_BitVector bv
    | e -> fatal (Error.UnsupportedExpr e)
  in
  expr_
