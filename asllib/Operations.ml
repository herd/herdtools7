open AST
open ASTUtils
open Error

let value_as_int pos = function
  | L_Int i -> (
      try Z.to_int i
      with Z.Overflow ->
        failwith "Cannot slice with an integer more than machine size.")
  | v ->
      fatal_from pos (Error.MismatchType (PP.literal_to_string v, [ integer' ]))

let is_positive z = Z.sign z != -1
let is_strict_positive z = Z.sign z = 1
let bv_same_length b1 b2 = Bitvector.(length b1 = length b2)

let exp_real q z =
  if Q.sign q = 0 then Q.zero
  else
    let q, z = if is_positive z then (q, z) else (Q.inv q, Z.neg z) in
    let num = Q.num q and den = Q.den q in
    let i = Z.to_int z in
    let res_num = Z.pow num i and res_den = Z.pow den i in
    Q.(res_num /// res_den)

let binop_values pos t op v1 v2 =
  match (op, v1, v2) with
  (* int -> int -> int *)
  | PLUS, L_Int v1, L_Int v2 -> L_Int (Z.add v1 v2)
  | MUL, L_Int v1, L_Int v2 -> L_Int (Z.mul v1 v2)
  | MINUS, L_Int v1, L_Int v2 -> L_Int (Z.sub v1 v2)
  | DIV, L_Int v1, L_Int v2 when is_strict_positive v2 && Z.divisible v1 v2 ->
      L_Int (Z.divexact v1 v2)
  | DIVRM, L_Int v1, L_Int v2 when is_strict_positive v2 ->
      L_Int (Z.fdiv v1 v2) (* Division rounded towards minus infinity. *)
  | MOD, L_Int v1, L_Int v2 when is_strict_positive v2 ->
      L_Int Z.(sub v1 (mul v2 (fdiv v1 v2)))
      (* We cannot use any rem function in Z as we need the rounded towards minus infinity reminder. *)
  | POW, L_Int v1, L_Int v2 when is_positive v2 -> L_Int Z.(pow v1 (to_int v2))
  | SHL, L_Int v1, L_Int v2 when is_positive v2 ->
      L_Int Z.(shift_left v1 (to_int v2))
  | SHR, L_Int v1, L_Int v2 when is_positive v2 ->
      L_Int Z.(shift_right v1 (to_int v2))
  (* int -> int -> bool*)
  | EQ_OP, L_Int v1, L_Int v2 -> L_Bool (Z.equal v1 v2)
  | NEQ, L_Int v1, L_Int v2 -> L_Bool (not (Z.equal v1 v2))
  | LEQ, L_Int v1, L_Int v2 -> L_Bool (Z.leq v1 v2)
  | LT, L_Int v1, L_Int v2 -> L_Bool (Z.lt v1 v2)
  | GEQ, L_Int v1, L_Int v2 -> L_Bool (Z.geq v1 v2)
  | GT, L_Int v1, L_Int v2 -> L_Bool (Z.gt v1 v2)
  (* bool -> bool -> bool *)
  | BAND, L_Bool b1, L_Bool b2 -> L_Bool (b1 && b2)
  | BOR, L_Bool b1, L_Bool b2 -> L_Bool (b1 || b2)
  | BEQ, L_Bool b1, L_Bool b2 -> L_Bool (b1 == b2)
  | IMPL, L_Bool b1, L_Bool b2 -> L_Bool ((not b1) || b2)
  | EQ_OP, L_Bool b1, L_Bool b2 -> L_Bool (b1 == b2)
  | NEQ, L_Bool b1, L_Bool b2 -> L_Bool (b1 <> b2)
  (* real -> real -> real *)
  | PLUS, L_Real v1, L_Real v2 -> L_Real (Q.add v1 v2)
  | MUL, L_Real v1, L_Real v2 -> L_Real (Q.mul v1 v2)
  | MINUS, L_Real v1, L_Real v2 -> L_Real (Q.sub v1 v2)
  | RDIV, L_Real v1, L_Real v2 -> L_Real (Q.div v1 v2)
  | POW, L_Real q1, L_Int z2 -> L_Real (exp_real q1 z2)
  (* real -> real -> bool *)
  | EQ_OP, L_Real v1, L_Real v2 -> L_Bool (Q.equal v1 v2)
  | NEQ, L_Real v1, L_Real v2 -> L_Bool (not (Q.equal v1 v2))
  | LEQ, L_Real v1, L_Real v2 -> L_Bool (Q.leq v1 v2)
  | LT, L_Real v1, L_Real v2 -> L_Bool (Q.lt v1 v2)
  | GEQ, L_Real v1, L_Real v2 -> L_Bool (Q.geq v1 v2)
  | GT, L_Real v1, L_Real v2 -> L_Bool (Q.gt v1 v2)
  (* bits -> bits -> bool *)
  | EQ_OP, L_BitVector b1, L_BitVector b2 when bv_same_length b1 b2 ->
      L_Bool (Bitvector.equal b1 b2)
  | NEQ, L_BitVector b1, L_BitVector b2 when bv_same_length b1 b2 ->
      L_Bool (not @@ Bitvector.equal b1 b2)
  (* bits -> bits -> bits *)
  | OR, L_BitVector b1, L_BitVector b2 when bv_same_length b1 b2 ->
      L_BitVector (Bitvector.logor b1 b2)
  | AND, L_BitVector b1, L_BitVector b2 when bv_same_length b1 b2 ->
      L_BitVector (Bitvector.logand b1 b2)
  | EOR, L_BitVector b1, L_BitVector b2 when bv_same_length b1 b2 ->
      L_BitVector (Bitvector.logxor b1 b2)
  | PLUS, L_BitVector b1, L_BitVector b2 when bv_same_length b1 b2 ->
      L_BitVector
        Bitvector.(
          of_z (length b1) (Z.add (to_z_unsigned b1) (to_z_unsigned b2)))
  | MINUS, L_BitVector b1, L_BitVector b2 when bv_same_length b1 b2 ->
      L_BitVector
        Bitvector.(
          of_z (length b1) (Z.sub (to_z_unsigned b1) (to_z_unsigned b2)))
  | BV_CONCAT, L_BitVector b1, L_BitVector b2 ->
      L_BitVector (Bitvector.concat [ b1; b2 ])
  (* bits -> integer -> bits *)
  | PLUS, L_BitVector b1, L_Int z2 ->
      L_BitVector Bitvector.(of_z (length b1) (Z.add (to_z_unsigned b1) z2))
  | MINUS, L_BitVector b1, L_Int z2 ->
      L_BitVector Bitvector.(of_z (length b1) (Z.sub (to_z_unsigned b1) z2))
  (* string -> string -> bool *)
  | EQ_OP, L_String s1, L_String s2 -> L_Bool (String.equal s1 s2)
  | NEQ, L_String s1, L_String s2 -> L_Bool (not (String.equal s1 s2))
  (* Failure *)
  | _ -> fatal_from pos (Error.UnsupportedBinop (t, op, v1, v2))

let unop_values pos t op v =
  match (op, v) with
  | NEG, L_Int i -> L_Int (Z.neg i)
  | NEG, L_Real r -> L_Real (Q.neg r)
  | BNOT, L_Bool b -> L_Bool (not b)
  | NOT, L_BitVector bv -> L_BitVector (Bitvector.lognot bv)
  | _ -> fatal_from pos (Error.UnsupportedUnop (t, op, v))
