open AST
open ASTUtils
module SEnv = StaticEnv

type env = SEnv.env

let fatal = Error.fatal
let fatal_from = Error.fatal_from

exception NotYetImplemented

let value_as_int pos = function
  | V_Int i -> i
  | v -> fatal_from pos (Error.MismatchType (v, [ T_Int None ]))

let binop_values pos op v1 v2 =
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
  | RDIV, V_Real v1, V_Real v2 -> V_Real (v1 /. v2)
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
  | OR, V_BitVector b1, V_BitVector b2 -> V_BitVector (Bitvector.logor b1 b2)
  | AND, V_BitVector b1, V_BitVector b2 -> V_BitVector (Bitvector.logand b1 b2)
  | EOR, V_BitVector b1, V_BitVector b2 -> V_BitVector (Bitvector.logxor b1 b2)
  (* TODO *)
  | (MOD | SHL | SHR), _, _ ->
      fatal_from pos
        (Error.NotYetImplemented ("Evaluation of " ^ PP.binop_to_string op))
  (* Failure *)
  | _ -> fatal_from pos (Error.UnsupportedBinop (op, v1, v2))

let unop_values pos op v =
  match (op, v) with
  | NEG, V_Int i -> V_Int ~-i
  | NEG, V_Real r -> V_Real ~-.r
  | BNOT, V_Bool b -> V_Bool (not b)
  | NOT, V_BitVector bv -> V_BitVector (Bitvector.lognot bv)
  | _ -> fatal_from pos (Error.UnsupportedUnop (op, v))

let rec static_eval (env : SEnv.env) : expr -> value =
  let rec expr_ e =
    match e.desc with
    | E_Literal v -> v
    | E_Var x -> (
        try SEnv.lookup_constants env x
        with Not_found -> Error.fatal_from e (Error.UndefinedIdentifier x))
    | E_Binop (op, e1, e2) ->
        let v1 = expr_ e1 and v2 = expr_ e2 in
        binop_values e op v1 v2
    | E_Unop (op, e) ->
        let v = expr_ e in
        unop_values e op v
    | E_Slice (e', slices) ->
        let bv =
          match expr_ e' with
          | V_Int i -> Bitvector.of_int i
          | V_BitVector bv -> bv
          | _ -> fatal_from e (Error.UnsupportedExpr e)
        and positions = slices_to_positions env slices in
        V_BitVector (Bitvector.extract_slice bv positions)
    | _ -> fatal_from e (Error.UnsupportedExpr e)
  in
  expr_

and slices_to_positions env =
  let eval_to_int e = static_eval env e |> value_as_int e in
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

module Normalize = struct
  type atom = identifier
  type 'a disjunction = Disjunction of 'a list

  module AtomOrdered = struct
    type t = atom

    let compare = String.compare
  end

  module AMap = Map.Make (AtomOrdered)

  type monomial = Prod of int AMap.t

  module MonomialOrdered = struct
    type t = monomial

    let compare (Prod ms1) (Prod ms2) = AMap.compare Int.compare ms1 ms2
  end

  module MMap = struct
    include Map.Make (MonomialOrdered)

    (* Available from 4.11.0 *)
    let filter_map f m =
      fold
        (fun key v r -> match f key v with None -> r | Some v -> add key v r)
        m empty
  end

  type polynomial = Sum of int MMap.t

  type sign =
    | Null
    | StrictPositive
    | Positive
    | Negative
    | StrictNegative
    | NotNull

  module PolynomialOrdered = struct
    type t = polynomial

    let compare (Sum p1) (Sum p2) = MMap.compare Int.compare p1 p2
  end

  module PMap = Map.Make (PolynomialOrdered)

  type ctnts = Conjunction of sign PMap.t | Bottom
  type ir_expr = (ctnts * polynomial) disjunction
  (* Wanted invariants for (e : ir_expr) :
     ⋁ {c | (c, d) ∈ e } <=> true                           (I₂)
     ∀ (cᵢ, eᵢ), (cⱼ, eⱼ) ∈ e, i != j => cⱼ ∩ cⱼ = ∅        (I₃)
  *)

  (* ----------------------------------------------------------------------- *)
  (* Printers *)

  let pp_mono f (Prod mono, factor) =
    let open Format in
    let mono = AMap.filter (fun _ p -> p != 0) mono in
    if AMap.is_empty mono then pp_print_int f factor
    else (
      pp_open_hbox f ();
      let pp_sep f () = fprintf f "@ \u{d7} " in
      if factor != 1 then (
        pp_print_int f factor;
        pp_sep f ());
      PP.pp_print_seq ~pp_sep
        (fun f (x, p) ->
          pp_print_string f x;
          match p with
          | 1 -> ()
          | 2 -> pp_print_string f "\u{b2}"
          | _ -> fprintf f "^%d" p)
        f (AMap.to_seq mono);
      pp_close_box f ())

  let pp_poly f (Sum poly) =
    let open Format in
    let poly = MMap.filter (fun _ f -> f != 0) poly in
    if MMap.is_empty poly then pp_print_string f "0"
    else (
      pp_open_hvbox f 2;
      let pp_sep f () = fprintf f "@ + " in
      PP.pp_print_seq ~pp_sep pp_mono f (MMap.to_seq poly);
      pp_close_box f ())

  let pp_sign f s =
    let s =
      match s with
      | Null -> "= 0"
      | NotNull -> "!= 0"
      | StrictPositive -> "> 0"
      | Positive -> "\u{2265} 0"
      | Negative -> "\u{2264} 0"
      | StrictNegative -> "< 0"
    in
    Format.pp_print_string f s

  let pp_ctnt f (p, s) =
    let open Format in
    fprintf f "@[<h>%a@ %a@]" pp_poly p pp_sign s

  let pp_ctnts f =
    let open Format in
    function
    | Bottom -> pp_print_string f "\u{22a5}"
    | Conjunction m ->
        if PMap.is_empty m then pp_print_string f "\u{22a4}"
        else
          let pp_sep f () = fprintf f "@ \u{2227} " in
          fprintf f "@[<hov 2>%a@]"
            (PP.pp_print_seq ~pp_sep pp_ctnt)
            (PMap.to_seq m)

  let pp_ctnts_and_poly f (ctnts, p) =
    Format.fprintf f "@[<2>%a@ -> %a@]" pp_ctnts ctnts pp_poly p

  let pp_ir f (Disjunction li) =
    let open Format in
    fprintf f "@[<v 2>%a@]"
      (pp_print_list ~pp_sep:pp_print_space pp_ctnts_and_poly)
      li

  (* ----------------------------------------------------------------------- *)
  (* Constructors *)

  let disjunction map = Disjunction map
  let ctnts_true : ctnts = Conjunction PMap.empty
  let ctnts_false : ctnts = Bottom
  let always e = Disjunction [ (ctnts_true, e) ]
  let mono_one = Prod AMap.empty
  let mono_of_var s = Prod (AMap.singleton s 1)
  let poly_zero = Sum MMap.empty
  let poly_of_var s = Sum (MMap.singleton (mono_of_var s) 1)
  let poly_of_int i = Sum (MMap.singleton mono_one i)
  let poly_neg (Sum monos) = Sum (MMap.map ( ~- ) monos)

  let poly_of_val = function
    | V_Int i -> poly_of_int i
    | v -> Error.fatal_unknown_pos (Error.MismatchType (v, [ T_Int None ]))

  let sign_not = function
    | NotNull -> Null
    | Null -> NotNull
    | Positive -> StrictNegative
    | Negative -> StrictPositive
    | StrictPositive -> Negative
    | StrictNegative -> Positive

  exception ConjunctionBottomInterrupt

  let sign_and _p s1 s2 =
    match (s1, s2) with
    | Null, Null
    | Null, Positive
    | Positive, Null
    | Negative, Null
    | Null, Negative
    | Negative, Positive
    | Positive, Negative ->
        Some Null
    | StrictPositive, StrictPositive
    | StrictPositive, Positive
    | Positive, StrictPositive
    | Positive, NotNull
    | NotNull, Positive
    | StrictPositive, NotNull
    | NotNull, StrictPositive ->
        Some StrictPositive
    | Positive, Positive -> Some Positive
    | Negative, Negative -> Some Negative
    | NotNull, NotNull -> Some NotNull
    | StrictNegative, StrictNegative
    | StrictNegative, Negative
    | Negative, StrictNegative
    | Negative, NotNull
    | NotNull, Negative
    | NotNull, StrictNegative
    | StrictNegative, NotNull ->
        Some StrictNegative
    | Null, NotNull
    | NotNull, Null
    | Negative, StrictPositive
    | StrictPositive, Negative
    | StrictNegative, Positive
    | Positive, StrictNegative
    | Null, StrictPositive
    | StrictPositive, Null
    | Null, StrictNegative
    | StrictNegative, Null
    | StrictNegative, StrictPositive
    | StrictPositive, StrictNegative ->
        raise_notrace ConjunctionBottomInterrupt

  let constant_satisfies c s =
    match s with
    | Null -> c = 0
    | NotNull -> c != 0
    | Positive -> c >= 0
    | StrictPositive -> c > 0
    | Negative -> c <= 0
    | StrictNegative -> c <= 0

  let ctnts_of_bool b = if b then ctnts_true else ctnts_false

  let ctnts_not = function
    | Bottom -> ctnts_true
    | Conjunction ctnts -> (
        try Conjunction (PMap.map sign_not ctnts)
        with ConjunctionBottomInterrupt -> Bottom)

  let sign_compare = Stdlib.compare
  let mono_compare = MonomialOrdered.compare
  let poly_compare (Sum p1) (Sum p2) = MMap.compare Int.compare p1 p2

  let ctnts_compare cs1 cs2 =
    match (cs1, cs2) with
    | Bottom, Bottom -> 0
    | Bottom, _ -> 1
    | _, Bottom -> -1
    | Conjunction ctnts1, Conjunction ctnts2 ->
        PMap.compare sign_compare ctnts1 ctnts2

  let ir_compare (Disjunction li1) (Disjunction li2) =
    ASTUtils.list_compare
      (fun (cs1, p1) (cs2, p2) ->
        let n = ctnts_compare cs1 cs2 in
        if n = 0 then poly_compare p1 p2 else n)
      li1 li2

  let add_mono_to_poly =
    let updater factor = function
      | None -> Some factor
      | Some f ->
          let f' = f + factor in
          if f' = 0 then None else Some f'
    in
    fun mono factor -> MMap.update mono (updater factor)

  let add_polys : polynomial -> polynomial -> polynomial =
   fun (Sum monos1) (Sum monos2) ->
    Sum (MMap.union (fun _mono c1 c2 -> Some (c1 + c2)) monos1 monos2)

  let mult_monos : monomial -> monomial -> monomial =
   fun (Prod map1) (Prod map2) ->
    Prod (AMap.union (fun _ p1 p2 -> Some (p1 + p2)) map1 map2)

  let mult_polys : polynomial -> polynomial -> polynomial =
   fun (Sum monos1) (Sum monos2) ->
    Sum
      (MMap.fold
         (fun m1 f1 ->
           MMap.fold
             (fun m2 f2 -> add_mono_to_poly (mult_monos m1 m2) (f1 * f2))
             monos2)
         monos1 MMap.empty)

  let ctnts_and : ctnts -> ctnts -> ctnts =
   fun c1 c2 ->
    match (c1, c2) with
    | Bottom, _ | _, Bottom -> Bottom
    | Conjunction ctnts1, Conjunction ctnts2 -> (
        try Conjunction (PMap.union sign_and ctnts1 ctnts2)
        with ConjunctionBottomInterrupt -> Bottom)

  let restrict (Disjunction ctntss1) (Disjunction li2) =
    Disjunction
      (ASTUtils.list_cross
         (fun ctnts1 (ctnts2, e2) -> (ctnts_and ctnts1 ctnts2, e2))
         ctntss1 li2)

  let disjunction_or (Disjunction li1) (Disjunction li2) =
    Disjunction (li1 @ li2)

  let cross_num (Disjunction li1) (Disjunction li2) f =
    let on_pair (ctnts1, e1) (ctnts2, e2) =
      (ctnts_and ctnts1 ctnts2, f e1 e2)
    in
    Disjunction (ASTUtils.list_cross on_pair li1 li2)

  let map_num f (Disjunction li1) =
    Disjunction (List.map (fun (ctnt, e) -> (ctnt, f e)) li1)

  let disjunction_cross f (Disjunction li1) (Disjunction li2) =
    Disjunction (ASTUtils.list_cross f li1 li2)

  let ir_to_cond sign (Disjunction li2) =
    Disjunction
      (List.map
         (fun (ctnts, p) ->
           ctnts_and (Conjunction (PMap.singleton p sign)) ctnts)
         li2)

  let rec to_ir env (e : expr) : ir_expr =
    match e.desc with
    | E_Literal (V_Int i) -> poly_of_int i |> always
    | E_Var s -> (
        try StaticEnv.lookup_constants env s |> poly_of_val |> always
        with Not_found -> (
          try
            let ty = StaticEnv.type_of env s in
            match ty.desc with
            | T_Int (Some [ Constraint_Exact e ]) -> to_ir env e
            | T_Int _ -> poly_of_var s |> always
            | _ ->
                Error.fatal_unknown_pos
                  (Error.ConflictingTypes ([ T_Int None ], ty))
          with Not_found ->
            Error.fatal_unknown_pos (Error.UndefinedIdentifier s)))
    | E_Binop (PLUS, e1, e2) ->
        let ir1 = to_ir env e1 and ir2 = to_ir env e2 in
        cross_num ir1 ir2 add_polys
    | E_Binop (MINUS, e1, e2) ->
        let e2 = E_Unop (NEG, e2) |> ASTUtils.add_pos_from_st e2 in
        E_Binop (PLUS, e1, e2) |> ASTUtils.add_pos_from_st e |> to_ir env
    | E_Binop (MUL, e1, e2) ->
        let ir1 = to_ir env e1 and ir2 = to_ir env e2 in
        cross_num ir1 ir2 mult_polys
    | E_Unop (NEG, e0) -> e0 |> to_ir env |> map_num poly_neg
    | E_Cond (cond, e1, e2) ->
        let Disjunction ctnts, Disjunction nctnts = to_cond env cond
        and (Disjunction ir1) = to_ir env e1
        and (Disjunction ir2) = to_ir env e2 in
        let restrict ctnts (ctnts', p) = (ctnts_and ctnts ctnts', p) in
        let ir1' = ASTUtils.list_cross restrict ctnts ir1
        and ir2' = ASTUtils.list_cross restrict nctnts ir2 in
        Disjunction (ir1' @ ir2')
    | _ -> (
        let v =
          try static_eval env e
          with Error.ASLException { desc = UnsupportedExpr _; _ } ->
            raise NotYetImplemented
        in
        match v with
        | V_Int i -> poly_of_int i |> always
        | _ -> raise NotYetImplemented)

  and to_cond env (e : expr) : ctnts disjunction * ctnts disjunction =
    let ( ||| ) = disjunction_or in
    let ( &&& ) = disjunction_cross ctnts_and in
    match e.desc with
    | E_Literal (V_Bool b) ->
        (Disjunction [ ctnts_of_bool b ], Disjunction [ ctnts_of_bool (not b) ])
    | E_Binop (BAND, e1, e2) ->
        let ctnts1, nctnts1 = to_cond env e1
        and ctnts2, nctnts2 = to_cond env e2 in
        (ctnts1 &&& ctnts2, nctnts1 ||| nctnts2)
    | E_Binop (BOR, e1, e2) ->
        let ctnts1, nctnts1 = to_cond env e1
        and ctnts2, nctnts2 = to_cond env e2 in
        (ctnts1 ||| ctnts2, nctnts1 &&& nctnts2)
    | E_Binop (EQ_OP, e1, e2) ->
        let e' = E_Binop (MINUS, e1, e2) |> ASTUtils.add_pos_from_st e in
        let ir = to_ir env e' in
        (ir_to_cond Null ir, ir_to_cond NotNull ir)
    | E_Cond (cond, e1, e2) ->
        let ctnts_cond, nctnts_cond = to_cond env cond
        and ctnts1, nctnts1 = to_cond env e1
        and ctnts2, nctnts2 = to_cond env e2 in
        ( ctnts_cond &&& ctnts1 ||| (nctnts_cond &&& ctnts2),
          nctnts_cond ||| nctnts1 &&& (ctnts_cond ||| nctnts2) )
    | _ -> raise NotYetImplemented

  (* ----------------------------------------------------------------------- *)
  (* Destructors *)
  (* ----------- *)

  let loc = dummy_annotated
  let e_of_int i = V_Int i |> literal
  let zero = e_of_int 0
  let one = e_of_int 1
  let cannot_happen_expr = zero
  let e_true = V_Bool true |> literal
  let e_false = V_Bool true |> literal
  let e_var s = E_Var s |> add_pos_from loc

  let e_band e1 e2 =
    if e1 == e_true then e2 else if e2 == e_true then e1 else binop BAND e1 e2

  let e_cond e1 e2 e3 = E_Cond (e1, e2, e3) |> add_pos_from loc

  let monomial_to_expr (Prod map) =
    let ( ** ) e1 e2 =
      if e1 = one then e2 else if e2 = one then e1 else binop MUL e1 e2
    in
    let rec ( ^^ ) e = function
      | 0 -> one
      | 1 -> e
      | 2 -> e ** e
      | 3 -> e ** e ** e
      | 4 -> e ** e ** e ** e
      | n ->
          let e' = e ^^ (n / 2) in
          if n mod 2 = 0 then e' ** e' else e' ** e' ** e
    in
    AMap.fold (fun s p e -> (e_var s ^^ p) ** e) map

  let polynomial_to_expr (Sum map) =
    let ( ++ ) e1 e2 =
      if e1 == zero then e2 else if e2 == zero then e1 else binop PLUS e1 e2
    in
    MMap.fold (fun m c e -> monomial_to_expr m (e_of_int c) ++ e) map zero

  let sign_to_binop = function
    | Null -> EQ_OP
    | NotNull -> NEQ
    | StrictPositive -> LT
    | Positive -> LEQ
    | Negative -> GEQ
    | StrictNegative -> GT

  let sign_to_expr sign e = binop (sign_to_binop sign) zero e

  let ctnt_to_expr (Sum p) sign =
    let c = try MMap.find mono_one p with Not_found -> 0
    and p = Sum (MMap.remove mono_one p) in
    binop (sign_to_binop sign) (e_of_int ~-c) (polynomial_to_expr p)

  let ctnts_to_expr : ctnts -> expr option = function
    | Bottom -> None
    | Conjunction map ->
        Some
          (PMap.fold
             (fun p sign e -> e_band (ctnt_to_expr p sign) e)
             map e_true)

  let of_ir : ir_expr -> expr = function
    | Disjunction [] -> zero
    | Disjunction [ (Conjunction map, p) ] when PMap.is_empty map ->
        polynomial_to_expr p
    | Disjunction [ (_, _) ] -> assert false
    | Disjunction map ->
        let map = List.rev map in
        List.fold_left
          (fun e (ctnts, p) ->
            match ctnts_to_expr ctnts with
            | None -> e
            | Some cond -> e_cond cond (polynomial_to_expr p) e)
          cannot_happen_expr map

  let reduce_mono (Prod _) factor = if factor == 0 then None else Some factor

  let rec int_exp x = function
    | 0 -> 1
    | 1 -> x
    | 2 -> x * x
    | 3 -> x * x * x
    | n ->
        let r = int_exp x (n / 2) in
        let r2 = r * r in
        if n mod 2 == 0 then r2 else r2 * x

  type affectation = atom * int * polynomial option
  type affectations = affectation list

  let subst_mono (affectations : affectations) (Prod m) factor =
    let m, factor =
      List.fold_left
        (fun (m, f) (a, v, _) ->
          match AMap.find_opt a m with
          | None -> (m, f)
          | Some power -> (AMap.remove a m, f * int_exp v power))
        (m, factor) affectations
    in
    (Prod m, factor)

  let subst_poly (affectations : affectations) (Sum map as poly) =
    Sum
      (MMap.fold
         (fun mono factor ->
           let affectations =
             List.filter
               (function _, _, Some p -> poly_compare p poly != 0 | _ -> true)
               affectations
           in
           let mono, factor = subst_mono affectations mono factor in
           if factor != 0 then add_mono_to_poly mono factor else Fun.id)
         map MMap.empty)

  let reduce_poly affectations : polynomial -> polynomial =
   fun (Sum ms) ->
    Sum (ms |> MMap.filter_map reduce_mono) |> subst_poly affectations

  let poly_get_constant_opt (Sum p) =
    if MMap.is_empty p then Some 0
    else if MMap.cardinal p = 1 then MMap.find_opt mono_one p
    else None

  let ctnt_is_trivial p s =
    match poly_get_constant_opt p with
    | Some c ->
        if constant_satisfies c s then true
        else raise_notrace ConjunctionBottomInterrupt
    | None -> false

  let reduce_ctnts affectations : ctnts -> ctnts = function
    | Bottom -> Bottom
    | Conjunction ctnts -> (
        try
          Conjunction
            (PMap.fold
               (fun p s ->
                 let p = reduce_poly affectations p in
                 if ctnt_is_trivial p s then Fun.id
                 else
                   PMap.update p (function
                     | None -> Some s
                     | Some s' -> sign_and p s s'))
               ctnts PMap.empty)
        with ConjunctionBottomInterrupt -> Bottom)

  let poly_get_linear (Sum ms) =
    let ms = MMap.filter (fun _ f -> f != 0) ms in
    let n = MMap.cardinal ms in
    if false && n > 2 then None
    else
      let exception NotLinear in
      let mono_get_linear (Prod m) =
        match AMap.bindings m |> List.filter (fun (_, p) -> p != 0) with
        | [] -> None
        | [ (x, 1) ] -> Some x
        | [ (_, 0) ] -> assert false
        | [ (_, _) ] -> raise NotLinear
        | _ :: _ :: _ -> raise NotLinear
      in
      let o, c =
        try
          MMap.fold
            (fun mono factor o ->
              match (o, mono_get_linear mono) with
              | (o, c), None -> (o, c - factor)
              | (None, c), Some x -> (Some x, c)
              | (Some x, _), Some x' ->
                  assert (not (String.equal x x'));
                  raise_notrace NotLinear)
            ms (None, 0)
        with NotLinear -> (None, 0)
      in
      match o with
      | None ->
          if c != 0 then raise_notrace ConjunctionBottomInterrupt else None
      | Some x -> Some (x, c)

  let deduce_equations : ctnts -> ctnts * affectations = function
    | Bottom -> (Bottom, [])
    | Conjunction map as ctnts -> (
        try
          let affectations =
            PMap.fold
              (fun p s affectations ->
                if s != Null then affectations
                else
                  match poly_get_linear p with
                  | None -> affectations
                  | Some (s, i) -> (s, i, Some p) :: affectations)
              map []
          in
          (reduce_ctnts affectations ctnts, affectations)
        with ConjunctionBottomInterrupt -> (Bottom, []))

  let ctnts_get_trivial_opt = function
    | Bottom -> Some false
    | Conjunction li -> (
        try if PMap.for_all ctnt_is_trivial li then Some true else None
        with ConjunctionBottomInterrupt -> Some false)

  let reduce (Disjunction ir) =
    Disjunction
      ( ir
      |> List.filter_map (fun (ctnts, p) ->
             let ctnts, affectations = deduce_equations ctnts in
             match ctnts with
             | Bottom -> None
             | Conjunction _ as c -> Some (c, reduce_poly affectations p))
      |> fun li ->
        List.fold_right
          (fun (ctnts, p) acc ->
            match ctnts_get_trivial_opt ctnts with
            | Some true -> [ (ctnts_true, p) ]
            | Some false -> acc
            | None -> (ctnts, p) :: acc)
          li [] )

  let normalize (env : env) (e : expr) : expr =
    e |> to_ir env |> reduce |> of_ir

  let free_variables (Disjunction li) =
    let mono_free (Prod map) = AMap.fold (fun s _ -> ISet.add s) map in
    let poly_free (Sum map) = MMap.fold (fun m _ -> mono_free m) map in
    let ctnt_free p _s = poly_free p in
    let ctnts_free = function
      | Bottom -> Fun.id
      | Conjunction map -> PMap.fold ctnt_free map
    in
    List.fold_left
      (fun acc (ctnts, p) -> acc |> poly_free p |> ctnts_free ctnts)
      ISet.empty li

  let equal_mod_branches (Disjunction li1) (Disjunction li2) =
    let to_cond (ctnts1, p1) (ctnts2, p2) =
      let equality =
        let p = add_polys p1 (poly_neg p2) in
        Conjunction (PMap.singleton p Null)
      in
      let ctnts = ctnts_and ctnts1 ctnts2 in
      let ctnts, affectations = deduce_equations ctnts in
      let affectations =
        List.map (fun (x, v, _) -> (x, v, None)) affectations
      in
      let () =
        if false then
          Format.eprintf
            "@[<hv 2>Equality between@ %a@ and %a @ gave affectations %a@.@]"
            pp_ctnts_and_poly (ctnts1, p1) pp_ctnts_and_poly (ctnts2, p2)
            Format.(
              pp_print_list ~pp_sep:pp_print_space (fun f (x, d, _) ->
                  fprintf f "%s/%d" x d))
            affectations
      in
      match ctnts with
      | Bottom -> Bottom
      | Conjunction _ ->
          let equality' = reduce_ctnts affectations equality in
          let () =
            if false then Format.eprintf "@[Gave %a@.@]" pp_ctnts equality'
          in
          equality'
    in
    list_cross to_cond li1 li2
    |> List.for_all (function
         | Bottom -> false
         | Conjunction m -> PMap.is_empty m)
end

let equal_in_env env e1 e2 =
  let dbg = false in
  let open Normalize in
  let () =
    if dbg then
      Format.eprintf "@[<hv 2>Are %a@ and %a@ equal?@]@ " PP.pp_expr e1
        PP.pp_expr e2
  in
  try
    let ir1 = to_ir env e1 |> reduce and ir2 = to_ir env e2 |> reduce in
    let () =
      if dbg then
        Format.eprintf "@[Reducing them to@ %a@ and %a.@]@ " Normalize.pp_ir ir1
          Normalize.pp_ir ir2
    in
    let res = equal_mod_branches ir1 ir2 in
    let () =
      if dbg then if res then Format.eprintf "YES@." else Format.eprintf "NO@."
    in
    res
  with NotYetImplemented ->
    let () = if dbg then Format.eprintf "Cannot answer this question yet." in
    false

let statically_free_variables env e =
  let open Normalize in
  try to_ir env e |> reduce |> free_variables
  with NotYetImplemented -> ASTUtils.ISet.empty

let bitwidth_statically_equal_in_env env =
  ASTUtils.bitwidth_equal (equal_in_env env)
