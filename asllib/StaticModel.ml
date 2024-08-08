open AST
open ASTUtils
open Error

exception NotYetImplemented

type atom = identifier
(** Our basic variables. *)

module AtomOrdered = struct
  type t = atom

  let compare = String.compare
end

module AMap = Map.Make (AtomOrdered)
(** A map from atoms. *)

(** A unitary monomial.

      They are unitary in the sense that they do not have any factors:
      {m 3 \times X^2 } is not unitary, while {m x^2 } is.

      For example: {m X^2 + Y^4 } represented by {m X \to 2, Y \to 4 },
      and {m 1 } is represented by the empty map. *)
type monomial = Prod of int AMap.t  (** Maps each variable to its exponent. *)

module MonomialOrdered = struct
  type t = monomial

  let compare (Prod ms1) (Prod ms2) = AMap.compare Int.compare ms1 ms2
end

(** A map from a monomial. *)
module MMap = struct
  include Map.Make (MonomialOrdered)

  (* Available from 4.11.0 *)
  let filter_map f m =
    fold
      (fun key v r -> match f key v with None -> r | Some v -> add key v r)
      m empty
end

(** A polynomial.

      For example, {m X^2 - X + 4 } is represented by
      {m X^2 \to 1, X \to -1, 1 \to 4 } *)
type polynomial = Sum of Q.t MMap.t  (** Maps each monomial to its factor. *)

module PolynomialOrdered = struct
  type t = polynomial

  let compare (Sum p1) (Sum p2) = MMap.compare Q.compare p1 p2
end

module PMap = Map.Make (PolynomialOrdered)
(** Map from polynomials. *)

(** A constraint on a numerical value. *)
type sign =
  | Null
  | StrictPositive
  | Positive
  | Negative
  | StrictNegative
  | NotNull

(** A conjunctive logical formulae with polynomials.

      For example, {m X^2 \leq 0 } is represented with {m X^2 \to \leq 0 }.
  *)
type ctnts = Conjunction of sign PMap.t | Bottom

(** Case disjunctions. *)
type 'a disjunction = Disjunction of 'a list

type ir_expr = (ctnts * polynomial) disjunction
(** Constrained polynomials.

      This is a branched tree of polynomials.
  *)
(* Wanted invariants for (e : ir_expr) :
   ⋁ {c | (c, d) ∈ e } <=> true                           (I₂)
   ∀ (cᵢ, eᵢ), (cⱼ, eⱼ) ∈ e, i != j => cⱼ ∩ cⱼ = ∅        (I₃)
*)

(* ----------------------------------------------------------------------- *)
(* Printers *)

let pp_mono f (Prod mono, factor) =
  let open Format in
  let mono = AMap.filter (fun _ p -> p != 0) mono in
  if AMap.is_empty mono then Q.pp_print f factor
  else (
    pp_open_hbox f ();
    let pp_sep f () = fprintf f "@ \u{d7} " in
    if Q.equal factor Q.one then ()
    else (
      Q.pp_print f factor;
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
  let poly = MMap.filter (fun _ f -> not (Q.equal Q.zero f)) poly in
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
let poly_of_var s = Sum (MMap.singleton (mono_of_var s) Q.one)
let poly_of_q i = Sum (MMap.singleton mono_one i)
let poly_of_z i = Q.of_bigint i |> poly_of_q
let poly_of_int i = Q.of_int i |> poly_of_q
let poly_neg (Sum monos) = Sum (MMap.map Q.neg monos)

let poly_of_val = function
  | L_Int i -> poly_of_z i
  | v ->
      Error.fatal_unknown_pos
        (Error.MismatchType (PP.literal_to_string v, [ integer' ]))

let sign_not = function
  | NotNull -> Null
  | Null -> NotNull
  | Positive -> StrictNegative
  | Negative -> StrictPositive
  | StrictPositive -> Negative
  | StrictNegative -> Positive

let sign_minus = function
  | (NotNull | Null) as s -> s
  | Positive -> Negative
  | Negative -> Positive
  | StrictPositive -> StrictNegative
  | StrictNegative -> StrictPositive

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
  let open Q in
  match s with
  | Null -> equal c zero
  | NotNull -> not (equal c zero)
  | Positive -> geq c zero
  | StrictPositive -> gt c zero
  | Negative -> leq c zero
  | StrictNegative -> lt c zero

let ctnts_of_bool b = if b then ctnts_true else ctnts_false

let ctnts_not = function
  | Bottom -> ctnts_true
  | Conjunction ctnts -> (
      try Conjunction (PMap.map sign_not ctnts)
      with ConjunctionBottomInterrupt -> Bottom)

let sign_compare = Stdlib.compare
let mono_compare = MonomialOrdered.compare
let poly_compare (Sum p1) (Sum p2) = MMap.compare Q.compare p1 p2

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
        let f' = Q.add f factor in
        if Q.equal f' Q.zero then None else Some f'
  in
  fun mono factor -> MMap.update mono (updater factor)

let add_polys : polynomial -> polynomial -> polynomial =
 fun (Sum monos1) (Sum monos2) ->
  Sum (MMap.union (fun _mono c1 c2 -> Some (Q.add c1 c2)) monos1 monos2)

let mult_monos : monomial -> monomial -> monomial =
 fun (Prod map1) (Prod map2) ->
  Prod (AMap.union (fun _ p1 p2 -> Some (p1 + p2)) map1 map2)

let mult_polys : polynomial -> polynomial -> polynomial =
 fun (Sum monos1) (Sum monos2) ->
  Sum
    (MMap.fold
       (fun m1 f1 ->
         MMap.fold
           (fun m2 f2 -> add_mono_to_poly (mult_monos m1 m2) (Q.mul f1 f2))
           monos2)
       monos1 MMap.empty)

let mult_poly_const : Q.t -> polynomial -> polynomial =
 fun f2 (Sum monos) -> Sum (MMap.map (fun f1 -> Q.mul f1 f2) monos)

let divide_monos : monomial -> monomial -> monomial =
 fun (Prod map1) (Prod map2) ->
  Prod
    (AMap.merge
       (fun _x o1 o2 ->
         match (o1, o2) with
         | _, None -> o1
         | None, Some 0 -> None
         | None, Some _ -> raise NotYetImplemented
         | Some p1, Some p2 when p1 > p2 -> Some (p2 - p1)
         | Some p1, Some p2 when p1 = p2 -> None
         | Some _, Some _ -> raise NotYetImplemented)
       map1 map2)

let divide_poly_mono : polynomial -> monomial -> polynomial =
 fun (Sum monos) m ->
  Sum
    (MMap.fold
       (fun m' f -> add_mono_to_poly (divide_monos m' m) f)
       monos MMap.empty)

let divide_polys : polynomial -> polynomial -> polynomial =
 fun poly1 (Sum monos2) ->
  if MMap.cardinal monos2 = 1 then
    let m, c = MMap.choose monos2 in
    divide_poly_mono poly1 m |> mult_poly_const (Q.inv c)
  else raise NotYetImplemented

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

let disjunction_or (Disjunction li1) (Disjunction li2) = Disjunction (li1 @ li2)

let cross_num (Disjunction li1) (Disjunction li2) f =
  let on_pair (ctnts1, e1) (ctnts2, e2) = (ctnts_and ctnts1 ctnts2, f e1 e2) in
  Disjunction (ASTUtils.list_cross on_pair li1 li2)

let map_num f (Disjunction li1 : ir_expr) : ir_expr =
  Disjunction (List.map (fun (ctnt, e) -> (ctnt, f e)) li1)

let disjunction_cross f (Disjunction li1) (Disjunction li2) =
  Disjunction (ASTUtils.list_cross f li1 li2)

let ir_to_cond sign (Disjunction li2) =
  Disjunction
    (List.map
       (fun (ctnts, p) -> ctnts_and (Conjunction (PMap.singleton p sign)) ctnts)
       li2)

let rec make_anonymous (env : StaticEnv.env) (ty : ty) : ty =
  match ty.desc with
  | T_Named x -> (
      match IMap.find_opt x env.global.declared_types with
      | Some ty' -> make_anonymous env ty'
      | None -> fatal_from ty (Error.UndefinedIdentifier x))
  | _ -> ty

let rec to_ir env (e : expr) : ir_expr =
  match e.desc with
  | E_Literal (L_Int i) -> poly_of_z i |> always
  | E_Var s -> (
      try StaticEnv.lookup_constants env s |> poly_of_val |> always
      with Not_found -> (
        try StaticEnv.lookup_immutable_expr env s |> to_ir env
        with Not_found | NotYetImplemented -> (
          let t =
            try StaticEnv.type_of env s
            with Not_found -> Error.fatal_from e (UndefinedIdentifier s)
          in
          let ty1 = make_anonymous env t in
          match ty1.desc with
          | T_Int (WellConstrained [ Constraint_Exact e ]) -> to_ir env e
          | T_Int _ -> poly_of_var s |> always
          | _ ->
              Error.fatal_unknown_pos
                (Error.ConflictingTypes ([ integer' ], ty1)))))
  | E_Binop (PLUS, e1, e2) ->
      let ir1 = to_ir env e1 and ir2 = to_ir env e2 in
      cross_num ir1 ir2 add_polys
  | E_Binop (MINUS, e1, e2) ->
      let e2 = E_Unop (NEG, e2) |> ASTUtils.add_pos_from_st e2 in
      E_Binop (PLUS, e1, e2) |> ASTUtils.add_pos_from_st e |> to_ir env
  | E_Binop (MUL, { desc = E_Binop (DIV, e1, e2); _ }, e3) ->
      to_ir env (binop DIV (binop MUL e1 e3) e2)
  | E_Binop (MUL, e1, { desc = E_Binop (DIV, e2, e3); _ }) ->
      to_ir env (binop DIV (binop MUL e1 e2) e3)
  | E_Binop (MUL, e1, e2) ->
      let ir1 = to_ir env e1 and ir2 = to_ir env e2 in
      cross_num ir1 ir2 mult_polys
  | E_Binop (DIV, e1, { desc = E_Literal (L_Int i2); _ }) ->
      let ir1 = to_ir env e1 and f2 = Q.(Z.one /// i2) in
      map_num (mult_poly_const f2) ir1
  | E_Binop (DIV, e1, e2) ->
      let ir1 = to_ir env e1 and ir2 = to_ir env e2 in
      cross_num ir1 ir2 divide_polys
  | E_Binop (SHL, e1, { desc = E_Literal (L_Int i2); _ }) when Z.leq Z.zero i2
    ->
      let ir1 = to_ir env e1
      and f2 = Z.to_int i2 |> Z.shift_left Z.one |> Q.of_bigint in
      map_num (mult_poly_const f2) ir1
  | E_Binop (op, { desc = E_Literal l1; _ }, { desc = E_Literal l2; _ }) ->
      Operations.binop_values e Error.Static op l1 l2 |> poly_of_val |> always
  | E_Unop (NEG, e0) -> e0 |> to_ir env |> map_num poly_neg
  | E_Cond (cond, e1, e2) ->
      let Disjunction ctnts, Disjunction nctnts = to_cond env cond
      and (Disjunction ir1) = to_ir env e1
      and (Disjunction ir2) = to_ir env e2 in
      let restrict ctnts (ctnts', p) = (ctnts_and ctnts ctnts', p) in
      let ir1' = ASTUtils.list_cross restrict ctnts ir1
      and ir2' = ASTUtils.list_cross restrict nctnts ir2 in
      Disjunction (ir1' @ ir2')
  | E_ATC (e', _) -> to_ir env e'
  | _ -> (
      let v =
        let open StaticInterpreter in
        try static_eval env e
        with
        | StaticEvaluationUnknown
        | Error.ASLException { desc = UnsupportedExpr _; _ }
        ->
          raise NotYetImplemented
      in
      match v with
      | L_Int i -> poly_of_z i |> always
      | _ -> raise NotYetImplemented)

and to_cond env (e : expr) : ctnts disjunction * ctnts disjunction =
  let ( ||| ) = disjunction_or in
  let ( &&& ) = disjunction_cross ctnts_and in
  match e.desc with
  | E_Literal (L_Bool b) ->
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
let zero = expr_of_int 0
let one = expr_of_int 1
let cannot_happen_expr = zero

let expr_of_z z =
  if Z.equal z Z.one then one
  else if Z.equal z Z.zero then zero
  else literal (L_Int z)

let expr_of_q q =
  if Q.equal q Q.one then one
  else if Q.equal q Q.zero then zero
  else if Z.equal (Q.den q) Z.one then expr_of_z (Q.num q)
  else binop DIV (expr_of_z (Q.num q)) (expr_of_z (Q.den q))

let e_true = L_Bool true |> literal
let e_false = L_Bool true |> literal
let e_var s = var_ s

let e_band e1 e2 =
  if e1 == e_true then e2 else if e2 == e_true then e1 else binop BAND e1 e2

let e_cond e1 e2 e3 = E_Cond (e1, e2, e3) |> add_pos_from loc
let unop op e = E_Unop (op, e) |> add_pos_from loc

let monomial_to_expr (Prod map) =
  let ( ** ) e1 e2 =
    if e1 == one then e2 else if e2 == one then e1 else binop MUL e1 e2
  in
  let ( ^^ ) e = function
    | 0 -> one
    | 1 -> e
    | 2 -> e ** e
    | p -> binop POW e (expr_of_int p)
  in
  let ( // ) e z = if Z.equal z Z.one then e else binop DIV e (expr_of_z z) in
  fun c ->
    let start = Q.num c |> expr_of_z in
    let num = AMap.fold (fun s p e -> e ** (e_var s ^^ p)) map start in
    num // Q.den c

let sign_of_q c =
  match Q.sign c with
  | 1 -> StrictPositive
  | 0 -> Null
  | -1 -> StrictNegative
  | _ -> assert false

let polynomial_to_expr (Sum map) =
  let add e1 (s, e2) = if s > 0 then binop PLUS e1 e2 else binop MINUS e1 e2 in
  List.fold_left
    (fun e (m, c) ->
      if e == zero then monomial_to_expr m c
      else if Q.sign c = 0 then e
      else
        let e_m = monomial_to_expr m (Q.abs c) in
        add e (Q.sign c, e_m))
    zero
    (MMap.bindings map |> List.rev)

let sign_to_binop = function
  | Null -> EQ_OP
  | NotNull -> NEQ
  | StrictPositive -> LT
  | Positive -> LEQ
  | Negative -> GEQ
  | StrictNegative -> GT

let sign_to_expr sign e = binop (sign_to_binop sign) zero e

let ctnt_to_expr (Sum p) sign =
  let c = try MMap.find mono_one p with Not_found -> Q.zero
  and p = Sum (MMap.remove mono_one p) in
  binop (sign_to_binop sign) (expr_of_q (Q.neg c)) (polynomial_to_expr p)

let ctnts_to_expr : ctnts -> expr option = function
  | Bottom -> None
  | Conjunction map ->
      Some
        (PMap.fold (fun p sign e -> e_band (ctnt_to_expr p sign) e) map e_true)

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

let reduce_mono (Prod _) factor =
  if Q.equal factor Q.zero then None else Some factor

let rec int_exp x = function
  | 0 -> 1
  | 1 -> x
  | 2 -> x * x
  | 3 -> x * x * x
  | n ->
      let r = int_exp x (n / 2) in
      let r2 = r * r in
      if n mod 2 == 0 then r2 else r2 * x

type affectation = atom * Z.t * polynomial option
type affectations = affectation list

let subst_mono (affectations : affectations) (Prod m) factor =
  let m, factor =
    List.fold_left
      (fun (m, f) (a, v, _) ->
        match AMap.find_opt a m with
        | None -> (m, f)
        | Some power -> (AMap.remove a m, Q.mul f (Z.pow v power |> Q.of_bigint)))
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
         if Q.equal Q.zero factor then Fun.id else add_mono_to_poly mono factor)
       map MMap.empty)

let reduce_poly affectations : polynomial -> polynomial =
 fun (Sum ms) ->
  Sum (ms |> MMap.filter_map reduce_mono) |> subst_poly affectations

let poly_get_constant_opt (Sum p) =
  if MMap.is_empty p then Some Q.zero
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
  let ms = MMap.filter (fun _ f -> not (Q.equal f Q.zero)) ms in
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
            | (o, c), None -> (o, Q.sub c factor)
            | (None, c), Some x -> (Some x, c)
            | (Some x, _), Some x' ->
                assert (not (String.equal x x'));
                raise_notrace NotLinear)
          ms (None, Q.zero)
      with NotLinear -> (None, Q.zero)
    in
    match o with
    | None ->
        if not (Q.equal Q.zero c) then raise_notrace ConjunctionBottomInterrupt
        else None
    | Some x -> if Z.equal Z.one (Q.den c) then Some (x, Q.num c) else None

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

let normalize (env : StaticEnv.env) (e : expr) : expr =
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
    let affectations = List.map (fun (x, v, _) -> (x, v, None)) affectations in
    let () =
      if false then
        Format.eprintf
          "@[<hv 2>Equality between@ %a@ and %a @ gave affectations %a@.@]"
          pp_ctnts_and_poly (ctnts1, p1) pp_ctnts_and_poly (ctnts2, p2)
          Format.(
            pp_print_list ~pp_sep:pp_print_space (fun f (x, d, _) ->
                fprintf f "%s/%a" x Z.pp_print d))
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

let equal_in_env env e1 e2 =
  let dbg = false in
  let () =
    if dbg then
      Format.eprintf "@[<hv 2>Are %a@ and %a@ equal?@]@ " PP.pp_expr e1
        PP.pp_expr e2
  in
  try
    let ir1 = to_ir env e1 |> reduce and ir2 = to_ir env e2 |> reduce in
    let () =
      if dbg then
        Format.eprintf "@[Reducing them to@ %a@ and %a.@]@ " pp_ir ir1 pp_ir ir2
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
  try to_ir env e |> reduce |> free_variables
  with NotYetImplemented -> ASTUtils.ISet.empty

let bitwidth_statically_equal_in_env env =
  ASTUtils.bitwidth_equal (equal_in_env env)

let try_normalize env e =
  try normalize env e with Error.ASLException _ | NotYetImplemented -> e
