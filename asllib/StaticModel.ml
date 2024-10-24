open AST
open ASTUtils
open Error

exception NotSupported

(*---------- Symbolic representation ----------*)

module Monomial : sig
  type t

  val compare : t -> t -> int
  val one : t
  val single_term : identifier -> t
  val mult : t -> t -> t
  val divide : t -> t -> t

  val to_scaled_expr : t -> Q.t -> expr
  (** Constructs an expression for [factor] * [monos]. *)

  val pp_with_factor : Format.formatter -> t * Q.t -> unit
end = struct
  module AtomMap = Map.Make (String)
  (** A map from identifiers. *)

  type t = int AtomMap.t
  (** A unitary monomial.
      They are unitary in the sense that they do not have any factors:
      {m 3 \times X^2 } is not unitary, while {m x^2 } is.

      Maps each variable to its exponent.
      For example: {m X^2 + Y^4 } represented by {m X \to 2, Y \to 4 },
      and {m 1 } is represented by the empty map.

      Invariant: all integer exponents are strictly positive. *)

  let compare = AtomMap.compare Int.compare
  let one = AtomMap.empty
  let single_term atom = AtomMap.singleton atom 1

  let mult mono1 mono2 =
    AtomMap.union
      (fun _ p1 p2 ->
        assert (p1 > 0 && p2 > 0);
        Some (p1 + p2))
      mono1 mono2

  let divide mono1 mono2 =
    let divide_unitary a1 a2 =
      match (a1, a2) with
      | _, None -> a1
      | Some p1, Some p2 when p1 > p2 -> Some (p1 - p2) (* not currently used *)
      | Some p1, Some p2 when p1 = p2 -> None
      | _ -> raise NotSupported
    in
    AtomMap.merge (fun _ -> divide_unitary) mono1 mono2

  let to_scaled_expr monos factor =
    let start = expr_of_z (Q.num factor) in
    let numerator =
      AtomMap.fold
        (fun atom exponent acc -> mul_expr acc (pow_expr (var_ atom) exponent))
        monos start
    in
    div_expr numerator (Q.den factor)

  let pp_with_factor f (monos, factor) =
    let open Format in
    if AtomMap.is_empty monos then Q.pp_print f factor
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
        f (AtomMap.to_seq monos);
      pp_close_box f ())
end

module Polynomial : sig
  type t

  val compare : t -> t -> int
  val single_term : Monomial.t -> Q.t -> t
  val to_mono : t -> (Monomial.t * Q.t) option
  val scale : Q.t -> t -> t
  val neg : t -> t
  val add : t -> t -> t
  val mult : t -> t -> t
  val divide_by_term : t -> Q.t -> Monomial.t -> t
  val extract_constant_term : t -> Q.t * t
  val is_constant : t -> Q.t option
  val to_expr : t -> expr
  val pp : Format.formatter -> t -> unit
end = struct
  module MonomialMap = Map.Make (Monomial)
  (** A map from a monomial. *)

  type t = Q.t MonomialMap.t
  (** A polynomial.
      Maps each monomial to its factor.
      For example, {m X^2 - X + 4 } is represented by
      {m X^2 \to 1, X \to -1, 1 \to 4 } *)

  let compare = MonomialMap.compare Q.compare

  let single_term mono factor =
    if Q.equal factor Q.zero then MonomialMap.empty
    else MonomialMap.singleton mono factor

  let to_mono poly =
    if MonomialMap.cardinal poly = 1 then Some (MonomialMap.choose poly)
    else None

  let add poly1 poly2 =
    MonomialMap.union
      (fun _ c1 c2 ->
        let coeff = Q.add c1 c2 in
        if Q.equal coeff Q.zero then None else Some coeff)
      poly1 poly2

  let scale factor poly =
    assert (factor <> Q.zero);
    MonomialMap.map (Q.mul factor) poly

  let termwise f poly =
    MonomialMap.fold
      (fun mono factor -> add (f factor mono))
      poly MonomialMap.empty

  let mult_mono poly factor mono =
    termwise
      (fun f m -> single_term (Monomial.mult m mono) (Q.mul f factor))
      poly

  let neg poly = MonomialMap.map Q.neg poly
  let mult poly1 poly2 = termwise (mult_mono poly1) poly2

  let divide_by_term poly factor mono =
    termwise
      (fun f m -> single_term (Monomial.divide m mono) (Q.div f factor))
      poly

  let extract_constant_term poly =
    let c = try MonomialMap.find Monomial.one poly with Not_found -> Q.zero
    and p = MonomialMap.remove Monomial.one poly in
    (c, p)

  let is_constant poly =
    if MonomialMap.is_empty poly then Some Q.zero
    else if MonomialMap.cardinal poly = 1 then
      MonomialMap.find_opt Monomial.one poly
    else None

  let to_expr poly =
    List.fold_left
      (fun acc (m, c) ->
        if ASTUtils.expr_equal (fun _ _ -> false) acc zero_expr then
          Monomial.to_scaled_expr m c
        else
          let e_m = Monomial.to_scaled_expr m (Q.abs c) in
          add_expr acc (Q.sign c, e_m))
      zero_expr
      (MonomialMap.bindings poly |> List.rev)

  let pp f poly =
    let open Format in
    if MonomialMap.is_empty poly then pp_print_string f "0"
    else (
      pp_open_hvbox f 2;
      let pp_sep f () = fprintf f "@ + " in
      PP.pp_print_seq ~pp_sep Monomial.pp_with_factor f
        (MonomialMap.to_seq poly);
      pp_close_box f ())
end

module Conjunction : sig
  type eq = Zero | NonZero
  type t

  val empty : t
  val is_bottom : t -> bool
  val is_empty : t -> bool
  val of_bool : bool -> t
  val single_conjunct : Polynomial.t -> eq -> t
  val conj : t -> t -> t
  val to_expr : t -> expr option

  type triviality = TriviallyTrue | TriviallyFalse | NonTrivial

  val get_triviality : t -> triviality
  val reduce : t -> t
  val pp : Format.formatter -> t -> unit
end = struct
  type eq = Zero | NonZero  (** A (in)equation for a numerical value. *)

  let satisfies_eq q eq =
    let eq_zero = Q.equal q Q.zero in
    match eq with Zero -> eq_zero | NonZero -> not eq_zero

  let pp_eq f s =
    let s = match s with Zero -> "= 0" | NonZero -> "!= 0" in
    Format.pp_print_string f s

  let eq_to_op = function Zero -> EQ_OP | NonZero -> NEQ

  module PolynomialMap = Map.Make (Polynomial)
  (** Map from polynomials. *)

  type t = eq PolynomialMap.t option
  (** A conjunctive logical formula with polynomials.

      We use the [option] to represent falsity as [None].  [Some map] is then a
      conjunction of constraints on polynomials, as dictated by [map].  For
      example, {m X^2 = 0} is represented with {m Some (X^2 \to Zero)}.
  *)

  let is_bottom = function None -> true | Some _ -> false

  let is_empty = function
    | None -> false
    | Some map -> PolynomialMap.is_empty map

  let empty = Some PolynomialMap.empty
  let of_bool b = if b then empty else None
  let single_conjunct p eq = Some (PolynomialMap.singleton p eq)

  let conj c1 c2 =
    let exception BottomInterrupt in
    let eq_and eq1 eq2 =
      if eq1 = eq2 then eq1 else raise_notrace BottomInterrupt
    in
    match (c1, c2) with
    | None, _ | _, None -> None
    | Some cjs1, Some cjs2 -> (
        try
          Some
            (PolynomialMap.union
               (fun _ eq1 eq2 -> Some (eq_and eq1 eq2))
               cjs1 cjs2)
        with BottomInterrupt -> None)

  let to_expr =
    let one_to_expr poly eq =
      let c, p = Polynomial.extract_constant_term poly in
      binop (eq_to_op eq) (expr_of_rational (Q.neg c)) (Polynomial.to_expr p)
    in
    Option.map (fun map ->
        PolynomialMap.fold
          (fun p eq e -> conj_expr (one_to_expr p eq) e)
          map (literal (L_Bool true)))

  let is_true p eq =
    match Polynomial.is_constant p with
    | Some q -> satisfies_eq q eq
    | None -> false

  let is_false p eq =
    match Polynomial.is_constant p with
    | Some q -> not (satisfies_eq q eq)
    | None -> false

  let reduce = function
    | None -> None
    | Some cjs ->
        let non_trivial =
          PolynomialMap.filter (fun p s -> not (is_true p s)) cjs
        in
        if PolynomialMap.exists is_false non_trivial then None
        else Some non_trivial

  type triviality = TriviallyTrue | TriviallyFalse | NonTrivial

  let get_triviality = function
    | None -> TriviallyFalse
    | Some cjs ->
        if PolynomialMap.for_all is_true cjs then TriviallyTrue
        else if PolynomialMap.exists is_false cjs then TriviallyFalse
        else NonTrivial

  let pp f =
    let open Format in
    let pp_one f (p, s) = fprintf f "@[<h>%a@ %a@]" Polynomial.pp p pp_eq s in
    function
    | None -> pp_print_string f "\u{22a5}"
    | Some m ->
        if PolynomialMap.is_empty m then pp_print_string f "\u{22a4}"
        else
          let pp_sep f () = fprintf f "@ \u{2227} " in
          fprintf f "@[<hov 2>%a@]"
            (PP.pp_print_seq ~pp_sep pp_one)
            (PolynomialMap.to_seq m)
end

module IR : sig
  type t

  val of_var : identifier -> t
  val of_int : Z.t -> t
  val combine : t -> t -> t

  val cross_combine :
    (Polynomial.t -> Polynomial.t -> Polynomial.t) -> t -> t -> t

  val map : (Polynomial.t -> Polynomial.t) -> t -> t
  val restrict : Conjunction.t list -> t -> t
  val to_conjuncts : Conjunction.eq -> t -> Conjunction.t list
  val to_expr : t -> expr
  val reduce : t -> t
  val equal_mod_branches : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end = struct
  type t = (Conjunction.t * Polynomial.t) list
  (** Case disjunctions: constrained polynomials.
      This is a branched tree of polynomials.
  *)
  (* Wanted invariants for (e : IR.t) :
     ∀ {c | (c, d) ∈ e } <=> true                           (I₂)
     ∀ (cᵢ, eᵢ), (cⱼ, eⱼ) ∈ e, i != j => cⱼ ∩ cⱼ = ∅        (I₃)
  *)

  let always e = [ (Conjunction.of_bool true, e) ]
  let of_var s = Polynomial.single_term (Monomial.single_term s) Q.one |> always
  let of_int i = Polynomial.single_term Monomial.one (Q.of_bigint i) |> always
  let combine = ( @ )

  let cross_combine f =
    let on_pair (cjs1, e1) (cjs2, e2) = (Conjunction.conj cjs1 cjs2, f e1 e2) in
    ASTUtils.list_cross on_pair

  let map f = List.map (fun (cj, e) -> (cj, f e))

  let restrict cjs ir =
    let restrict_one cjs (cjs', p) = (Conjunction.conj cjs cjs', p) in
    ASTUtils.list_cross restrict_one cjs ir

  let to_conjuncts eq ir =
    List.map
      (fun (cjs, p) -> Conjunction.conj (Conjunction.single_conjunct p eq) cjs)
      ir

  let to_expr = function
    | [] -> zero_expr
    | [ (cjs, p) ] ->
        assert (Conjunction.is_empty cjs);
        Polynomial.to_expr p
    | map ->
        let cannot_happen_expr = zero_expr in
        List.fold_left
          (fun e (cjs, p) ->
            match Conjunction.to_expr cjs with
            | None -> e
            | Some condition -> cond_expr condition (Polynomial.to_expr p) e)
          cannot_happen_expr (List.rev map)

  let reduce ir =
    ir
    |> List.filter_map (fun (cjs, poly) ->
           let cjs = Conjunction.reduce cjs in
           if Conjunction.is_bottom cjs then None else Some (cjs, poly))
    |> fun ir ->
    List.fold_right
      (fun (cjs, poly) acc ->
        match Conjunction.get_triviality cjs with
        | TriviallyTrue -> [ (Conjunction.empty, poly) ]
        | TriviallyFalse -> acc
        | NonTrivial -> (cjs, poly) :: acc)
      ir []

  let equal_mod_branches ir1 ir2 =
    let to_cond (cjs1, poly1) (cjs2, poly2) =
      let equality =
        let poly = Polynomial.add poly1 (Polynomial.neg poly2) in
        Conjunction.single_conjunct poly Zero
      in
      let cjs = Conjunction.conj cjs1 cjs2 in
      if Conjunction.is_bottom cjs then Conjunction.empty
      else
        let equality = Conjunction.reduce equality in
        let () =
          if false then Format.eprintf "@[Gave %a@.@]" Conjunction.pp equality
        in
        equality
    in
    ASTUtils.list_cross to_cond ir1 ir2
    |> List.for_all (fun cjs ->
           if Conjunction.is_bottom cjs then false else Conjunction.is_empty cjs)

  let pp f li =
    let open Format in
    let pp_one f (cjs, poly) =
      Format.fprintf f "@[<2>%a@ -> %a@]" Conjunction.pp cjs Polynomial.pp poly
    in
    fprintf f "@[<v 2>%a@]" (pp_print_list ~pp_sep:pp_print_space pp_one) li
end

(*---------- Converting expressions to symbolic representation ----------*)

let rec make_anonymous (env : StaticEnv.env) (ty : ty) : ty =
  match ty.desc with
  | T_Named x -> (
      match IMap.find_opt x env.global.declared_types with
      | Some ty' -> make_anonymous env ty'
      | None -> fatal_from ty (Error.UndefinedIdentifier x))
  | _ -> ty

(* Begin ToIR *)
let rec to_ir env (e : expr) =
  let of_lit = function L_Int i -> IR.of_int i | _ -> raise NotSupported in
  match e.desc with
  | E_Literal (L_Int i) -> IR.of_int i
  | E_Var s -> (
      try StaticEnv.lookup_constants env s |> of_lit
      with Not_found -> (
        try StaticEnv.lookup_immutable_expr env s |> to_ir env
        with Not_found | NotSupported -> (
          let t =
            try StaticEnv.type_of env s
            with Not_found -> Error.fatal_from e (UndefinedIdentifier s)
          in
          let ty1 = make_anonymous env t in
          match ty1.desc with
          | T_Int (WellConstrained [ Constraint_Exact e ]) -> to_ir env e
          | T_Int _ -> IR.of_var s
          | _ -> raise NotSupported)))
  | E_Binop (PLUS, e1, e2) ->
      let ir1 = to_ir env e1 and ir2 = to_ir env e2 in
      IR.cross_combine Polynomial.add ir1 ir2
  | E_Binop (MINUS, e1, e2) ->
      let e2 = E_Unop (NEG, e2) |> ASTUtils.add_pos_from_st e2 in
      E_Binop (PLUS, e1, e2) |> ASTUtils.add_pos_from_st e |> to_ir env
  | E_Binop (MUL, { desc = E_Binop (DIV, e1, e2); _ }, e3) ->
      to_ir env (binop DIV (binop MUL e1 e3) e2)
  | E_Binop (MUL, e1, { desc = E_Binop (DIV, e2, e3); _ }) ->
      to_ir env (binop DIV (binop MUL e1 e2) e3)
  | E_Binop (MUL, e1, e2) ->
      let ir1 = to_ir env e1 and ir2 = to_ir env e2 in
      IR.cross_combine Polynomial.mult ir1 ir2
  | E_Binop (DIV, e1, { desc = E_Literal (L_Int i2); _ }) ->
      let ir1 = to_ir env e1 and f2 = Q.(Z.one /// i2) in
      IR.map (Polynomial.scale f2) ir1
  | E_Binop (DIV, e1, e2) ->
      let ir1 = to_ir env e1 and ir2 = to_ir env e2 in
      IR.cross_combine
        (fun poly1 poly2 ->
          match Polynomial.to_mono poly2 with
          | Some (mono, factor) -> Polynomial.divide_by_term poly1 factor mono
          | None -> raise NotSupported)
        ir1 ir2
  | E_Binop (SHL, e1, { desc = E_Literal (L_Int i2); _ }) when Z.leq Z.zero i2
    ->
      let ir1 = to_ir env e1
      and f2 = Z.to_int i2 |> Z.shift_left Z.one |> Q.of_bigint in
      IR.map (Polynomial.scale f2) ir1
  | E_Binop (op, { desc = E_Literal l1; _ }, { desc = E_Literal l2; _ }) ->
      Operations.binop_values e Error.Static op l1 l2 |> of_lit
  | E_Unop (NEG, e0) -> IR.map Polynomial.neg (to_ir env e0)
  | E_Cond (cond, e1, e2) ->
      let cjs, neg_cjs = to_cond env cond
      and ir1 = to_ir env e1
      and ir2 = to_ir env e2 in
      let ir1' = IR.restrict cjs ir1 and ir2' = IR.restrict neg_cjs ir2 in
      IR.combine ir1' ir2'
  | E_ATC (e', _) -> to_ir env e'
  | _ -> raise NotSupported
(* End *)

and to_cond env (e : expr) : Conjunction.t list * Conjunction.t list =
  let ( ||| ) = ( @ ) and ( &&& ) = ASTUtils.list_cross Conjunction.conj in
  match e.desc with
  | E_Literal (L_Bool b) ->
      ([ Conjunction.of_bool b ], [ Conjunction.of_bool (not b) ])
  | E_Binop (BAND, e1, e2) ->
      let cjs1, neg_cjs1 = to_cond env e1 and cjs2, neg_cjs2 = to_cond env e2 in
      (cjs1 &&& cjs2, neg_cjs1 ||| neg_cjs2)
  | E_Binop (BOR, e1, e2) ->
      let cjs1, neg_cjs1 = to_cond env e1 and cjs2, neg_cjs2 = to_cond env e2 in
      (cjs1 ||| cjs2, neg_cjs1 &&& neg_cjs2)
  | E_Binop (EQ_OP, e1, e2) ->
      let e' = E_Binop (MINUS, e1, e2) |> ASTUtils.add_pos_from_st e in
      let ir = to_ir env e' in
      (IR.to_conjuncts Zero ir, IR.to_conjuncts NonZero ir)
  | E_Cond (cond, e1, e2) ->
      let cjs_cond, neg_cjs_cond = to_cond env cond
      and cjs1, neg_cjs1 = to_cond env e1
      and cjs2, neg_cjs2 = to_cond env e2 in
      ( cjs_cond &&& cjs1 ||| (neg_cjs_cond &&& cjs2),
        neg_cjs_cond ||| neg_cjs1 &&& (cjs_cond ||| neg_cjs2) )
  | _ -> raise NotSupported

(*---------- Solving in the symbolic representation  ----------*)

(* Begin Normalize *)
let normalize env e =
  e |> to_ir env |> IR.reduce |> IR.to_expr |> with_pos_from e
(* End *)

let try_normalize env e =
  try normalize env e with Error.ASLException _ | NotSupported -> e

let equal_in_env env e1 e2 =
  let dbg = false in
  let () =
    if dbg then
      Format.eprintf "@[<hv 2>Are %a@ and %a@ equal?@]@ " PP.pp_expr e1
        PP.pp_expr e2
  in
  try
    let ir1 = to_ir env e1 |> IR.reduce and ir2 = to_ir env e2 |> IR.reduce in
    let () =
      if dbg then
        Format.eprintf "@[Reducing them to@ %a@ and %a.@]@ " IR.pp ir1 IR.pp ir2
    in
    let res = IR.equal_mod_branches ir1 ir2 in
    let () =
      if dbg then if res then Format.eprintf "YES@." else Format.eprintf "NO@."
    in
    res
  with NotSupported ->
    let () = if dbg then Format.eprintf "Cannot answer this question yet." in
    false

(* Begin ReduceToZOpt *)
let reduce_to_z_opt env e =
  match (try_normalize env e).desc with
  | E_Literal (L_Int z) -> Some z
  | _ -> None
(* End *)
