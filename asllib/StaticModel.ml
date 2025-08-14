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

  val equal : t -> t -> bool
  val of_int : Z.t -> t
  val of_var : string -> t
  val to_mono : t -> (Monomial.t * Q.t) option
  val scale : Q.t -> t -> t
  val neg : t -> t
  val add : t -> t -> t
  val mult : t -> t -> t
  val divide_by_term : t -> Q.t -> Monomial.t -> t
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

  let equal = MonomialMap.equal Q.equal

  let single_term mono factor =
    if Q.equal factor Q.zero then MonomialMap.empty
    else MonomialMap.singleton mono factor

  let of_int z = single_term Monomial.one (Q.of_bigint z)
  let of_var s = single_term (Monomial.single_term s) Q.one

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

(*---------- Converting expressions to symbolic representation ----------*)

let rec make_anonymous (env : StaticEnv.env) (ty : ty) : ty =
  match ty.desc with
  | T_Named x -> (
      match IMap.find_opt x env.global.declared_types with
      | Some (ty', _) -> make_anonymous env ty'
      | None -> fatal_from ty (Error.UndefinedIdentifier (Static, x)))
  | _ -> ty

let of_lit = function L_Int i -> Polynomial.of_int i | _ -> raise NotSupported

(* Begin ToIR *)
let rec to_ir env (e : expr) =
  match e.desc with
  | E_Literal l -> of_lit l
  | E_Var s -> (
      try StaticEnv.lookup_constant env s |> of_lit
      with Not_found -> (
        try StaticEnv.lookup_immutable_expr env s |> to_ir env
        with Not_found | NotSupported -> (
          let t =
            try StaticEnv.type_of env s
            with Not_found ->
              Error.fatal_from e (UndefinedIdentifier (Static, s))
          in
          let ty1 = make_anonymous env t in
          match ty1.desc with
          | T_Int (WellConstrained ([ Constraint_Exact e ], _)) -> to_ir env e
          | T_Int _ -> Polynomial.of_var s
          | _ -> raise NotSupported)))
  | E_Binop (`ADD, e1, e2) ->
      let ir1 = to_ir env e1 and ir2 = to_ir env e2 in
      Polynomial.add ir1 ir2
  | E_Binop (`SUB, e1, e2) ->
      let e2 = E_Unop (NEG, e2) |> ASTUtils.add_pos_from_st e2 in
      E_Binop (`ADD, e1, e2) |> ASTUtils.add_pos_from_st e |> to_ir env
  | E_Binop (`MUL, { desc = E_Binop (`DIV, e1, e2); _ }, e3) ->
      to_ir env (binop `DIV (binop `MUL e1 e3) e2)
  | E_Binop (`MUL, e1, { desc = E_Binop (`DIV, e2, e3); _ }) ->
      to_ir env (binop `DIV (binop `MUL e1 e2) e3)
  | E_Binop (`MUL, e1, e2) ->
      let ir1 = to_ir env e1 and ir2 = to_ir env e2 in
      Polynomial.mult ir1 ir2
  | E_Binop (`DIV, e1, { desc = E_Literal (L_Int i2); _ }) ->
      let ir1 = to_ir env e1 and f2 = Q.(Z.one /// i2) in
      (Polynomial.scale f2) ir1
  | E_Binop (`DIV, e1, e2) -> (
      let ir1 = to_ir env e1 and ir2 = to_ir env e2 in
      match Polynomial.to_mono ir2 with
      | Some (mono, factor) -> Polynomial.divide_by_term ir1 factor mono
      | None -> raise NotSupported)
  | E_Binop (`SHL, e1, { desc = E_Literal (L_Int i2); _ }) when Z.leq Z.zero i2
    ->
      let ir1 = to_ir env e1
      and f2 = Z.to_int i2 |> Z.shift_left Z.one |> Q.of_bigint in
      Polynomial.scale f2 ir1
  | E_Binop (op, { desc = E_Literal l1; _ }, { desc = E_Literal l2; _ }) ->
      Operations.binop_values e Error.Static op l1 l2 |> of_lit
  | E_Unop (NEG, e0) -> Polynomial.neg (to_ir env e0)
  | E_ATC (e', _) -> to_ir env e'
  | _ -> raise NotSupported
(* End *)

(*---------- Solving in the symbolic representation  ----------*)

(* Begin Normalize *)
let normalize env e =
  let { desc } = e |> to_ir env |> Polynomial.to_expr in
  add_pos_from e desc
(* End *)

let try_normalize env e =
  try normalize env e with Error.ASLException _ | NotSupported -> e

let normalize_opt env e =
  try Some (normalize env e) with Error.ASLException _ | NotSupported -> None

let equal_in_env env e1 e2 =
  let dbg = false in
  let () =
    if dbg then
      Format.eprintf "@[<hv 2>Are %a@ and %a@ equal?@]@ " PP.pp_expr e1
        PP.pp_expr e2
  in
  try
    let ir1 = to_ir env e1 and ir2 = to_ir env e2 in
    let () =
      if dbg then
        Format.eprintf "@[<hv 2>i.e. are %a@ and %a@ equal?@]@ " Polynomial.pp
          ir1 Polynomial.pp ir2
    in
    let res = Polynomial.equal ir1 ir2 in
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
