open AST
open ASTUtils

let exact e = Constraint_Exact e
let range a b = Constraint_Range (a, b)

let constraint_plus c1 c2 =
  let plus = binop PLUS in
  match (c1, c2) with
  | Constraint_Exact a, Constraint_Exact c -> exact (plus a c)
  | Constraint_Exact a, Constraint_Range (c, d) -> range (plus a c) (plus a d)
  | Constraint_Range (a, b), Constraint_Exact c -> range (plus a c) (plus b c)
  | Constraint_Range (a, b), Constraint_Range (c, d) ->
      range (plus a c) (plus b d)

let constraint_minus c1 c2 =
  let minus = binop MINUS in
  match (c1, c2) with
  | Constraint_Exact a, Constraint_Exact c -> exact (minus a c)
  | Constraint_Exact a, Constraint_Range (c, d) -> range (minus a d) (minus a c)
  | Constraint_Range (a, b), Constraint_Exact c -> range (minus a c) (minus b c)
  | Constraint_Range (a, b), Constraint_Range (c, d) ->
      range (minus a d) (minus b c)

let constraint_mod = function
  | Constraint_Exact e | Constraint_Range (_, e) -> range zero_expr e

let constraint_divisions op c1 c2 =
  let div = binop op in
  match (c1, c2) with
  | Constraint_Exact a, Constraint_Exact c -> [ exact (div a c) ]
  | Constraint_Range (a, b), Constraint_Exact c -> [ range (div a c) (div b c) ]
  | Constraint_Exact a, Constraint_Range (_c, d) ->
      (* {a} DIV {b..c} == {a .. (a DIV c), (a DIV c) .. a} *)
      [ range a (div a d); range (div a d) a ]
  | Constraint_Range (a, b), Constraint_Range (_c, _d) ->
      (* {a..b} DIV {c..d} == {a..-1, 0, 1..b} *)
      [ range a minus_one_expr; exact zero_expr; range one_expr b ]

let constraint_mult c1 c2 =
  let mul = binop MUL in
  match (c1, c2) with
  | Constraint_Exact a, Constraint_Exact c -> [ exact (mul a c) ]
  | Constraint_Range (a, b), Constraint_Exact c ->
      (* {a..b} MUL c is {(a MUL c) .. (b MUL c), (b MUL c) .. (a MUL c)} *)
      [ range (mul a c) (mul b c); range (mul b c) (mul a c) ]
  | Constraint_Exact a, Constraint_Range (c, d) ->
      (* a MUL {c..d} is {(a MUL c) .. (a MUL d), (a MUL d) .. (a MUL c)} *)
      [ range (mul a c) (mul a d); range (mul a d) (mul a c) ]
  | Constraint_Range (a, b), Constraint_Range (c, d) ->
      let ac = mul a c and bd = mul b d and ad = mul a d and bc = mul b c in
      (* {a..b} MUL {c..d} is { *)
      [
        (* (a MUL c) .. (b MUL d), *)
        range ac bd;
        (* (a MUL d) .. (b MUL c), *)
        range ad bc;
        (* (a MUL d) .. (b MUL d), *)
        range ad bd;
        (* (a MUL d) .. (a MUL c), *)
        range ad ac;
        (* (b MUL c) .. (a MUL d), *)
        range bc ad;
        (* (b MUL c) .. (b MUL d), *)
        range bc bd;
        (* (b MUL c) .. (a MUL c), *)
        range bc ac;
        (* (b MUL d) .. (a MUL c) *)
        range bd ac;
        (* } *)
      ]

let constraint_shr c1 c2 =
  let shr = binop SHR in
  match (c1, c2) with
  | Constraint_Exact a, Constraint_Exact c -> [ exact (shr a c) ]
  | Constraint_Range (a, b), Constraint_Exact c ->
      (* {a..b} SHR c is {(a SHR c) .. (b SHR c)} *)
      [ range (shr a c) (shr b c) ]
  | Constraint_Exact a, Constraint_Range (_c, d) ->
      (* a SHR {c..d} is {a .. (a SHR d), (a SHR d) .. a}. *)
      [ range a (shr a d); range (shr a d) a ]
  | Constraint_Range (a, b), Constraint_Range (_c, _d) ->
      (* {a..b} SHR {c..d} is {a..0, 0..b} *)
      [ range a zero_expr; range zero_expr b ]

let constraint_shl c1 c2 =
  let shl = binop SHL in
  match (c1, c2) with
  | Constraint_Exact a, Constraint_Exact c -> [ exact (shl a c) ]
  | Constraint_Range (a, b), Constraint_Exact c ->
      (* {a..b} SHL c is {(a SHL c) .. (b SHL c)}. *)
      [ range (shl a c) (shl b c) ]
  | Constraint_Exact a, Constraint_Range (c, d) ->
      (* a SHL {c...d} is {
         (a SHL c) .. (a SHL d), a..(a SHL d),
         (a SHL d) .. (a SHL c), (a SHL d) ..a} *)
      let ac = shl a c and ad = shl a d in
      [ range ac ad; range a ad; range ad ac; range ad a ]
  | Constraint_Range (a, b), Constraint_Range (c, d) ->
      (* {a..b} SHL {c..d} is {
           (a SHL c) .. (b SHL d), a .. (b SHL d),
            (a SHL d) .. (b SHL d),
            (a SHL d) .. (b SHL c), (a SHL d) ..b,
         } *)
      let ac = shl a c and bd = shl b d and ad = shl a d and bc = shl b c in
      [ range ac bd; range a bd; range ad bd; range ad bc; range ad b ]

let constraint_pow c1 c2 =
  let pow = binop POW and neg = unop NEG in
  match (c1, c2) with
  | Constraint_Exact a, Constraint_Exact c -> [ exact (pow a c) ]
  | Constraint_Range (a, b), Constraint_Exact c ->
      (* {a..b} POW c is {0.. (b POW c), (- ((-a) POW c)) ..
         ((-a) POW c)} *)
      let mac = pow (neg a) c in
      [ range zero_expr (pow b c); range (neg mac) mac; exact one_expr ]
  | Constraint_Exact a, Constraint_Range (c, d) ->
      (* a POW {c..d} is {(a POW c) .. (a POW d), 1.. (a POW d), (-((-a) POW d)) .. ((-a) POW d)} *)
      let mad = pow (neg a) d and ad = pow a d in
      [
        range (pow a c) ad;
        range one_expr ad;
        range (neg mad) mad;
        exact one_expr;
      ]
  | Constraint_Range (a, b), Constraint_Range (_c, d) ->
      (* {a..b} POW {c..d} is {0.. (b POW d), (- ((-a) POW d)) .. ((-a) POW d)} *)
      let mad = pow (neg a) d in
      [ range zero_expr (pow b d); range (neg mad) mad; exact one_expr ]

(* Begin ConstraintBinop *)
let constraint_binop op cs1 cs2 =
  match op with
  | PLUS -> list_cross constraint_plus cs1 cs2
  | MINUS -> list_cross constraint_minus cs1 cs2
  | DIV | DIVRM -> list_flat_cross (constraint_divisions op) cs1 cs2
  | MUL -> list_flat_cross constraint_mult cs1 cs2
  | SHR -> list_flat_cross constraint_shr cs1 cs2
  | MOD -> List.map constraint_mod cs2
  | SHL -> list_flat_cross constraint_shl cs1 cs2
  | POW -> list_flat_cross constraint_pow cs1 cs2
  | AND | BAND | BEQ | BOR | EOR | EQ_OP | GT | GEQ | IMPL | LT | LEQ | NEQ | OR
  | RDIV ->
      assert false
(* End *)

(* Begin FilterReduceConstraintDiv *)
let filter_reduce_constraint_div =
  let get_literal_div_opt e =
    match e.desc with
    | E_Binop (DIV, a, b) -> (
        match (a.desc, b.desc) with
        | E_Literal (L_Int z1), E_Literal (L_Int z2) -> Some (z1, z2)
        | _ -> None)
    | _ -> None
  in
  function
  | Constraint_Exact e as c -> (
      match get_literal_div_opt e with
      | Some (z1, z2) when Z.sign z2 > 0 ->
          if Z.divisible z1 z2 then Some c else None
      | _ -> Some c)
  | Constraint_Range (e1, e2) as c -> (
      let z1_opt =
        match get_literal_div_opt e1 with
        | Some (z1, z2) when Z.sign z2 > 0 ->
            let zdiv, zmod = Z.ediv_rem z1 z2 in
            let zres = if Z.sign zmod = 0 then zdiv else Z.succ zdiv in
            Some zres
        | _ -> None
      and z2_opt =
        match get_literal_div_opt e2 with
        | Some (z1, z2) when Z.sign z2 > 0 -> Some (Z.ediv z1 z2)
        | _ -> None
      in
      match (z1_opt, z2_opt) with
      | Some z1, Some z2 ->
          let () =
            if false then
              Format.eprintf "Reducing %a DIV %a@ got z1=%a and z2=%a@."
                PP.pp_expr e1 PP.pp_expr e2 Z.pp_print z1 Z.pp_print z2
          in
          if Z.equal z1 z2 then Some (exact (expr_of_z z1))
          else if Z.leq z1 z2 then Some (range (expr_of_z z1) (expr_of_z z2))
          else None
      | Some z1, None -> Some (range (expr_of_z z1) e2)
      | None, Some z2 -> Some (range e1 (expr_of_z z2))
      | None, None -> Some c)
(* End *)

type strictness = [ `Silence | `Warn | `TypeCheck ]

module type CONFIG = sig
  val fail : unit -> 'a
  val check : strictness
  val output_format : Error.output_format
end

module Make (C : CONFIG) = struct
  module EP = Error.ErrorPrinter (C)

  let list_filter_map_modified f =
    let rec aux (accu, flag) = function
      | [] -> (List.rev accu, flag)
      | x :: l -> (
          match f x with
          | None -> aux (accu, true) l
          | Some v -> aux (v :: accu, v <> x || flag) l)
    in
    aux ([], false)

  (* Begin RefineConstraintBySign *)
  let refine_constraint_by_sign env sign_predicate = function
    | Constraint_Exact e as c -> (
        match StaticModel.reduce_to_z_opt env e with
        | Some z when sign_predicate (Z.sign z) -> Some c
        | Some _ -> None
        | None -> Some c)
    | Constraint_Range (e1, e2) as c -> (
        match
          ( StaticModel.reduce_to_z_opt env e1,
            StaticModel.reduce_to_z_opt env e2 )
        with
        | Some z1, Some z2 -> (
            match (sign_predicate (Z.sign z1), sign_predicate (Z.sign z2)) with
            | true, true -> Some c
            | false, true ->
                Some
                  (range (if sign_predicate 0 then zero_expr else one_expr) e2)
            | true, false ->
                Some
                  (range e1
                     (if sign_predicate 0 then zero_expr else minus_one_expr))
            | false, false -> None)
        | None, Some z2 ->
            Some
              (if sign_predicate (Z.sign z2) then c
               else
                 range e1
                   (if sign_predicate 0 then zero_expr else minus_one_expr))
        | Some z1, None ->
            Some
              (if sign_predicate (Z.sign z1) then c
               else range (if sign_predicate 0 then zero_expr else one_expr) e2)
        | None, None -> Some c)
  (* End *)

  (* Begin RefineConstraints *)
  let refine_constraints ~loc op filter constraints =
    let pp_constraints f cs =
      Format.fprintf f "@[<h>{%a}@]" PP.pp_int_constraints cs
    in
    let constraints', modified = list_filter_map_modified filter constraints in
    match constraints' with
    | [] ->
        let () =
          Format.eprintf
            "@[%a:@ All@ values@ in@ constraints@ %a@ would@ fail@ with@ op \
             %s,@ operation@ will@ always@ fail.@]@."
            PP.pp_pos loc pp_constraints constraints
            PP.(binop_to_string op)
        in
        C.fail ()
    | _ ->
        let () =
          if modified then
            EP.warn_from ~loc
              Error.(
                RemovingValuesFromConstraints
                  { op; prev = constraints; after = constraints' })
          else if false then
            Format.eprintf "Unmodified for op %s: %a = %a@."
              PP.(binop_to_string op)
              pp_constraints constraints pp_constraints constraints'
        in
        constraints'
  (* End *)

  let filter_sign ~loc env op sign_predicate constraints =
    refine_constraints ~loc op
      (refine_constraint_by_sign env sign_predicate)
      constraints

  (* Begin BinopFilterRight *)

  (** Filters out values from the right-hand-side operand of [op] that will definitely
  result in a dynamic error. *)
  let binop_filter_rhs ~loc env op =
    match op with
    | SHL | SHR | POW -> filter_sign ~loc env op @@ fun x -> x >= 0
    | MOD | DIV | DIVRM -> filter_sign ~loc env op @@ fun x -> x > 0
    | MINUS | MUL | PLUS -> Fun.id
    | AND | BAND | BEQ | BOR | EOR | EQ_OP | GT | GEQ | IMPL | LT | LEQ | NEQ
    | OR | RDIV ->
        assert false
  (* End *)

  let refine_constraint_for_div ~loc op cs =
    match op with
    | DIV -> (
        let res = List.filter_map filter_reduce_constraint_div cs in
        match res with
        | [] ->
            let () =
              Format.eprintf
                "@[%a:@ Division@ will@ result@ in@ empty@ constraint@ set,@ \
                 so@ will@ always@ fail.@]@."
                PP.pp_pos loc
            in
            C.fail ()
        | _ -> res)
    | _ -> cs

  (* Begin ReduceConstraint *)
  let reduce_constraint env = function
    | Constraint_Exact e -> Constraint_Exact (StaticModel.try_normalize env e)
    | Constraint_Range (e1, e2) ->
        Constraint_Range
          (StaticModel.try_normalize env e1, StaticModel.try_normalize env e2)
  (* End *)

  let list_remove_duplicates eq =
    let rec aux prev acc = function
      | [] -> List.rev acc
      | x :: li -> if eq prev x then aux prev acc li else aux x (x :: acc) li
    in
    function [] -> [] | x :: li -> aux x [ x ] li

  let simplify_static_constraints =
    let module DZ = Diet.Z in
    let acc_diet (diet, non_static) = function
      | Constraint_Exact e as c -> (
          match e.desc with
          | E_Literal (L_Int z) ->
              (DZ.(add Interval.(make z z) diet), non_static)
          | _ -> (diet, c :: non_static))
      | Constraint_Range (e1, e2) as c -> (
          match (e1.desc, e2.desc) with
          | E_Literal (L_Int z1), E_Literal (L_Int z2) when Z.leq z1 z2 ->
              DZ.(add Interval.(make z1 z2) diet, non_static)
          | _ -> (diet, c :: non_static))
    in
    let constraint_of_interval interval =
      let x = DZ.Interval.x interval and y = DZ.Interval.y interval in
      if Z.equal x y then Constraint_Exact (expr_of_z x)
      else Constraint_Range (expr_of_z x, expr_of_z y)
    in
    fun constraints ->
      let diet, non_static =
        List.fold_left acc_diet (DZ.empty, []) constraints
      in
      DZ.fold
        (fun interval acc -> constraint_of_interval interval :: acc)
        diet non_static

  (* Begin ReduceConstraints *)
  let reduce_constraints env constraints =
    List.map (reduce_constraint env) constraints
    |> simplify_static_constraints |> List.sort compare
    |> list_remove_duplicates (constraint_equal (StaticModel.equal_in_env env))
  (* End *)

  (** [binop_is_exploding op] returns [true] if [constraint_binop op] looses
      precision on intervals. *)
  let binop_is_exploding = function
    | PLUS | MINUS -> false
    | MUL | SHL | POW | DIV | DIVRM | MOD | SHR -> true
    | AND | BAND | BEQ | BOR | EOR | EQ_OP | GT | GEQ | IMPL | LT | LEQ | NEQ
    | OR | RDIV ->
        assert false

  (* Begin ExplodeIntervals *)
  let explode_intervals =
    let rec make_interval ~loc acc a b =
      if Z.leq a b then
        let eb = E_Literal (L_Int b) |> add_pos_from loc in
        let acc' = Constraint_Exact eb :: acc in
        make_interval ~loc acc' a (Z.pred b)
      else acc
    in
    let[@warning "-44"] interval_too_large z1 z2 =
      let open Z in
      let max_interval_size = ~$1 lsl 14 in
      Compare.(abs (z1 - z2) > max_interval_size)
    in
    let explode_constraint ~loc env = function
      | Constraint_Exact _ as c -> [ c ]
      | Constraint_Range (a, b) as c -> (
          match
            ( StaticModel.reduce_to_z_opt env a,
              StaticModel.reduce_to_z_opt env b )
          with
          | Some za, Some zb ->
              if interval_too_large za zb then
                let () =
                  EP.warn_from ~loc Error.(IntervalTooBigToBeExploded (za, zb))
                in
                [ c ]
              else make_interval [] ~loc za zb
          | _ -> [ c ])
    in
    fun ~loc env -> list_concat_map (explode_constraint ~loc env)
  (* End *)

  (* Begin AnnotateConstraintBinop *)
  let annotate_constraint_binop ~loc env op cs1 cs2 =
    match op with
    | SHL | SHR | POW | MOD | DIVRM | MINUS | MUL | PLUS | DIV ->
        let cs2_f = binop_filter_rhs ~loc env op cs2 in
        let () =
          if false then
            Format.eprintf
              "Reduction of binop %s@ on@ constraints@ %a@ and@ %a@."
              (PP.binop_to_string op) PP.pp_int_constraints cs1
              PP.pp_int_constraints cs2
        in
        let cs1_arg, cs2_arg =
          if binop_is_exploding op then
            (explode_intervals ~loc env cs1, explode_intervals ~loc env cs2_f)
          else (cs1, cs2_f)
        in
        let annotated_cs =
          constraint_binop op cs1_arg cs2_arg
          |> refine_constraint_for_div ~loc op
          |> reduce_constraints env
        in
        let () =
          if false then
            Format.eprintf
              "Reduction of binop %s@ on@ constraints@ %a@ and@ %a@ gave@ %a@."
              (PP.binop_to_string op) PP.pp_int_constraints cs1_arg
              PP.pp_int_constraints cs2_arg PP.pp_int_constraints annotated_cs
        in
        annotated_cs
    | AND | BAND | BEQ | BOR | EOR | EQ_OP | GT | GEQ | IMPL | LT | LEQ | NEQ
    | OR | RDIV ->
        assert false
  (* End *)
end
