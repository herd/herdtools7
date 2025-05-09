open AST
open ASTUtils
module TypingRule = Instrumentation.TypingRule

let ( |: ) = Instrumentation.TypingNoInstr.use_with
let exact e = Constraint_Exact e
let range a b = Constraint_Range (a, b)

type int3_binop =
  [ `PLUS | `MINUS | `DIV | `DIVRM | `SHR | `SHL | `POW | `MOD | `MUL ]

type extremities_binops = [ `PLUS | `MINUS | `DIV | `DIVRM | `SHR | `SHL | `MUL ]

(* Begin ConstraintMod *)
let constraint_mod = function
  | Constraint_Exact e | Constraint_Range (_, e) ->
      range zero_expr (binop `MINUS e one_expr) |: TypingRule.ConstraintMod
(* End *)

(* Begin PossibleExtremitiesLeft *)

(** [possible_extremities_left op a b] is given a range [a..b] the set of
    needed extremities of intervals for the left-hand-side of an operation
    [op]. *)
let possible_extremities_left (op : extremities_binops) a b =
  match op with
  (* MUL is not left-increasing: if c is negative, then the following is not
     true: if x < y then x op c < y op c This is why we have to add the
     reversed intervals. We also have to add the intervals (a, a) and (b, b)
     for the case where a < 0 < b and c < 0 < d. *)
  | `MUL ->
      [ (a, a); (a, b); (b, a); (b, b) ] |: TypingRule.PossibleExtremitiesLeft
  (* All the following operations are left-increasing:
     for any operation op among those, if x < y, and c a valid value for the
     right-hand side of op, x op c < y op c *)
  | `DIV | `DIVRM | `SHR | `SHL | `PLUS | `MINUS -> [ (a, b) ]
(* End *)

(* Begin PossibleExtremitiesRIght *)

(** [possible_extremities_right op a b] is given a range [a..b] the set of
    needed extremities of intervals for the right-hand-side of an operation
    [op]. *)
let possible_extremities_right (op : extremities_binops) c d =
  match op with
  (* PLUS is right-increasing. *)
  | `PLUS -> [ (c, d) ] |: TypingRule.PossibleExtremitiesRight
  (* MINUS simply reverse the intervals. *)
  | `MINUS -> [ (d, c) ]
  (* We need:
      - the normal interval if the left-hand-side value is positive
      - the reversed interval if the right-hand-side value is negative
      - the singletons at bounds for the case where a < 0 < b and c < 0 < d. *)
  | `MUL -> [ (c, c); (c, d); (d, c); (d, d) ]
  (* For SHR and SHL, we can replace [c] by [0] because this handle the case where c is
     negative, that will need to be treated anyway. Then if the right-hand side
     is negative, we need to reverse the intervals. *)
  | `SHL | `SHR -> [ (d, zero_expr); (zero_expr, d) ]
  (* Same as SHR/SHL, but with [1] for divisions. *)
  | `DIV | `DIVRM -> [ (one_expr, d); (d, one_expr) ]
(* End *)

(* Begin ApplyBinopExtremities *)

(** [apply_binop_extremities op c1 c2] applies [op] to the slices [c1] and [c2].

    It produces a list of all possible slices, by using the functions
    [possible_extremities_left/right], and taking the cartesian product of
    their results. *)
let apply_binop_extremities (op : extremities_binops) c1 c2 =
  let op' = (op :> binop) in
  match (c1, c2) with
  | Constraint_Exact a, Constraint_Exact c ->
      [ exact (binop op' a c) ] |: TypingRule.ApplyBinopExtremities
  | Constraint_Range (a, b), Constraint_Exact c ->
      List.map
        (fun (a', b') -> range (binop op' a' c) (binop op' b' c))
        (possible_extremities_left op a b)
  | Constraint_Exact a, Constraint_Range (c, d) ->
      List.map
        (fun (c', d') -> range (binop op' a c') (binop op' a d'))
        (possible_extremities_right op c d)
  | Constraint_Range (a, b), Constraint_Range (c, d) ->
      list_cross
        (fun (a', b') (c', d') -> range (binop op' a' c') (binop op' b' d'))
        (possible_extremities_left op a b)
        (possible_extremities_right op c d)
(* End *)

(* Begin ConstraintPow *)

(** [constraint_pow c1 c2] applies [POW] to [c1] and [c2]. *)
let constraint_pow c1 c2 =
  let pow = binop `POW and neg = unop NEG in
  match (c1, c2) with
  | Constraint_Exact a, Constraint_Exact c ->
      [ exact (pow a c) ] |: TypingRule.ConstraintPow
  | Constraint_Range (a, b), Constraint_Exact c ->
      (* We need:
         - the case a positive is included in the case a negative.
         - 0..b POW c for the positive values
         - (- ((-a) POW c)) .. ((-a) POW c) for the negative values
         - the case 0 POW 0 can only happen if c = 0, and then this is included in 0..b POW c
      *)
      let mac = pow (neg a) c in
      [ range zero_expr (pow b c); range (neg mac) mac ]
  | Constraint_Exact a, Constraint_Range (_c, d) ->
      (* We need here:
         - 1 for 0 POW 0 that can be included everywhere and is the only time that POW is very unpredictable
         - (- ((-a) POW d)) .. ((-a) POW d) for the negative values
         - 0 .. ad for the positive values
      *)
      let mad = pow (neg a) d and ad = pow a d in
      [ range zero_expr ad; range (neg mad) mad; exact one_expr ]
  | Constraint_Range (a, b), Constraint_Range (_c, d) ->
      (* We need here:
         - 1 for 0 POW 0 that can be included everywhere and is the only time that POW is very unpredictable
         - (- ((-a) POW d)) .. ((-a) POW d) for the negative values
         - 0 .. bd for the positive values
      *)
      let mad = pow (neg a) d in
      [ range zero_expr (pow b d); range (neg mad) mad; exact one_expr ]
(* End *)

(* Begin ConstraintBinop *)
let constraint_binop (op : int3_binop) cs1 cs2 =
  match op with
  | #extremities_binops as op ->
      list_flat_cross (apply_binop_extremities op) cs1 cs2
      |: TypingRule.ConstraintBinop
  | `MOD -> List.map constraint_mod cs2
  | `POW -> list_flat_cross constraint_pow cs1 cs2
(* End *)

(* Begin FilterReduceConstraintDiv *)
let filter_reduce_constraint_div =
  let get_literal_div_opt e =
    match e.desc with
    | E_Binop (`DIV, a, b) -> (
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

module type CONFIG = sig
  val fail : string -> 'a
  val warn_from : loc:'a annotated -> Error.warning_desc -> unit
end

module Make (C : CONFIG) = struct
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
  let refine_constraints ~loc (op : binop) filter constraints =
    let pp_constraints f cs =
      Format.fprintf f "@[<h>{%a}@]" PP.pp_int_constraints cs
    in
    let constraints', modified = list_filter_map_modified filter constraints in
    match constraints' with
    | [] ->
        let msg =
          Format.asprintf
            "@[%a:@ All@ values@ in@ constraints@ %a@ would@ fail@ with@ op \
             %s,@ operation@ will@ always@ fail.@]@."
            PP.pp_pos loc pp_constraints constraints
            PP.(binop_to_string op)
        in
        C.fail msg
    | _ ->
        let () =
          if modified then
            C.warn_from ~loc
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

  let filter_sign ~loc env (op : int3_binop) sign_predicate constraints =
    refine_constraints ~loc
      (op :> binop)
      (refine_constraint_by_sign env sign_predicate)
      constraints

  (* Begin BinopFilterRight *)

  (** Filters out values from the right-hand-side operand of [op] that will definitely
  result in a dynamic error. *)
  let binop_filter_rhs ~loc env (op : int3_binop) =
    match op with
    | `SHL | `SHR | `POW -> filter_sign ~loc env op @@ fun x -> x >= 0
    | `MOD | `DIV | `DIVRM -> filter_sign ~loc env op @@ fun x -> x > 0
    | `MINUS | `MUL | `PLUS -> Fun.id
  (* End *)

  (* Begin RefineConstraintForDIV *)
  let refine_constraint_for_div ~loc op cs =
    match op with
    | `DIV -> (
        let res = List.filter_map filter_reduce_constraint_div cs in
        match res with
        | [] ->
            let msg =
              Format.asprintf
                "@[%a:@ Division@ will@ result@ in@ empty@ constraint@ set,@ \
                 so@ will@ always@ fail.@]@."
                PP.pp_pos loc
            in
            C.fail msg
        | _ -> res)
    | _ -> cs
  (* End *)

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
    let to_diets (diets, non_static) = function
      | Constraint_Exact e as c -> (
          match e.desc with
          | E_Literal (L_Int z) -> (DZ.singleton z :: diets, non_static)
          | _ -> (diets, c :: non_static))
      | Constraint_Range (e1, e2) as c -> (
          match (e1.desc, e2.desc) with
          | E_Literal (L_Int z1), E_Literal (L_Int z2) when Z.leq z1 z2 ->
              DZ.(add Interval.(make z1 z2) empty :: diets, non_static)
          | _ -> (diets, c :: non_static))
    in
    let constraint_of_interval interval =
      let x = DZ.Interval.x interval and y = DZ.Interval.y interval in
      if Z.equal x y then Constraint_Exact (expr_of_z x)
      else Constraint_Range (expr_of_z x, expr_of_z y)
    in
    fun constraints ->
      let diets, non_static = List.fold_left to_diets ([], []) constraints in
      let diet = DZ.unions diets in
      DZ.fold
        (fun interval acc -> constraint_of_interval interval :: acc)
        diet non_static

  (* Begin ReduceConstraints *)
  let reduce_constraints env constraints =
    List.map (reduce_constraint env) constraints
    |> simplify_static_constraints |> List.sort compare
    |> list_remove_duplicates (constraint_equal (StaticModel.equal_in_env env))
  (* End *)

  (** [binop_is_exploding op] returns [true] if [constraint_binop op] loses
      precision on intervals. *)
  let binop_is_exploding : int3_binop -> bool = function
    | `PLUS | `MINUS -> false
    | `MUL | `SHL | `POW | `DIV | `DIVRM | `MOD | `SHR -> true

  let log_max_constraint_size = 17
  let max_constraint_size = Z.shift_left Z.one log_max_constraint_size
  let max_exploded_interval_size = Z.shift_left Z.one 14

  (* Begin ExplodeIntervals *)
  let explode_intervals =
    let rec make_interval ~loc acc a b =
      if Z.leq a b then
        let eb = E_Literal (L_Int b) |> add_pos_from loc in
        let acc' = Constraint_Exact eb :: acc in
        make_interval ~loc acc' a (Z.pred b)
      else acc
    in
    let interval_too_large z1 z2 =
      let expected_size = Z.sub z2 z1 in
      Z.lt max_exploded_interval_size expected_size
    in
    let explode_constraint ~loc env (prev_constraints, prev_prec) = function
      | Constraint_Exact _ as c -> (c :: prev_constraints, prev_prec)
      | Constraint_Range (a, b) as c -> (
          match
            ( StaticModel.reduce_to_z_opt env a,
              StaticModel.reduce_to_z_opt env b )
          with
          | Some za, Some zb ->
              if interval_too_large za zb then
                let warn () =
                  C.warn_from ~loc Error.(IntervalTooBigToBeExploded (za, zb))
                in
                let prec = register_precision_loss prev_prec warn in
                (c :: prev_constraints, prec)
              else (make_interval [] ~loc za zb, prev_prec)
          | _ -> (c :: prev_constraints, prev_prec))
    in
    fun ~loc env cs ->
      List.fold_left (explode_constraint ~loc env) ([], Precision_Full) cs
  (* End *)

  (* Begin AnnotateConstraintBinop *)
  let annotate_constraint_binop ~loc env (op : int3_binop) cs1 cs2 =
    let cs2_f = binop_filter_rhs ~loc env op cs2 in
    let () =
      if false then
        Format.eprintf "Reduction of binop %s@ on@ constraints@ %a@ and@ %a@."
          (PP.binop_to_string (op :> binop))
          PP.pp_int_constraints cs1 PP.pp_int_constraints cs2
    in
    let cs1_arg, cs2_arg, precision_loss =
      if binop_is_exploding op then
        let ex_cs1, p1 = explode_intervals ~loc env cs1
        and ex_cs2, p2 = explode_intervals ~loc env cs2_f in
        let l1 = List.length ex_cs1 and l2 = List.length ex_cs2 in
        let p0 = precision_join p1 p2 in
        let expected_constraint_length =
          Z.(if op = `MOD then ~$l2 else mul ~$l1 ~$l2)
        in
        if Z.leq expected_constraint_length max_constraint_size then
          (ex_cs1, ex_cs2, p0)
        else
          let warn () =
            C.warn_from ~loc
              Error.(
                ConstraintSetPairToBigToBeExploded
                  {
                    op :> binop;
                    left = cs1;
                    right = cs2_f;
                    log_max = log_max_constraint_size;
                  })
          in
          let p = register_precision_loss p0 warn in
          (cs1, cs2_f, p)
      else (cs1, cs2_f, Precision_Full)
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
          (PP.binop_to_string (op :> binop))
          PP.pp_int_constraints cs1_arg PP.pp_int_constraints cs2_arg
          PP.pp_int_constraints annotated_cs
    in
    (annotated_cs, precision_loss) |: TypingRule.AnnotateConstraintBinop
  (* End *)
end
