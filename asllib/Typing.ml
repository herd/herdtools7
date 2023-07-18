open AST
open ASTUtils
open StaticEnv

let fatal_from = Error.fatal_from
let not_yet_implemented pos s = fatal_from pos (Error.NotYetImplemented s)
let undefined_identifier pos x = fatal_from pos (Error.UndefinedIdentifier x)

let conflict pos expected provided =
  fatal_from pos (Error.ConflictingTypes (expected, provided))

(* Control Warning outputs. *)
let _warn = false

type strictness = [ `Silence | `Warn | `TypeCheck ]

let rec resolve_root_name (env : env) (ty : ty) : ty =
  match ty.desc with
  | T_Named x -> (
      match IMap.find_opt x env.global.declared_types with
      | Some ty -> resolve_root_name env ty
      | None -> undefined_identifier ty x)
  | _ -> ty

(* -------------------------------------------------------------------------

                        Functionnal polymorphism

   ------------------------------------------------------------------------- *)

module FunctionRenaming = struct
  (* Returns true iff type lists type-clash element-wise. *)
  let has_arg_clash env caller callee =
    List.compare_lengths caller callee == 0
    && List.for_all2
         (fun t_caller (_, t_callee) ->
           Types.type_clashes env t_caller t_callee)
         caller callee

  (* Return true if two subprogram are forbidden with the same argument types. *)
  let has_subprogram_type_clash s1 s2 =
    match (s1, s2) with
    | ST_Function, _ | _, ST_Function | ST_Procedure, _ | _, ST_Procedure ->
        true
    | ST_Getter, ST_Getter | ST_Setter, ST_Setter -> true
    | ST_Getter, ST_Setter | ST_Setter, ST_Getter -> false

  (* Deduce renamings from match between calling and callee types. *)
  let deduce_eqs env =
    (* Here we assume [has_arg_clash env caller callee] *)
    (* Thus [List.length caller == List.length callee]. *)
    let folder prev_eqs caller (_name, callee) =
      match callee.desc with
      | T_Bits (BitWidth_SingleExpr { desc = E_Var x; _ }, _) -> (
          match (Types.get_structure env caller).desc with
          | T_Bits (BitWidth_SingleExpr e_caller, _) ->
              (x, e_caller) :: prev_eqs
          | T_Bits _ -> prev_eqs
          | _ ->
              (* We know that callee type_clashes with caller, and that it
                 cannot be a name. *)
              assert false)
      | _ -> prev_eqs
    in
    List.fold_left2 folder []

  let add_new_func env name arg_types subpgm_type =
    match IMap.find_opt name env.global.subprogram_renamings with
    | None ->
        let env = set_renamings name (ISet.singleton name) env in
        (env, name)
    | Some set ->
        let name' = name ^ "-" ^ string_of_int (ISet.cardinal set) in
        let clash =
          let arg_types = List.map snd arg_types in
          (not (ISet.is_empty set))
          && ISet.exists
               (fun name'' ->
                 let other_func_sig = IMap.find name'' env.global.subprograms in
                 has_subprogram_type_clash subpgm_type
                   other_func_sig.subprogram_type
                 && has_arg_clash env arg_types other_func_sig.args)
               set
        in
        let () =
          if clash && _warn then
            Format.eprintf "Function %s@[(%a)@] is declared multiple times.@."
              name
              Format.(
                pp_print_list
                  ~pp_sep:(fun f () -> fprintf f ",@ ")
                  PP.pp_typed_identifier)
              arg_types
        in
        let env = set_renamings name (ISet.add name' set) env in
        (env, name')

  let find_name_strict loc env name caller_arg_types =
    let () =
      if false then Format.eprintf "Trying to rename call to %S@." name
    in
    match IMap.find_opt name env.global.subprogram_renamings with
    | None -> (
        match IMap.find_opt name env.global.subprograms with
        | Some func_sig ->
            let callee_arg_types = func_sig.args in
            if has_arg_clash env caller_arg_types callee_arg_types then
              let () =
                if false then
                  Format.eprintf "Found already translated name: %S.@." name
              in
              ( deduce_eqs env caller_arg_types callee_arg_types,
                name,
                callee_arg_types,
                func_sig.return_type )
            else fatal_from loc (Error.NoCallCandidate (name, caller_arg_types))
        | None -> undefined_identifier loc name)
    | Some set -> (
        let finder name' acc =
          let func_sig = IMap.find name' env.global.subprograms in
          let callee_arg_types = func_sig.args in
          if has_arg_clash env caller_arg_types callee_arg_types then
            ( deduce_eqs env caller_arg_types callee_arg_types,
              name',
              callee_arg_types,
              func_sig.return_type )
            :: acc
          else acc
        in
        match ISet.fold finder set [] with
        | [] -> fatal_from loc (Error.NoCallCandidate (name, caller_arg_types))
        | [ (eqs, name', callee_arg_types, ret_type) ] ->
            (eqs, name', callee_arg_types, ret_type)
        | _ :: _ ->
            fatal_from loc
              (Error.TooManyCallCandidates (name, caller_arg_types)))

  let find_name env name caller_arg_types =
    match IMap.find_opt name env.global.subprogram_renamings with
    | None -> (
        match IMap.find_opt name env.global.subprograms with
        | Some func_sig ->
            let callee_arg_types = func_sig.args in
            if has_arg_clash env caller_arg_types callee_arg_types then
              let () =
                if false then
                  Format.eprintf "Found already translated name: %S.@." name
              in
              (deduce_eqs env caller_arg_types callee_arg_types, name)
            else ([], name)
        | None -> ([], name) (* Will trigger runtime exception *))
    | Some set -> (
        let finder name' acc =
          let func_sig = IMap.find name' env.global.subprograms in
          let callee_arg_types = func_sig.args in
          if has_arg_clash env caller_arg_types callee_arg_types then
            (deduce_eqs env caller_arg_types callee_arg_types, name') :: acc
          else acc
        in
        match ISet.fold finder set [] with
        | [] ->
            let () =
              if _warn then
                Format.eprintf
                  "@[No found function %s with the right types:@ @[%a@]@]@."
                  name
                  Format.(pp_print_list ~pp_sep:pp_print_space PP.pp_ty)
                  caller_arg_types
            in
            ([], name)
            (* Will trigger runtime exception *)
        | [ (eqs, name') ] -> (eqs, name')
        | (_, name') :: _ as li ->
            let () =
              if _warn then
                Format.eprintf
                  "Ambiguous call to %s. Many conflicting declared functions.@."
                  name
            in
            (* We select all possible equations, hoping that there are no
               conflicting ones. Args keep precendence over type-equations, so
               there should not be any conflicts with those. *)
            let eqs = li |> List.map fst |> List.concat in
            (eqs, name')
        (* If ambiguous, I don't know what happens *))
end

(******************************************************************************)
(*                                                                            *)
(*                         Type manipulation helpers                          *)
(*                                                                            *)
(******************************************************************************)

let expr_of_int i = literal (V_Int i)
let plus = binop PLUS
let t_bits_bitwidth e = T_Bits (BitWidth_SingleExpr e, [])

let reduce_expr env e =
  try StaticInterpreter.Normalize.normalize env e
  with StaticInterpreter.NotYetImplemented -> e

let reduce_constants env e =
  try StaticInterpreter.static_eval env e
  with
  | Error.ASLException { desc = Error.UndefinedIdentifier _; _ } as error -> (
    let () =
      if false then
        Format.eprintf
          "@[<hov>Static evaluation failed. Trying to reduce.@ For %a@ at \
           %a@]@."
          PP.pp_expr e PP.pp_pos e
    in
    try
      StaticInterpreter.Normalize.normalize env e
      |> StaticInterpreter.static_eval env
    with StaticInterpreter.NotYetImplemented -> raise error)

let sum = function
  | [] -> expr_of_int 0
  | [ x ] -> x
  | h :: t -> List.fold_left plus h t

let slices_length env =
  let minus = binop MINUS in
  let one = expr_of_int 1 in
  let slice_length = function
    | Slice_Single _ -> one
    | Slice_Length (_, e) -> e
    | Slice_Range (e1, e2) -> plus one (minus e1 e2)
  in
  fun li -> List.map slice_length li |> sum |> reduce_expr env

let width_plus env acc w =
  match (acc, w) with
  | BitWidth_SingleExpr e1, BitWidth_SingleExpr e2 ->
      BitWidth_SingleExpr (plus e1 e2 |> reduce_expr env)
  | BitWidth_Constraints cs1, BitWidth_SingleExpr e2 ->
      BitWidth_Constraints (constraint_binop PLUS cs1 [ Constraint_Exact e2 ])
  | BitWidth_SingleExpr e1, BitWidth_Constraints cs2 ->
      BitWidth_Constraints (constraint_binop PLUS [ Constraint_Exact e1 ] cs2)
  | BitWidth_Constraints cs1, BitWidth_Constraints cs2 ->
      BitWidth_Constraints (constraint_binop PLUS cs1 cs2)
  | _ ->
      failwith "Not yet implemented: concatening slices constrained from type."

let rename_ty_eqs : (AST.identifier * AST.expr) list -> AST.ty -> AST.ty =
  let subst_constraint eqs = function
    | Constraint_Exact e -> Constraint_Exact (subst_expr eqs e)
    | Constraint_Range (e1, e2) ->
        Constraint_Range (subst_expr eqs e1, subst_expr eqs e2)
  in
  let subst_constraints eqs = List.map (subst_constraint eqs) in
  fun eqs ty ->
    match ty.desc with
    | T_Bits (BitWidth_SingleExpr e, fields) ->
        let new_e = subst_expr eqs e in
        T_Bits (BitWidth_SingleExpr new_e, fields) |> add_pos_from_st ty
    | T_Bits (BitWidth_Constraints constraints, fields) ->
        let constraints = subst_constraints eqs constraints in
        T_Bits (BitWidth_Constraints constraints, fields) |> add_pos_from_st ty
    | T_Int (Some constraints) ->
        let constraints = subst_constraints eqs constraints in
        T_Int (Some constraints) |> add_pos_from_st ty
    | _ -> ty

let infer_value = function
  | V_Int i -> T_Int (Some [ Constraint_Exact (expr_of_int i) ])
  | V_Bool _ -> T_Bool
  | V_Real _ -> T_Real
  | V_String _ -> T_String
  | V_BitVector bv -> Bitvector.length bv |> expr_of_int |> t_bits_bitwidth
  | _ -> not_yet_implemented dummy_annotated "static complex values"

(**********************************************)
(* Approximate min and max on integer domains *)
(**********************************************)

exception ConstraintMinMaxTop

let min_constraint env = function
  | Constraint_Exact e | Constraint_Range (e, _) -> (
      let e = reduce_expr env e in
      match e.desc with
      | E_Literal (V_Int i) -> i
      | _ -> raise ConstraintMinMaxTop)

let max_constraint env = function
  | Constraint_Exact e | Constraint_Range (_, e) -> (
      let e = reduce_expr env e in
      match e.desc with
      | E_Literal (V_Int i) -> i
      | _ -> raise ConstraintMinMaxTop)

let min_max_constraints m_constraint m =
  let rec do_rec env = function
    | [] -> raise ConstraintMinMaxTop (* for underconstraint bitvector types. *)
    | [ c ] -> m_constraint env c
    | c :: cs ->
        let i = m_constraint env c and j = do_rec env cs in
        m i j
  in
  do_rec

(* NB: functions raise [ConstraintMinMaxTop] if no approximation can be found *)
let min_constraints = min_max_constraints min_constraint min
and max_constraints = min_max_constraints max_constraint max

(* -------------------------------------------------------------------------

                          Getter/Setter handling

   -------------------------------------------------------------------------- *)

let should_reduce_to_call env name =
  IMap.mem name env.global.subprogram_renamings

let should_slices_reduce_to_call env name slices =
  let args =
    try Some (List.map slice_as_single slices) with Invalid_argument _ -> None
  in
  match args with
  | None -> None
  | Some args -> if should_reduce_to_call env name then Some args else None

(******************************************************************************)
(*                                                                            *)
(*                               Annotate AST                                 *)
(*                                                                            *)
(******************************************************************************)

module type ANNOTATE_CONFIG = sig
  val check : strictness
end

module Annotate (C : ANNOTATE_CONFIG) = struct
  exception TypingAssumptionFailed

  let _warn =
    match C.check with `Warn | `TypeCheck -> true | `Silence -> false

  let check =
    match C.check with
    | `TypeCheck -> fun f x -> f x
    | `Warn -> (
        fun f x ->
          try f x
          with Error.ASLException e ->
            Error.eprintln e;
            x)
    | `Silence -> fun _f x -> x

  let best_effort =
    match C.check with
    | `TypeCheck -> fun x f -> f x
    | `Warn -> (
        fun x f ->
          try f x
          with Error.ASLException e ->
            Error.eprintln e;
            x)
    | `Silence -> ( fun x f -> try f x with Error.ASLException _ -> x)

  let[@inline] ( let+ ) m f = check m () |> f

  let[@inline] both f1 f2 x =
    let _ = f1 x in
    f2 x

  let either f1 f2 x =
    try f1 x with TypingAssumptionFailed | Error.ASLException _ -> f2 x

  let rec any li x =
    match li with
    | [] -> raise (Invalid_argument "any")
    | [ f ] -> f x
    | f :: li -> either f (any li) x

  let assumption_failed () = raise_notrace TypingAssumptionFailed [@@inline]
  let check_true b fail () = if b then () else fail () [@@inline]
  let check_true' b = check_true b assumption_failed [@@inline]

  let check_type_satisfies' env t1 t2 () =
    let () =
      if false then
        Format.eprintf "@[<hv 2>Checking %a@ <: %a@]@." PP.pp_ty t1 PP.pp_ty t2
    in
    if Types.type_satisfies env t1 t2 then () else assumption_failed ()

  let get_bitvector_width' env t =
    match (Types.get_structure env t).desc with
    | T_Bits (n, _) -> n
    | _ -> assumption_failed ()

  let get_bitvector_width loc env t =
    try get_bitvector_width' env t
    with TypingAssumptionFailed -> conflict loc [ default_t_bits ] t

  let get_fields loc env t =
    match (Types.get_structure env t).desc with
    | T_Exception fields | T_Record fields -> fields
    | _ -> conflict loc [ T_Record [] ] t

  (** [check_type_satisfies t1 t2] if [t1 <: t2]. *)
  let check_type_satisfies loc env t1 t2 () =
    if Types.type_satisfies env t1 t2 then () else conflict loc [ t1.desc ] t2

  (** [check_structure_boolean env t1] checks that [t1] has the structure of a boolean. *)
  let check_structure_boolean loc env t1 () =
    match (Types.get_structure env t1).desc with
    | T_Bool -> ()
    | _ -> conflict loc [ T_Bool ] t1

  let check_structure_bits loc env t () =
    match (Types.get_structure env t).desc with
    | T_Bits _ -> ()
    | _ -> conflict loc [ default_t_bits ] t

  let check_structure_integer loc env t () =
    match (Types.get_structure env t).desc with
    | T_Int _ -> ()
    | _ -> conflict loc [ T_Int None ] t

  let check_structure_exception loc env t () =
    let t_struct = Types.get_structure env t in
    match t_struct.desc with
    | T_Exception _ -> ()
    | _ -> conflict loc [ T_Exception [] ] t_struct

  let storage_is_pure loc (env : env) s =
    (* Definition DDYW:
       Any expression consisting solely of an immutable storage element or a
       literal value is a statically evaluable expression.
    *)
    match IMap.find_opt s env.local.storage_types with
    | Some (_, (LDK_Constant | LDK_Let)) -> true
    | Some (_, LDK_Var) -> false
    | None -> (
        match IMap.find_opt s env.global.storage_types with
        | Some (_, (GDK_Constant | GDK_Config | GDK_Let)) -> true
        | Some (_, GDK_Var) -> false
        | None -> undefined_identifier loc s)

  let check_pure (env : env) e () =
    let e = reduce_expr env e in
    let use_set = use_e ISet.empty e in
    if ISet.for_all (storage_is_pure e env) use_set then ()
    else fatal_from e (Error.UnpureExpression e)

  let check_bv_have_same_determined_bitwidth' env t1 t2 () =
    let n = get_bitvector_width' env t1 and m = get_bitvector_width' env t2 in
    if bitwidth_equal (StaticInterpreter.equal_in_env env) n m then
      (* TODO: Check statically evaluable? *) ()
    else
      match (n, m) with
      | BitWidth_SingleExpr e_n, BitWidth_SingleExpr e_m ->
          if StaticInterpreter.equal_in_env env e_n e_m then ()
          else assumption_failed ()
      | _ -> assumption_failed ()

  let check_bv_have_same_determined_bitwidth loc env t1 t2 () =
    try check_bv_have_same_determined_bitwidth' env t1 t2 ()
    with TypingAssumptionFailed ->
      fatal_from loc (Error.UnreconciableTypes (t1, t2))

  let has_bitvector_structure env t =
    match (Types.get_structure env t).desc with T_Bits _ -> true | _ -> false

  let t_bool = T_Bool |> add_dummy_pos
  let t_int = T_Int None |> add_dummy_pos

  let check_binop loc env op t1 t2 : ty =
    let () =
      if false then
        Format.eprintf "Checking binop %s between %a and %a@."
          (PP.binop_to_string op) PP.pp_ty t1 PP.pp_ty t2
    in
    let with_loc = add_pos_from loc in
    either
      (fun () ->
        match op with
        | BAND | BOR | BEQ | IMPL ->
            let+ () = check_type_satisfies' env t1 t_bool in
            let+ () = check_type_satisfies' env t2 t_bool in
            T_Bool |> with_loc
        | AND | OR | EOR ->
            (* Rule KXMR: If the operands of a primitive operation are
               bitvectors, the widths of the operands must be equivalent
               statically evaluable expressions. *)
            let+ () = check_bv_have_same_determined_bitwidth' env t1 t2 in
            let n = get_bitvector_width' env t1 in
            T_Bits (n, []) |> with_loc
        | (PLUS | MINUS) when has_bitvector_structure env t1 ->
            (* Rule KXMR: If the operands of a primitive operation are
               bitvectors, the widths of the operands must be equivalent
               statically evaluable expressions. *)
            let+ () =
              either
                (check_bv_have_same_determined_bitwidth' env t1 t2)
                (check_type_satisfies' env t2 t_int)
            in
            let n = get_bitvector_width' env t1 in
            T_Bits (n, []) |> with_loc
        | EQ_OP | NEQ ->
            (* Wrong! *)
            let+ () =
              any
                [
                  (* Optimisation. *)
                  check_true'
                    (type_equal (StaticInterpreter.equal_in_env env) t1 t2);
                  (* If an argument of a comparison operation is a constrained
                     integer then it is treated as an unconstrained integer. *)
                  both
                    (check_type_satisfies' env t1 t_int)
                    (check_type_satisfies' env t2 t_int);
                  (* If the arguments of a comparison operation are bitvectors
                     then they must have the same determined width. *)
                  check_bv_have_same_determined_bitwidth' env t1 t2;
                  (* The rest are redundancies from the first equal types
                     cases, but provided for completeness. *)
                  both
                    (check_type_satisfies' env t1 t_bool)
                    (check_type_satisfies' env t2 t_bool);
                  (fun () ->
                    match (t1.desc, t2.desc) with
                    | T_Enum li1, T_Enum li2 ->
                        check_true' (list_equal String.equal li1 li2) ()
                    | _ -> assumption_failed ());
                ]
            in
            T_Bool |> with_loc
        | LEQ | GEQ | GT | LT ->
            let+ () = check_type_satisfies' env t1 t_int in
            let+ () = check_type_satisfies' env t2 t_int in
            T_Bool |> with_loc
        | MUL | DIV | MOD | SHL | SHR | PLUS | MINUS -> (
            (* TODO: ensure that they mean "has the structure of" instead of
               "is" *)
            let struct1 = Types.get_structure env t1
            and struct2 = Types.get_structure env t2 in
            match (struct1.desc, struct2.desc) with
            | T_Int None, T_Int _ | T_Int _, T_Int None ->
                (* Rule ZYWY: If both operands of an integer binary primitive
                   operator are integers and at least one of them is an
                   unconstrained integer then the result shall be an
                   unconstrained integer. *)
                (* TODO: check that no other checks are necessary. *)
                T_Int None |> with_loc
            | T_Int (Some []), T_Int (Some _) | T_Int (Some _), T_Int (Some [])
              ->
                (* Rule BZKW: If both operands of an integer binary primitive
                   operator are constrained integers and at least one of them
                   is the under-constrained integer then the result shall be an
                   under-constrained integer. *)
                T_Int (Some []) |> with_loc
            | T_Int (Some cs1), T_Int (Some cs2) ->
                (* Rule KFYS: If both operands of an integer binary primitive
                   operation are well-constrained integers, then it shall
                   return a constrained integer whose constraint is calculated
                   by applying the operation to all possible value pairs. *)
                (* TODO: check for division by zero? cf I YHRP: The calculation
                   of constraints shall cause an error if necessary, for
                   example where a division by zero occurs, etc. *)
                T_Int (Some (constraint_binop op cs1 cs2)) |> with_loc
            | _ -> assumption_failed ())
        | RDIV ->
            let+ () = check_type_satisfies' env t1 (T_Real |> add_dummy_pos) in
            T_Real |> with_loc)
      (fun () -> fatal_from loc (Error.BadTypesForBinop (op, t1, t2)))
      ()

  let check_unop loc env op t =
    match op with
    | BNOT ->
        let+ () = check_type_satisfies loc env t t_bool in
        T_Bool |> add_pos_from loc
    | NEG -> (
        let+ () = check_type_satisfies loc env t t_int in
        match (Types.get_structure env t).desc with
        | T_Int None -> T_Int None |> add_pos_from loc
        | T_Int (Some cs) ->
            let neg e = E_Unop (NEG, e) |> add_pos_from e in
            let constraint_minus = function
              | Constraint_Exact e -> Constraint_Exact (neg e)
              | Constraint_Range (top, bot) ->
                  Constraint_Range (neg bot, neg top)
            in
            T_Int (Some (List.map constraint_minus cs)) |> add_pos_from loc
        | _ -> (* fail case *) t)
    | NOT ->
        let+ () = check_structure_bits loc env t in
        t

  let can_assign_to env s t =
    (* Rules:
       - GNTS: It is illegal for a storage element whose type has the structure
         of the under-constrained integer to be assigned a value whose type has
         the structure of the under-constrained integer.
       - LXQZ: A storage element of type S, where S is any type that does not have the
         structure of the under-constrained integer type, may only be
         assigned or initialized with a value of type T if T type-satisfies S
    *)
    (* TODO: incomplete. *)
    let s_struct = Types.get_structure env s
    and t_struct = Types.get_structure env t in
    match (s_struct.desc, t_struct.desc) with
    | T_Int (Some []), T_Int (Some []) -> false
    | _ -> Types.type_satisfies env t s

  let check_can_assign_to loc env s t () =
    if can_assign_to env s t then ()
    else
      let () =
        if false then Format.eprintf "%a <-- %a@." PP.pp_ty s PP.pp_ty t
      in
      fatal_from loc (Error.ConflictingTypes ([ s.desc ], t))

  let rec annotate_slices env =
    (* Rules:
       - Rule WZCS: The width of a bitslice must be any non-negative,
         statically evaluable integer expression (including zero).
       - Rule KTBG: It is an error if any bits selected by a bitslice are not
         in range for the expression being sliced. If the offset of a bitslice
         depends on a statically evaluable expression then this shall be
         checked at compile time. Otherwise a bounds check will occur at
         execution-time and an implementation defined exception shall be thrown
         if it fails.
       TODO: check them
    *)
    let tr_one = function
      | Slice_Single e ->
          (* First rule trivially true. *)
          (* TODO: try evaluate this at compile time, and check it against sliced
             expression type. *)
          let t_e, e = annotate_expr env e in
          let+ () = check_structure_integer e env t_e in
          let+ () = check_pure env e in
          Slice_Single e
      | Slice_Range (e1, e2) as slice ->
          let t_e1, e1 = annotate_expr env e1
          and t_e2, e2 = annotate_expr env e2 in
          let+ () = check_structure_integer e1 env t_e1 in
          let+ () = check_structure_integer e2 env t_e2 in
          let length = slices_length env [ slice ] in
          let+ () = check_pure env length in
          Slice_Range (e1, e2)
      | Slice_Length (offset, length) ->
          let t_offset, offset = annotate_expr env offset
          and t_length, length = annotate_expr env length in
          let+ () = check_structure_integer offset env t_offset in
          let+ () = check_structure_integer length env t_length in
          let+ () = check_pure env length in
          (* TODO: if offset is statically evaluable, check that it is less
             than sliced expression width. *)
          Slice_Length (offset, length)
    in
    List.map tr_one

  and annotate_pattern loc env t = function
    | Pattern_All as p -> p
    | Pattern_Any li -> Pattern_Any (List.map (annotate_pattern loc env t) li)
    | Pattern_Not p -> Pattern_Not (annotate_pattern loc env t p)
    | Pattern_Single e ->
        let t_e, e = annotate_expr env e in
        let+ () =
         fun () ->
          let t_struct = Types.get_structure env t
          and t_e_struct = Types.get_structure env t_e in
          match (t_struct.desc, t_e_struct.desc) with
          | T_Bool, T_Bool | T_Real, T_Real -> ()
          | T_Int _, T_Int _ -> ()
          | T_Bits _, T_Bits _ ->
              check_bv_have_same_determined_bitwidth loc env t_struct t_e_struct
                ()
          (* TODO: Multiple discriminants can be matched at once by forming
             a tuple of discriminants and a tuple used in the pattern_set.
             Both tuples must have the same number of elements. A
             successful pattern match occurs when each discriminant term
             matches the respective term of the pattern tuple. *)
          | T_Enum li1, T_Enum li2 when list_equal String.equal li1 li2 -> ()
          | _ -> fatal_from loc (Error.BadTypesForBinop (EQ_OP, t, t_e))
        in
        Pattern_Single e
    | Pattern_Geq e ->
        let t_e, e = annotate_expr env e in
        let+ () = check_pure env e in
        let+ () =
          both
            (check_structure_integer loc env t)
            (check_structure_integer loc env t_e)
        in
        Pattern_Geq e
    | Pattern_Leq e ->
        let t_e, e = annotate_expr env e in
        let+ () = check_pure env e in
        let+ () =
          both
            (check_structure_integer loc env t)
            (check_structure_integer loc env t_e)
        in
        Pattern_Leq e
    | Pattern_Range (e1, e2) ->
        let t_e1, e1 = annotate_expr env e1
        and t_e2, e2 = annotate_expr env e2 in
        let+ () =
          both
            (check_structure_integer loc env t)
            (both
               (check_structure_integer loc env t_e1)
               (check_structure_integer loc env t_e2))
        in
        Pattern_Range (e1, e2)
    | Pattern_Mask m as p ->
        let+ () = check_structure_bits loc env t in
        let+ () =
          let n = literal (V_Int (Bitvector.mask_length m)) in
          let t_m = T_Bits (BitWidth_SingleExpr n, []) |> add_pos_from loc in
          check_type_satisfies loc env t t_m
        in
        p
    | Pattern_Tuple li -> (
        let t_struct = Types.get_structure env t in
        match t_struct.desc with
        | T_Tuple ts when List.compare_lengths li ts != 0 ->
            Error.fatal_from loc
              (Error.BadArity
                 ("pattern matching on tuples", List.length li, List.length ts))
        | T_Tuple ts ->
            Pattern_Tuple (List.map2 (annotate_pattern loc env) ts li)
        | _ -> conflict loc [ T_Tuple [] ] t)

  and annotate_call loc env name args eqs call_type =
    let () =
      if false then
        Format.eprintf "Annotating call to %S at %a.@." name PP.pp_pos loc
    in
    let caller_arg_typed = List.map (annotate_expr env) args in
    let caller_arg_types, args = List.split caller_arg_typed in
    let extra_nargs, name, callee_arg_types, ret_ty =
      either
        (fun () ->
          FunctionRenaming.find_name_strict loc env name caller_arg_types)
        (fun () ->
          let extra_nargs, name =
            FunctionRenaming.find_name env name caller_arg_types
          in
          match IMap.find_opt name env.global.subprograms with
          | None -> undefined_identifier loc ("function " ^ name)
          | Some { args = callee_arg_types; return_type; _ } ->
              (extra_nargs, name, callee_arg_types, return_type))
        ()
    in
    let eqs = List.rev_append eqs extra_nargs in
    let () =
      if List.compare_lengths callee_arg_types args != 0 then
        fatal_from loc
        @@ Error.BadArity (name, List.length callee_arg_types, List.length args)
    in
    let eqs =
      let folder acc (x, ty) (t_e, e) =
        match ty.desc with
        | T_Int _ -> (x, e) :: acc
        | T_Bits (BitWidth_SingleExpr { desc = E_Var x; _ }, _) -> (
            match (Types.get_structure env t_e).desc with
            | T_Bits (BitWidth_SingleExpr e, _) -> (x, e) :: acc
            | T_Bits _ -> (x, E_Unknown t_e |> add_dummy_pos) :: acc
            | _ -> acc)
        | _ -> acc
      in
      List.fold_left2 folder eqs callee_arg_types caller_arg_typed
    in
    let () =
      if false then
        let open Format in
        eprintf "@[<hov 2>Eqs for this call are: %a@]@."
          (pp_print_list ~pp_sep:pp_print_space (fun f (name, e) ->
               fprintf f "%S<--%a" name PP.pp_expr e))
          eqs
    in
    let () =
      List.iter2
        (fun (_, callee_arg) caller_arg ->
          let callee_arg = rename_ty_eqs eqs callee_arg in
          let () =
            if false then
              Format.eprintf "Checking calling arg from %a to %a@." PP.pp_ty
                caller_arg PP.pp_ty callee_arg
          in
          let+ () = check_can_assign_to loc env callee_arg caller_arg in
          ())
        callee_arg_types caller_arg_types
    in
    let () =
      if false && not (String.equal name name) then
        Format.eprintf "Renaming call from %s to %s@ at %a.@." name name
          PP.pp_pos loc
    in
    let ret_ty =
      match (call_type, ret_ty) with
      | (ST_Function | ST_Getter), Some ty -> Some (rename_ty_eqs eqs ty)
      | (ST_Setter | ST_Procedure), None -> None
      | _ -> fatal_from loc @@ Error.MismatchedReturnValue name
    in
    let () = if false then Format.eprintf "Annotated call to %S.@." name in
    (name, args, eqs, ret_ty)

  and annotate_expr env (e : expr) : ty * expr =
    let () = if false then Format.eprintf "@[Annotating %a@]@." PP.pp_expr e in
    let here x = add_pos_from e x in
    match e.desc with
    | E_Literal v -> (infer_value v |> here, e)
    | E_Typed (e', t) ->
        let t_e, e'' = annotate_expr env e' in
        (* - If type-checking determines that the expression type-satisfies
             the required type, then no further check is required.
           - If the expression only fails to type-satisfy the required type
             because the domain of its type is not a subset of the domain of
             the required type, an execution-time check that the expression
             evaluates to a value in the domain of the required type is
             required. *)
        best_effort
          (t, E_Typed (e'', t) |> here)
          (fun res ->
            let env' = env in
            if Types.structural_subtype_satisfies env' t_e t then
              if Types.domain_subtype_satisfies env' t_e t then (t, e'')
              else res
            else conflict e [ t.desc ] t_e)
    | E_Var x -> (
        let () = if false then Format.eprintf "Looking at %S.@." x in
        if should_reduce_to_call env x then
          let () =
            if false then
              Format.eprintf "@[Reducing getter %S@ at %a@]@." x PP.pp_pos e
          in
          let name, args, eqs, ty =
            annotate_call (to_pos e) env x [] [] ST_Getter
          in
          let ty = match ty with Some ty -> ty | None -> assert false in
          (ty, E_Call (name, args, eqs) |> here)
        else
          let () =
            if false then
              Format.eprintf "@[Choosing not to reduce var %S@ at @[%a@]@]@." x
                PP.pp_pos e
          in
          try
            match IMap.find x env.local.storage_types with
            | ty, LDK_Constant ->
                let v = IMap.find x env.local.constants_values in
                let e = E_Literal v |> here in
                (ty, e)
            | ty, _ -> (ty, e)
          with Not_found -> (
            try
              match IMap.find x env.global.storage_types with
              | ty, GDK_Constant -> (
                  match IMap.find_opt x env.global.constants_values with
                  | Some v -> (ty, E_Literal v |> here)
                  | None -> (ty, e))
              | ty, _ -> (ty, e)
            with Not_found ->
              let () =
                if false then
                  Format.eprintf "@[Cannot find %s in env@ %a.@]@." x pp_env env
              in
              undefined_identifier e x))
    | E_Binop (BAND, e1, e2) ->
        E_Cond (e1, e2, E_Literal (V_Bool false) |> here)
        |> here |> annotate_expr env
    | E_Binop (BOR, e1, e2) ->
        E_Cond (e1, E_Literal (V_Bool true) |> here, e2)
        |> here |> annotate_expr env
    | E_Binop (op, e1, e2) ->
        let t1, e1' = annotate_expr env e1 in
        let t2, e2' = annotate_expr env e2 in
        let t = check_binop e env op t1 t2 in
        (t, E_Binop (op, e1', e2') |> here)
    | E_Unop (op, e') ->
        let t'', e'' = annotate_expr env e' in
        let t = check_unop e env op t'' in
        (t, E_Unop (op, e'') |> here)
    | E_Call (name, args, eqs) ->
        let name, args, eqs, ty =
          annotate_call (to_pos e) env name args eqs ST_Function
        in
        let ty = match ty with Some ty -> ty | None -> assert false in
        (ty, E_Call (name, args, eqs) |> here)
    | E_Cond (e_cond, e_true, e_false) ->
        let t_cond, e_cond = annotate_expr env e_cond in
        let+ () = check_structure_boolean e env t_cond in
        let t_true, e_true = annotate_expr env e_true
        and t_false, e_false = annotate_expr env e_false in
        let t =
          best_effort t_true (fun _ ->
              match Types.lowest_common_ancestor env t_true t_false with
              | None ->
                  fatal_from e (Error.UnreconciableTypes (t_true, t_false))
              | Some t -> t)
        in
        (t, E_Cond (e_cond, e_true, e_false) |> here)
    | E_Tuple li ->
        let ts, es = List.map (annotate_expr env) li |> List.split in
        (T_Tuple ts |> here, E_Tuple es |> here)
    | E_Concat [] ->
        (T_Bits (BitWidth_SingleExpr (expr_of_int 0), []) |> here, e)
    | E_Concat (_ :: _ as li) ->
        let ts, es = List.map (annotate_expr env) li |> List.split in
        let w =
          best_effort (BitWidth_Constraints []) (fun _ ->
              let widths = List.map (get_bitvector_width e env) ts in
              let wh = List.hd widths and wts = List.tl widths in
              List.fold_left (width_plus env) wh wts)
        in
        (T_Bits (w, []) |> here, E_Concat es |> here)
    | E_Record (t, fields) ->
        (* Rule WBCQ: The identifier in a record expression must be a named type
           with the structure of a record type, and whose fields have the values
           given in the field_assignment_list. *)
        let+ () =
          check_true (Types.is_named t) (fun () ->
              failwith "Typing error: should be a named type")
        in
        let field_types = get_fields e env t in
        let fields =
          best_effort fields (fun _ ->
              (* Rule DYQZ: A record expression shall assign every field of the record. *)
              if
                List.for_all
                  (fun (name, _) -> List.mem_assoc name fields)
                  field_types
              then ()
              else fatal_from e (Error.BadFields (List.map fst fields, t));
              (* and whose fields have the values given in the field_assignment_list. *)
              List.map
                (fun (name, e') ->
                  let t', e' = annotate_expr env e' in
                  let t_spec' =
                    match List.assoc_opt name field_types with
                    | None -> fatal_from e (Error.BadField (name, t))
                    | Some t_spec' -> t_spec'
                  in
                  (* TODO: No type checking rule exists here, I interprete
                     Rule LXQZ: A storage element of type S, where S is any
                     type that does not have the structure of the
                     under-constrained integer type, may only be assigned
                     or initialized with a value of type T if T
                     type-satisfies S. *)
                  let+ () = check_type_satisfies e env t' t_spec' in
                  (name, e'))
                fields)
        in
        (t, E_Record (t, fields) |> here)
    | E_Unknown t ->
        let t = Types.get_structure env t in
        (t, E_Unknown t |> here)
    | E_Slice (e', slices) -> (
        let reduced =
          match e'.desc with
          | E_Var x ->
              should_slices_reduce_to_call env x slices |> Option.map (pair x)
          | _ -> None
        in
        match reduced with
        | Some (name, args) ->
            let name, args, eqs, ty =
              annotate_call (to_pos e) env name args [] ST_Getter
            in
            let ty = match ty with Some ty -> ty | None -> assert false in
            (ty, E_Call (name, args, eqs) |> here)
        | None ->
            let t_e', e' = annotate_expr env e' in
            let+ () =
              either
                (check_structure_bits e env t_e')
                (check_structure_integer e env t_e')
            in
            let w = slices_length env slices in
            (* TODO: check that:
               - Rule SNQJ: An expression or subexpression which may result in
                 a zero-length bitvector must not be side-effecting.
            *)
            let slices = best_effort slices (annotate_slices env) in
            ( T_Bits (BitWidth_SingleExpr w, []) |> here,
              E_Slice (e', slices) |> here ))
    | E_GetField (e', field) -> (
        let t_e', e' = annotate_expr env e' in
        let t_e' = resolve_root_name env t_e' in
        match t_e'.desc with
        | T_Exception fields | T_Record fields -> (
            match List.assoc_opt field fields with
            | None -> fatal_from e (Error.BadField (field, t_e'))
            | Some t -> (t, E_GetField (e', field) |> here))
        | T_Bits (_, bitfields) -> (
            match List.assoc_opt field bitfields with
            | None -> fatal_from e (Error.BadField (field, t_e'))
            | Some slice -> E_Slice (e', slice) |> here |> annotate_expr env)
        | _ -> conflict e [ default_t_bits; T_Record []; T_Exception [] ] t_e')
    | E_GetFields (e', fields) ->
        let t_e', e' = annotate_expr env e' in
        let t_e' = resolve_root_name env t_e' in
        let bitfields =
          match t_e'.desc with
          | T_Bits (_, bitfields) -> bitfields
          | _ -> conflict e [ default_t_bits ] t_e'
        in
        let one_field field =
          match List.assoc_opt field bitfields with
          | None -> fatal_from e (Error.BadField (field, t_e'))
          | Some slices -> slices
        in
        E_Slice (e', list_concat_map one_field fields)
        |> here |> annotate_expr env
    | E_Pattern (e', patterns) ->
        (*
         Rule ZNDL states that

            The IN operator is equivalent to testing its first operand for
            equality against each value in the (possibly infinite) set denoted
            by the second operand, and taking the logical OR of the result.
            Values denoted by a bitmask_lit comprise all bitvectors that could
            match the bit-mask. It is not an error if any or all of the values
            denoted by the first operand can be statically determined to never
            compare equal with the second operand.

          e IN pattern            is sugar for
             "-"                      ->          TRUE
           | e1=expr                  ->          e == e1
           | bitmask_lit              ->          not yet implemented
           | e1=expr ".." e2=expr     ->          e1 <= e && e <= e2
           | "<=" e1=expr             ->          e <= e1
           | ">=" e1=expr             ->          e >= e1
           |  { p0 , ... pN }         ->          e IN p0 || ... e IN pN
           | !{ p0 , ... pN }         ->          not (e IN p0) && ... e IN pN

         We cannot reduce them here (as otherwise e might be evaluated a bad
         number of times), but we will apply the same typing rules as for those
         desugared expressions.
         *)
        let t_e', e' = annotate_expr env e' in
        let patterns = best_effort patterns (annotate_pattern e env t_e') in
        (T_Bool |> here, E_Pattern (e', patterns) |> here)

  let rec annotate_lexpr env le t_e =
    let here x = add_pos_from le x in
    match le.desc with
    | LE_Var x ->
        (* TODO: Handle setting global var *)
        let+ () =
         fun () ->
          let ty =
            match IMap.find_opt x env.local.storage_types with
            | Some (ty, LDK_Var) -> ty
            | Some _ -> fatal_from le @@ Error.AssignToImmutable x
            | None -> (
                match IMap.find_opt x env.global.storage_types with
                | Some (ty, _) ->
                    (* TODO: check that the keyword is a variable. *)
                    ty
                | None -> undefined_identifier le x)
          in
          check_can_assign_to le env ty t_e ()
        in
        le
    | LE_Ignore -> le
    | LE_TupleUnpack les -> (
        match t_e.desc with
        | T_Tuple sub_tys ->
            if List.compare_lengths sub_tys les != 0 then
              Error.fatal_from le
                (Error.BadArity
                   ("tuple unpacking", List.length sub_tys, List.length les))
            else
              let les' = List.map2 (annotate_lexpr env) les sub_tys in
              LE_TupleUnpack les' |> here
        | _ -> conflict le [ T_Tuple [] ] t_e)
    | LE_Slice (le', slices) ->
        let t_le, _ = expr_of_lexpr le' |> annotate_expr env in
        let+ () = check_structure_bits le env t_le in
        let le' = annotate_lexpr env le' t_le in
        let+ () =
         fun () ->
          let length = slices_length env slices |> reduce_expr env in
          let t = T_Bits (BitWidth_SingleExpr length, []) |> here in
          check_can_assign_to le env t t_e ()
        in
        let slices = best_effort slices (annotate_slices env) in
        LE_Slice (le', slices) |> here
    | LE_SetField (le', field) -> (
        let t_le', _ = expr_of_lexpr le' |> annotate_expr env in
        let le' = annotate_lexpr env le' t_le' in
        let t_le'_struct = Types.get_structure env t_le' in
        match t_le'_struct.desc with
        | T_Exception fields | T_Record fields ->
            let t =
              match List.assoc_opt field fields with
              | None -> fatal_from le (Error.BadField (field, t_le'))
              | Some t -> t
            in
            let+ () = check_can_assign_to le env t t_e in
            LE_SetField (le', field) |> here
        | T_Bits (_, bitfields) -> (
            match List.assoc_opt field bitfields with
            | None -> fatal_from le (Error.BadField (field, t_le'_struct))
            | Some slice ->
                let w = slices_length env slice in
                let t = T_Bits (BitWidth_SingleExpr w, []) |> here in
                let+ () = check_can_assign_to le env t t_e in
                let le = LE_Slice (le', slice) |> here in
                annotate_lexpr env le t_e)
        | _ -> conflict le [ default_t_bits; T_Record []; T_Exception [] ] t_e)
    | LE_SetFields (le', fields) ->
        let t_le', _ = expr_of_lexpr le' |> annotate_expr env in
        let le' = annotate_lexpr env le' t_le' in
        let t_le'_struct = Types.get_structure env t_le' in
        let bitfields =
          match t_le'_struct.desc with
          | T_Bits (_, bitfields) -> bitfields
          | _ -> conflict le [ default_t_bits ] t_le'
        in
        let one_field field =
          match List.assoc_opt field bitfields with
          | None -> fatal_from le (Error.BadField (field, t_le'_struct))
          | Some slices -> slices
        in
        let new_le = LE_Slice (le', list_concat_map one_field fields) |> here in
        annotate_lexpr env new_le t_e

  let can_be_initialized_with env s t =
    (* Rules:
       - ZCVD: It is illegal for a storage element whose type has the
         structure of the under-constrained integer to be initialized with a
         value whose type has the structure of the under-constrained integer,
         unless the type is omitted from the declaration (and therefore the
         type can be unambiguously inferred) or the initialization expression
         is omitted (and therefore the type is not omitted from the
         declaration).
       - LXQZ: A storage element of type S, where S is any type that does not have the
         structure of the under-constrained integer type, may only be
         assigned or initialized with a value of type T if T type-satisfies
         S)
    *)
    let s_struct = Types.get_structure env s in
    match s_struct.desc with
    | T_Int (Some []) -> (* TODO *) assert false
    | _ -> Types.type_satisfies env t s

  let check_can_be_initialized_with loc env s t () =
    if can_be_initialized_with env s t then () else conflict loc [ s.desc ] t

  let check_var_not_in_env loc env x () =
    if
      IMap.mem x env.local.storage_types
      || IMap.mem x env.global.storage_types
      || IMap.mem x env.global.subprograms
    then fatal_from loc (Error.AlreadyDeclaredIdentifier x)
    else ()

  let rec annotate_local_decl_item loc (env : env) ty ldk ldi =
    match ldi with
    | LDI_Ignore None -> (env, ldi)
    | LDI_Ignore (Some t) ->
        let+ () = check_can_be_initialized_with loc env t ty in
        (env, ldi)
    | LDI_Var (x, ty_opt) ->
        let t =
          best_effort ty (fun _ ->
              match ty_opt with
              | None -> ty
              | Some t ->
                  let+ () = check_can_be_initialized_with loc env t ty in
                  t)
        in
        (* Rule LCFD: A local declaration shall not declare an identifier
           which is already in scope at the point of declaration. *)
        let+ () = check_var_not_in_env loc env x in
        let env = add_local x t ldk env in
        (env, LDI_Var (x, Some t))
    | LDI_Tuple ([ ldi ], None) -> annotate_local_decl_item loc env ty ldk ldi
    | LDI_Tuple (ldis, None) ->
        let tys =
          match (Types.get_structure env ty).desc with
          | T_Tuple tys when List.compare_lengths tys ldis = 0 -> tys
          | T_Tuple tys ->
              fatal_from loc
                (Error.BadArity
                   ("tuple initialization", List.length tys, List.length ldis))
          | _ -> conflict loc [ T_Tuple [] ] ty
        in
        let env, ldis' =
          List.fold_right2
            (fun ty' ldi' (env', les) ->
              let env', le = annotate_local_decl_item loc env' ty' ldk ldi' in
              (env', le :: les))
            tys ldis (env, [])
        in
        (env, LDI_Tuple (ldis', None))
    | LDI_Tuple (_ldis, Some _t) ->
        (* TODO: I don't know what to do in that case, for me the LRM is
           ambiguous in this case. *)
        assert false

  let rec annotate_local_decl_item_uninit loc (env : env) ldi =
    (* Here implicitely ldk=LDK_Var *)
    let here s = add_pos_from loc s in
    match ldi with
    | LDI_Ignore _ -> (env, S_Pass |> here)
    | LDI_Var (_, None) ->
        fatal_from loc
          (Error.NotYetImplemented "Variable declaration needs a type.")
    | LDI_Var (x, Some t) ->
        let+ () = check_var_not_in_env loc env x in
        let e = Types.base_value loc env t in
        ( add_local x t LDK_Var env,
          S_Decl (LDK_Var, LDI_Var (x, Some t), Some e) |> here )
    | LDI_Tuple (ldis, None) ->
        let env, ss =
          list_fold_left_map (annotate_local_decl_item_uninit loc) env ldis
        in
        (env, stmt_from_list ss)
    | LDI_Tuple (ldis, Some t) ->
        let env, les =
          list_fold_left_map
            (fun env' -> annotate_local_decl_item loc env' t LDK_Var)
            env ldis
        in
        let e = Types.base_value loc env t in
        let ss =
          List.map (fun ldi -> S_Decl (LDK_Var, ldi, Some e) |> here) les
        in
        (env, stmt_from_list ss)

  let rec annotate_stmt env return_type s =
    let () =
      if false then
        match s.desc with
        | S_Then _ -> ()
        | _ -> Format.eprintf "@[<3>Annotating@ @[%a@]@]@." PP.pp_stmt s
    in
    let here x = add_pos_from s x in
    match s.desc with
    | S_Pass -> (s, env)
    | S_Then (s1, s2) ->
        let s1, env = try_annotate_stmt env return_type s1 in
        let s2, env = try_annotate_stmt env return_type s2 in
        (S_Then (s1, s2) |> here, env)
    | S_Assign (le, e) -> (
        let t_e, e = annotate_expr env e in
        let reduced = setter_should_reduce_to_call_s env le e in
        match reduced with
        | Some s -> (s, env)
        | None ->
            let le = annotate_lexpr env le t_e in
            (S_Assign (le, e) |> here, env))
    | S_Call (name, args, eqs) ->
        let name, args, eqs, ty =
          annotate_call (to_pos s) env name args eqs ST_Procedure
        in
        let () = assert (ty = None) in
        (* TODO: check that call does not returns anything. *)
        (S_Call (name, args, eqs) |> here, env)
    | S_Return e_opt -> (
        (* Rule NYWH: A return statement appearing in a setter or procedure must
           have no return value expression. *)
        (* Rule PHNZ: A return statement appearing in a getter or function
           requires a return value expression that type-satisfies the return
           type of the subprogram. *)
        match (return_type, e_opt) with
        | None, Some _ | Some _, None ->
            fatal_from s (Error.BadReturnStmt return_type)
        | None, None -> (S_Return None |> here, env)
        | Some t, Some e ->
            let t_e', e' = annotate_expr env e in
            let () =
              if false then
                Format.eprintf
                  "Can I return %a(of type %a) when return_type = %a?@."
                  PP.pp_expr e PP.pp_ty t_e' PP.pp_ty t
            in
            let+ () = check_type_satisfies s env t_e' t in
            (S_Return (Some e') |> here, env))
    | S_Cond (e, s1, s2) ->
        let t_cond, e = annotate_expr env e in
        let+ () = check_type_satisfies e env t_cond t_bool in
        let s1 = try_annotate_block env return_type s1 in
        let s2 = try_annotate_block env return_type s2 in
        (S_Cond (e, s1, s2) |> here, env)
    | S_Case (e, cases) ->
        let t_e, e = annotate_expr env e in
        let annotate_case (acc, env) case =
          let p, s = case.desc in
          let p = annotate_pattern e env t_e p in
          let s = try_annotate_block env return_type s in
          (add_pos_from_st case (p, s) :: acc, env)
        in
        let cases, env = List.fold_left annotate_case ([], env) cases in
        (S_Case (e, List.rev cases) |> here, env)
    | S_Assert e ->
        let t_e', e' = annotate_expr env e in
        let+ () = check_type_satisfies s env t_e' t_bool in
        (S_Assert e' |> here, env)
    | S_While (e, s) ->
        let t, e = annotate_expr env e in
        let+ () = check_type_satisfies e env t t_bool in
        let s = try_annotate_block env return_type s in
        (S_While (e, s) |> here, env)
    | S_Repeat (s, e) ->
        let s = try_annotate_block env return_type s in
        let t, e = annotate_expr env e in
        let+ () = check_type_satisfies e env t t_bool in
        (S_Repeat (s, e) |> here, env)
    | S_For (id, e1, dir, e2, s) ->
        let t1, e1 = annotate_expr env e1 and t2, e2 = annotate_expr env e2 in
        let+ () = check_structure_integer s env t1 in
        let+ () = check_structure_integer s env t2 in
        let cs =
          match
            ( (Types.get_structure env t1).desc,
              (Types.get_structure env t2).desc )
          with
          | T_Int None, T_Int _ | T_Int _, T_Int None -> None
          | T_Int (Some cs1), T_Int (Some cs2) -> (
              try
                let bot_cs, top_cs =
                  match dir with Up -> (cs1, cs2) | Down -> (cs2, cs1)
                in
                let bot = min_constraints env bot_cs
                and top = max_constraints env top_cs in
                if bot <= top then
                  Some [ Constraint_Range (expr_of_int bot, expr_of_int top) ]
                else Some cs1
              with ConstraintMinMaxTop ->
                (* TODO: this case is not specified by the LRM. *)
                Some [])
          | _ -> None
          (* only happens in relaxed type-checking mode because of check_structure_integer earlier. *)
        in
        let ty = T_Int cs |> here in
        let s =
          let+ () = check_var_not_in_env s env id in
          let env' = add_local id ty LDK_Let env in
          try_annotate_block env' return_type s
        in
        (S_For (id, e1, dir, e2, s) |> here, env)
    | S_Decl (ldk, ldi, e_opt) -> (
        match (ldk, e_opt) with
        | _, Some e ->
            let t_e, e = annotate_expr env e in
            let env, ldi = annotate_local_decl_item s env t_e ldk ldi in
            (* TODO:
               - The initialization expression in a local constant declaration
                 must be a compile-time-constant expression.
               - A local storage element declared with constant is initialized
                 with the value of its initialization expression during
                 compilation.
               - Initialization expressions in local constant declarations must
                 be non-side-effecting.
            *)
            (S_Decl (ldk, ldi, Some e) |> here, env)
        | LDK_Var, None ->
            let env, s = annotate_local_decl_item_uninit s env ldi in
            (s, env)
        | (LDK_Constant | LDK_Let), None ->
            (* by construction in Parser. *)
            assert false)
    | S_Throw (Some (e, _)) ->
        let t_e, e = annotate_expr env e in
        let+ () = check_structure_exception s env t_e in
        (S_Throw (Some (e, Some t_e)) |> here, env)
    | S_Throw None ->
        (* TODO: verify that this is allowed? *)
        (s, env)
    | S_Try (s', catchers, otherwise) ->
        let s' = try_annotate_block env return_type s' in
        let otherwise =
          Option.map (try_annotate_block env return_type) otherwise
        in
        let catchers = List.map (annotate_catcher env return_type) catchers in
        (S_Try (s', catchers, otherwise) |> here, env)

  and annotate_catcher env return_type (name_opt, ty, stmt) =
    let+ () = check_structure_exception ty env ty in
    let env' =
      match name_opt with
      | None -> env
      | Some name ->
          let+ () = check_var_not_in_env stmt env name in
          add_local name ty LDK_Let env
    in
    let stmt = try_annotate_block env' return_type stmt in
    (name_opt, ty, stmt)

  and try_annotate_block env return_type s =
    (*
        See rule JFRD:
           A local identifier declared with var, let or constant is in scope
           from the point immediately after its declaration until the end of the
           immediately enclosing block.

        From that follows that we can discard the environment at the end of an
        enclosing block.
    *)
    best_effort s (fun _ -> annotate_stmt env return_type s |> fst)

  and try_annotate_stmt env return_type s =
    best_effort (s, env) (fun _ -> annotate_stmt env return_type s)

  and setter_should_reduce_to_call_s env le e : stmt option =
    let here d = add_pos_from le d in
    let s_then = s_then in
    let to_expr = expr_of_lexpr in
    let with_temp old_le sub_le =
      let x = fresh_var "setter_setfield" in
      let le_x = LE_Var x |> here in
      match setter_should_reduce_to_call_s env sub_le (E_Var x |> here) with
      | None -> None
      | Some s ->
          let s1, _env1 =
            S_Assign (le_x, to_expr sub_le) |> here |> annotate_stmt env None
          and s2, _env2 =
            S_Assign (old_le le_x, e) |> here |> annotate_stmt env None
          in
          Some (s_then (s_then s1 s2) s)
    in
    match le.desc with
    | LE_Ignore -> None
    | LE_SetField (sub_le, field) ->
        let old_le le' = LE_SetField (le', field) |> here in
        with_temp old_le sub_le
    | LE_SetFields (sub_le, fields) ->
        let old_le le' = LE_SetFields (le', fields) |> here in
        with_temp old_le sub_le
    | LE_Slice ({ desc = LE_Var x; _ }, slices) -> (
        let slices = Slice_Single e :: slices in
        match should_slices_reduce_to_call env x slices with
        | None -> None
        | Some args ->
            let name, args, eqs, ret_ty =
              annotate_call (to_pos le) env x args [] ST_Setter
            in
            let () = assert (ret_ty = None) in
            Some (S_Call (name, args, eqs) |> here))
    | LE_Slice (sub_le, slices) ->
        let old_le le' = LE_Slice (le', slices) |> here in
        with_temp old_le sub_le
    | LE_TupleUnpack _ -> None
    | LE_Var x ->
        if should_reduce_to_call env x then
          let name, args, eqs, ret_ty =
            annotate_call (to_pos le) env x [ e ] [] ST_Setter
          in
          let () = assert (ret_ty = None) in
          Some (S_Call (name, args, eqs) |> here)
        else None

  let annotate_func (env : env) (f : 'p AST.func) : 'p AST.func =
    let () = if false then Format.eprintf "Annotating %s.@." f.name in
    (* Build typing local environment. *)
    let env' = { env with local = empty_local } in
    let env' =
      let one_arg env' (x, ty) = add_local x ty LDK_Let env' in
      List.fold_left one_arg env' f.args
    in
    (* Add explicit parameters *)
    let env' =
      let one_param env' (x, ty_opt) =
        let ty =
          match ty_opt with
          | Some ty -> ty
          | None -> ASTUtils.underconstrained_integer
        in
        add_local x ty LDK_Let env'
      in
      List.fold_left one_param env' f.parameters
    in
    (* Add dependently typed identifiers. *)
    let add_dependently_typed_from_ty env'' ty =
      match ty.desc with
      | T_Bits (BitWidth_SingleExpr { desc = E_Var x; _ }, _) -> (
          match StaticEnv.type_of_opt env x with
          | Some { desc = T_Int None; _ } ->
              add_local x ASTUtils.underconstrained_integer LDK_Let env''
          | Some _ -> env''
          | None -> add_local x ASTUtils.underconstrained_integer LDK_Let env'')
      | _ -> env''
    in
    (* Resolve dependently typed identifiers in the arguments. *)
    let env' =
      let one_arg env'' (_, ty) = add_dependently_typed_from_ty env'' ty in
      List.fold_left one_arg env' f.args
    in
    (* Resolve dependently typed identifiers in the result type. *)
    let env' =
      match f.return_type with
      | None -> env'
      | Some { desc = T_Bits (BitWidth_SingleExpr { desc = E_Var x; _ }, _); _ }
        -> (
          match StaticEnv.type_of_opt env x with
          | Some { desc = T_Int None; _ } ->
              add_local x ASTUtils.underconstrained_integer LDK_Let env'
          | _ -> env')
      | _ -> env'
    in
    (* Annotate body *)
    let body =
      match f.body with SB_ASL body -> body | SB_Primitive _ -> assert false
    in
    let body = try_annotate_block env' f.return_type body in
    (* Optionnally rename the function if needs be *)
    let name =
      let args = List.map snd f.args in
      best_effort f.name
        (either
           (fun _ ->
             let _, name, _, _ =
               FunctionRenaming.find_name_strict dummy_annotated env f.name args
             in
             let () =
               if false then
                 Format.eprintf "Renaming decl of %s (%a) to %s.@." f.name
                   (Format.pp_print_list PP.pp_ty)
                   args name
             in
             name)
           (fun _ ->
             let _, name = FunctionRenaming.find_name env f.name args in
             name))
    in
    { f with body = SB_ASL body; name }

  let try_annotate_func env f = best_effort f (annotate_func env)

  let annotate_gsd env gsd =
    match gsd with
    | { initial_value = Some e; ty = None; _ } ->
        let t, e = annotate_expr env e in
        { gsd with initial_value = Some e; ty = Some t }
    | { initial_value = Some e; ty = Some t; _ } ->
        let t', e = annotate_expr env e in
        let+ () = check_can_be_initialized_with e env t t' in
        { gsd with initial_value = Some e; ty = Some t }
    | _ -> gsd

  let try_annotate_gsd env gsd = best_effort gsd (annotate_gsd env)
end

module TypeCheck = Annotate (struct
  let check = `TypeCheck
end)

module TypeInferWarn = Annotate (struct
  let check = `Warn
end)

module TypeInferSilence = Annotate (struct
  let check = `Silence
end)

(******************************************************************************)
(*                                                                            *)
(*                           Global env and funcs                             *)
(*                                                                            *)
(******************************************************************************)

let declare_one_func (func_sig : 'a func) env =
  let env, name' =
    try
      FunctionRenaming.add_new_func env func_sig.name func_sig.args
        func_sig.subprogram_type
    with Error.ASLException e ->
      if _warn then Error.eprintln e;
      (env, func_sig.name)
  in
  let () =
    if false then
      let open Format in
      eprintf
        "@[<hov>Adding function %s to env with@ return-type: %a@ and \
         argtypes:@ %a@."
        name' (pp_print_option PP.pp_ty) func_sig.return_type
        (pp_print_list ~pp_sep:pp_print_space PP.pp_typed_identifier)
        func_sig.args
  in
  add_subprogram name' (ast_func_to_func_sig func_sig) env

let declare_const name t v env =
  if IMap.mem name env.global.storage_types then
    Error.fatal_unknown_pos (Error.AlreadyDeclaredIdentifier name)
  else add_global_storage name t GDK_Constant env |> add_global_constant name v

let declare_type name ty s env =
  let env =
    match s with
    | None -> env
    | Some s ->
        if Types.subtype_satisfies env ty (T_Named s |> add_dummy_pos) then
          add_subtype name s env
        else
          Error.fatal_unknown_pos (Error.ConflictingTypes ([ T_Named s ], ty))
  in
  match ty.desc with
  | T_Enum ids ->
      let env = add_type name ty env in
      let t = T_Named name |> add_pos_from ty in
      let add_one_id env x =
        let v = V_Int (IMap.cardinal env.global.constants_values) in
        declare_const x t v env
      in
      List.fold_left add_one_id env ids
  | _ -> add_type name ty env

let declare_global_storage gsd env =
  let () = if false then Format.eprintf "Declaring %s@." gsd.name in
  try
    match gsd with
    | { keyword = GDK_Constant; initial_value = Some e; ty = None; name } ->
        let v = reduce_constants env e in
        let t = infer_value v |> add_pos_from e in
        declare_const name t v env
    | { keyword = GDK_Constant; initial_value = Some e; ty = Some ty; name } ->
        let v = reduce_constants env e in
        let t = infer_value v |> add_pos_from e in
        if Types.type_satisfies env t ty then declare_const name ty v env
        else conflict e [ ty.desc ] t
    | { keyword = GDK_Constant | GDK_Let; initial_value = None; _ } ->
        (* Shouldn't happen because of parser construction. *)
        Error.fatal_unknown_pos
          (Error.NotYetImplemented
             "Constants or let-bindings have to be initialized.")
    | { keyword; initial_value = None; ty = Some ty; name } ->
        (* Here keyword = GDK_Var or GDK_Config. *)
        if IMap.mem name env.global.storage_types then
          Error.fatal_unknown_pos (Error.AlreadyDeclaredIdentifier name)
        else add_global_storage name ty keyword env
    | { keyword; initial_value = Some e; ty = None; name } ->
        let t, _e = TypeInferSilence.annotate_expr env e in
        add_global_storage name t keyword env
    | { keyword; initial_value = Some e; ty = Some ty; name } ->
        let t, e = TypeInferSilence.annotate_expr env e in
        if not (Types.type_satisfies env t ty) then conflict e [ ty.desc ] t
        else add_global_storage name ty keyword env
    | { initial_value = None; ty = None; _ } ->
        (* Shouldn't happen because of parser construction. *)
        Error.fatal_unknown_pos
          (Error.NotYetImplemented
             "Global storage declaration should have an initial value or a \
              type.")
  with Error.ASLException err ->
    if _warn then Error.eprintln err;
    env

let build_global ast =
  let def = function
    | D_Func { name; _ } | D_GlobalStorage { name; _ } | D_TypeDecl (name, _, _)
      ->
        name
  in
  let use =
    let use_e e acc = ASTUtils.use_e acc e in
    let use_ty _ty acc = acc (* TODO *) in
    fun d ->
      match d with
      | D_GlobalStorage { initial_value = Some e; ty = Some ty; _ } ->
          ISet.empty |> use_e e |> use_ty ty
      | D_GlobalStorage { initial_value = None; ty = Some ty; _ } ->
          ISet.empty |> use_ty ty
      | D_GlobalStorage { initial_value = Some e; ty = None; _ } ->
          ISet.empty |> use_e e
      | D_GlobalStorage _ -> ISet.empty
      | D_TypeDecl (_, ty, s) -> use_ty ty (ISet.of_option s)
      | D_Func _ ->
          ISet.empty (* TODO: pure functions that can be used in constants? *)
  in
  let process_one_decl = function
    | D_Func func_sig -> declare_one_func func_sig
    | D_GlobalStorage gsd -> declare_global_storage gsd
    | D_TypeDecl (x, ty, s) -> declare_type x ty s
  in
  ASTUtils.dag_fold def use process_one_decl ast

let rename_primitive env (f : 'a AST.func) =
  let _, name, _, _ =
    FunctionRenaming.find_name_strict dummy_annotated env f.name
      (List.map snd f.args)
  in
  { f with name }

(******************************************************************************)
(*                                                                            *)
(*                                Entry point                                 *)
(*                                                                            *)
(******************************************************************************)

let annotate_ast (ast : 'a AST.t) env : 'a AST.t * env =
  let env = build_global ast env in
  let annotate_func f = TypeInferSilence.try_annotate_func env f in
  let one_decl d =
    match d with
    | D_Func ({ body = SB_ASL _; _ } as f) -> D_Func (annotate_func f)
    | d -> d
  in
  (List.map one_decl ast, env)

let type_check_ast strictness ast env =
  let env = build_global ast env in
  let () =
    if false then
      Format.eprintf "@[<v>Typing in env:@ %a@]@." StaticEnv.pp_env env
  in
  let annotate_func =
    match strictness with
    | `TypeCheck -> TypeCheck.annotate_func
    | `Warn -> TypeInferWarn.try_annotate_func
    | `Silence ->
        if _warn then TypeInferWarn.try_annotate_func
        else TypeInferSilence.try_annotate_func
  in
  let annotate_gsd =
    match strictness with
    | `TypeCheck -> TypeCheck.annotate_gsd
    | `Warn -> TypeInferWarn.try_annotate_gsd
    | `Silence -> TypeInferSilence.try_annotate_gsd
  in
  let annotate = function
    | D_Func ({ body = SB_ASL _; _ } as f) -> D_Func (annotate_func env f)
    | D_Func ({ body = SB_Primitive _; _ } as f) ->
        D_Func (rename_primitive env f)
    | D_GlobalStorage gsd -> D_GlobalStorage (annotate_gsd env gsd)
    | d -> d
  in
  (List.map annotate ast, env)
