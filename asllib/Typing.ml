(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

open AST
open ASTUtils
open Infix
open StaticEnv
module TypingRule = Instrumentation.TypingRule

let ( |: ) = Instrumentation.TypingNoInstr.use_with
let fatal_from = Error.fatal_from
let undefined_identifier pos x = fatal_from pos (Error.UndefinedIdentifier x)
let invalid_expr e = fatal_from e (Error.InvalidExpr e)
let unsupported_expr e = Error.fatal_from e Error.(UnsupportedExpr (Static, e))

let conflict pos expected provided =
  fatal_from pos (Error.ConflictingTypes (expected, provided))

let plus = binop PLUS
let t_bits_bitwidth e = T_Bits (e, [])

let rec list_mapi2 f i l1 l2 =
  match (l1, l2) with
  | [], [] -> []
  | a1 :: l1, a2 :: l2 ->
      let r = f i a1 a2 in
      r :: list_mapi2 f (i + 1) l1 l2
  | _, _ -> invalid_arg "List.map2"

(* Begin ReduceConstants *)
let reduce_constants env e =
  let open StaticInterpreter in
  let open StaticModel in
  let eval_expr env e =
    try static_eval env e with NotYetImplemented -> unsupported_expr e
  in
  try eval_expr env e
  with StaticEvaluationUnknown ->
    (let () =
       if false then
         Format.eprintf
           "@[<hov>Static evaluation failed. Trying to reduce.@ For %a@ at \
            %a@]@."
           PP.pp_expr e PP.pp_pos e
     in
     try StaticModel.try_normalize env e |> eval_expr env
     with StaticEvaluationUnknown -> unsupported_expr e)
    |: TypingRule.ReduceConstants
(* End *)

let reduce_to_z_opt env e =
  match (StaticModel.try_normalize env e).desc with
  | E_Literal (L_Int z) -> Some z
  | _ -> None

let reduce_constraint env = function
  | Constraint_Exact e -> Constraint_Exact (StaticModel.try_normalize env e)
  | Constraint_Range (e1, e2) ->
      Constraint_Range
        (StaticModel.try_normalize env e1, StaticModel.try_normalize env e2)

let reduce_constraints env = function
  | (UnConstrained | UnderConstrained _) as c -> c
  | WellConstrained constraints ->
      WellConstrained (List.map (reduce_constraint env) constraints)

let sum = function [] -> !$0 | [ x ] -> x | h :: t -> List.fold_left plus h t

let slices_width env =
  let minus = binop MINUS in
  let one = !$1 in
  let slice_length = function
    | Slice_Single _ -> one
    | Slice_Star (_, e) | Slice_Length (_, e) -> e
    | Slice_Range (e1, e2) -> plus one (minus e1 e2)
  in
  fun li -> List.map slice_length li |> sum |> StaticModel.try_normalize env

let width_plus env acc w = plus acc w |> StaticModel.try_normalize env

let rename_ty_eqs : env -> (AST.identifier * AST.expr) list -> AST.ty -> AST.ty
    =
  let subst_expr env eqs e =
    subst_expr eqs e |> StaticModel.try_normalize env
  in
  let subst_constraint env eqs = function
    | Constraint_Exact e -> Constraint_Exact (subst_expr env eqs e)
    | Constraint_Range (e1, e2) ->
        Constraint_Range (subst_expr env eqs e1, subst_expr env eqs e2)
  in
  let subst_constraints env eqs = List.map (subst_constraint env eqs) in
  let rec rename env eqs ty =
    match ty.desc with
    | T_Bits (e, fields) ->
        T_Bits (subst_expr env eqs e, fields) |> add_pos_from_st ty
    | T_Int (WellConstrained constraints) ->
        let constraints = subst_constraints env eqs constraints in
        T_Int (WellConstrained constraints) |> add_pos_from_st ty
    | T_Int (UnderConstrained (_uid, name)) ->
        let e = E_Var name |> add_pos_from ty |> subst_expr env eqs in
        T_Int (WellConstrained [ Constraint_Exact e ]) |> add_pos_from ty
    | T_Tuple tys -> T_Tuple (List.map (rename env eqs) tys) |> add_pos_from ty
    | _ -> ty
  in
  rename

(* Begin Lit *)
let annotate_literal = function
  | L_Int _ as v -> integer_exact' (literal v)
  | L_Bool _ -> T_Bool
  | L_Real _ -> T_Real
  | L_String _ -> T_String
  | L_BitVector bv -> Bitvector.length bv |> expr_of_int |> t_bits_bitwidth
(* End *)

let get_first_duplicate extractor li =
  let exception Duplicate of identifier in
  let folder acc elt =
    let x = extractor elt in
    let acc' = ISet.add x acc in
    if acc' == acc then raise (Duplicate x) else acc'
  in
  try
    let _ = List.fold_left folder ISet.empty li in
    None
  with Duplicate x -> Some x

(** [set_filter_map f set] is the list of [y] such that [f x = Some y] for all
    elements [x] of [set]. *)
let set_filter_map f set =
  let folder e acc = match f e with None -> acc | Some x -> x :: acc in
  ISet.fold folder set []

(* ---------------------------------------------------------------------------

                              Properties handling

   ---------------------------------------------------------------------------*)

type strictness = [ `Silence | `Warn | `TypeCheck ]

module type ANNOTATE_CONFIG = sig
  val check : strictness
end

module Property (C : ANNOTATE_CONFIG) = struct
  exception TypingAssumptionFailed

  type ('a, 'b) property = 'a -> 'b
  type prop = (unit, unit) property

  let strictness_string =
    match C.check with
    | `TypeCheck -> "type-checking-strict"
    | `Warn -> "type-checking-warn"
    | `Silence -> "type-inference"

  let check : prop -> prop =
    match C.check with
    | `TypeCheck -> fun f () -> f ()
    | `Warn -> (
        fun f () -> try f () with Error.ASLException e -> Error.eprintln e)
    | `Silence -> fun _f () -> ()

  let best_effort' : ('a, 'a) property -> ('a, 'a) property =
    match C.check with
    | `TypeCheck -> fun f x -> f x
    | `Warn -> (
        fun f x ->
          try f x
          with Error.ASLException e ->
            Error.eprintln e;
            x)
    | `Silence -> ( fun f x -> try f x with Error.ASLException _ -> x)

  let best_effort : 'a -> ('a, 'a) property -> 'a = fun x f -> best_effort' f x
  let[@inline] ( let+ ) m f = check m () |> f

  let[@inline] both (p1 : prop) (p2 : prop) () =
    let () = p1 () in
    let () = p2 () in
    ()

  let either (p1 : ('a, 'b) property) (p2 : ('a, 'b) property) x =
    try p1 x with TypingAssumptionFailed | Error.ASLException _ -> p2 x

  let rec any (li : prop list) : prop =
    match li with
    | [] -> raise (Invalid_argument "any")
    | [ f ] -> f
    | p :: li -> either p (any li)

  let assumption_failed () = raise TypingAssumptionFailed [@@inline]
  let ok () = () [@@inline]
  let check_true b fail () = if b then () else fail () [@@inline]
  let check_true' b = check_true b assumption_failed [@@inline]
end

(* -------------------------------------------------------------------------

                        Functional polymorphism

   ------------------------------------------------------------------------- *)

module FunctionRenaming (C : ANNOTATE_CONFIG) = struct
  open Property (C)

  (* Begin HasArgClash *)
  (* Returns true iff type lists type-clash element-wise. *)
  let has_arg_clash env caller callee =
    List.compare_lengths caller callee == 0
    && List.for_all2
         (fun t_caller (_, t_callee) ->
           Types.type_clashes env t_caller t_callee)
         caller callee
       |: TypingRule.HasArgClash
  (* End *)

  (* Return true if two subprogram are forbidden with the same argument types. *)
  let has_subprogram_type_clash s1 s2 =
    match (s1, s2) with
    | ST_Getter, ST_Setter
    | ST_Setter, ST_Getter
    | ST_EmptyGetter, ST_EmptySetter
    | ST_EmptySetter, ST_EmptyGetter ->
        false
    | _ -> true

  (* Deduce renamings from match between calling and callee types. *)
  let deduce_eqs env =
    (* Here we assume [has_arg_clash env caller callee] *)
    (* Thus [List.length caller == List.length callee]. *)
    let folder prev_eqs caller (_name, callee) =
      match callee.desc with
      | T_Bits ({ desc = E_Var x; _ }, _) -> (
          match (Types.get_structure env caller).desc with
          | T_Bits (e_caller, _) -> (x, e_caller) :: prev_eqs
          | _ ->
              (* We know that callee type_clashes with caller, and that it
                 cannot be a name. *)
              assert false)
      | _ -> prev_eqs
    in
    List.fold_left2 folder []

  (* Begin AddNewFunc *)
  let add_new_func loc env name formals subpgm_type =
    match IMap.find_opt name env.global.subprogram_renamings with
    | None ->
        let new_env = set_renamings name (ISet.singleton name) env in
        (new_env, name)
    | Some other_names ->
        let new_name = name ^ "-" ^ string_of_int (ISet.cardinal other_names) in
        let clash =
          let formal_types = List.map snd formals in
          (not (ISet.is_empty other_names))
          && ISet.exists
               (fun name' ->
                 let other_func_sig = IMap.find name' env.global.subprograms in
                 has_subprogram_type_clash subpgm_type
                   other_func_sig.subprogram_type
                 && has_arg_clash env formal_types other_func_sig.args)
               other_names
        in
        let+ () =
         fun () ->
          if clash then
            let () =
              if false then
                Format.eprintf
                  "Function %s@[(%a)@] is declared multiple times.@." name
                  Format.(
                    pp_print_list
                      ~pp_sep:(fun f () -> fprintf f ",@ ")
                      PP.pp_typed_identifier)
                  formals
            in
            Error.fatal_from loc (Error.AlreadyDeclaredIdentifier name)
        in
        let new_env = set_renamings name (ISet.add new_name other_names) env in
        (new_env, new_name) |: TypingRule.AddNewFunc
  (* End *)

  (* Begin SubprogramForName *)
  let subprogram_for_name loc env name caller_arg_types =
    let () =
      if false then Format.eprintf "Trying to rename call to %S@." name
    in
    let renaming_set =
      try IMap.find name env.global.subprogram_renamings
      with Not_found -> undefined_identifier loc name
    in
    let get_func_sig name' =
      match IMap.find_opt name' env.global.subprograms with
      | Some func_sig when has_arg_clash env caller_arg_types func_sig.args ->
          Some (name', func_sig)
      | _ -> None
    in
    let matching_renamings = set_filter_map get_func_sig renaming_set in
    match matching_renamings with
    | [ (name', func_sig) ] ->
        (deduce_eqs env caller_arg_types func_sig.args, name', func_sig)
        |: TypingRule.SubprogramForName
    | [] -> fatal_from loc (Error.NoCallCandidate (name, caller_arg_types))
    | _ :: _ ->
        fatal_from loc (Error.TooManyCallCandidates (name, caller_arg_types))
  (* End *)

  let try_subprogram_for_name =
    match C.check with
    | `TypeCheck -> subprogram_for_name
    | `Warn | `Silence -> (
        fun loc env name caller_arg_types ->
          try subprogram_for_name loc env name caller_arg_types
          with Error.ASLException _ as error -> (
            try
              match IMap.find_opt name env.global.subprograms with
              | None -> undefined_identifier loc ("function " ^ name)
              | Some func_sig ->
                  if false then
                    Format.eprintf "@[<2>%a:@ No extra arguments for %s@]@."
                      PP.pp_pos loc name;
                  ([], name, func_sig)
            with Error.ASLException _ -> raise error))
end

(* ---------------------------------------------------------------------------

                           Main type-checking module

   ---------------------------------------------------------------------------*)

module Annotate (C : ANNOTATE_CONFIG) = struct
  open Property (C)
  module Fn = FunctionRenaming (C)

  let should_reduce_to_call env name st =
    match IMap.find_opt name env.global.subprogram_renamings with
    | None -> false
    | Some set ->
        ISet.exists
          (fun name' ->
            match IMap.find_opt name' env.global.subprograms with
            | None -> assert false
            | Some func_sig -> func_sig.subprogram_type = st)
          set

  let disjoint_slices_to_diet loc env slices =
    let eval env e =
      match reduce_constants env e with
      | L_Int z -> Z.to_int z
      | _ -> fatal_from e Error.(UnsupportedExpr (Static, e))
    in
    let module DI = Diet.Int in
    let one_slice loc env diet slice =
      let interval =
        let make x y =
          if x > y then fatal_from loc @@ Error.OverlappingSlices [ slice ]
          else DI.Interval.make x y
        in
        match slice with
        | Slice_Single e ->
            let x = eval env e in
            make x x
        | Slice_Range (e1, e2) ->
            let x = eval env e2 and y = eval env e1 in
            make x y
        | Slice_Length (e1, e2) ->
            let x = eval env e1 and y = eval env e2 in
            make x (x + y - 1)
        | Slice_Star (e1, e2) ->
            let x = eval env e1 and y = eval env e2 in
            make (x * y) ((x * (y + 1)) - 1)
      in
      let new_diet = DI.add interval DI.empty in
      if DI.is_empty (Diet.Int.inter new_diet diet) then DI.add interval diet
      else fatal_from loc Error.(OverlappingSlices slices)
    in
    List.fold_left (one_slice loc env) Diet.Int.empty slices

  let check_disjoint_slices loc env slices =
    if List.length slices <= 1 then ok
    else fun () ->
      let _ = disjoint_slices_to_diet loc env slices in
      ()

  exception NoSingleField

  let to_singles env =
    let eval e =
      match reduce_constants env e with
      | L_Int z -> Z.to_int z
      | _ -> raise NoSingleField
    in
    let one slice k =
      match slice with
      | Slice_Single e -> e :: k
      | Slice_Length (e1, e2) ->
          let i1 = eval e1 and i2 = eval e2 in
          let rec do_rec n =
            if n >= i2 then k
            else
              let e = E_Literal (L_Int (Z.of_int (i1 + n))) |> add_dummy_pos in
              e :: do_rec (n + 1)
          in
          do_rec 0
      | Slice_Range (e1, e2) ->
          let i1 = eval e1 and i2 = eval e2 in
          let rec do_rec i =
            if i > i1 then k
            else
              let e = E_Literal (L_Int (Z.of_int i)) |> add_dummy_pos in
              e :: do_rec (i + 1)
          in
          do_rec i2
      | Slice_Star _ -> raise NoSingleField
    in
    fun slices -> List.fold_right one slices []

  let slices_of_bitfield = function
    | BitField_Simple (_, slices)
    | BitField_Nested (_, slices, _)
    | BitField_Type (_, slices, _) ->
        slices

  let field_to_single env bf field =
    match find_bitfield_opt field bf with
    | Some bitfield -> to_singles env (slices_of_bitfield bitfield)
    | None -> raise NoSingleField

  let should_fields_reduce_to_call env name ty fields =
    match (Types.make_anonymous env ty).desc with
    | T_Bits (_, bf) -> (
        try Some (name, list_concat_map (field_to_single env bf) fields)
        with NoSingleField -> None)
    | _ -> None

  let should_field_reduce_to_call env name ty field =
    should_fields_reduce_to_call env name ty [ field ]

  (* -------------------------------------------------------------------------

                              Annotate AST

     -------------------------------------------------------------------------- *)

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

  let get_bitvector_const_width loc env t =
    let e_width = get_bitvector_width loc env t in
    match reduce_constants env e_width with
    | L_Int z -> Z.to_int z
    | _ -> assert false

  (** [check_type_satisfies t1 t2] if [t1 <: t2]. *)
  let check_type_satisfies loc env t1 t2 () =
    let () =
      if false then
        Format.eprintf "@[<hv 2>Checking %a@ <: %a@]@." PP.pp_ty t1 PP.pp_ty t2
    in
    if Types.type_satisfies env t1 t2 then () else conflict loc [ t2.desc ] t1

  (* CheckStructureBoolean *)

  (** [check_structure_boolean env t1] checks that [t1] has the structure of a boolean. *)
  let check_structure_boolean loc env t1 () =
    match (Types.get_structure env t1).desc with
    | T_Bool -> ()
    | _ -> conflict loc [ T_Bool ] t1
  (* End *)

  (* CheckStructureBits *)
  let check_structure_bits loc env t () =
    match (Types.get_structure env t).desc with
    | T_Bits _ -> ()
    | _ -> conflict loc [ default_t_bits ] t
  (* End *)

  (* Begin CheckStructureInteger *)
  let check_structure_integer loc env t () =
    let () =
      if false then
        Format.eprintf "Checking that %a is an integer.@." PP.pp_ty t
    in
    match (Types.make_anonymous env t).desc with
    | T_Int _ -> ()
    | _ -> conflict loc [ integer' ] t
  (* End *)

  (* Begin CheckConstrainedInteger *)
  let check_constrained_integer ~loc env t () =
    match (Types.make_anonymous env t).desc with
    | T_Int UnConstrained -> fatal_from loc Error.(ConstrainedIntegerExpected t)
    | T_Int (WellConstrained _ | UnderConstrained _) -> ()
    | _ -> conflict loc [ integer' ] t
  (* End *)

  (* Begin CheckStructureException *)
  let check_structure_exception loc env t () =
    let t_struct = Types.get_structure env t in
    match t_struct.desc with
    | T_Exception _ -> ()
    | _ -> conflict loc [ T_Exception [] ] t_struct
  (* End *)

  (* Begin StorageIsPure *)
  let storage_is_pure ~loc (env : env) s =
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
        | None ->
            if
              IMap.mem s env.global.subprograms
              || IMap.mem s env.global.declared_types
            then false
            else undefined_identifier loc s)
  (* End *)

  (* Begin CheckStaticallyEvaluable *)
  let is_statically_evaluable ~loc env e =
    let use_set = use_e e ISet.empty in
    ISet.for_all (storage_is_pure ~loc env) use_set

  let check_statically_evaluable (env : env) e () =
    if is_statically_evaluable ~loc:e env e then ()
    else fatal_from e (Error.UnpureExpression e)
  (* End *)

  let check_bits_equal_width' env t1 t2 () =
    let n = get_bitvector_width' env t1 and m = get_bitvector_width' env t2 in
    if bitwidth_equal (StaticModel.equal_in_env env) n m then ()
    else assumption_failed ()

  (* Begin CheckBitsEqualWidth *)
  let check_bits_equal_width loc env t1 t2 () =
    try check_bits_equal_width' env t1 t2 ()
    with TypingAssumptionFailed ->
      fatal_from loc (Error.UnreconciliableTypes (t1, t2))
  (* End *)

  let has_bitvector_structure env t =
    match (Types.get_structure env t).desc with T_Bits _ -> true | _ -> false

  let expr_is_strict_positive e =
    match e.desc with
    | E_Literal (L_Int i) -> Z.sign i = 1
    | E_Var _n -> false
    | _ -> fatal_from e Error.(UnsupportedExpr (Static, e))

  let constraint_is_strict_positive = function
    | Constraint_Exact e | Constraint_Range (e, _) -> expr_is_strict_positive e

  let constraints_is_strict_positive =
    List.for_all constraint_is_strict_positive

  let expr_is_non_negative e =
    match e.desc with
    | E_Literal (L_Int i) -> Z.sign i != -1
    | E_Var _n -> false
    | _ -> fatal_from e Error.(UnsupportedExpr (Static, e))

  let constraint_is_non_negative = function
    | Constraint_Exact e | Constraint_Range (e, _) -> expr_is_non_negative e

  let constraints_is_non_negative = List.for_all constraint_is_non_negative

  let binop_is_exploding = function
    | MUL | SHL | POW -> true
    | PLUS | DIV | MINUS | MOD | SHR | DIVRM -> false
    | AND | BAND | BEQ | BOR | EOR | EQ_OP | GT | GEQ | IMPL | LT | LEQ | NEQ
    | OR | RDIV ->
        assert false

  let explode_intervals =
    let rec make_interval ~loc acc a b =
      if Z.leq a b then
        let eb = E_Literal (L_Int b) |> add_pos_from loc in
        let acc' = Constraint_Exact eb :: acc in
        make_interval ~loc acc' a (Z.pred b)
      else acc
    in
    let[@warning "-44"] interval_is_too_big a b =
      let open Z in
      let max_interval_size = ~$1 lsl 14 in
      Compare.(abs (a - b) > max_interval_size)
    in
    let explode_constraint env = function
      | Constraint_Exact _ as c -> [ c ]
      | Constraint_Range (a, b) as c -> (
          match (reduce_to_z_opt env a, reduce_to_z_opt env b) with
          | Some za, Some zb ->
              if interval_is_too_big za zb then
                let () =
                  Format.eprintf
                    "@[Interval too large: @[<h>[ %a .. %a ]@].@ Keeping it as \
                     an interval.@]@."
                    Z.pp_print za Z.pp_print zb
                in
                [ c ]
              else make_interval [] ~loc:b za zb
          | _ -> [ c ])
    in
    fun env -> list_concat_map (explode_constraint env)

  let constraint_binop env op cs1 cs2 =
    let cs1, cs2 =
      if binop_is_exploding op then
        (explode_intervals env cs1, explode_intervals env cs2)
      else (cs1, cs2)
    in
    let res = constraint_binop op cs1 cs2 |> reduce_constraints env in
    let () =
      if false then
        Format.eprintf
          "Reduction of binop %s@ on@ constraints@ %a@ and@ %a@ gave@ %a@."
          (PP.binop_to_string op) PP.pp_int_constraints cs1
          PP.pp_int_constraints cs2 PP.pp_ty
          (T_Int res |> add_dummy_pos)
    in
    res

  (* Begin TypeOfArrayLength *)
  let type_of_array_length ~loc = function
    | ArrayLength_Enum (s, _) -> T_Named s |> add_pos_from loc
    | ArrayLength_Expr _ -> integer |: TypingRule.TypeOfArrayLength
  (* End *)

  (* Begin CheckBinop *)
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
            let+ () = check_type_satisfies' env t1 boolean in
            let+ () = check_type_satisfies' env t2 boolean in
            T_Bool |> with_loc
        | AND | OR | EOR (* when has_bitvector_structure env t1 ? *) ->
            (* Rule KXMR: If the operands of a primitive operation are
               bitvectors, the widths of the operands must be equivalent
               statically evaluable expressions. *)
            let+ () = check_bits_equal_width' env t1 t2 in
            let w = get_bitvector_width' env t1 in
            T_Bits (w, []) |> with_loc
        | (PLUS | MINUS) when has_bitvector_structure env t1 ->
            (* Rule KXMR: If the operands of a primitive operation are
               bitvectors, the widths of the operands must be equivalent
               statically evaluable expressions. *)
            let+ () =
              either
                (check_bits_equal_width' env t1 t2)
                (check_type_satisfies' env t2 integer)
            in
            let w = get_bitvector_width' env t1 in
            T_Bits (w, []) |> with_loc
        | EQ_OP | NEQ ->
            (* Wrong! *)
            let t1_anon = Types.make_anonymous env t1
            and t2_anon = Types.make_anonymous env t2 in
            let+ () =
              any
                [
                  (* If an argument of a comparison operation is a
                     constrained integer then it is treated as an
                     unconstrained integer. *)
                  both
                    (check_type_satisfies' env t1_anon integer)
                    (check_type_satisfies' env t2_anon integer);
                  (* If the arguments of a comparison operation are
                     bitvectors then they must have the same determined
                     width. *)
                  check_bits_equal_width' env t1_anon t2_anon;
                  both
                    (check_type_satisfies' env t1_anon boolean)
                    (check_type_satisfies' env t2_anon boolean);
                  both
                    (check_type_satisfies' env t1_anon real)
                    (check_type_satisfies' env t2_anon real);
                  both
                    (check_type_satisfies' env t1_anon string)
                    (check_type_satisfies' env t2_anon string);
                  (fun () ->
                    match (t1_anon.desc, t2_anon.desc) with
                    | T_Enum li1, T_Enum li2 ->
                        check_true' (list_equal String.equal li1 li2) ()
                    | _ -> assumption_failed ());
                ]
            in
            T_Bool |> with_loc
        | LEQ | GEQ | GT | LT ->
            let+ () =
              either
                (both
                   (check_type_satisfies' env t1 integer)
                   (check_type_satisfies' env t2 integer))
                (both
                   (check_type_satisfies' env t1 real)
                   (check_type_satisfies' env t2 real))
            in
            T_Bool |> with_loc
        | MUL | DIV | DIVRM | MOD | SHL | SHR | POW | PLUS | MINUS -> (
            let struct1 = Types.get_well_constrained_structure env t1
            and struct2 = Types.get_well_constrained_structure env t2 in
            match (struct1.desc, struct2.desc) with
            | T_Int UnConstrained, T_Int _ | T_Int _, T_Int UnConstrained ->
                (* Rule ZYWY: If both operands of an integer binary primitive
                   operator are integers and at least one of them is an
                   unconstrained integer then the result shall be an
                   unconstrained integer. *)
                T_Int UnConstrained |> with_loc
            | T_Int (UnderConstrained _), _ | _, T_Int (UnderConstrained _) ->
                assert false (* We used to_well_constrained before *)
            | T_Int (WellConstrained cs1), T_Int (WellConstrained cs2) ->
                (* Rule KFYS: If both operands of an integer binary primitive
                   operation are well-constrained integers, then it shall
                   return a constrained integer whose constraint is calculated
                   by applying the operation to all possible value pairs. *)
                let+ () =
                  match op with
                  | DIV ->
                      (* TODO cs1 divides cs2 ? How is it expressable in term of constraints? *)
                      check_true' (constraints_is_strict_positive cs2)
                  | DIVRM | MOD ->
                      (* assert cs2 strict-positive *)
                      check_true' (constraints_is_strict_positive cs2)
                  | SHL | SHR ->
                      (* assert cs2 non-negative *)
                      check_true' (constraints_is_non_negative cs2)
                  | _ -> fun () -> ()
                in
                let cs =
                  best_effort UnConstrained (fun _ ->
                      constraint_binop env op cs1 cs2)
                in
                T_Int cs |> with_loc
            | T_Real, T_Real -> (
                match op with
                | PLUS | MINUS | MUL -> T_Real |> with_loc
                | _ -> assumption_failed ())
            | T_Real, T_Int _ -> (
                match op with
                | POW -> T_Real |> with_loc
                | _ -> assumption_failed ())
            | _ -> assumption_failed ())
        | RDIV ->
            let+ () =
              both
                (check_type_satisfies' env t1 real)
                (check_type_satisfies' env t2 real)
            in
            T_Real |> with_loc)
      (fun () -> fatal_from loc (Error.BadTypesForBinop (op, t1, t2)))
      ()
    |: TypingRule.CheckBinop
  (* End *)

  (* Begin CheckUnop *)
  let check_unop loc env op t1 =
    match op with
    | BNOT ->
        let+ () = check_type_satisfies loc env t1 boolean in
        T_Bool |> add_pos_from loc
    | NEG -> (
        let+ () =
          either
            (check_type_satisfies loc env t1 integer)
            (check_type_satisfies loc env t1 real)
        in
        let struct1 = Types.get_well_constrained_structure env t1 in
        match struct1.desc with
        | T_Int UnConstrained -> T_Int UnConstrained |> add_pos_from loc
        | T_Int (WellConstrained cs) ->
            let neg e = E_Unop (NEG, e) |> add_pos_from e in
            let constraint_minus = function
              | Constraint_Exact e -> Constraint_Exact (neg e)
              | Constraint_Range (top, bot) ->
                  Constraint_Range (neg bot, neg top)
            in
            T_Int (WellConstrained (List.map constraint_minus cs))
            |> add_pos_from loc
        | T_Int (UnderConstrained _) ->
            assert false (* We used to_well_constrained just before. *)
        | _ -> (* fail case *) t1)
    | NOT ->
        let+ () = check_structure_bits loc env t1 in
        t1 |: TypingRule.CheckUnop
  (* End *)

  let var_in_env ?(local = true) env x =
    (local && IMap.mem x env.local.storage_types)
    || IMap.mem x env.global.storage_types
    || IMap.mem x env.global.subprograms
    || IMap.mem x env.global.declared_types

  let check_var_not_in_env ?(local = true) loc env x () =
    if var_in_env ~local env x then
      fatal_from loc (Error.AlreadyDeclaredIdentifier x)
    else ()

  let check_var_not_in_genv loc = check_var_not_in_env ~local:false loc

  let get_variable_enum' env e =
    match e.desc with
    | E_Var x -> (
        match IMap.find_opt x env.global.declared_types with
        | Some t -> (
            match (Types.make_anonymous env t).desc with
            | T_Enum li -> Some (x, List.length li)
            | _ -> None)
        | None -> None)
    | _ -> None

  let check_diet_in_width loc slices width diet () =
    let x = Diet.Int.min_elt diet |> Diet.Int.Interval.x
    and y = Diet.Int.max_elt diet |> Diet.Int.Interval.y in
    if 0 <= x && y < width then ()
    else fatal_from loc (BadSlices (Error.Static, slices, width))

  let check_slices_in_width loc env width slices () =
    let diet = disjoint_slices_to_diet loc env slices in
    check_diet_in_width loc slices width diet ()

  (* Begin TBitField *)
  let rec annotate_bitfield ~loc env width bitfield : bitfield =
    match bitfield with
    | BitField_Simple (name, slices) ->
        let slices1 = annotate_slices env slices in
        let+ () = check_slices_in_width loc env width slices1 in
        BitField_Simple (name, slices1) |: TypingRule.TBitField
    | BitField_Nested (name, slices, bitfields') ->
        let slices1 = annotate_slices env slices in
        let diet = disjoint_slices_to_diet loc env slices1 in
        let+ () = check_diet_in_width loc slices1 width diet in
        let width' = Diet.Int.cardinal diet |> expr_of_int in
        let bitfields'' = annotate_bitfields ~loc env width' bitfields' in
        BitField_Nested (name, slices1, bitfields'') |: TypingRule.TBitField
    | BitField_Type (name, slices, ty) ->
        let ty' = annotate_type ~loc env ty in
        let slices1 = annotate_slices env slices in
        let diet = disjoint_slices_to_diet loc env slices1 in
        let+ () = check_diet_in_width loc slices1 width diet in
        let width' = Diet.Int.cardinal diet |> expr_of_int in
        let+ () =
          t_bits_bitwidth width' |> add_dummy_pos
          |> check_bits_equal_width loc env ty
        in
        BitField_Type (name, slices1, ty') |: TypingRule.TBitField
  (* End *)

  (* Begin TBitFields *)
  and annotate_bitfields ~loc env e_width bitfields =
    let+ () =
      match get_first_duplicate bitfield_get_name bitfields with
      | None -> ok
      | Some x -> fun () -> fatal_from loc (Error.AlreadyDeclaredIdentifier x)
    in
    let width =
      let v = reduce_constants env e_width in
      match v with L_Int i -> Z.to_int i | _ -> assert false
    in
    List.map (annotate_bitfield ~loc env width) bitfields
    |: TypingRule.TBitFields
  (* End *)

  and annotate_type ?(decl = false) ~(loc : 'a annotated) env ty : ty =
    let () =
      if false then
        Format.eprintf "Annotating@ %a@ in env:@ %a@." PP.pp_ty ty
          StaticEnv.pp_env env
    in
    let here t = add_pos_from ty t in
    best_effort ty @@ fun _ ->
    match ty.desc with
    (* Begin TString *)
    | T_String -> ty |: TypingRule.TString
    (* Begin TReal *)
    | T_Real -> ty |: TypingRule.TReal
    (* Begin TBool *)
    | T_Bool -> ty |: TypingRule.TBool
    (* Begin TNamed *)
    | T_Named x ->
        let+ () =
          if IMap.mem x env.global.declared_types then ok
          else fun () -> undefined_identifier loc x
        in
        ty |: TypingRule.TNamed
    (* Begin TInt *)
    | T_Int constraints ->
        (match constraints with
        | WellConstrained constraints ->
            let new_constraints =
              List.map (annotate_constraint ~loc env) constraints
            in
            T_Int (WellConstrained new_constraints) |> here
        | UnderConstrained _ | UnConstrained -> ty)
        |: TypingRule.TInt
    (* Begin TBits *)
    | T_Bits (e_width, bitfields) ->
        let e_width' = annotate_static_constrained_integer ~loc env e_width in
        let bitfields' =
          if bitfields = [] then bitfields
          else annotate_bitfields ~loc env e_width' bitfields
        in
        T_Bits (e_width', bitfields') |> here |: TypingRule.TBits
    (* Begin TTuple *)
    | T_Tuple tys ->
        let tys' = List.map (annotate_type ~loc env) tys in
        T_Tuple tys' |> here |: TypingRule.TTuple
    (* Begin TArray *)
    | T_Array (index, t) ->
        let t' = annotate_type ~loc env t
        and index' =
          match index with
          | ArrayLength_Expr e -> (
              match get_variable_enum' env e with
              | Some (s, i) -> ArrayLength_Enum (s, i)
              | None ->
                  let e' = annotate_static_integer ~loc env e in
                  ArrayLength_Expr e')
          | ArrayLength_Enum (s, i) -> (
              let t_s = T_Named s |> here in
              match (Types.make_anonymous env t_s).desc with
              | T_Enum li when List.length li = i -> index
              | _ -> conflict loc [ T_Enum [] ] t_s)
        in
        T_Array (index', t') |> here |: TypingRule.TArray
    (* Begin TRecordExceptionDecl *)
    | (T_Record fields | T_Exception fields) when decl -> (
        let+ () =
          match get_first_duplicate fst fields with
          | None -> ok
          | Some x ->
              fun () -> fatal_from loc (Error.AlreadyDeclaredIdentifier x)
        in
        let fields' =
          List.map (fun (x, ty) -> (x, annotate_type ~loc env ty)) fields
        in
        match ty.desc with
        | T_Record _ ->
            T_Record fields' |> here |: TypingRule.TRecordExceptionDecl
        | T_Exception _ ->
            T_Exception fields' |> here |: TypingRule.TRecordExceptionDecl
        | _ -> assert false
        (* Begin TEnumDecl *))
    | T_Enum li when decl ->
        let+ () =
          match get_first_duplicate Fun.id li with
          | None -> ok
          | Some x ->
              fun () -> fatal_from loc (Error.AlreadyDeclaredIdentifier x)
        in
        let+ () =
         fun () -> List.iter (fun s -> check_var_not_in_genv ty env s ()) li
        in
        ty |: TypingRule.TEnumDecl
        (* Begin TNonDecl *)
    | T_Enum _ | T_Record _ | T_Exception _ ->
        if decl then assert false
        else
          fatal_from loc
            (Error.NotYetImplemented
               " Cannot use non anonymous form of enumerations, record, or \
                exception here.")
          |: TypingRule.TNonDecl
  (* End *)

  and annotate_static_integer ~(loc : 'a annotated) env e =
    let t, e' = annotate_expr env e in
    let+ () = check_structure_integer loc env t in
    let+ () = check_statically_evaluable env e in
    StaticModel.try_normalize env e'

  (* Begin StaticConstrainedInteger *)
  and annotate_static_constrained_integer ~(loc : 'a annotated) env e =
    let t, e' = annotate_expr env e in
    let+ () = check_constrained_integer ~loc env t in
    let+ () = check_statically_evaluable env e in
    StaticModel.try_normalize env e'
  (* End *)

  and annotate_constraint ~loc env = function
    | Constraint_Exact e ->
        let e' = annotate_static_constrained_integer ~loc env e in
        Constraint_Exact e'
    | Constraint_Range (e1, e2) ->
        let e1' = annotate_static_constrained_integer ~loc env e1
        and e2' = annotate_static_constrained_integer ~loc env e2 in
        Constraint_Range (e1', e2')

  and annotate_slices env =
    (* Rules:
       - Rule WZCS: The width of a bitslice must be any non-negative,
         statically evaluable integer expression (including zero).
    *)
    let rec tr_one s =
      let () =
        if false then
          Format.eprintf "Annotating slice %a@." PP.pp_slice_list [ s ]
      in
      match s with
      (* Begin SliceSingle *)
      | Slice_Single i ->
          (* LRM R_GXKG:
             The notation b[i] is syntactic sugar for b[i +: 1].
          *)
          tr_one (Slice_Length (i, !$1)) |: TypingRule.SliceSingle
      (* End *)
      (* Begin SliceLength *)
      | Slice_Length (offset, length) ->
          let t_offset, offset' = annotate_expr env offset
          and length' =
            annotate_static_constrained_integer ~loc:(to_pos length) env length
          in
          let+ () = check_structure_integer offset' env t_offset in
          Slice_Length (offset', length') |: TypingRule.SliceLength
      (* End *)
      (* Begin SliceRange *)
      | Slice_Range (j, i) ->
          (* LRM R_GXKG:
             The notation b[j:i] is syntactic sugar for b[i +: j-i+1].
          *)
          let pre_length = binop MINUS j i |> binop PLUS !$1 in
          tr_one (Slice_Length (i, pre_length)) |: TypingRule.SliceRange
      (* End *)
      (* Begin SliceStar *)
      | Slice_Star (factor, pre_length) ->
          (* LRM R_GXQG:
             The notation b[i *: n] is syntactic sugar for b[i*n +: n]
          *)
          let pre_offset = binop MUL factor pre_length in
          tr_one (Slice_Length (pre_offset, pre_length)) |: TypingRule.SliceStar
      (* End *)
    in
    List.map tr_one

  and annotate_pattern loc env t = function
    (* Begin PAll *)
    | Pattern_All as p -> p |: TypingRule.PAll
    (* End *)
    (* Begin PAny *)
    | Pattern_Any li ->
        let new_li = List.map (annotate_pattern loc env t) li in
        Pattern_Any new_li |: TypingRule.PAny
    (* End *)
    (* Begin PNot *)
    | Pattern_Not q ->
        let new_q = annotate_pattern loc env t q in
        Pattern_Not new_q |: TypingRule.PNot
    (* End *)
    (* Begin PSingle *)
    | Pattern_Single e ->
        let t_e, e' = annotate_expr env e in
        let+ () =
         fun () ->
          let t_struct = Types.make_anonymous env t
          and t_e_struct = Types.make_anonymous env t_e in
          match (t_struct.desc, t_e_struct.desc) with
          | T_Bool, T_Bool | T_Real, T_Real | T_Int _, T_Int _ -> ()
          | T_Bits _, T_Bits _ ->
              check_bits_equal_width loc env t_struct t_e_struct ()
          | T_Enum li1, T_Enum li2 when list_equal String.equal li1 li2 -> ()
          | _ -> fatal_from loc (Error.BadTypesForBinop (EQ_OP, t, t_e))
        in
        Pattern_Single e' |: TypingRule.PSingle
    (* End *)
    (* Begin PGeq *)
    | Pattern_Geq e ->
        let t_e, e' = annotate_expr env e in
        let+ () = check_statically_evaluable env e' in
        let+ () =
         fun () ->
          let t_struct = Types.get_structure env t
          and t_e_struct = Types.get_structure env t_e in
          match (t_struct.desc, t_e_struct.desc) with
          | T_Real, T_Real | T_Int _, T_Int _ -> ()
          | _ -> fatal_from loc (Error.BadTypesForBinop (GEQ, t, t_e))
        in
        Pattern_Geq e' |: TypingRule.PGeq
    (* End *)
    (* Begin PLeq *)
    | Pattern_Leq e ->
        let t_e, e' = annotate_expr env e in
        let+ () = check_statically_evaluable env e' in
        let+ () =
         fun () ->
          let t_anon = Types.make_anonymous env t
          and t_e_anon = Types.make_anonymous env t_e in
          match (t_anon.desc, t_e_anon.desc) with
          | T_Real, T_Real | T_Int _, T_Int _ -> ()
          | _ -> fatal_from loc (Error.BadTypesForBinop (LEQ, t, t_e))
        in
        Pattern_Leq e' |: TypingRule.PLeq
    (* End *)
    (* Begin PRange *)
    | Pattern_Range (e1, e2) ->
        let t_e1, e1' = annotate_expr env e1
        and t_e2, e2' = annotate_expr env e2 in
        let+ () =
         fun () ->
          let t_anon = Types.make_anonymous env t
          and t_e1_anon = Types.make_anonymous env t_e1
          and t_e2_anon = Types.make_anonymous env t_e2 in
          match (t_anon.desc, t_e1_anon.desc, t_e2_anon.desc) with
          | T_Real, T_Real, T_Real | T_Int _, T_Int _, T_Int _ -> ()
          | _, T_Int _, T_Int _ | _, T_Real, T_Real ->
              fatal_from loc (Error.BadTypesForBinop (GEQ, t, t_e1))
          | _ -> fatal_from loc (Error.BadTypesForBinop (GEQ, t_e1, t_e2))
        in
        Pattern_Range (e1', e2') |: TypingRule.PRange
    (* End *)
    (* Begin PMask *)
    | Pattern_Mask m as p ->
        let+ () = check_structure_bits loc env t in
        let+ () =
          let n = !$(Bitvector.mask_length m) in
          let t_m = T_Bits (n, []) |> add_pos_from loc in
          check_type_satisfies loc env t t_m
        in
        p |: TypingRule.PMask
    (* End *)
    (* Begin PTuple *)
    | Pattern_Tuple li -> (
        let t_struct = Types.get_structure env t in
        match t_struct.desc with
        | T_Tuple ts when List.compare_lengths li ts != 0 ->
            Error.fatal_from loc
              (Error.BadArity
                 ("pattern matching on tuples", List.length li, List.length ts))
        | T_Tuple ts ->
            let new_li = List.map2 (annotate_pattern loc env) ts li in
            Pattern_Tuple new_li |: TypingRule.PTuple
        | _ -> conflict loc [ T_Tuple [] ] t
        (* End *))

  and annotate_call loc env name args eqs call_type =
    let () = assert (List.length eqs == 0) in
    (* Begin FindCheckDeduce *)
    let () =
      if false then
        Format.eprintf "Annotating call to %S (%s) at %a.@." name
          (Serialize.subprogram_type_to_string call_type)
          PP.pp_pos loc
    in
    let caller_arg_typed = List.map (annotate_expr env) args in
    annotate_call_arg_typed loc env name caller_arg_typed call_type

  and annotate_call_arg_typed loc env name caller_arg_typed call_type =
    let caller_arg_types, args1 = List.split caller_arg_typed in
    let eqs1, name1, callee =
      Fn.try_subprogram_for_name loc env name caller_arg_types
    in
    let () =
      if false then
        Format.eprintf "@[Found candidate decl:@ @[%a@]@]@." PP.pp_t
          [ D_Func callee |> add_dummy_pos ]
    in
    let+ () =
      check_true (callee.subprogram_type = call_type) @@ fun () ->
      fatal_from loc (MismatchedReturnValue name)
    in
    let () =
      if false then
        let open Format in
        eprintf "Parameters for this call: %a@."
          (pp_print_list ~pp_sep:pp_print_space (fun f (name, e) ->
               fprintf f "%S<--%a" name (pp_print_option PP.pp_ty) e))
          callee.parameters
    in
    let () =
      if false then
        match eqs1 with
        | [] -> ()
        | _ ->
            Format.eprintf "@[<2>%a: Adding@ @[{%a}@]@ to call of %s@."
              PP.pp_pos loc
              (Format.pp_print_list
                 ~pp_sep:(fun f () -> Format.fprintf f ";@ ")
                 (fun f (n, e) ->
                   Format.fprintf f "@[%s@ <- %a@]" n PP.pp_expr e))
              eqs1 name
    in
    let () =
      if List.compare_lengths callee.args args1 != 0 then
        fatal_from loc
        @@ Error.BadArity (name, List.length callee.args, List.length args1)
    in
    let eqs2 =
      let folder acc (_x, ty) (t_e, _e) =
        match ty.desc with
        | T_Bits ({ desc = E_Var param_name; _ }, _) -> (
            match (Types.make_anonymous env t_e).desc with
            | T_Bits (param_actual_e, _) -> (
                (* If param has another defining expression, we need to check that they are equal. *)
                match List.assoc_opt param_name acc with
                | Some param_actual_e2
                  when StaticModel.equal_in_env env param_actual_e
                         param_actual_e2 ->
                    (* If they are, we can ignore the other expression. *)
                    acc
                | Some _
                (* If they are not equal, the parameter satisfaction later will fail. *)
                | None
                  (* If there are no other defining expression, there is nothing to check. *)
                  ->
                    (* We don't need to check that param_actual_e is a static constrained integer, because it comes from a bitvector type. *)
                    (param_name, param_actual_e) :: acc)
            | _ -> acc)
        | _ -> acc
      in
      match C.check with
      | `TypeCheck -> eqs1
      | `Warn | `Silence ->
          List.fold_left2 folder eqs1 callee.args caller_arg_typed
    in
    let eqs3 =
      (* Checking that all implicit parameters are static constrained integers. *)
      List.fold_left2
        (fun eqs (callee_x, _) (caller_ty, caller_e) ->
          (* If [callee_x] is an implicit parameter. *)
          if
            List.exists
              (fun (p_name, _ty) -> String.equal callee_x p_name)
              callee.parameters
          then
            let+ () = check_statically_evaluable env caller_e in
            let+ () = check_constrained_integer ~loc env caller_ty in
            (callee_x, caller_e) :: eqs
          else eqs)
        eqs2 callee.args caller_arg_typed
    in
    let () =
      if false then
        let open Format in
        eprintf "@[<hov 2>Eqs for this call are: %a@]@."
          (pp_print_list ~pp_sep:pp_print_space (fun f (name, e) ->
               fprintf f "%S<--%a" name PP.pp_expr e))
          eqs3
    in
    let () =
      List.iter2
        (fun (callee_arg_name, callee_arg) caller_arg ->
          let callee_arg = rename_ty_eqs env eqs3 callee_arg in
          let () =
            if false then
              Format.eprintf "Checking calling arg %s from %a to %a@."
                callee_arg_name PP.pp_ty caller_arg PP.pp_ty callee_arg
          in
          let+ () = check_type_satisfies loc env caller_arg callee_arg in
          ())
        callee.args caller_arg_types
    in
    let () =
      if false && not (String.equal name name1) then
        Format.eprintf "Renaming call from %s to %s@ at %a.@." name name1
          PP.pp_pos loc
    in
    let () =
      List.iter
        (function
          | _, None -> ()
          | s, Some { desc = T_Int (UnderConstrained (_, s')); _ }
            when String.equal s' s ->
              ()
          | callee_param_name, Some callee_param_t ->
              let callee_param_t_renamed =
                rename_ty_eqs env eqs3 callee_param_t
              in
              let caller_param_e =
                match List.assoc_opt callee_param_name eqs3 with
                | None ->
                    assert false
                    (* Bad behaviour, there should be a defining expression *)
                | Some e -> e
              in
              let caller_param_t, _ = annotate_expr env caller_param_e in
              let () =
                if false then
                  Format.eprintf
                    "Checking calling param %s from %a to %a (i.e. %a)@."
                    callee_param_name PP.pp_ty caller_param_t PP.pp_ty
                    callee_param_t PP.pp_ty callee_param_t_renamed
              in
              let+ () =
                check_type_satisfies loc env caller_param_t
                  callee_param_t_renamed
              in
              ())
        callee.parameters
      |: TypingRule.FindCheckDeduce
    in
    (* End *)
    (* Begin FCall *)
    let ret_ty1 =
      match (call_type, callee.return_type) with
      | (ST_Function | ST_Getter | ST_EmptyGetter), Some ty ->
          Some (rename_ty_eqs env eqs3 ty)
      | (ST_Setter | ST_EmptySetter | ST_Procedure), None -> None
      | _ -> fatal_from loc @@ Error.MismatchedReturnValue name
    in
    let () = if false then Format.eprintf "Annotated call to %S.@." name1 in
    (name1, args1, eqs3, ret_ty1) |: TypingRule.FCall
  (* End *)

  and annotate_expr env (e : expr) : ty * expr =
    let () = if false then Format.eprintf "@[Annotating %a@]@." PP.pp_expr e in
    let here x = add_pos_from e x and loc = to_pos e in
    match e.desc with
    (* Begin ELit *)
    | E_Literal v -> (annotate_literal v |> here, e) |: TypingRule.ELit
    (* End *)
    (* Begin ATC *)
    | E_ATC (e', ty) ->
        let t, e'' = annotate_expr env e' in
        let t_struct = Types.get_structure env t in
        let ty' = annotate_type ~loc env ty in
        let ty_struct = Types.get_structure env ty' in
        (if Types.type_equal env t_struct ty_struct then (ty', e'')
         else
           match (t_struct.desc, ty_struct.desc) with
           | T_Bits _, T_Bits _ | T_Int _, T_Int _ ->
               (ty', E_ATC (e'', ty_struct) |> here)
           | _ -> fatal_from e (BadATC (t, ty')))
        |: TypingRule.ATC
    (* End *)
    | E_Var x -> (
        let () = if false then Format.eprintf "Looking at %S.@." x in
        if should_reduce_to_call env x ST_EmptyGetter then
          let () =
            if false then
              Format.eprintf "@[Reducing getter %S@ at %a@]@." x PP.pp_pos e
          in
          let name, args, eqs, ty =
            annotate_call (to_pos e) env x [] [] ST_EmptyGetter
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
            (* Begin ELocalVarConstant *)
            | ty, LDK_Constant ->
                let e =
                  try
                    let v = lookup_constants env x in
                    E_Literal v |> here
                  with Not_found -> e
                in
                (ty, e) |: TypingRule.ELocalVarConstant
            (* End *)
            (* Begin ELocalVar *)
            | ty, _ -> (ty, e) |: TypingRule.ELocalVar
            (* End *)
          with Not_found -> (
            try
              match IMap.find x env.global.storage_types with
              (* Begin EGlobalVarConstant *)
              | ty, GDK_Constant -> (
                  match IMap.find_opt x env.global.constant_values with
                  | Some v ->
                      (ty, E_Literal v |> here)
                      |: TypingRule.EGlobalVarConstantVal
                  (* End *)
                  (* Begin EGlobalVarConstantNoVal *)
                  | None -> (ty, e) |: TypingRule.EGlobalVarConstantNoVal)
              (* End *)
              (* Begin EGlobalVar *)
              | ty, _ -> (ty, e) |: TypingRule.EGlobalVar
              (* End *)
              (* Begin EUndefIdent *)
            with Not_found ->
              let () =
                if false then
                  Format.eprintf "@[Cannot find %s in env@ %a.@]@." x pp_env env
              in
              undefined_identifier e x |: TypingRule.EUndefIdent))
    (* End *)
    (* Begin Binop *)
    | E_Binop (op, e1, e2) ->
        let t1, e1' = annotate_expr env e1 in
        let t2, e2' = annotate_expr env e2 in
        let t = check_binop e env op t1 t2 in
        (t, E_Binop (op, e1', e2') |> here) |: TypingRule.Binop
    (* End *)
    (* Begin Unop *)
    | E_Unop (op, e') ->
        let t'', e'' = annotate_expr env e' in
        let t = check_unop e env op t'' in
        (t, E_Unop (op, e'') |> here) |: TypingRule.Unop
    (* End *)
    (* Begin ECall *)
    | E_Call (name, args, eqs) ->
        let () =
          if List.length eqs == 0 then ()
          else (
            Format.eprintf "Re-annotating expression %a@." PP.pp_expr e;
            assert false)
        in
        let name', args', eqs', ty_opt =
          annotate_call (to_pos e) env name args [] ST_Function
        in
        let t = match ty_opt with Some ty -> ty | None -> assert false in
        (t, E_Call (name', args', eqs') |> here) |: TypingRule.ECall
    (* End *)
    (* Begin ECond *)
    | E_Cond (e_cond, e_true, e_false) ->
        let t_cond, e_cond' = annotate_expr env e_cond in
        let+ () = check_structure_boolean e env t_cond in
        let t_true, e_true' = annotate_expr env e_true
        and t_false, e_false' = annotate_expr env e_false in
        let t =
          best_effort t_true (fun _ ->
              match Types.lowest_common_ancestor env t_true t_false with
              | None ->
                  fatal_from e (Error.UnreconciliableTypes (t_true, t_false))
              | Some t -> t)
        in
        (t, E_Cond (e_cond', e_true', e_false') |> here) |: TypingRule.ECond
    (* End *)
    (* Begin ETuple *)
    | E_Tuple li ->
        let ts, es = List.map (annotate_expr env) li |> List.split in
        (T_Tuple ts |> here, E_Tuple es |> here) |: TypingRule.ETuple
    (* End *)
    | E_Concat [] -> fatal_from loc UnrespectedParserInvariant
    (* Begin EConcat *)
    | E_Concat (_ :: _ as li) ->
        let ts, es = List.map (annotate_expr env) li |> List.split in
        let w =
          let widths = List.map (get_bitvector_width e env) ts in
          let wh = List.hd widths and wts = List.tl widths in
          List.fold_left (width_plus env) wh wts
        in
        (T_Bits (w, []) |> here, E_Concat es |> here) |: TypingRule.EConcat
    (* End *)
    | E_Record (ty, fields) ->
        (* Rule WBCQ: The identifier in a record expression must be a named type
           with the structure of a record type, and whose fields have the values
           given in the field_assignment_list.
           Rule WZWC: The identifier in a exception expression must be a named
           type with the structure of an exception type, and whose fields have
           the values given in the field_assignment_list.
        *)
        let+ () =
          check_true (Types.is_named ty) (fun () ->
              failwith "Typing error: should be a named type")
        in
        let field_types =
          (* Begin EStructuredNotStructured *)
          match (Types.make_anonymous env ty).desc with
          | T_Exception fields | T_Record fields -> fields
          | _ ->
              conflict e [ T_Record [] ] ty
              |: TypingRule.EStructuredNotStructured
          (* End *)
        in
        let fields' =
          best_effort fields (fun _ ->
              (* Rule DYQZ: A record expression shall assign every field of the record. *)
              let () =
                if
                  List.for_all
                    (fun (name, _) -> List.mem_assoc name fields)
                    field_types
                then ()
                else
                  (* Begin EStructuredMissingField *)
                  fatal_from e (Error.MissingField (List.map fst fields, ty))
                  |: TypingRule.EStructuredMissingField
                (* End *)
                (* and whose fields have the values given in the field_assignment_list. *)
              in
              let+ () =
                match get_first_duplicate fst fields with
                | None -> ok
                | Some x ->
                    fun () -> fatal_from loc (Error.AlreadyDeclaredIdentifier x)
              in
              (* Begin ERecord *)
              List.map
                (fun (name, e') ->
                  let t', e'' = annotate_expr env e' in
                  let t_spec' =
                    match List.assoc_opt name field_types with
                    | None -> fatal_from e (Error.BadField (name, ty))
                    | Some t_spec' -> t_spec'
                  in
                  let+ () = check_type_satisfies e env t' t_spec' in
                  (name, e''))
                fields)
        in
        (ty, E_Record (ty, fields') |> here) |: TypingRule.ERecord
    (* End *)
    (* Begin EUnknown *)
    | E_Unknown ty ->
        let ty1 = annotate_type ~loc env ty in
        let ty2 = Types.get_structure env ty1 in
        (ty1, E_Unknown ty2 |> here) |: TypingRule.EUnknown
    (* End *)
    | E_Slice (e', slices) -> (
        (* Begin ReduceSlicesToCall *)
        match e'.desc with
        | E_Var name
          when should_reduce_to_call env name ST_Getter
               && List.for_all slice_is_single slices ->
            let args =
              try List.map slice_as_single slices
              with Invalid_argument _ -> assert false
            in
            let name1, args1, eqs, ty =
              annotate_call (to_pos e) env name args [] ST_Getter
            in
            let ty = match ty with Some ty -> ty | None -> assert false in
            (ty, E_Call (name1, args1, eqs) |> here) |: TypingRule.ESetter
            (* End ReduceSlicesToCall *)
        | _ -> (
            let t_e', e'' = annotate_expr env e' in
            let struct_t_e' = Types.make_anonymous env t_e' in
            match struct_t_e'.desc with
            (* Begin ESlice *)
            | T_Int _ | T_Bits _ ->
                let w = slices_width env slices in
                (* TODO: check that:
                   - Rule SNQJ: An expression or subexpression which
                     may result in a zero-length bitvector must not be
                     side-effecting.
                *)
                let slices' = best_effort slices (annotate_slices env) in
                (T_Bits (w, []) |> here, E_Slice (e'', slices') |> here)
                |: TypingRule.ESlice
            (* End *)
            (* Begin EGetArray *)
            | T_Array (size, ty') -> (
                match slices with
                | [ Slice_Single e_index ] ->
                    let t_index', e_index' = annotate_expr env e_index in
                    let wanted_t_index = type_of_array_length ~loc:e size in
                    let+ () =
                      check_type_satisfies e env t_index' wanted_t_index
                    in
                    (ty', E_GetArray (e'', e_index') |> here)
                    |: TypingRule.EGetArray
                | _ -> conflict e [ integer'; default_t_bits ] t_e')
            (* End *)
            (* Begin ESliceOrEGetArrayError *)
            | _ ->
                conflict e [ integer'; default_t_bits ] t_e'
                |: TypingRule.ESliceOrEGetArrayError
            (* End *)))
    | E_GetField (e1, field_name) -> (
        let reduced =
          match e1.desc with
          | E_Var name when should_reduce_to_call env name ST_Getter ->
              let empty_getter = E_Slice (e1, []) |> add_pos_from e1 in
              let ty, _ = annotate_expr env empty_getter in
              should_field_reduce_to_call env name ty field_name
          | _ -> None
        in
        match reduced with
        | Some (name, args) ->
            let name, args, eqs, ty =
              annotate_call (to_pos e) env name args [] ST_Getter
            in
            let ty = match ty with Some ty -> ty | None -> assert false in
            (ty, E_Call (name, args, eqs) |> here)
        | None -> (
            let t_e2, e2 = annotate_expr env e1 in
            match (Types.make_anonymous env t_e2).desc with
            | T_Exception fields | T_Record fields -> (
                match List.assoc_opt field_name fields with
                (* Begin EGetBadRecordField *)
                | None ->
                    fatal_from e (Error.BadField (field_name, t_e2))
                    |: TypingRule.EGetBadRecordField
                (* End *)
                (* Begin EGetRecordField *)
                | Some t ->
                    (t, E_GetField (e2, field_name) |> here)
                    |: TypingRule.EGetRecordField
                    (* End *))
            | T_Bits (_, bitfields) -> (
                match find_bitfield_opt field_name bitfields with
                (* Begin EGetBadBitField *)
                | None ->
                    fatal_from e (Error.BadField (field_name, t_e2))
                    |: TypingRule.EGetBadBitField
                (* End *)
                (* Begin EGetBitField *)
                | Some (BitField_Simple (_field, slices)) ->
                    let e3 = E_Slice (e1, slices) |> here in
                    annotate_expr env e3 |: TypingRule.EGetBitField
                (* End *)
                (* Begin EGetBitFieldNested *)
                | Some (BitField_Nested (_field, slices, bitfields')) ->
                    let e3 = E_Slice (e1, slices) |> here in
                    let t_e4, new_e = annotate_expr env e3 in
                    let t_e5 =
                      match t_e4.desc with
                      | T_Bits (width, _bitfields) ->
                          T_Bits (width, bitfields') |> add_pos_from t_e2
                      | _ -> assert false
                    in
                    (t_e5, new_e) |: TypingRule.EGetBitFieldNested
                (* End *)
                (* Begin EGetBitFieldTyped *)
                | Some (BitField_Type (_field, slices, t)) ->
                    let e3 = E_Slice (e1, slices) |> here in
                    let t_e4, new_e = annotate_expr env e3 in
                    let+ () = check_type_satisfies new_e env t_e4 t in
                    (t, new_e) |: TypingRule.EGetBitFieldTyped
                    (* End *))
            (* Begin EGetTupleItem *)
            | T_Tuple tys ->
                let index =
                  try Scanf.sscanf field_name "item%u" Fun.id
                  with Scanf.Scan_failure _ | Failure _ | End_of_file ->
                    fatal_from e (Error.BadField (field_name, t_e2))
                in
                if 0 <= index && index < List.length tys then
                  (List.nth tys index, E_GetItem (e2, index) |> add_pos_from e)
                else
                  fatal_from e (Error.BadField (field_name, t_e2))
                  |: TypingRule.EGetTupleItem
            (* End *)
            (* Begin EGetBadField *)
            | _ ->
                fatal_from e (Error.BadField (field_name, t_e2))
                |: TypingRule.EGetBadField)
        (* End *))
    | E_GetFields (e1, fields) -> (
        let reduced =
          match e1.desc with
          | E_Var name when should_reduce_to_call env name ST_Getter ->
              let empty_getter = E_Slice (e1, []) |> add_pos_from e1 in
              let ty, _ = annotate_expr env empty_getter in
              should_fields_reduce_to_call env name ty fields
          | _ -> None
        in
        match reduced with
        | Some (name, args) ->
            let name, args, eqs, ty =
              annotate_call (to_pos e) env name args [] ST_Getter
            in
            let ty = match ty with Some ty -> ty | None -> assert false in
            (ty, E_Call (name, args, eqs) |> here)
        | None -> (
            let t_e2, e2 = annotate_expr env e1 in
            match (Types.make_anonymous env t_e2).desc with
            | T_Bits (_, bitfields) ->
                let one_field field =
                  match find_bitfields_slices_opt field bitfields with
                  | None -> fatal_from e (Error.BadField (field, t_e2))
                  | Some slices -> slices
                in
                E_Slice (e1, list_concat_map one_field fields)
                |> here |> annotate_expr env |: TypingRule.EGetBitFields
            | T_Record tfields ->
                let one_field field =
                  match List.assoc_opt field tfields with
                  | None -> fatal_from e (Error.BadField (field, t_e2))
                  | Some t -> get_bitvector_width loc env t
                in
                let widths = List.map one_field fields in
                let w =
                  let wh = List.hd widths and wts = List.tl widths in
                  List.fold_left (width_plus env) wh wts
                in
                (T_Bits (w, []) |> here, E_GetFields (e2, fields) |> here)
            | _ -> conflict e [ default_t_bits ] t_e2))
    (* End *)
    (* Begin EPattern *)
    | E_Pattern (e1, pat) ->
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
             "-"                      ->       TRUE
           | e1=expr                  ->       e == e1
           | bitmask_lit              ->       not yet implemented
           | e1=expr ".." e2=expr     ->       e1 <= e && e <= e2
           | "<=" e1=expr             ->       e <= e1
           | ">=" e1=expr             ->       e >= e1
           |  { p0 , ... pN }         ->       e IN p0 || ... e IN pN
           | !{ p0 , ... pN }         ->       not (e IN p0) && ... e IN pN

         We cannot reduce them here (as otherwise e might be evaluated a
         bad number of times), but we will apply the same typing rules as for
         those desugared expressions.
         *)
        let t_e2, e2 = annotate_expr env e1 in
        let pat' = best_effort pat (annotate_pattern e env t_e2) in
        (T_Bool |> here, E_Pattern (e2, pat') |> here) |: TypingRule.EPattern
    (* End *)
    | E_GetItem _ -> assert false
    | E_GetArray _ -> assert false |: TypingRule.EGetArray

  let rec annotate_lexpr env le t_e =
    let () =
      if false then
        Format.eprintf "Typing lexpr: @[%a@] to @[%a@]@." PP.pp_lexpr le
          PP.pp_ty t_e
    in
    let here x = add_pos_from le x in
    match le.desc with
    (* Begin LEDiscard *)
    | LE_Discard -> le |: TypingRule.LEDiscard
    (* End *)
    | LE_Var x ->
        let+ () =
         fun () ->
          let ty =
            (* Begin LELocalVar *)
            match IMap.find_opt x env.local.storage_types with
            | Some (ty, LDK_Var) -> ty |: TypingRule.LELocalVar
            (* End *)
            | Some _ -> fatal_from le @@ Error.AssignToImmutable x
            | None -> (
                (* Begin LEGlobalVar *)
                match IMap.find_opt x env.global.storage_types with
                | Some (ty, GDK_Var) -> ty |: TypingRule.LEGlobalVar
                (* End *)
                | Some _ -> fatal_from le @@ Error.AssignToImmutable x
                | None -> undefined_identifier le x)
          in
          check_type_satisfies le env t_e ty ()
        in
        le
    (* Begin LEDestructuring *)
    | LE_Destructuring les ->
        (match t_e.desc with
        | T_Tuple sub_tys ->
            if List.compare_lengths sub_tys les != 0 then
              Error.fatal_from le
                (Error.BadArity
                   ("LEDestructuring", List.length sub_tys, List.length les))
            else
              let les' = List.map2 (annotate_lexpr env) les sub_tys in
              LE_Destructuring les' |> here
        | _ -> conflict le [ T_Tuple [] ] t_e)
        |: TypingRule.LEDestructuring
    (* End *)
    | LE_Slice (le1, slices) -> (
        let t_le1, _ = expr_of_lexpr le1 |> annotate_expr env in
        let struct_t_le1 = Types.make_anonymous env t_le1 in
        (* Begin LESlice *)
        match struct_t_le1.desc with
        | T_Bits _ ->
            let le2 = annotate_lexpr env le1 t_le1 in
            let+ () =
             fun () ->
              let width =
                slices_width env slices |> StaticModel.try_normalize env
              in
              let t = T_Bits (width, []) |> here in
              check_type_satisfies le env t_e t ()
            in
            let slices2 = best_effort slices (annotate_slices env) in
            let+ () = check_disjoint_slices le env slices2 in
            LE_Slice (le2, slices2) |> here |: TypingRule.LESlice
        (* End *)
        (* Begin LESetArray *)
        | T_Array (size, t) -> (
            let le2 = annotate_lexpr env le1 t_le1 in
            let+ () = check_type_satisfies le2 env t_e t in
            match slices with
            | [ Slice_Single e_index ] ->
                let t_index', e_index' = annotate_expr env e_index in
                let wanted_t_index = type_of_array_length ~loc:le size in
                let+ () =
                  check_type_satisfies le2 env t_index' wanted_t_index
                in
                LE_SetArray (le2, e_index') |> here |: TypingRule.LESetArray
            (* End *)
            | _ -> invalid_expr (expr_of_lexpr le1))
        | _ -> conflict le1 [ default_t_bits ] t_le1)
    | LE_SetField (le1, field) ->
        (let t_le1, _ = expr_of_lexpr le1 |> annotate_expr env in
         let le2 = annotate_lexpr env le1 t_le1 in
         let t_le1_struct = Types.make_anonymous env t_le1 in
         match t_le1_struct.desc with
         | T_Exception fields | T_Record fields ->
             let t =
               match List.assoc_opt field fields with
               (* Begin LESetBadStructuredField *)
               | None ->
                   fatal_from le (Error.BadField (field, t_le1))
                   |: TypingRule.LESetBadStructuredField
               (* End *)
               (* Begin LESetStructuredField *)
               | Some t -> t
             in
             let+ () = check_type_satisfies le env t_e t in
             LE_SetField (le2, field) |> here |: TypingRule.LESetStructuredField
             (* End *)
         | T_Bits (_, bitfields) ->
             let bits slices bitfields =
               T_Bits (slices_width env slices, bitfields) |> here
             in
             let t, slices =
               match find_bitfield_opt field bitfields with
               (* Begin LESetBadBitField *)
               | None ->
                   fatal_from le1 (Error.BadField (field, t_le1_struct))
                   |: TypingRule.LESetBadBitField
               (* End *)
               (* Begin LESetBitField *)
               | Some (BitField_Simple (_field, slices)) ->
                   (bits slices [], slices) |: TypingRule.LESetBitField
               (* End *)
               (* Begin LESetBitFieldNested *)
               | Some (BitField_Nested (_field, slices, bitfields')) ->
                   (bits slices bitfields', slices)
                   |: TypingRule.LESetBitFieldNested
               (* End *)
               (* Begin LESetBitFieldTyped *)
               | Some (BitField_Type (_field, slices, t)) ->
                   let t' = bits slices [] in
                   let+ () = check_type_satisfies le env t' t in
                   (t, slices) |: TypingRule.LESetBitFieldTyped
               (* End *)
             in
             let+ () = check_type_satisfies le1 env t_e t in
             let le2 = LE_Slice (le1, slices) |> here in
             annotate_lexpr env le2 t_e
         (* Begin LESetBadField *)
         | _ -> conflict le1 [ default_t_bits; T_Record []; T_Exception [] ] t_e)
        |: TypingRule.LESetBadField
        (* End *)
    | LE_SetFields (le', fields, []) -> (
        let t_le', _ = expr_of_lexpr le' |> annotate_expr env in
        let le' = annotate_lexpr env le' t_le' in
        let t_le'_struct = Types.make_anonymous env t_le' in
        match t_le'_struct.desc with
        | T_Bits (_, bitfields) ->
            let one_field field =
              match find_bitfields_slices_opt field bitfields with
              | None -> fatal_from le (Error.BadField (field, t_le'_struct))
              | Some slices -> slices
            in
            let new_le =
              LE_Slice (le', list_concat_map one_field fields) |> here
            in
            annotate_lexpr env new_le t_e |: TypingRule.LESetFields
        | T_Record tfields ->
            let one_field field (start, slices) =
              match List.assoc_opt field tfields with
              | None -> fatal_from le (Error.BadField (field, t_le'_struct))
              | Some t ->
                  let w = get_bitvector_const_width le env t in
                  (start + w, (start, w) :: slices)
            in
            let length, slices = List.fold_right one_field fields (0, []) in
            let t = T_Bits (expr_of_int length, []) |> here in
            let+ () = check_type_satisfies le env t_e t in
            LE_SetFields (le', fields, slices) |> here
        | _ -> conflict le [ default_t_bits ] t_le')
    | LE_SetArray _ -> assert false
    | LE_SetFields (_, _, _ :: _) -> assert false
    (* Begin LEConcat *)
    | LE_Concat (les, _) ->
        let e_eq = expr_of_lexpr le in
        let t_e_eq, _e_eq = annotate_expr env e_eq in
        let+ () = check_bits_equal_width le env t_e_eq t_e in
        let bv_length t = get_bitvector_const_width le env t in
        let annotate_one (les, widths, sum) le =
          let e = expr_of_lexpr le in
          let t_e1, _e = annotate_expr env e in
          let width = bv_length t_e1 in
          let t_e2 = T_Bits (expr_of_int width, []) |> add_pos_from le in
          let le1 = annotate_lexpr env le t_e2 in
          (le1 :: les, width :: widths, sum + width)
        in
        let rev_les, rev_widths, _real_width =
          List.fold_left annotate_one ([], [], 0) les
        in
        (* as the first check, we have _real_width == bv_length t_e *)
        let les1 = List.rev rev_les and widths = List.rev rev_widths in
        LE_Concat (les1, Some widths) |> add_pos_from le |: TypingRule.LEConcat
  (* End *)

  let can_be_initialized_with env s t =
    (* Rules:
       - ZCVD: It is illegal for a storage element whose type has the
         structure of the under-constrained integer to be initialized with a
         value whose type has the structure of the under-constrained integer,
         unless the type is omitted from the declaration (and therefore the
         type can be unambiguously inferred) or the initialization expression
         is omitted (and therefore the type is not omitted from the
         declaration).
       - LXQZ: A storage element of type S, where S is
         any type that does not have the structure of
         the under-constrained integer type, may only be
         assigned or initialized with a value of type T
         if T type-satisfies S)
    *)
    let s_struct = Types.get_structure env s in
    match s_struct.desc with
    | T_Int (UnderConstrained _) -> assert false
    | _ -> Types.type_satisfies env t s

  let check_can_be_initialized_with loc env s t () =
    if can_be_initialized_with env s t then () else conflict loc [ s.desc ] t

  let add_immutable_expressions ~loc env ldk e_opt x =
    match (ldk, e_opt) with
    | (LDK_Constant | LDK_Let), Some e when is_statically_evaluable ~loc env e
      ->
        let e' = StaticModel.try_normalize env e in
        add_local_immutable_expr x e' env
    | _ -> env

  let rec annotate_local_decl_item loc (env : env) ty ldk ?e ldi =
    let () =
      if false then Format.eprintf "Annotating %a.@." PP.pp_local_decl_item ldi
    in
    match ldi with
    (* Begin LDDiscard *)
    | LDI_Discard -> (env, ldi) |: TypingRule.LDDiscard
    (* End *)
    (* Begin LDTyped *)
    | LDI_Typed (ldi', t) ->
        let t' = annotate_type ~loc env t in
        let+ () = check_can_be_initialized_with loc env t' ty in
        let new_env, new_ldi' =
          annotate_local_decl_item loc env t' ldk ?e ldi'
        in
        (new_env, LDI_Typed (new_ldi', t')) |: TypingRule.LDTyped
    (* End *)
    (* Begin LDVar *)
    | LDI_Var x ->
        (* Rule LCFD: A local declaration shall not declare an identifier
           which is already in scope at the point of declaration. *)
        let+ () = check_var_not_in_env loc env x in
        let env2 = add_local x ty ldk env in
        let new_env = add_immutable_expressions ~loc env2 ldk e x in
        (new_env, LDI_Var x) |: TypingRule.LDVar
    (* End *)
    (* Begin LDTuple *)
    | LDI_Tuple ldis ->
        let tys =
          match (Types.make_anonymous env ty).desc with
          | T_Tuple tys when List.compare_lengths tys ldis = 0 -> tys
          | T_Tuple tys ->
              fatal_from loc
                (Error.BadArity
                   ("tuple initialization", List.length tys, List.length ldis))
          | _ -> conflict loc [ T_Tuple [] ] ty
        in
        let new_env, new_ldis =
          List.fold_right2
            (fun ty' ldi' (env', les) ->
              let env', le = annotate_local_decl_item loc env' ty' ldk ldi' in
              (env', le :: les))
            tys ldis (env, [])
        in
        (new_env, LDI_Tuple new_ldis) |: TypingRule.LDTuple
  (* End *)

  let annotate_local_decl_item_uninit loc (env : env) ldi =
    (* Here implicitly ldk=LDK_Var *)
    match ldi with
    | LDI_Discard -> (env, LDI_Discard)
    | LDI_Var _ ->
        fatal_from loc (Error.BadLDI ldi) |: TypingRule.LDUninitialisedVar
    | LDI_Tuple _ldis ->
        fatal_from loc (Error.BadLDI ldi) |: TypingRule.LDUninitialisedTuple
    | LDI_Typed (ldi', t) ->
        let t' = annotate_type ~loc env t in
        let new_env, new_ldi' =
          annotate_local_decl_item loc env t' LDK_Var ldi'
        in
        (new_env, LDI_Typed (new_ldi', t')) |: TypingRule.LDUninitialisedTyped

  let declare_local_constant =
    let rec add_constants v env ldi =
      match ldi with
      | LDI_Discard -> env
      | LDI_Var x -> add_local_constant x v env
      | LDI_Tuple _ -> (* Not yet implemented *) env
      | LDI_Typed (ldi, _ty) -> add_constants v env ldi
    in
    fun env v ldi -> add_constants v env ldi

  let rec annotate_stmt env s =
    let () =
      if false then
        match s.desc with
        | S_Seq _ -> ()
        | _ -> Format.eprintf "@[<3>Annotating@ @[%a@]@]@." PP.pp_stmt s
    in
    let here x = add_pos_from s x and loc = to_pos s in
    match s.desc with
    (* Begin SPass *)
    | S_Pass -> (s, env) |: TypingRule.SPass
    (* Begin SSeq *)
    | S_Seq (s1, s2) ->
        let new_s1, env1 = try_annotate_stmt env s1 in
        let new_s2, env2 = try_annotate_stmt env1 s2 in
        (S_Seq (new_s1, new_s2) |> here, env2) |: TypingRule.SSeq
    (* Begin SAssign *)
    | S_Assign (le, re, ver) ->
        (let () =
           if false then
             Format.eprintf "@[<3>Annotating assignment@ @[%a@]@]@." PP.pp_stmt
               s
         in
         let t_re, re1 = annotate_expr env re in
         let reduced = setter_should_reduce_to_call_s env le (t_re, re1) in
         match reduced with
         | Some new_s -> (new_s, env)
         | None ->
             let env1 =
               match ver with
               | V1 -> env
               | V0 -> (
                   (*
                    * In version V0, variable declaration is optional,
                    * As a result typing will be partial and some
                    * function calls may lack extra parameters.
                    * Fix this by typing first assignments of
                    * undeclared variables as declarations.
                    *)
                   match ASTUtils.lid_of_lexpr le with
                   | None -> env
                   | Some ldi ->
                       let rec undefined = function
                         | LDI_Discard -> true
                         | LDI_Var x -> StaticEnv.is_undefined x env
                         | LDI_Tuple ldis -> List.for_all undefined ldis
                         | LDI_Typed (ldi', _) -> undefined ldi'
                       in
                       if undefined ldi then
                         let () =
                           if false then
                             Format.eprintf
                               "@[<3>Assignment@ @[%a@] as declaration@]@."
                               PP.pp_stmt s
                         in
                         let ldk = LDK_Var in
                         let env2, _ldi =
                           annotate_local_decl_item loc env t_re ldk ldi
                         in
                         env2
                       else env)
             in
             let le1 = annotate_lexpr env1 le t_re in
             (S_Assign (le1, re1, ver) |> here, env1))
        |: TypingRule.SAssign
    (* End *)
    (* Begin SCall *)
    | S_Call (name, args, eqs) ->
        let () = assert (List.length eqs == 0) in
        let new_name, new_args, new_eqs, ty =
          annotate_call loc env name args eqs ST_Procedure
        in
        let () = assert (ty = None) in
        (S_Call (new_name, new_args, new_eqs) |> here, env) |: TypingRule.SCall
    (* End *)
    | S_Return e_opt ->
        (* Rule NYWH: A return statement appearing in a setter or procedure must
           have no return value expression. *)
        (* Rule PHNZ: A return statement appearing in a getter or function
           requires a return value expression that type-satisfies the return
           type of the subprogram. *)
        (match (env.local.return_type, e_opt) with
        (* Begin SReturnOne *)
        | None, Some _ | Some _, None ->
            fatal_from loc (Error.BadReturnStmt env.local.return_type)
            |: TypingRule.SReturnOne
        (* End *)
        (* Begin SReturnNone *)
        | None, None -> (S_Return None |> here, env) |: TypingRule.SReturnNone
        (* End *)
        (* Begin SReturnSome *)
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
        |: TypingRule.SReturnSome
    (* End *)
    (* Begin SCond *)
    | S_Cond (e, s1, s2) ->
        let t_cond, e_cond = annotate_expr env e in
        let+ () = check_type_satisfies e_cond env t_cond boolean in
        let s1' = try_annotate_block env s1 in
        let s2' = try_annotate_block env s2 in
        (S_Cond (e_cond, s1', s2') |> here, env) |: TypingRule.SCond
    (* End *)
    (* Begin SCase *)
    | S_Case (e, cases) ->
        let t_e, e1 = annotate_expr env e in
        let annotate_case case =
          let { pattern = p0; where = w0; stmt = s0 } = case.desc in
          let p1 = annotate_pattern e1 env t_e p0
          and s1 = try_annotate_block env s0
          and w1 =
            match w0 with
            | None -> None
            | Some e_w0 ->
                let twe, e_w1 = (annotate_expr env) e_w0 in
                let+ () = check_structure_boolean e_w0 env twe in
                Some e_w1
          in
          add_pos_from_st case { pattern = p1; where = w1; stmt = s1 }
        in
        let cases1 = List.map annotate_case cases in
        (S_Case (e1, cases1) |> here, env) |: TypingRule.SCase
    (* End *)
    (* Begin SAssert *)
    | S_Assert e ->
        let t_e', e' = annotate_expr env e in
        let+ () = check_type_satisfies s env t_e' boolean in
        (S_Assert e' |> here, env) |: TypingRule.SAssert
    (* End *)
    (* Begin SWhile *)
    | S_While (e1, s1) ->
        let t, e2 = annotate_expr env e1 in
        let+ () = check_type_satisfies e2 env t boolean in
        let s2 = try_annotate_block env s1 in
        (S_While (e2, s2) |> here, env) |: TypingRule.SWhile
    (* End *)
    (* Begin SRepeat *)
    | S_Repeat (s1, e1) ->
        let s2 = try_annotate_block env s1 in
        let t, e2 = annotate_expr env e1 in
        let+ () = check_type_satisfies e2 env t boolean in
        (S_Repeat (s2, e2) |> here, env) |: TypingRule.SRepeat
    (* End *)
    (* Begin SFor *)
    | S_For (id, e1, dir, e2, s') ->
        let t1, e1' = annotate_expr env e1 and t2, e2' = annotate_expr env e2 in
        let struct1 = Types.make_anonymous env t1
        and struct2 = Types.make_anonymous env t2 in
        let cs =
          match (struct1.desc, struct2.desc) with
          | T_Int UnConstrained, T_Int _ | T_Int _, T_Int UnConstrained ->
              UnConstrained
          | T_Int _, T_Int _ ->
              let e1n = StaticModel.try_normalize env e1'
              and e2n = StaticModel.try_normalize env e2' in
              let e_bot, e_top =
                match dir with Up -> (e1n, e2n) | Down -> (e2n, e1n)
              in
              WellConstrained [ Constraint_Range (e_bot, e_top) ]
          | T_Int _, _ -> conflict s [ integer' ] t2
          | _, _ -> conflict s [ integer' ] t1
          (* only happens in relaxed type-checking mode because of check_structure_integer earlier. *)
        in
        let ty = T_Int cs |> here in
        let s'' =
          let+ () = check_var_not_in_env s' env id in
          let env' = add_local id ty LDK_Let env in
          try_annotate_block env' s'
        in
        (S_For (id, e1', dir, e2', s'') |> here, env) |: TypingRule.SFor
    (* End *)
    | S_Decl (ldk, ldi, e_opt) -> (
        match (ldk, e_opt) with
        (* Begin SDeclSome *)
        | _, Some e ->
            let t_e, e' = annotate_expr env e in
            let env1, ldi1 =
              annotate_local_decl_item loc env t_e ldk ~e:e' ldi
            in
            let env2 =
              match ldk with
              | LDK_Let | LDK_Var -> env1
              | LDK_Constant -> (
                  try
                    let v = reduce_constants env1 e in
                    declare_local_constant env1 v ldi1
                  with Error.(ASLException _) -> env1)
            in
            (S_Decl (ldk, ldi1, Some e') |> here, env2) |: TypingRule.SDeclSome
        (* End *)
        (* Begin SDeclNone *)
        | LDK_Var, None ->
            let env', ldi' = annotate_local_decl_item_uninit loc env ldi in
            (S_Decl (LDK_Var, ldi', None) |> here, env') |: TypingRule.SDeclNone
        | (LDK_Constant | LDK_Let), None ->
            fatal_from s UnrespectedParserInvariant)
    (* End *)
    (* Begin SThrowSome *)
    | S_Throw (Some (e, _)) ->
        let t_e, e' = annotate_expr env e in
        let+ () = check_structure_exception s env t_e in
        (S_Throw (Some (e', Some t_e)) |> here, env) |: TypingRule.SThrowSome
    (* End *)
    (* Begin SThrowNone *)
    | S_Throw None ->
        (* TODO: verify that this is allowed? *)
        (s, env) |: TypingRule.SThrowNone
    (* End *)
    (* Begin STry *)
    | S_Try (s', catchers, otherwise) ->
        let s'' = try_annotate_block env s' in
        let otherwise' = Option.map (try_annotate_block env) otherwise in
        let catchers' = List.map (annotate_catcher loc env) catchers in
        (S_Try (s'', catchers', otherwise') |> here, env) |: TypingRule.STry
    (* End *)
    | S_Print { args; debug } ->
        let args' = List.map (fun e -> annotate_expr env e |> snd) args in
        (S_Print { args = args'; debug } |> here, env) |: TypingRule.SDebug

  and annotate_catcher loc env (name_opt, ty, stmt) =
    let ty' = annotate_type ~loc env ty in
    let+ () = check_structure_exception ty' env ty' in
    let env' =
      match name_opt with
      (* Begin CatcherNone *)
      | None -> env |: TypingRule.CatcherNone
      (* End *)
      (* Begin CatcherSome *)
      | Some name ->
          let+ () = check_var_not_in_env stmt env name in
          add_local name ty LDK_Let env |: TypingRule.CatcherSome
      (* End *)
    in
    let new_stmt = try_annotate_block env' stmt in
    (name_opt, ty, new_stmt)

  (* Begin Block *)
  and try_annotate_block env s =
    (*
        See rule JFRD:
           A local identifier declared with var, let or constant
           is in scope from the point immediately after its declaration
           until the end of the immediately enclosing block.

        From that follows that we can discard the environment at the end
        of an enclosing block.
    *)
    best_effort s (fun _ -> annotate_stmt env s |> fst) |: TypingRule.Block
  (* End *)

  and try_annotate_stmt env s =
    best_effort (s, env) (fun _ -> annotate_stmt env s)

  and set_fields_should_reduce_to_call env le x fields (t_e, e) =
    (*
     * Field indices are extracted from the return type
     * of "associated" getter.
     *)
    let ( let* ) = Option.bind in
    let _, _, callee =
      try Fn.try_subprogram_for_name le env x []
      with Error.ASLException _ -> assert false
    in
    let* ty = callee.return_type in
    let ty = Types.make_anonymous env ty in
    let* name, args = should_fields_reduce_to_call env x ty fields in
    let args = (t_e, e) :: List.map (annotate_expr env) args in
    let name, args, eqs, ret_ty =
      annotate_call_arg_typed (to_pos le) env name args ST_Setter
    in
    let () = assert (ret_ty = None) in
    Some (S_Call (name, args, eqs) |> add_pos_from le)

  and setter_should_reduce_to_call_recurse ~loc env (t_e, e) make_old_le sub_le
      =
    let x = fresh_var "__setter_setfield" in
    let here le = add_pos_from loc le in
    let t_sub_re, sub_re = expr_of_lexpr sub_le |> annotate_expr env in
    let env1, ldi_x =
      annotate_local_decl_item loc env t_sub_re LDK_Var (LDI_Var x)
    in
    let s1 = S_Decl (LDK_Var, ldi_x, Some sub_re) |> here in
    let s2 =
      let old_le = make_old_le (LE_Var x |> here) in
      let old_le' = annotate_lexpr env1 old_le t_e in
      S_Assign (old_le', e, V1) |> here
    in
    let typed_e_x = annotate_expr env1 (E_Var x |> here) in
    match setter_should_reduce_to_call_s env1 sub_le typed_e_x with
    | None -> None
    | Some s -> Some (s_then (s_then s1 s2) s)

  and setter_should_reduce_to_call_s env le (t_e, e) : stmt option =
    let () =
      if false then
        Format.eprintf "@[<2>setter_..._s@ @[%a@]@ @[%a@]@]@." PP.pp_lexpr le
          PP.pp_expr e
    in
    let loc = to_pos le in
    let here d = add_pos_from loc d in
    (if false then (fun o ->
       Format.eprintf "@[Setter@ @[%a@ = %a@]@ gave %a@.@]" PP.pp_lexpr le
         PP.pp_expr e
         (Format.pp_print_option PP.pp_stmt)
         o;
       o)
     else Fun.id)
    @@
    match le.desc with
    | LE_Discard -> None
    | LE_SetField (sub_le, field) -> (
        match sub_le.desc with
        | LE_Var x when should_reduce_to_call env x ST_Setter ->
            set_fields_should_reduce_to_call env le x [ field ] (t_e, e)
        | _ ->
            let old_le le' = LE_SetField (le', field) |> here in
            setter_should_reduce_to_call_recurse ~loc env (t_e, e) old_le sub_le
        )
    | LE_SetFields (sub_le, fields, slices) -> (
        match sub_le.desc with
        | LE_Var x when should_reduce_to_call env x ST_Setter ->
            set_fields_should_reduce_to_call env le x fields (t_e, e)
        | _ ->
            let old_le le' = LE_SetFields (le', fields, slices) |> here in
            setter_should_reduce_to_call_recurse ~loc env (t_e, e) old_le sub_le
        )
    | LE_Slice (sub_le, slices) -> (
        match sub_le.desc with
        | LE_Var x
          when should_reduce_to_call env x ST_Setter
               && List.for_all slice_is_single slices ->
            let args =
              try List.map slice_as_single slices
              with Invalid_argument _ -> assert false
            in
            let typed_args = (t_e, e) :: List.map (annotate_expr env) args in
            let name, args, eqs, ret_ty =
              annotate_call_arg_typed loc env x typed_args ST_Setter
            in
            let () = assert (ret_ty = None) in
            Some (S_Call (name, args, eqs) |> here)
        | _ ->
            let old_le le' = LE_Slice (le', slices) |> here in
            setter_should_reduce_to_call_recurse ~loc env (t_e, e) old_le sub_le
        )
    | LE_Destructuring les -> (
        match (Types.make_anonymous env t_e).desc with
        | T_Tuple t_es when List.compare_lengths les t_es = 0 ->
            let x = fresh_var "__setter_destructuring" in
            let env1, ldi_x =
              annotate_local_decl_item loc env t_e LDK_Let ~e (LDI_Var x)
            in
            let sub_e i = E_GetItem (E_Var x |> here, i) |> here in
            let recurse_one i sub_le t_sub_e =
              setter_should_reduce_to_call_s env1 sub_le (t_sub_e, sub_e i)
            in
            let subs = list_mapi2 recurse_one 0 les t_es in
            if List.for_all Option.is_none subs then None
            else
              let s0 = S_Decl (LDK_Let, ldi_x, Some e) |> here in
              let produce_one i sub_le = function
                | None -> S_Assign (sub_le, sub_e i, V1) |> here
                | Some s -> s
              in
              list_mapi2 produce_one 0 les subs
              |> List.cons s0 |> stmt_from_list |> Option.some
        | _ -> None)
    | LE_Var x ->
        let st = ST_EmptySetter in
        if should_reduce_to_call env x st then
          let args = [ (t_e, e) ] in
          let name, args, eqs, ret_ty =
            annotate_call_arg_typed loc env x args st
          in
          let () = assert (ret_ty = None) in
          Some (S_Call (name, args, eqs) |> here)
        else None
    | LE_Concat (_les, _) -> None
    | LE_SetArray _ -> assert false

  let fold_types_func_sig folder f init =
    let from_args =
      List.fold_left (fun acc (_x, t) -> folder acc t) init f.args
    in
    match f.return_type with None -> from_args | Some t -> folder from_args t

  (* Begin GetUndeclaredDefining *)

  (** Returns the set of variables that are parameter defining, without the
      ones previously declared in the environment. *)
  let get_undeclared_defining env =
    let rec of_ty acc ty =
      match ty.desc with
      | T_Bits ({ desc = E_Var x; _ }, _) ->
          if StaticEnv.is_undefined x env then ISet.add x acc else acc
      | T_Tuple tys -> List.fold_left of_ty acc tys
      | _ -> acc
    in
    fun f ->
      fold_types_func_sig of_ty f ISet.empty |: TypingRule.GetUndeclaredDefining
  (* End *)

  (** [use_func_sig f] returns the set of identifiers appearing in the
      types of arguments of [f] and in the return type of [f], if there is one.
  *)
  let use_func_sig f =
    fold_types_func_sig (Fun.flip ASTUtils.use_ty) f ISet.empty

  (* Begin AnnotateFuncSig *)
  let annotate_func_sig ~loc env1 (func_sig : AST.func) : env * AST.func =
    (* Build typing local environment. *)
    let env1 = { env1 with local = empty_local } in
    let () =
      if false then
        Format.eprintf "Annotating %s in env:@ %a.@." func_sig.name
          StaticEnv.pp_env env1
    in
    let potential_params = get_undeclared_defining env1 func_sig in
    (* Add explicit parameters *)
    let env2, declared_params =
      let () =
        if false then
          Format.eprintf "Defined potential parameters: %a@." ISet.pp_print
            potential_params
      in
      let folder (env1', acc) (x, ty_opt) =
        let+ () = check_var_not_in_env loc env1' x in
        let+ () =
          check_true (ISet.mem x potential_params) @@ fun () ->
          fatal_from loc (Error.ParameterWithoutDecl x)
        in
        let t =
          match ty_opt with
          | None | Some { desc = T_Int UnConstrained; _ } ->
              Types.under_constrained_ty x
          | Some t1 -> annotate_type ~loc env1 t1
          (* Type should be valid in the env with no param declared. *)
        in
        let+ () = check_constrained_integer ~loc env1 t in
        (add_local x t LDK_Let env1', IMap.add x t acc)
        |: TypingRule.AnnotateOneParam
      in
      List.fold_left folder (env1, IMap.empty) func_sig.parameters
      |: TypingRule.AnnotateParams
    in
    let () =
      if false then
        Format.eprintf "Explicit parameters added to env %a.@."
          StaticEnv.pp_local env2.local
    in
    (* Add arguments as parameters. *)
    let env3, arg_params =
      let used =
        use_func_sig func_sig
        |> ISet.filter (fun s ->
               StaticEnv.is_undefined s env1 && not (IMap.mem s declared_params))
      in
      let () =
        if false then
          Format.eprintf "Undefined used in func sig: %a@." ISet.pp_print used
      in
      let folder (env2', acc) (x, ty) =
        if ISet.mem x used then
          let+ () = check_var_not_in_env loc env2' x in
          let t =
            match ty.desc with
            | T_Int UnConstrained -> Types.under_constrained_ty x
            | _ -> annotate_type ~loc env2 ty
            (* Type sould be valid in env with explicit parameters added, but no implicit parameter from args added. *)
          in
          let+ () = check_constrained_integer ~loc env2 t in
          (add_local x t LDK_Let env2', IMap.add x t acc)
        else (env2', acc)
      in
      List.fold_left folder (env2, IMap.empty) func_sig.args
      |: TypingRule.ArgsAsParams
    in
    let parameters =
      List.append (IMap.bindings declared_params) (IMap.bindings arg_params)
      |> List.map (fun (x, t) -> (x, Some t))
    in
    let env3, parameters =
      (* Do not transliterate, only for v0: promote potential params as params. *)
      if C.check = `TypeCheck then (env3, parameters)
      else
        let folder x (env3', parameters) =
          if var_in_env env3 x then (env3', parameters)
          else
            let t = Types.under_constrained_ty x in
            (add_local x t LDK_Let env3', (x, Some t) :: parameters)
        in
        ISet.fold folder potential_params (env3, parameters)
    in
    let () =
      if false then
        let open Format in
        eprintf "@[Parameters identified for func %s:@ @[%a@]@]@." func_sig.name
          (pp_print_list ~pp_sep:pp_print_space (fun f (s, ty_opt) ->
               fprintf f "%s:%a" s (pp_print_option PP.pp_ty) ty_opt))
          parameters
    in
    let () =
      if false then
        Format.eprintf "@[<hov>Annotating arguments in env:@ %a@]@."
          StaticEnv.pp_local env3.local
    in
    (* Add arguments. *)
    let env4, args =
      let one_arg env3' (x, ty) =
        if IMap.mem x arg_params then
          let ty' = annotate_type ~loc env2 ty in
          (env3', (x, ty'))
        else
          let () = if false then Format.eprintf "Adding argument %s.@." x in
          let+ () = check_var_not_in_env loc env3' x in
          (* Subtlety here: the type should be valid in the env with parameters declared, i.e. [env3]. *)
          let ty' = annotate_type ~loc env3 ty in
          let env3'' = add_local x ty' LDK_Let env3' in
          (env3'', (x, ty'))
      in
      list_fold_left_map one_arg env3 func_sig.args |: TypingRule.AnnotateArgs
    in
    (* Check return type. *)
    let env5, return_type =
      match func_sig.return_type with
      | None -> (env4, func_sig.return_type)
      | Some ty ->
          let () =
            if false then
              Format.eprintf "@[<hov>Annotating return-type in env:@ %a@]@."
                StaticEnv.pp_local env4.local
          in
          (* Subtlety here: the type should be valid in the env with parameters declared, i.e. [env3]. *)
          let ty' = annotate_type ~loc env3 ty in
          let return_type = Some ty' in
          let env4' =
            StaticEnv.{ env4 with local = { env4.local with return_type } }
          in
          let () =
            if false then
              Format.eprintf "@[<hov>Env after annotating return-type:@ %a@]@."
                StaticEnv.pp_local env4'.local
          in
          (env4', return_type)
    in
    (env5, { func_sig with parameters; args; return_type })
    |: TypingRule.AnnotateFuncSig
  (* End *)

  (* Begin Subprogram *)
  let annotate_subprogram (env : env) (f : AST.func) : AST.func =
    let () =
      if false then
        Format.eprintf "@[<hov>Annotating body in env:@ %a@]@." StaticEnv.pp_env
          env
    in
    (* Annotate body *)
    let body =
      match f.body with SB_ASL body -> body | SB_Primitive -> assert false
    in
    let new_body = try_annotate_block env body in
    { f with body = SB_ASL new_body } |: TypingRule.Subprogram
  (* End *)

  let try_annotate_subprogram env f = best_effort f (annotate_subprogram env)

  (******************************************************************************)
  (*                                                                            *)
  (*                           Global env and funcs                             *)
  (*                                                                            *)
  (******************************************************************************)

  (* Begin CheckSetterHasGetter *)
  let check_setter_has_getter ~loc env (func_sig : AST.func) =
    let fail () =
      fatal_from loc (Error.SetterWithoutCorrespondingGetter func_sig)
    in
    let check_true thing = check_true thing fail in
    match func_sig.subprogram_type with
    | ST_Getter | ST_EmptyGetter | ST_Function | ST_Procedure -> ok
    | ST_EmptySetter | ST_Setter ->
        let ret_type, arg_types =
          match func_sig.args with
          | [] -> fatal_from loc Error.UnrespectedParserInvariant
          | (_, ret_type) :: args -> (ret_type, List.map snd args)
        in
        let _, _, func_sig' =
          try Fn.subprogram_for_name loc env func_sig.name arg_types
          with
          | Error.(
              ASLException
                { desc = NoCallCandidate _ | TooManyCallCandidates _; _ })
          ->
            fail ()
        in
        (* Check that func_sig' is a getter *)
        let wanted_getter_type =
          match func_sig.subprogram_type with
          | ST_Setter -> ST_Getter
          | ST_EmptySetter -> ST_EmptyGetter
          | _ -> assert false
        in
        let+ () = check_true (func_sig'.subprogram_type = wanted_getter_type) in
        let+ () =
          (* Check that args match *)
          let () = assert (List.compare_lengths func_sig'.args arg_types = 0) in
          check_true
          @@ List.for_all2
               (fun (_, t1) t2 -> Types.type_equal env t1 t2)
               func_sig'.args arg_types
        in
        let+ () =
          (* Check that return types match. *)
          match func_sig'.return_type with
          | None ->
              assert
                false (* By type-checking invariant: func_sig' is a getter. *)
          | Some t -> check_true @@ Types.type_equal env ret_type t
        in
        ok |: TypingRule.CheckSetterHasGetter
  (* End *)

  (* Begin DeclareOneFunc *)
  let declare_one_func loc (func_sig : func) env =
    let env1, name' =
      best_effort (env, func_sig.name) @@ fun _ ->
      Fn.add_new_func loc env func_sig.name func_sig.args
        func_sig.subprogram_type
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
    let+ () = check_var_not_in_genv loc env1 name' in
    let+ () = check_setter_has_getter ~loc env1 func_sig in
    let new_func_sig = { func_sig with name = name' } in
    (add_subprogram name' new_func_sig env1, new_func_sig)
    |: TypingRule.DeclareOneFunc
  (* End *)

  (* Begin AnnotateAndDeclareFunc *)
  let annotate_and_declare_func ~loc func_sig env =
    let env1, func_sig1 = annotate_func_sig ~loc env func_sig in
    declare_one_func loc func_sig1 env1 |: TypingRule.AnnotateAndDeclareFunc
  (* End *)

  let add_global_storage loc name keyword env ty =
    if is_global_ignored name then env
    else
      let+ () = check_var_not_in_genv loc env name in
      add_global_storage name ty keyword env

  let declare_const loc name t v env =
    add_global_storage loc name GDK_Constant env t |> add_global_constant name v

  (* Begin DeclareType *)
  let declare_type loc name ty s env =
    let () =
      if false then Format.eprintf "Declaring type %s of %a@." name PP.pp_ty ty
    in
    let+ () = check_var_not_in_genv loc env name in
    let env1, t1 =
      match s with
      | None -> (env, ty)
      | Some (super, extra_fields) ->
          let+ () =
           fun () ->
            if Types.subtype_satisfies env ty (T_Named super |> add_pos_from loc)
            then ()
            else conflict loc [ T_Named super ] ty
          in
          let new_ty =
            if extra_fields = [] then ty
            else
              match IMap.find_opt super env.global.declared_types with
              | Some { desc = T_Record fields; _ } ->
                  T_Record (fields @ extra_fields) |> add_pos_from_st ty
              | Some { desc = T_Exception fields; _ } ->
                  T_Exception (fields @ extra_fields) |> add_pos_from_st ty
              | Some _ -> conflict loc [ T_Record []; T_Exception [] ] ty
              | None -> undefined_identifier loc super
          and env = add_subtype name super env in
          (env, new_ty)
    in
    let t2 = annotate_type ~decl:true ~loc env1 t1 in
    let env2 = add_type name t2 env1 in
    let new_tenv =
      match t2.desc with
      | T_Enum ids ->
          let t = T_Named name |> add_pos_from ty in
          let declare_one (env2, i) x =
            (declare_const loc x t (L_Int (Z.of_int i)) env2, succ i)
          in
          let env3, _ = List.fold_left declare_one (env2, 0) ids in
          env3
      | _ -> env2
    in
    let () = if false then Format.eprintf "Declared %s.@." name in
    new_tenv
  (* End *)

  let try_add_global_constant name env e =
    try
      let v = reduce_constants env e in
      add_global_constant name v env
    with Error.(ASLException { desc = UnsupportedExpr _; _ }) -> env

  (* Begin DeclareGlobalStorage *)
  let declare_global_storage loc gsd env =
    let () = if false then Format.eprintf "Declaring %s@." gsd.name in
    best_effort (gsd, env) @@ fun _ ->
    let { keyword; initial_value; ty = ty_opt; name } = gsd in
    let+ () = check_var_not_in_genv loc env name in
    let ty_opt' =
      match ty_opt with
      | Some t -> Some (annotate_type ~loc env t)
      | None -> ty_opt
    in
    let initial_value_type, initial_value' =
      match initial_value with
      | Some e ->
          let t, e' = annotate_expr env e in
          (Some t, Some e')
      | None -> (None, None)
    in
    let declared_t =
      match (initial_value_type, ty_opt') with
      | Some t1, Some t2 ->
          let+ () = check_type_satisfies loc env t1 t2 in
          t2
      | None, Some t2 -> t2
      | Some t1, None -> t1
      | None, None -> Error.fatal_from loc UnrespectedParserInvariant
    in
    let env1 = add_global_storage loc name keyword env declared_t in
    let env2 =
      match (keyword, initial_value') with
      | GDK_Constant, Some e -> try_add_global_constant name env1 e
      | GDK_Let, Some e when is_statically_evaluable ~loc env1 e ->
          let e' = StaticModel.try_normalize env1 e in
          add_global_immutable_expr name e' env1
      | (GDK_Constant | GDK_Let), None ->
          Error.fatal_from loc UnrespectedParserInvariant
      | _ -> env1
    in
    ({ gsd with ty = ty_opt'; initial_value = initial_value' }, env2)
  (* End *)

  let rename_primitive loc env (f : AST.func) =
    let name =
      best_effort f.name @@ fun _ ->
      let _, name, _ =
        Fn.subprogram_for_name loc env f.name (List.map snd f.args)
      in
      name
    in
    { f with name }

  (******************************************************************************)
  (*                                                                            *)
  (*                                Entry point                                 *)
  (*                                                                            *)
  (******************************************************************************)

  let type_check_decl d (acc, env) =
    let here = add_pos_from_st d and loc = to_pos d in
    let () =
      if false then
        Format.eprintf "@[<v>Typing with %s in env:@ %a@]@." strictness_string
          StaticEnv.pp_env env
      else if false then Format.eprintf "@[Typing %a.@]@." PP.pp_t [ d ]
    in
    match d.desc with
    (* Begin TypecheckFunc *)
    | D_Func ({ body = SB_ASL _; _ } as f) ->
        let new_env, f1 = annotate_and_declare_func ~loc f env in
        let new_d = D_Func (try_annotate_subprogram new_env f1) |> here in
        (new_d :: acc, new_env) |: TypingRule.TypecheckFunc
    (* End *)
    | D_Func ({ body = SB_Primitive; _ } as f) ->
        let new_env, f1 = annotate_and_declare_func ~loc f env in
        let new_d = D_Func f1 |> here in
        (new_d :: acc, new_env)
    (* Begin TypecheckGlobalStorage *)
    | D_GlobalStorage gsd ->
        let gsd', new_env = declare_global_storage loc gsd env in
        let new_d = D_GlobalStorage gsd' |> here in
        (new_d :: acc, new_env) |: TypingRule.TypecheckGlobalStorage
    (* End *)
    (* Begin TypecheckTypeDecl *)
    | D_TypeDecl (x, ty, s) ->
        let new_env = declare_type loc x ty s env in
        (d :: acc, new_env) |: TypingRule.TypecheckTypeDecl
  (* End *)

  let type_check_mutually_rec ds (acc, env) =
    let () =
      if false then
        let open Format in
        eprintf "@[Type-checking@ mutually@ recursive@ declarations:@ %a@]@."
          (pp_print_list ~pp_sep:pp_print_space pp_print_string)
          (List.map identifier_of_decl ds)
    in
    let env_and_fs =
      List.map
        (fun d ->
          match d.desc with
          | D_Func f ->
              let loc = to_pos d in
              let env', f = annotate_func_sig ~loc env f in
              (env'.local, f, loc)
          | _ ->
              fatal_from d
                (Error.BadRecursiveDecls
                   (List.map ASTUtils.identifier_of_decl ds)))
        ds
    in
    let env_and_fs =
      (* Setters last as they need getters declared. *)
      let others, setters =
        List.partition
          (fun (_, f, _) ->
            match f.subprogram_type with
            | ST_Setter | ST_EmptySetter -> true
            | _ -> false)
          env_and_fs
      in
      List.rev_append setters others
    in
    let genv, fs =
      list_fold_left_map
        (fun genv (lenv, f, loc) ->
          let env = { global = genv; local = lenv } in
          let env', f = declare_one_func loc f env in
          (env'.global, (env'.local, f, loc)))
        env.global env_and_fs
    in
    let ds =
      List.map
        (fun (lenv, f, loc) ->
          let here = add_pos_from loc in
          let env' = { local = lenv; global = genv } in
          match f.body with
          | SB_ASL _ ->
              let () =
                if false then Format.eprintf "@[Analysing decl %s.@]@." f.name
              in
              D_Func (try_annotate_subprogram env' f) |> here
          | SB_Primitive -> D_Func (rename_primitive loc env' f) |> here)
        fs
    in
    (List.rev_append ds acc, { env with global = genv })

  (* Begin Specification *)
  let type_check_ast =
    let fold = function
      | TopoSort.ASTFold.Single d -> type_check_decl d
      | TopoSort.ASTFold.Recursive ds -> type_check_mutually_rec ds
    in
    let fold =
      if false then (fun d e ->
        let res = fold d e in
        Format.eprintf "Ended type-checking of this declaration.@.";
        res)
      else fold
    in
    let fold_topo ast acc = TopoSort.ASTFold.fold fold ast acc in
    fun ast env ->
      let ast_rev, env = fold_topo ast ([], env) in
      (List.rev ast_rev, env)
  (* End *)
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

let type_check_ast = function
  | `TypeCheck -> TypeCheck.type_check_ast
  | `Warn -> TypeInferWarn.type_check_ast
  | `Silence -> TypeInferSilence.type_check_ast
