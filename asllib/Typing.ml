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
module SES = SideEffect.SES
module TimeFrame = SideEffect.TimeFrame

let ( |: ) = Instrumentation.TypingNoInstr.use_with
let fatal_from ~loc = Error.fatal_from loc
let undefined_identifier ~loc x = fatal_from ~loc (Error.UndefinedIdentifier x)
let invalid_expr e = fatal_from ~loc:e (Error.InvalidExpr e)
let add_pos_from ~loc = add_pos_from loc

let conflicting_side_effects_error ~loc (s1, s2) =
  fatal_from ~loc Error.(ConflictingSideEffects (s1, s2))

let ses_non_conflicting_union ~loc =
  SES.non_conflicting_union ~fail:(conflicting_side_effects_error ~loc)

let ses_non_conflicting_unions ~loc =
  SES.non_conflicting_unions ~fail:(conflicting_side_effects_error ~loc)

let conflict ~loc expected provided =
  fatal_from ~loc (Error.ConflictingTypes (expected, provided))

let plus = binop PLUS
let t_bits_bitwidth e = T_Bits (e, [])

let rec list_mapi2 f i l1 l2 =
  match (l1, l2) with
  | [], [] -> []
  | a1 :: l1, a2 :: l2 ->
      let r = f i a1 a2 in
      r :: list_mapi2 f (i + 1) l1 l2
  | _, _ -> invalid_arg "List.map2"

let rec list_mapi3 f i l1 l2 l3 =
  match (l1, l2, l3) with
  | [], [], [] -> []
  | a1 :: l1, a2 :: l2, a3 :: l3 ->
      let r = f i a1 a2 a3 in
      r :: list_mapi3 f (i + 1) l1 l2 l3
  | _, _, _ -> invalid_arg "List.mapi3"

let sum = function [] -> !$0 | [ x ] -> x | h :: t -> List.fold_left plus h t

(* Begin SlicesWidth *)
let slices_width env =
  let minus = binop MINUS in
  let slice_width = function
    | Slice_Single _ -> one_expr
    | Slice_Star (_, e) | Slice_Length (_, e) -> e
    | Slice_Range (e1, e2) -> plus one_expr (minus e1 e2)
  in
  fun li -> List.map slice_width li |> sum |> StaticModel.try_normalize env
(* End *)

let width_plus env acc w = plus acc w |> StaticModel.try_normalize env

(* Begin RenameTyEqs *)
let rename_ty_eqs : env -> (AST.identifier * AST.expr) list -> AST.ty -> AST.ty
    =
  let subst_expr_normalize env eqs e =
    subst_expr eqs e |> StaticModel.try_normalize env
  in
  let subst_constraint env eqs = function
    | Constraint_Exact e -> Constraint_Exact (subst_expr_normalize env eqs e)
    | Constraint_Range (e1, e2) ->
        Constraint_Range
          (subst_expr_normalize env eqs e1, subst_expr_normalize env eqs e2)
  in
  let subst_constraints env eqs = List.map (subst_constraint env eqs) in
  let rec rename env eqs ty =
    let here desc = add_pos_from ~loc:ty desc in
    match ty.desc with
    | T_Bits (e, fields) ->
        T_Bits (subst_expr_normalize env eqs e, fields) |> here
    | T_Int (WellConstrained constraints) ->
        let constraints = subst_constraints env eqs constraints in
        T_Int (WellConstrained constraints) |> here
    | T_Int (Parameterized (_uid, name)) ->
        let e = E_Var name |> here |> subst_expr_normalize env eqs in
        T_Int (WellConstrained [ Constraint_Exact e ]) |> here
    | T_Tuple tys -> T_Tuple (List.map (rename env eqs) tys) |> here
    | _ -> ty
  in
  rename |: TypingRule.RenameTyEqs
(* End *)

(* Begin Lit *)
let annotate_literal env = function
  | L_Int _ as v -> integer_exact' (literal v)
  | L_Bool _ -> T_Bool
  | L_Real _ -> T_Real
  | L_String _ -> T_String
  | L_BitVector bv -> Bitvector.length bv |> expr_of_int |> t_bits_bitwidth
  | L_Label label -> (
      try IMap.find label env.global.declared_types |> fst |> desc
      with Not_found -> assert false)
(* End *)

(** [set_filter_map f set] is the list of [y] such that [f x = Some y] for all
    elements [x] of [set]. *)
let set_filter_map f set =
  let folder e acc = match f e with None -> acc | Some x -> x :: acc in
  ISet.fold folder set []

(* ---------------------------------------------------------------------------

                              Properties handling

   ---------------------------------------------------------------------------*)

type strictness = Silence | Warn | TypeCheck | TypeCheckNoWarn

module type ANNOTATE_CONFIG = sig
  val check : strictness
  val output_format : Error.output_format
  val print_typed : bool
  val use_field_getter_extension : bool
end

module type S = sig
  val type_check_ast : AST.t -> AST.t * global
  val type_check_ast_in_env : global -> AST.t -> AST.t * global
end

module Property (C : ANNOTATE_CONFIG) = struct
  module EP = Error.ErrorPrinter (C)

  exception TypingAssumptionFailed

  type ('a, 'b) property = 'a -> 'b
  type prop = (unit, unit) property

  let strictness_string =
    match C.check with
    | TypeCheck -> "type-checking-strict"
    | TypeCheckNoWarn -> "type-checking-strict-no-warn"
    | Warn -> "type-checking-warn"
    | Silence -> "type-inference"

  let check : prop -> prop =
    match C.check with
    | TypeCheckNoWarn | TypeCheck -> fun f () -> f ()
    | Warn -> (
        fun f () -> try f () with Error.ASLException e -> EP.eprintln e)
    | Silence -> fun _f () -> ()

  let best_effort' : ('a, 'a) property -> ('a, 'a) property =
    match C.check with
    | TypeCheckNoWarn | TypeCheck -> fun f x -> f x
    | Warn -> (
        fun f x ->
          try f x
          with Error.ASLException e ->
            EP.eprintln e;
            x)
    | Silence -> ( fun f x -> try f x with Error.ASLException _ -> x)

  let warn_from =
    match C.check with
    | TypeCheckNoWarn | Silence -> fun ~loc:_ _ -> ()
    | TypeCheck | Warn -> EP.warn_from

  let best_effort : 'a -> ('a, 'a) property -> 'a = fun x f -> best_effort' f x
  let[@inline] ( let+ ) m f = check m () |> f

  let either (p1 : ('a, 'b) property) (p2 : ('a, 'b) property) x =
    try p1 x with TypingAssumptionFailed | Error.ASLException _ -> p2 x

  let assumption_failed () = raise TypingAssumptionFailed [@@inline]
  let ok () = () [@@inline]
  let check_true b fail () = if b then () else fail () [@@inline]
  let check_all2 li1 li2 f () = List.iter2 (fun x1 x2 -> f x1 x2 ()) li1 li2
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
  let add_new_func ~loc env name formals subpgm_type =
    match IMap.find_opt name env.global.overloaded_subprograms with
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
                 let other_func_sig, _ses =
                   IMap.find name' env.global.subprograms
                 in
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
            fatal_from ~loc (Error.AlreadyDeclaredIdentifier name)
        in
        let new_env = set_renamings name (ISet.add new_name other_names) env in
        (new_env, new_name) |: TypingRule.AddNewFunc
  (* End *)

  (* Begin SubprogramForName *)
  let subprogram_for_name ~loc env version name caller_arg_types =
    let () =
      if false then Format.eprintf "Trying to rename call to %S@." name
    in
    let renaming_set =
      try IMap.find name env.global.overloaded_subprograms
      with Not_found -> undefined_identifier ~loc name
    in
    let get_func_sig name' =
      match IMap.find_opt name' env.global.subprograms with
      | Some (func_sig, ses)
        when has_arg_clash env caller_arg_types func_sig.args ->
          Some (name', func_sig, ses)
      | _ -> None
    in
    let matching_renamings = set_filter_map get_func_sig renaming_set in
    match matching_renamings with
    | [ (name', func_sig, ses) ] -> (
        match version with
        | V0 ->
            (deduce_eqs env caller_arg_types func_sig.args, name', func_sig, ses)
        | V1 -> ([], name', func_sig, ses) |: TypingRule.SubprogramForName)
    | [] -> fatal_from ~loc (Error.NoCallCandidate (name, caller_arg_types))
    | _ :: _ ->
        fatal_from ~loc (Error.TooManyCallCandidates (name, caller_arg_types))
  (* End *)

  let try_subprogram_for_name =
    match C.check with
    | TypeCheckNoWarn | TypeCheck -> subprogram_for_name
    | Warn | Silence -> (
        fun ~loc env version name caller_arg_types ->
          try subprogram_for_name ~loc env version name caller_arg_types
          with Error.ASLException _ as error -> (
            try
              match IMap.find_opt name env.global.subprograms with
              | None -> undefined_identifier ~loc ("function " ^ name)
              | Some (func_sig, ses) ->
                  if false then
                    Format.eprintf "@[<2>%a:@ No extra arguments for %s@]@."
                      PP.pp_pos loc name;
                  ([], name, func_sig, ses)
            with Error.ASLException _ -> raise error))
end

(* ---------------------------------------------------------------------------

                           Main type-checking module

   ---------------------------------------------------------------------------*)

module Annotate (C : ANNOTATE_CONFIG) : S = struct
  open Property (C)
  module Fn = FunctionRenaming (C)

  module SOp = StaticOperations.Make (struct
    let fail = assumption_failed
    let warn_from = warn_from
  end)

  (* Begin LDKIsImmutable *)
  let ldk_is_immutable = function
    | LDK_Constant | LDK_Let -> true
    | LDK_Var -> false
  (* End *)

  (* Begin GDKIsImmutable *)
  let gdk_is_immutable = function
    | GDK_Config | GDK_Constant | GDK_Let -> true
    | GDK_Var -> false
  (* End *)

  (* Begin ShouldReduceToCall *)
  let should_reduce_to_call env name st =
    match IMap.find_opt name env.global.overloaded_subprograms with
    | None -> false
    | Some set ->
        ISet.exists
          (fun name' ->
            match IMap.find_opt name' env.global.subprograms with
            | None -> assert false
            | Some (func_sig, _ses) -> func_sig.subprogram_type = st)
          set
        |: TypingRule.ShouldReduceToCall
  (* End *)

  (* Begin DisjointSlicesToPositions *)
  let disjoint_slices_to_positions ~loc env slices =
    let module DI = Diet.Int in
    let exception NonStatic in
    let eval env e =
      match StaticModel.reduce_to_z_opt env e with
      | Some z -> Z.to_int z
      | None -> raise NonStatic
    in
    let interval_of_slice env slice =
      let make_interval x y =
        if x > y then fatal_from ~loc @@ Error.(BadSlice slice)
        else DI.Interval.make x y
      in
      match slice with
      | Slice_Single e ->
          let x = eval env e in
          make_interval x x |: TypingRule.BitfieldSliceToPositions
      | Slice_Range (e1, e2) ->
          let x = eval env e2 and y = eval env e1 in
          make_interval x y |: TypingRule.BitfieldSliceToPositions
      | Slice_Length (e1, e2) ->
          let x = eval env e1 and y = eval env e2 in
          make_interval x (x + y - 1) |: TypingRule.BitfieldSliceToPositions
      | Slice_Star (e1, e2) ->
          let x = eval env e1 and y = eval env e2 in
          make_interval (x * y) ((x * (y + 1)) - 1)
          |: TypingRule.BitfieldSliceToPositions
    in
    let bitfield_slice_to_positions ~loc env diet slice =
      try
        let interval = interval_of_slice env slice in
        let new_diet = DI.add interval DI.empty in
        if DI.is_empty (Diet.Int.inter new_diet diet) then DI.add interval diet
        else fatal_from ~loc Error.(OverlappingSlices (slices, Static))
      with NonStatic -> diet
    in
    List.fold_left (bitfield_slice_to_positions ~loc env) Diet.Int.empty slices
    |: TypingRule.DisjointSlicesToPositions
  (* End *)

  (* Begin TypingRule.CheckDisjointSlices *)
  let check_disjoint_slices ~loc env slices =
    if List.length slices <= 1 then ok
    else fun () ->
      let _ = disjoint_slices_to_positions ~loc env slices in
      () |: TypingRule.CheckDisjointSlices
  (* End *)

  exception NoSingleField

  (** [to_singles env slices] is a list of [Slice_Single] slices
      for each bit position of each bitfield slice in [slices]. *)
  let to_singles env =
    let eval e =
      match StaticInterpreter.static_eval env e with
      | L_Int z -> Z.to_int z
      | _ -> raise NoSingleField
    in
    let one slice acc =
      match slice with
      | Slice_Single e -> e :: acc
      | Slice_Length (e1, e2) ->
          let i1 = eval e1 and i2 = eval e2 in
          let rec do_rec n =
            if n >= i2 then acc
            else
              let e =
                E_Literal (L_Int (Z.of_int (i1 + n))) |> add_dummy_annotation
              in
              e :: do_rec (n + 1)
          in
          do_rec 0
      | Slice_Range (e1, e2) ->
          let i1 = eval e1 and i2 = eval e2 in
          let rec do_rec i =
            if i > i1 then acc
            else
              let e = E_Literal (L_Int (Z.of_int i)) |> add_dummy_annotation in
              e :: do_rec (i + 1)
          in
          do_rec i2
      | Slice_Star _ -> raise NoSingleField
    in
    fun slices -> List.fold_right one slices []

  (** Retrieves the slices associated with the given bitfield
      without recursing into nested bitfields. *)
  let slices_of_bitfield = function
    | BitField_Simple (_, slices)
    | BitField_Nested (_, slices, _)
    | BitField_Type (_, slices, _) ->
        slices

  (** Retrieves the slice of [Slice_Single] slices for each position
      of the bitfield [field], if it is found in [bf]. *)
  let field_to_single env bf field =
    match find_bitfield_opt field bf with
    | Some bitfield -> to_singles env (slices_of_bitfield bitfield)
    | None -> raise NoSingleField

  (** Checks that all bitfields listed in [fields] are delcared in the
      bitvector type [ty]. If so, retrieves a list of [Slice_Single] slices for
      each bit position of each bitfield slice of each bitfield in [fields].
      [name] is passed along, if the result is not [None] for convenience of
      use.

      It is an ASLRef extension, guarded by [C.use_field_getter_extension].
  *)
  let should_fields_reduce_to_call env name ty fields =
    assert C.use_field_getter_extension;
    match (Types.make_anonymous env ty).desc with
    | T_Bits (_, bf) -> (
        try Some (name, list_concat_map (field_to_single env bf) fields)
        with NoSingleField -> None)
    | _ -> None
  (* End *)

  (* -------------------------------------------------------------------------

                              Annotate AST

     -------------------------------------------------------------------------- *)

  (* Begin GetBitvectorWidth *)
  let get_bitvector_width' env t =
    match (Types.get_structure env t).desc with
    | T_Bits (n, _) -> n
    | _ -> assumption_failed ()

  let get_bitvector_width ~loc env t =
    try get_bitvector_width' env t |: TypingRule.GetBitvectorWidth
    with TypingAssumptionFailed -> conflict ~loc [ default_t_bits ] t
  (* End *)

  (* Begin GetBitvectorConstWidth *)
  let get_bitvector_const_width ~loc env t =
    let e_width = get_bitvector_width ~loc env t in
    match StaticInterpreter.static_eval env e_width with
    | L_Int z -> Z.to_int z |: TypingRule.GetBitvectorConstWidth
    | _ -> assert false
  (* End *)

  (** [check_type_satisfies t1 t2] if [t1 <: t2]. *)
  let check_type_satisfies ~loc env t1 t2 () =
    let () =
      if false then
        Format.eprintf "@[<hv 2>Checking %a@ <: %a@]@." PP.pp_ty t1 PP.pp_ty t2
    in
    if Types.type_satisfies env t1 t2 then () else conflict ~loc [ t2.desc ] t1

  (* CheckStructureBoolean *)

  (** [check_structure_boolean env t1] checks that [t1] has the structure of a boolean. *)
  let check_structure_boolean ~loc env t1 () =
    match (Types.get_structure env t1).desc with
    | T_Bool -> ()
    | _ -> conflict ~loc [ T_Bool ] t1
  (* End *)

  (* CheckStructureBits *)
  let check_structure_bits ~loc env t () =
    match (Types.get_structure env t).desc with
    | T_Bits _ -> ()
    | _ -> conflict ~loc [ default_t_bits ] t
  (* End *)

  (* Begin CheckStructureInteger *)
  let check_structure_integer ~loc env t () =
    let () =
      if false then
        Format.eprintf "Checking that %a is an integer.@." PP.pp_ty t
    in
    match (Types.make_anonymous env t).desc with
    | T_Int _ -> ()
    | _ -> conflict ~loc [ integer' ] t
  (* End *)

  (* Begin CheckConstrainedInteger *)
  let check_constrained_integer ~loc env t () =
    match (Types.make_anonymous env t).desc with
    | T_Int UnConstrained ->
        fatal_from ~loc Error.(ConstrainedIntegerExpected t)
    | T_Int (WellConstrained _ | Parameterized _) -> ()
    | _ -> conflict ~loc [ integer' ] t
  (* End *)

  (* Begin CheckStructureException *)
  let check_structure_exception ~loc env t () =
    let t_struct = Types.get_structure env t in
    match t_struct.desc with
    | T_Exception _ -> ()
    | _ -> conflict ~loc [ T_Exception [] ] t_struct
  (* End *)

  (* Begin CheckStaticallyEvaluable *)
  let check_statically_evaluable expr_for_error ses () =
    if SES.is_statically_evaluable ses then
      () |: TypingRule.CheckStaticallyEvaluable
    else
      fatal_from ~loc:expr_for_error
        (Error.ImpureExpression (expr_for_error, ses))
  (* End *)

  let check_is_deterministic expr_for_error ses () =
    if SES.is_deterministic ses then ()
    else
      fatal_from ~loc:expr_for_error
        (Error.ImpureExpression (expr_for_error, ses))

  let check_is_pure expr_for_error ses () =
    if SES.is_pure ses then ()
    else
      fatal_from ~loc:expr_for_error
        (Error.ImpureExpression (expr_for_error, SES.remove_pure ses))

  let leq_config_time ses =
    TimeFrame.is_before (SES.max_time_frame ses) TimeFrame.Config

  let check_leq_config_time ~loc (_, e, ses_e) () =
    if leq_config_time ses_e then ()
    else fatal_from ~loc Error.(ConfigTimeBroken (e, ses_e))

  let leq_constant_time ses =
    TimeFrame.is_before (SES.max_time_frame ses) TimeFrame.Constant

  let check_leq_constant_time ~loc (_, e, ses_e) () =
    if leq_constant_time ses_e then ()
    else fatal_from ~loc Error.(ConstantTimeBroken (e, ses_e))

  let check_is_time_frame =
    let open TimeFrame in
    function
    | TimeFrame.Constant -> check_leq_constant_time
    | TimeFrame.Config -> check_leq_config_time
    | TimeFrame.Execution -> fun ~loc:_ _ -> ok

  let check_bits_equal_width' env t1 t2 () =
    let n = get_bitvector_width' env t1 and m = get_bitvector_width' env t2 in
    if bitwidth_equal (StaticModel.equal_in_env env) n m then ()
    else assumption_failed ()

  (* Begin CheckBitsEqualWidth *)
  let check_bits_equal_width ~loc env t1 t2 () =
    try check_bits_equal_width' env t1 t2 ()
    with TypingAssumptionFailed ->
      fatal_from ~loc (Error.UnreconciliableTypes (t1, t2))
  (* End *)

  let binop_is_ordered = function
    | BAND | BOR | IMPL -> true
    | AND | BEQ | DIV | DIVRM | EOR | EQ_OP | GT | GEQ | LT | LEQ | MOD | MINUS
    | MUL | NEQ | OR | PLUS | POW | RDIV | SHL | SHR | BV_CONCAT ->
        false

  (* Begin TypeOfArrayLength *)
  let type_of_array_length ~loc = function
    | ArrayLength_Enum (s, _) -> T_Named s |> add_pos_from ~loc
    | ArrayLength_Expr _ -> integer |: TypingRule.TypeOfArrayLength
  (* End *)

  (* Begin ApplyBinopTypes *)
  let rec apply_binop_types ~loc env op t1 t2 : ty =
    let () =
      if false then
        Format.eprintf "Checking binop %s between %a and %a@."
          (PP.binop_to_string op) PP.pp_ty t1 PP.pp_ty t2
    in
    let here x = add_pos_from ~loc x in
    (match (op, (t1.desc, t2.desc)) with
    | _, (T_Named _, _) | _, (_, T_Named _) ->
        let t1_anon = Types.make_anonymous env t1
        and t2_anon = Types.make_anonymous env t2 in
        apply_binop_types ~loc env op t1_anon t2_anon
    | (BAND | BOR | BEQ | IMPL), (T_Bool, T_Bool) -> T_Bool |> here
    | (AND | OR | EOR | PLUS | MINUS), (T_Bits (w1, _), T_Bits (w2, _))
      when bitwidth_equal (StaticModel.equal_in_env env) w1 w2 ->
        T_Bits (w1, []) |> here
    | BV_CONCAT, (T_Bits (w1, _), T_Bits (w2, _)) ->
        T_Bits (width_plus env w1 w2, []) |> here
    | (PLUS | MINUS), (T_Bits (w, _), T_Int _) -> T_Bits (w, []) |> here
    | (LEQ | GEQ | GT | LT), (T_Int _, T_Int _ | T_Real, T_Real)
    | ( (EQ_OP | NEQ),
        (T_Int _, T_Int _ | T_Bool, T_Bool | T_Real, T_Real | T_String, T_String)
      ) ->
        T_Bool |> here
    | (EQ_OP | NEQ), (T_Bits (w1, _), T_Bits (w2, _))
      when bitwidth_equal (StaticModel.equal_in_env env) w1 w2 ->
        T_Bool |> here
    | (EQ_OP | NEQ), (T_Enum li1, T_Enum li2)
      when list_equal String.equal li1 li2 ->
        T_Bool |> here
    | ( (MUL | DIV | DIVRM | MOD | SHL | SHR | POW | PLUS | MINUS),
        (T_Int c1, T_Int c2) ) -> (
        match (c1, c2) with
        | PendingConstrained, _ | _, PendingConstrained -> assert false
        | UnConstrained, _ | _, UnConstrained -> T_Int UnConstrained |> here
        | Parameterized _, _ | _, Parameterized _ ->
            let t1_well_constrained = Types.to_well_constrained t1
            and t2_well_constrained = Types.to_well_constrained t2 in
            apply_binop_types ~loc env op t1_well_constrained
              t2_well_constrained
        | WellConstrained cs1, WellConstrained cs2 -> (
            best_effort integer @@ fun _ ->
            try
              let cs = SOp.annotate_constraint_binop ~loc env op cs1 cs2 in
              T_Int (WellConstrained cs) |> here
            with TypingAssumptionFailed ->
              fatal_from ~loc (Error.BadTypesForBinop (op, t1, t2))))
    | (PLUS | MINUS | MUL), (T_Real, T_Real)
    | POW, (T_Real, T_Int _)
    | RDIV, (T_Real, T_Real) ->
        T_Real |> here
    | _ -> fatal_from ~loc (Error.BadTypesForBinop (op, t1, t2)))
    |: TypingRule.ApplyBinopTypes
  (* End *)

  (* Begin ApplyUnopType *)
  let apply_unop_type ~loc env op t =
    let here desc = add_pos_from ~loc desc in
    match op with
    | BNOT ->
        let+ () = check_type_satisfies ~loc env t boolean in
        T_Bool |> here
    | NEG -> (
        let+ () =
          either
            (check_type_satisfies ~loc env t integer)
            (check_type_satisfies ~loc env t real)
        in
        let t_struct = Types.get_well_constrained_structure env t in
        match t_struct.desc with
        | T_Int UnConstrained -> T_Int UnConstrained |> here
        | T_Int (WellConstrained cs) ->
            let neg e = unop NEG e in
            let constraint_minus = function
              | Constraint_Exact e -> Constraint_Exact (neg e)
              | Constraint_Range (top, bot) ->
                  Constraint_Range (neg bot, neg top)
            in
            T_Int (WellConstrained (List.map constraint_minus cs)) |> here
        | T_Int (Parameterized _) ->
            assert false (* We used to_well_constrained just before. *)
        | _ -> (* fail case *) t)
    | NOT ->
        let+ () = check_structure_bits ~loc env t in
        t |: TypingRule.ApplyUnopType
  (* End *)

  (* Begin CheckATC *)
  let rec check_atc ~fail env t1 t2 =
    if Types.type_equal env t1 t2 then ok
    else
      match (t1.desc, t2.desc) with
      | T_Int _, T_Int _ | T_Bits _, T_Bits _ -> ok
      | T_Tuple l1, T_Tuple l2 when List.compare_lengths l1 l2 = 0 ->
          check_all2 l1 l2 (check_atc ~fail env)
      | T_Named _, _ | _, T_Named _ -> assert false
      | _ -> fail |: TypingRule.CheckATC
  (* End *)

  (* Begin CheckVarNotInEnv *)
  let check_var_not_in_env ~loc env x () =
    if is_undefined x env then () |: TypingRule.CheckVarNotInEnv
    else fatal_from ~loc (Error.AlreadyDeclaredIdentifier x)
  (* End *)

  (* Begin CheckVarNotInGEnv *)
  let check_var_not_in_genv ~loc genv x () =
    if is_global_undefined x genv then () |: TypingRule.CheckVarNotInGEnv
    else fatal_from ~loc (Error.AlreadyDeclaredIdentifier x)
  (* End *)

  (* Begin GetVariableEnum *)
  let get_variable_enum' env e =
    match e.desc with
    | E_Var x -> (
        match IMap.find_opt x env.global.declared_types with
        | Some (t, _) -> (
            match (Types.make_anonymous env t).desc with
            | T_Enum labels -> Some (x, labels)
            | _ -> None)
        | None -> None)
    | _ -> None
  (* End *)

  (* Begin CheckPositionsInWidth *)
  let check_diet_in_width ~loc slices width diet () =
    let min_pos = Diet.Int.min_elt diet |> Diet.Int.Interval.x
    and max_pos = Diet.Int.max_elt diet |> Diet.Int.Interval.y in
    if 0 <= min_pos && max_pos < width then
      () |: TypingRule.CheckPositionsInWidth
    else fatal_from ~loc (BadSlices (Error.Static, slices, width))
  (* End *)

  (* Begin CheckSlicesInWidth *)
  let check_slices_in_width ~loc env width slices () =
    let diet = disjoint_slices_to_positions ~loc env slices in
    check_diet_in_width ~loc slices width diet ()
    |: TypingRule.CheckSlicesInWidth
  (* End *)

  (** A module for checking that all bitfields of a given bitvector type
      that share the same name and exist in the same scope (defined below)
      also define the same slice of the bitvector type.
  *)
  module CheckCommonBitfieldsAlign : sig
    val check :
      loc:'a annotated -> StaticEnv.env -> bitfield list -> int -> unit
  end = struct
    type interval = int * int
    (** [(i, j)] is the set of integers from [i] to [j], inclusive. *)

    type absolute_bitfield = {
      name : identifier;
      abs_scope : identifier list;
      abs_slices : interval list;
    }
    (** An absolute bitfield [abs_f] corresponds to a bitfield [f].
        It consists of the following fields:
        - [name] the name of the bitfield as declared;
        - [abs_scope] is the list of names of ancestor bitfields, starting from the top; and
        - [abs_slices] is a list of intervals that represent the sequence of indices,
          corresponding to the slices defined for [f],
          relative to the bitvector type that declares [f].

        For example in
        [
          type Nested_Type of bits(3) {
            [2:1] f1 {
              [0] f2
            }
          };
        ]
        we have the follwing absolute fields:
        [
          {name="f1"; abs_cope=[];      abs_slices=[2:1]}
          {name="f2"; abs_cope=["f1"];  abs_slices=[1:1]}
        ]
    *)

    let pp_abs_name fmt abs_name =
      let abs_name_minus_top =
        match abs_name with
        | h :: t ->
            let () = assert (String.equal h "") in
            t
        | _ -> assert false
      in
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ".")
        (fun fmt id -> Format.fprintf fmt "%s" id)
        fmt abs_name_minus_top

    let pp_abs_slice fmt (from_idx, to_idx) =
      Format.fprintf fmt "%i:%i" to_idx from_idx

    let pp_abs_slices intervals =
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
        pp_abs_slice intervals

    let pp_absolute_bitfield fmt { name; abs_scope; abs_slices } =
      Format.fprintf fmt "[%a] %a" pp_abs_slices abs_slices pp_abs_name
        (List.append abs_scope [ name ])

    let interval_equal (interval1 : interval) (interval2 : interval) =
      let lo1, hi1 = interval1 in
      let lo2, hi2 = interval2 in
      lo1 == lo2 && hi1 == hi2

    let intervals_equal intervals1 intervals2 =
      list_equal interval_equal intervals1 intervals2

    (** [either_prefix list1 list2] is true if either [list1] is a prefix of [list2] or
        [list2] is a prefix of [list1].
    *)
    let rec either_prefix list1 list2 =
      match (list1, list2) with
      | [], _ | _, [] -> true
      | h1 :: t1, h2 :: t2 -> String.equal h1 h2 && either_prefix t1 t2

    let exist_in_same_scope abs_f1 abs_f2 =
      either_prefix abs_f1.abs_scope abs_f2.abs_scope

    (** {iter_ordered_pairs f [e_1;...;e_k]} applies [f e_i e_j] for every [1 <= i < j <= k].
    *)
    let rec iter_ordered_pairs f l =
      match l with
      | [] | [ _ ] -> ()
      | h :: t ->
          let () = List.iter (fun e_t -> f h e_t) t in
          iter_ordered_pairs f t

    (** Returns the sequence of indices given by [slice]
        as an interval.
    *)
    let slice_to_indices env slice : interval =
      match slice with
      | Slice_Length (i, w) ->
          let z_i = StaticInterpreter.static_eval_to_int env i in
          let z_w = StaticInterpreter.static_eval_to_int env w in
          (z_i, z_i + z_w - 1)
      | _ ->
          (* should have been de-sugated into Slice_Length *)
          assert false

    (** Merge all adjacent intervals that can be represented by a single interval.
    *)
    let coalesce_intervals intervals =
      List.fold_left
        (fun acc_intervals slice ->
          match acc_intervals with
          | [] -> [ slice ]
          | _ ->
              let split_prefix_last list =
                match List.rev list with
                | [] -> assert false
                | h :: t -> (List.rev t, h)
              in
              let prefix, (last_lo, last_hi) =
                split_prefix_last acc_intervals
              in
              let lo, hi = slice in
              if lo == last_hi + 1 then List.append prefix [ (last_lo, hi) ]
              else List.append acc_intervals [ slice ])
        [] intervals

    (** Assuming that [(int_lo, int_hi)] can be sliced with [(slice_lo, slice_hi)]
            into a non-empty interval, return the sliced interval as [intersected]
            and any indices of [(slice_lo, slice_hi)] that reach beyond
            [(int_lo, int_hi)] in [remainder_opt], if ther are any.
    *)
    let slice_interval_by_slice (int_lo, int_hi) (slice_lo, slice_hi) :
        interval * interval option =
      let shifted_lo, shifted_hi = (slice_lo + int_lo, slice_hi + int_lo) in
      let intersected = (shifted_lo, min shifted_hi int_hi) in
      let remainder_lo = int_hi - int_lo + 1 in
      let remainder_opt =
        if int_hi < shifted_hi then Some (remainder_lo, slice_hi) else None
      in
      (intersected, remainder_opt)

    (** Returns the slice of the sequence of indices represented by [intervals],
        defined by the relative positions given by [slice].
    *)
    let rec slice_indices_by_slice acc intervals ((slice_lo, slice_hi) as slice)
        =
      (* slice_indices_by_slice [(9,12); (2,7)] (2,5) = [(11,12); (2,3)] *)
      match intervals with
      | [] -> []
      | ((head_lo, head_hi) as head) :: intervals_tail -> (
          let head_last_idx = head_hi - head_lo in
          let head_len = head_last_idx + 1 in
          if slice_lo > head_last_idx then
            slice_indices_by_slice acc intervals_tail
              (slice_lo - head_len, slice_hi - head_len)
          else
            let intersected, remainder_opt =
              slice_interval_by_slice head slice
            in
            let next_acc = intersected :: acc in
            match remainder_opt with
            | None -> List.rev next_acc
            | Some slice' -> slice_indices_by_slice next_acc intervals slice')

    (** Returns the slice of the sequence of indices given by [indices],
        defined by the relative positions given by [slices].
    *)
    let slice_indices indices slices =
      list_concat_map (slice_indices_by_slice [] indices) slices
      |> coalesce_intervals

    (** Returns the list of absolute bitfields for the bitfield [bf]
        given that [absolute_parent] is the absolute bitfield
        for the bitfield where [bf] is declared.
    *)
    let rec bitfield_to_absolute env bf absolute_parent =
      let { name; abs_scope; abs_slices } = absolute_parent in
      let bf_name = bitfield_get_name bf in
      let bf_scope = List.append abs_scope [ name ] in
      let slices_as_indices =
        List.map (slice_to_indices env) (bitfield_get_slices bf)
      in
      let bf_indices = slice_indices abs_slices slices_as_indices in
      let bf_absolute =
        { name = bf_name; abs_scope = bf_scope; abs_slices = bf_indices }
      in
      let nested = bitfield_get_nested bf in
      bf_absolute :: bitfields_to_absolute env nested bf_absolute

    (** Returns the list of absolute bitfields for the bitfields [bitfields]
        given that [absolute_parent] is the absolute bitfield
        where [bitfields] are declared.
    *)
    and bitfields_to_absolute env bitfields absolute_parent =
      list_concat_map
        (fun bf -> bitfield_to_absolute env bf absolute_parent)
        bitfields

    (** Tests whether absolute fields [f1] and [f2] are aligned.
        If the two fields don't share a name or don't exist in the same
        scope, the result is true.
    *)
    let absolute_fields_align f1 f2 =
      if String.equal f1.name f2.name && exist_in_same_scope f1 f2 then
        let { abs_slices = indices1 } = f1 in
        let { abs_slices = indices2 } = f2 in
        intervals_equal indices1 indices2
      else true

    (* Begin TypingRule.CheckCommonBitfieldsAlign *)
    let check ~loc env bitfields width =
      (* define a fake absolute field representing the entire bitvector. *)
      let top_absolute =
        { name = ""; abs_scope = []; abs_slices = [ (0, width - 1) ] }
      in
      let absolute_bitfields =
        bitfields_to_absolute env bitfields top_absolute
      in
      let () =
        if false then
          List.iter
            (fun f ->
              Format.eprintf "absolute field %a@." pp_absolute_bitfield f)
            absolute_bitfields
      in
      iter_ordered_pairs
        (fun f1 f2 ->
          let { name = name1; abs_scope = scope1; abs_slices = indices1 } =
            f1
          in
          let { name = name2; abs_scope = scope2; abs_slices = indices2 } =
            f2
          in
          let () =
            if false then
              Format.eprintf
                "checking %a and %a | same scope: %b | same name: %b | equal \
                 slices: %b@."
                pp_absolute_bitfield f1 pp_absolute_bitfield f2
                (exist_in_same_scope f1 f2)
                (String.equal name1 name2)
                (intervals_equal indices1 indices2)
          in
          if not (absolute_fields_align f1 f2) then
            let abs_name1 = scope1 @ [ name1 ] in
            let abs_name2 = scope2 @ [ name2 ] in
            let abs_name1_str = Format.asprintf "%a" pp_abs_name abs_name1 in
            let abs_name2_str = Format.asprintf "%a" pp_abs_name abs_name2 in
            let indices1_str = Format.asprintf "[%a]" pp_abs_slices indices1 in
            let indices2_str = Format.asprintf "[%a]" pp_abs_slices indices2 in
            fatal_from ~loc
              (Error.BitfieldsDontAlign
                 {
                   field1_absname = abs_name1_str;
                   field2_absname = abs_name2_str;
                   field1_absslices = indices1_str;
                   field2_absslices = indices2_str;
                 }))
        absolute_bitfields
    (* End *)
  end

  (** Check for a standard library declaration name{n}(bits(n), ...) or
    name{m,n}(bits(n), ...). *)
  let can_omit_stdlib_param func_sig =
    func_sig.builtin
    &&
    let declared_param =
      (* parameter N, declared as {N} or {M,N} *)
      match func_sig.parameters with
      | [ (n, _) ] | [ _; (n, _) ] -> Some n
      | _ -> None
    and declared_first_arg_width =
      (* first argument, declared as bits(N') *)
      match func_sig.args with
      | (_, { desc = T_Bits ({ desc = E_Var n' }, _) }) :: _ -> Some n'
      | _ -> None
    in
    match (declared_param, declared_first_arg_width) with
    | Some n, Some n' -> String.equal n n'
    | _ -> false

  (** Special treatment to infer the single input parameter [N] of a
    stdlib/primitive function with a first argument of type [bits(N)]. *)
  let insert_stdlib_param ~loc env func_sig ~params ~arg_types =
    if
      can_omit_stdlib_param func_sig
      && List.compare_lengths params func_sig.parameters < 0
      && not (list_is_empty arg_types)
    then
      let width = get_bitvector_width ~loc env (List.hd arg_types) in
      let param_type = integer_exact' width |> add_pos_from ~loc:width in
      let ses_param =
        (* This is enough as the bitvector width is statically evaluable and
           its timeframe is earlier than the argument itself. *)
        SES.empty
      in
      params @ [ (param_type, width, ses_param) ]
    else params

  (* Begin TBitField *)
  let rec annotate_bitfield ~loc env width bitfield : bitfield * SES.t =
    match bitfield with
    | BitField_Simple (name, slices) ->
        let slices1, ses_slices = annotate_slices ~loc env slices in
        let+ () = check_slices_in_width ~loc env width slices1 in
        (BitField_Simple (name, slices1), ses_slices) |: TypingRule.TBitField
    | BitField_Nested (name, slices, bitfields') ->
        let slices1, ses_slices = annotate_slices ~loc env slices in
        let diet = disjoint_slices_to_positions ~loc env slices1 in
        let+ () = check_diet_in_width ~loc slices1 width diet in
        let width' = Diet.Int.cardinal diet |> expr_of_int in
        let new_bitfields, ses_bitfields =
          annotate_bitfields ~loc env width' bitfields'
        in
        let ses = SES.union ses_slices ses_bitfields in
        (BitField_Nested (name, slices1, new_bitfields), ses)
        |: TypingRule.TBitField
    | BitField_Type (name, slices, ty) ->
        let ty', ses_ty = annotate_type ~loc env ty in
        let slices1, ses_slices = annotate_slices ~loc env slices in
        let diet = disjoint_slices_to_positions ~loc env slices1 in
        let+ () = check_diet_in_width ~loc slices1 width diet in
        let width' = Diet.Int.cardinal diet |> expr_of_int in
        let+ () =
          t_bits_bitwidth width' |> add_dummy_annotation
          |> check_bits_equal_width ~loc env ty
        in
        let ses = SES.union ses_ty ses_slices in
        (BitField_Type (name, slices1, ty'), ses) |: TypingRule.TBitField
  (* End *)

  (* Begin TBitFields *)
  and annotate_bitfields ~loc env e_width bitfields =
    let+ () =
      match get_first_duplicate (List.map bitfield_get_name bitfields) with
      | None -> ok
      | Some x -> fun () -> fatal_from ~loc (Error.AlreadyDeclaredIdentifier x)
    in
    let width =
      let v = StaticInterpreter.static_eval env e_width in
      match v with L_Int i -> Z.to_int i | _ -> assert false
    in
    let new_bitfields, sess =
      list_map_split (annotate_bitfield ~loc env width) bitfields
    in
    let ses = ses_non_conflicting_unions ~loc sess in
    (new_bitfields, ses) |: TypingRule.TBitFields
  (* End *)

  and annotate_type ?(decl = false) ~(loc : 'a annotated) env ty : ty * SES.t =
    let () =
      if false then
        Format.eprintf "Annotating@ %a@ in env:@ %a@." PP.pp_ty ty pp_env env
    in
    let here t = add_pos_from ~loc:ty t in
    best_effort (ty, SES.empty) @@ fun _ ->
    match ty.desc with
    (* Begin TString *)
    | T_String -> (ty, SES.empty) |: TypingRule.TString
    (* Begin TReal *)
    | T_Real -> (ty, SES.empty) |: TypingRule.TReal
    (* Begin TBool *)
    | T_Bool -> (ty, SES.empty) |: TypingRule.TBool
    (* Begin TNamed *)
    | T_Named x ->
        let ses =
          (* As expression on which types depend are statically evaluable,
             using a named type is as reading a global immutable storage
             element. *)
          let time_frame =
            match IMap.find_opt x env.global.declared_types with
            | Some (_, t) -> t
            | None -> undefined_identifier ~loc x
          and immutable = true in
          SES.reads_global x time_frame immutable
        in
        (ty, ses) |: TypingRule.TNamed
    (* Begin TInt *)
    | T_Int constraints ->
        (match constraints with
        | PendingConstrained ->
            fatal_from ~loc Error.UnexpectedPendingConstrained
        | WellConstrained [] -> fatal_from ~loc Error.EmptyConstraints
        | WellConstrained constraints ->
            let new_constraints, sess =
              list_map_split (annotate_constraint ~loc env) constraints
            in
            let ses = SES.unions sess in
            (T_Int (WellConstrained new_constraints) |> here, ses)
        | Parameterized (_, name) ->
            (ty, SES.reads_local name TimeFrame.Constant true)
        | UnConstrained -> (ty, SES.empty))
        |: TypingRule.TInt
    (* Begin TBits *)
    | T_Bits (e_width, bitfields) ->
        let t_width, e_width', ses_width = annotate_expr env e_width in
        let+ () = check_statically_evaluable e_width ses_width in
        let+ () = check_constrained_integer ~loc:e_width env t_width in
        let bitfields', ses_bitfields =
          if bitfields = [] then (bitfields, SES.empty)
          else
            let annotated_bitfields, ses_bitfields =
              annotate_bitfields ~loc env e_width' bitfields
            in
            let () =
              let width =
                match StaticInterpreter.static_eval env e_width' with
                | L_Int i -> Z.to_int i
                | _ -> assert false
              in
              CheckCommonBitfieldsAlign.check ~loc:ty env annotated_bitfields
                width
              |: TypingRule.CheckCommonBitfieldsAlign
            in
            (annotated_bitfields, ses_bitfields)
        in
        let ses = SES.union ses_width ses_bitfields in
        (T_Bits (e_width', bitfields') |> here, ses) |: TypingRule.TBits
    (* Begin TTuple *)
    | T_Tuple tys ->
        let tys', sess = list_map_split (annotate_type ~loc env) tys in
        let ses = SES.unions sess in
        (T_Tuple tys' |> here, ses) |: TypingRule.TTuple
    (* Begin TArray *)
    | T_Array (index, t) ->
        let t', ses_t = annotate_type ~loc env t
        and index', ses_index =
          match index with
          | ArrayLength_Expr e -> (
              match get_variable_enum' env e with
              | Some (s, labels) -> (ArrayLength_Enum (s, labels), SES.empty)
              | None ->
                  let e', ses = annotate_static_integer ~loc env e in
                  (ArrayLength_Expr e', ses))
          | ArrayLength_Enum (_, _) ->
              assert (* Enumerated indices only exist in the typed AST. *)
                     false
        in
        let ses = SES.union ses_t ses_index in
        (T_Array (index', t') |> here, ses) |: TypingRule.TArray
    (* Begin TStructuredDecl *)
    | (T_Record fields | T_Exception fields) when decl -> (
        let+ () =
          match get_first_duplicate (List.map fst fields) with
          | None -> ok
          | Some x ->
              fun () -> fatal_from ~loc (Error.AlreadyDeclaredIdentifier x)
        in
        let fields', sess =
          list_map_split
            (fun (x, ty) ->
              let ty', ses = annotate_type ~loc env ty in
              ((x, ty'), ses))
            fields
        in
        let ses = SES.unions sess in
        match ty.desc with
        | T_Record _ ->
            (T_Record fields' |> here, ses) |: TypingRule.TStructuredDecl
        | T_Exception _ ->
            (T_Exception fields' |> here, ses) |: TypingRule.TStructuredDecl
        | _ -> assert false
        (* Begin TEnumDecl *))
    | T_Enum li when decl ->
        let+ () =
          match get_first_duplicate li with
          | None -> ok
          | Some x ->
              fun () -> fatal_from ~loc (Error.AlreadyDeclaredIdentifier x)
        in
        let+ () =
         fun () ->
          List.iter (fun s -> check_var_not_in_genv ~loc env.global s ()) li
        in
        (ty, SES.empty) |: TypingRule.TEnumDecl
        (* Begin TNonDecl *)
    | T_Enum _ | T_Record _ | T_Exception _ ->
        if decl then assert false
        else
          fatal_from ~loc
            (Error.NotYetImplemented
               " Cannot use non anonymous form of enumerations, record, or \
                exception here.")
          |: TypingRule.TNonDecl
  (* End *)

  (* Begin AnnotateStaticallyEvaluableExpr *)
  and annotate_statically_evaluable_expr env e =
    let t, e', ses = annotate_expr env e in
    let+ () = check_statically_evaluable e ses in
    (t, e', ses)
  (* End *)

  (* Begin AnnotateStaticInteger *)
  and annotate_static_integer ~(loc : 'a annotated) env e =
    let t, e', ses = annotate_statically_evaluable_expr env e in
    let+ () = check_structure_integer ~loc env t in
    (StaticModel.try_normalize env e', ses)
  (* End *)

  (* Begin StaticConstrainedInteger *)
  and annotate_static_constrained_integer ~(loc : 'a annotated) env e =
    let t, e', ses = annotate_statically_evaluable_expr env e in
    let+ () = check_constrained_integer ~loc env t in
    (StaticModel.try_normalize env e', ses)
  (* End *)

  (* Begin AnnotateConstraint *)
  and annotate_constraint ~loc env = function
    | Constraint_Exact e ->
        let e', ses = annotate_static_constrained_integer ~loc env e in
        (Constraint_Exact e', ses)
    | Constraint_Range (e1, e2) ->
        let e1', ses1 = annotate_static_constrained_integer ~loc env e1
        and e2', ses2 = annotate_static_constrained_integer ~loc env e2 in
        let ses = SES.union ses1 ses2 in
        (Constraint_Range (e1', e2'), ses)
  (* End *)

  and annotate_slices env ~loc =
    (* Rules:
       - Rule WZCS: The width of a bitslice must be any non-negative,
         statically evaluable integer expression (including zero).
    *)
    (* Begin Slice *)
    let rec annotate_slice s =
      let () =
        if false then
          Format.eprintf "Annotating slice %a@." PP.pp_slice_list [ s ]
      in
      match s with
      | Slice_Single i ->
          (* LRM R_GXKG:
             The notation b[i] is syntactic sugar for b[i +: 1].
          *)
          annotate_slice (Slice_Length (i, one_expr)) |: TypingRule.Slice
      | Slice_Length (offset, length) ->
          let t_offset, offset', ses_offset = annotate_expr env offset
          and length', ses_length =
            annotate_static_constrained_integer ~loc env length
          in
          let+ () = check_structure_integer ~loc:offset env t_offset in
          let ses = SES.union ses_length ses_offset in
          (Slice_Length (offset', length'), ses |: TypingRule.Slice)
      | Slice_Range (j, i) ->
          (* LRM R_GXKG:
             The notation b[j:i] is syntactic sugar for b[i +: j-i+1].
          *)
          let pre_length = binop MINUS j i |> binop PLUS !$1 in
          annotate_slice (Slice_Length (i, pre_length)) |: TypingRule.Slice
      | Slice_Star (factor, pre_length) ->
          (* LRM R_GXQG:
             The notation b[i *: n] is syntactic sugar for b[i*n +: n]
          *)
          let pre_offset = binop MUL factor pre_length in
          annotate_slice (Slice_Length (pre_offset, pre_length))
          |: TypingRule.Slice
      (* End *)
    in
    fun slices ->
      let slices, sess = list_map_split annotate_slice slices in
      let ses = ses_non_conflicting_unions ~loc sess in
      (slices, ses)

  and annotate_pattern ~loc env t p =
    let here = add_pos_from ~loc:p in
    match p.desc with
    (* Begin PAll *)
    | Pattern_All -> (p, SES.empty) |: TypingRule.PAll
    (* End *)
    (* Begin PAny *)
    | Pattern_Any li ->
        let new_li, sess = list_map_split (annotate_pattern ~loc env t) li in
        let ses =
          (* They can't be conflicting because they are statically evaluable *)
          SES.unions sess
        in
        (Pattern_Any new_li |> here, ses) |: TypingRule.PAny
    (* End *)
    (* Begin PNot *)
    | Pattern_Not q ->
        let new_q, ses = annotate_pattern ~loc env t q in
        (Pattern_Not new_q |> here, ses) |: TypingRule.PNot
    (* End *)
    (* Begin PSingle *)
    | Pattern_Single e ->
        let t_e, e', ses = annotate_expr env e in
        let+ () = check_statically_evaluable e ses in
        let+ () =
         fun () ->
          let t_struct = Types.make_anonymous env t
          and t_e_struct = Types.make_anonymous env t_e in
          match (t_struct.desc, t_e_struct.desc) with
          | T_Bool, T_Bool
          | T_Real, T_Real
          | T_Int _, T_Int _
          | T_String, T_String ->
              ()
          | T_Bits _, T_Bits _ ->
              check_bits_equal_width ~loc env t_struct t_e_struct ()
          | T_Enum li1, T_Enum li2 when list_equal String.equal li1 li2 -> ()
          | _ -> fatal_from ~loc (Error.BadPattern (p, t))
        in
        (Pattern_Single e' |> here, ses) |: TypingRule.PSingle
    (* End *)
    (* Begin PGeq *)
    | Pattern_Geq e ->
        let t_e, e', ses = annotate_expr env e in
        let+ () = check_statically_evaluable e ses in
        let+ () =
         fun () ->
          let t_struct = Types.get_structure env t
          and t_e_struct = Types.get_structure env t_e in
          match (t_struct.desc, t_e_struct.desc) with
          | T_Real, T_Real | T_Int _, T_Int _ -> ()
          | _ -> fatal_from ~loc (Error.BadPattern (p, t))
        in
        (Pattern_Geq e' |> here, ses) |: TypingRule.PGeq
    (* End *)
    (* Begin PLeq *)
    | Pattern_Leq e ->
        let t_e, e', ses = annotate_expr env e in
        let+ () = check_statically_evaluable e ses in
        let+ () =
         fun () ->
          let t_anon = Types.make_anonymous env t
          and t_e_anon = Types.make_anonymous env t_e in
          match (t_anon.desc, t_e_anon.desc) with
          | T_Real, T_Real | T_Int _, T_Int _ -> ()
          | _ -> fatal_from ~loc (Error.BadPattern (p, t))
        in
        (Pattern_Leq e' |> here, ses) |: TypingRule.PLeq
    (* End *)
    (* Begin PRange *)
    | Pattern_Range (e1, e2) ->
        let t_e1, e1', ses1 = annotate_statically_evaluable_expr env e1
        and t_e2, e2', ses2 = annotate_statically_evaluable_expr env e2 in
        let ses =
          (* They can't be conflicting because they are statically evaluable *)
          SES.union ses1 ses2
        in
        let+ () =
         fun () ->
          let t_anon = Types.make_anonymous env t
          and t_e1_anon = Types.make_anonymous env t_e1
          and t_e2_anon = Types.make_anonymous env t_e2 in
          match (t_anon.desc, t_e1_anon.desc, t_e2_anon.desc) with
          | T_Real, T_Real, T_Real | T_Int _, T_Int _, T_Int _ -> ()
          | _ -> fatal_from ~loc (Error.BadPattern (p, t))
        in
        (Pattern_Range (e1', e2') |> here, ses) |: TypingRule.PRange
    (* End *)
    (* Begin PMask *)
    | Pattern_Mask m ->
        let+ () = check_structure_bits ~loc env t in
        let+ () =
          let n = !$(Bitvector.mask_length m) in
          let t_m = T_Bits (n, []) |> add_pos_from ~loc in
          check_type_satisfies ~loc env t t_m
        in
        (p, SES.empty) |: TypingRule.PMask
    (* End *)
    (* Begin PTuple *)
    | Pattern_Tuple li -> (
        let t_struct = Types.get_structure env t in
        match t_struct.desc with
        | T_Tuple ts when List.compare_lengths li ts != 0 ->
            fatal_from ~loc
              (Error.BadArity
                 ( Static,
                   "pattern matching on tuples",
                   List.length li,
                   List.length ts ))
        | T_Tuple ts ->
            let new_li, sess =
              List.map2 (annotate_pattern ~loc env) ts li |> List.split
            in
            let ses =
              SES.unions
                (* They can't be conflicting because they are static *) sess
            in
            (Pattern_Tuple new_li |> here, ses) |: TypingRule.PTuple
        | _ -> conflict ~loc [ T_Tuple [] ] t
        (* End *))

  (* Begin AnnotateCall *)
  and annotate_call ~loc env (call_info : call) =
    let () =
      if false then
        Format.eprintf "Annotating call to %S (%s) at %a.@." call_info.name
          (Serialize.subprogram_type_to_string call_info.call_type)
          PP.pp_pos loc
    in
    let args = List.map (annotate_expr env) call_info.args in
    match (loc.version, call_info.params) with
    | V0, [] ->
        let () = assert (List.length call_info.params = 0) in
        annotate_call_v0 ~loc env call_info.name args call_info.call_type
    | (V1 | V0), _ ->
        let params = List.map (annotate_expr env) call_info.params in
        annotate_call_v1 ~loc env call_info.name ~params ~args
          ~call_type:call_info.call_type
        |: TypingRule.AnnotateCall
  (* End *)

  (* Begin AnnotateCallActualsTyped *)
  and annotate_call_v1 ~loc env name ~params ~args ~call_type =
    let arg_types, args, sess_args = list_split3 args in
    let ses_args = ses_non_conflicting_unions ~loc sess_args in
    let _, name, func_sig, ses_call =
      Fn.try_subprogram_for_name ~loc env V1 name arg_types
    in
    let ses = SES.union ses_args ses_call in
    (* Check call and subprogram types match *)
    let+ () =
      check_true
        (func_sig.subprogram_type = call_type
        (* Getters are syntactically identical to functions in V1 - so what
           looks like a function call may really be a getter call *)
        || (func_sig.subprogram_type = ST_Getter && call_type = ST_Function))
      @@ fun () -> fatal_from ~loc (MismatchedReturnValue name)
    in
    (* Insert omitted parameter for standard library call *)
    let params = insert_stdlib_param ~loc env func_sig ~params ~arg_types in
    (* Check correct number of parameters/arguments supplied *)
    let () =
      if List.compare_lengths func_sig.parameters params != 0 then
        fatal_from ~loc
        @@ Error.BadParameterArity
             ( Static,
               V1,
               name,
               List.length func_sig.parameters,
               List.length params )
      else if List.compare_lengths func_sig.args args != 0 then
        fatal_from ~loc
        @@ Error.BadArity
             (Static, name, List.length func_sig.args, List.length args)
    in
    (* Check that call parameters are statically evaluable and type-satisfy the
       declaration parameters *)
    let () =
      (* CheckParamsTypeSat( *)
      List.iter2
        (fun (name, ty_declared_opt) (ty_actual, e_actual, ses_actual) ->
          let+ () = check_statically_evaluable e_actual ses_actual in
          (* That's enough of a check on Side Effects for parameters:
             - parameters can't conflict with anything once they are statically
               evaluable;
             - the time-frame of parameters has to be earlier than the
               arguments that use their types.
          *)
          let+ () = check_constrained_integer ~loc env ty_actual in
          match ty_declared_opt with
          | None ->
              (* declared parameters have already been elaborated *)
              assert false
          | Some { desc = T_Int (Parameterized (_, name')) }
            when String.equal name name' ->
              ()
          | Some ty_declared ->
              let+ () = check_type_satisfies ~loc env ty_actual ty_declared in
              ())
        func_sig.parameters params
      (* CheckParamsTypeSat) *)
    in
    (* Check that call arguments type-satisfy the declared arguments *)
    let eqs =
      List.map2
        (fun (name, _) (_, e, _) -> (name, e))
        func_sig.parameters params
    in
    (* CheckArgsTypeSat( *)
    let () =
      List.iter2
        (fun (_, declared_ty) actual_ty ->
          let expected_ty = rename_ty_eqs env eqs declared_ty in
          let+ () = check_type_satisfies ~loc env actual_ty expected_ty in
          ())
        func_sig.args arg_types
      (* CheckArgsTypeSat) *)
    in
    (* Check the function returns as expected, and substitute parameters into
       the return type *)
    let return_type =
      match (call_type, func_sig.return_type) with
      | (ST_Function | ST_Getter), Some ty -> Some (rename_ty_eqs env eqs ty)
      | (ST_Procedure | ST_Setter), None -> None
      | _ -> fatal_from ~loc @@ Error.MismatchedReturnValue name
    in
    ( {
        name;
        args;
        params = List.map (fun (_, e, _) -> e) params;
        call_type = func_sig.subprogram_type;
      },
      return_type,
      ses )
  (* End *)

  and annotate_call_v0 ~loc env name caller_args_typed call_type =
    let caller_arg_types, args1, sess = list_split3 caller_args_typed in
    let ses1 = ses_non_conflicting_unions ~loc sess in
    let eqs1, name1, callee, ses2 =
      Fn.try_subprogram_for_name ~loc env V0 name caller_arg_types
    in
    let ses3 = SES.union ses1 ses2 in
    let () =
      if false then
        Format.eprintf "@[Found candidate decl:@ @[%a@]@]@." PP.pp_t
          [ D_Func callee |> add_dummy_annotation ]
    in
    let+ () =
      check_true (callee.subprogram_type = call_type) @@ fun () ->
      fatal_from ~loc (MismatchedReturnValue name)
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
        fatal_from ~loc
        @@ Error.BadArity
             (Static, name, List.length callee.args, List.length args1)
    in
    let eqs2 =
      let folder acc (_x, ty) (t_e, _e, _ses) =
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
      | TypeCheckNoWarn | TypeCheck -> eqs1
      | Warn | Silence ->
          List.fold_left2 folder eqs1 callee.args caller_args_typed
    in
    let eqs3 =
      (* Checking that all parameter-defining arguments are static constrained integers. *)
      List.fold_left2
        (fun eqs (callee_x, _) (caller_ty, caller_e, caller_ses) ->
          (* If [callee_x] is a parameter-defining argument. *)
          if
            List.exists
              (fun (p_name, _ty) -> String.equal callee_x p_name)
              callee.parameters
          then
            let+ () = check_statically_evaluable caller_e caller_ses in
            let+ () = check_constrained_integer ~loc env caller_ty in
            (callee_x, caller_e) :: eqs
          else eqs)
        eqs2 callee.args caller_args_typed
    in
    let () =
      if false then
        let open Format in
        eprintf "@[<hov 2>Eqs for this call are: %a@]@."
          (pp_print_list ~pp_sep:pp_print_space (fun f (name, e) ->
               fprintf f "%S<--%a" name PP.pp_expr e))
          eqs3
    in
    (* check that the caller argument types type-satisfy their corresponding
        callee formal types.
    *)
    let () =
      List.iter2
        (fun (callee_arg_name, callee_arg) caller_arg ->
          let callee_arg1 = rename_ty_eqs env eqs3 callee_arg in
          let () =
            if false then
              Format.eprintf "Checking calling arg %s from %a to %a@."
                callee_arg_name PP.pp_ty caller_arg PP.pp_ty callee_arg1
          in
          let+ () = check_type_satisfies ~loc env caller_arg callee_arg1 in
          ())
        callee.args caller_arg_types
    in
    let () =
      if false && not (String.equal name name1) then
        Format.eprintf "Renaming call from %s to %s@ at %a.@." name name1
          PP.pp_pos loc
    in
    (* check_callee_params: check that the callee parameters are correctly typed with respect
       to the parameter expressions.
    *)
    let () =
      List.iter
        (function
          | _, None -> ()
          | s, Some { desc = T_Int (Parameterized (_, s')); _ }
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
              let caller_param_t, _, _ =
                annotate_statically_evaluable_expr env caller_param_e
              in
              let () =
                if false then
                  Format.eprintf
                    "Checking calling param %s from %a to %a (i.e. %a)@."
                    callee_param_name PP.pp_ty caller_param_t PP.pp_ty
                    callee_param_t PP.pp_ty callee_param_t_renamed
              in
              let+ () =
                check_type_satisfies ~loc env caller_param_t
                  callee_param_t_renamed
              in
              ())
        callee.parameters
    in
    (* check that the formal return type matches [call_type] and
       substitute actual parameter arguments in the formal return type.
    *)
    let ret_ty_opt =
      match (call_type, callee.return_type) with
      | (ST_Function | ST_Getter | ST_EmptyGetter), Some ty ->
          Some (rename_ty_eqs env eqs3 ty)
      | (ST_Setter | ST_EmptySetter | ST_Procedure), None -> None
      | _ -> fatal_from ~loc @@ Error.MismatchedReturnValue name
    in
    let () = if false then Format.eprintf "Annotated call to %S.@." name1 in
    let params =
      List.filter_map
        (fun (name, _) -> List.assoc_opt name eqs3)
        callee.parameters
    in
    let+ () =
      check_true (List.length params = List.length callee.parameters)
      @@ fun () ->
      fatal_from ~loc
        (Error.BadParameterArity
           (Static, V0, name, List.length callee.parameters, List.length params))
    in
    ( { name = name1; args = args1; params; call_type = callee.subprogram_type },
      ret_ty_opt,
      ses3 )

  and annotate_expr env (e : expr) : ty * expr * SES.t =
    let () = if false then Format.eprintf "@[Annotating %a@]@." PP.pp_expr e in
    let here x = add_pos_from ~loc:e x and loc = to_pos e in
    match e.desc with
    (* Begin ELit *)
    | E_Literal v ->
        (annotate_literal env v |> here, e, SES.empty) |: TypingRule.ELit
    (* End *)
    (* Begin ATC *)
    | E_ATC (e', ty) ->
        let t, e'', ses_e = annotate_expr env e' in
        let t_struct = Types.get_structure env t in
        let ty', ses_ty = annotate_type ~loc env ty in
        let ty_struct = Types.get_structure env ty' in
        let+ () =
          check_atc env t_struct ty_struct ~fail:(fun () ->
              fatal_from ~loc (BadATC (t, ty)))
        in
        let ses = SES.union ses_ty @@ SES.add_assertion ses_e in
        (if Types.subtype_satisfies env t_struct ty_struct then (ty', e'', ses_e)
         else (ty', E_ATC (e'', ty_struct) |> here, ses))
        |: TypingRule.ATC
    (* End *)
    (* Begin EVar *)
    | E_Var x -> (
        let () = if false then Format.eprintf "Looking at %S.@." x in
        if e.version = V0 && should_reduce_to_call env x ST_EmptyGetter then
          let () =
            if false then
              Format.eprintf "@[Reducing getter %S@ at %a@]@." x PP.pp_pos e
          in
          let call_type = ST_EmptyGetter in
          let call, ty, ses =
            annotate_call ~loc:(to_pos e) env
              { name = x; params = []; args = []; call_type }
          in
          let ty = match ty with Some ty -> ty | None -> assert false in
          (ty, E_Call call |> here, ses)
        else
          let () =
            if false then
              Format.eprintf "@[Choosing not to reduce var %S@ at @[%a@]@]@." x
                PP.pp_pos e
          in
          try
            match IMap.find x env.local.storage_types with
            | ty, LDK_Constant when Storage.mem x env.local.constant_values ->
                let v = Storage.find x env.local.constant_values in
                (ty, E_Literal v |> here, SES.empty) |: TypingRule.EVar
            | ty, ldk ->
                let ses =
                  SES.reads_local x (TimeFrame.of_ldk ldk)
                    (ldk_is_immutable ldk)
                in
                (ty, e, ses) |: TypingRule.EVar
          with Not_found -> (
            try
              match IMap.find x env.global.storage_types with
              | ty, GDK_Constant when Storage.mem x env.global.constant_values
                ->
                  let v = Storage.find x env.global.constant_values in
                  (ty, E_Literal v |> here, SES.empty) |: TypingRule.EVar
              | ty, gdk ->
                  let ses =
                    SES.reads_global x (TimeFrame.of_gdk gdk)
                      (gdk_is_immutable gdk)
                  in
                  (ty, e, ses) |: TypingRule.EVar
            with Not_found ->
              let () =
                if false then
                  Format.eprintf "@[Cannot find %s in env@ %a.@]@." x pp_env env
              in
              undefined_identifier ~loc:e x |: TypingRule.EVar))
    (* End *)
    (* Begin Binop *)
    | E_Binop (op, e1, e2) ->
        let t1, e1', ses1 = annotate_expr env e1 in
        let t2, e2', ses2 = annotate_expr env e2 in
        let t = apply_binop_types ~loc env op t1 t2 in
        let ses =
          if binop_is_ordered op then SES.union ses1 ses2
          else ses_non_conflicting_union ~loc ses1 ses2
        in
        (t, E_Binop (op, e1', e2') |> here, ses) |: TypingRule.Binop
    (* End *)
    (* Begin Unop *)
    | E_Unop (op, e') ->
        let t'', e'', ses = annotate_expr env e' in
        let t = apply_unop_type ~loc env op t'' in
        (t, E_Unop (op, e'') |> here, ses) |: TypingRule.Unop
    (* End *)
    (* Begin ECall *)
    | E_Call call ->
        let call, ret_ty_opt, ses = annotate_call ~loc:(to_pos e) env call in
        let t = match ret_ty_opt with Some ty -> ty | None -> assert false in
        (t, E_Call call |> here, ses) |: TypingRule.ECall
    (* End *)
    (* Begin ECond *)
    | E_Cond (e_cond, e_true, e_false) ->
        let t_cond, e_cond', ses_cond = annotate_expr env e_cond in
        let+ () = check_structure_boolean ~loc env t_cond in
        let t_true, e_true', ses_true = annotate_expr env e_true
        and t_false, e_false', ses_false = annotate_expr env e_false in
        let t =
          best_effort t_true (fun _ ->
              match Types.lowest_common_ancestor env t_true t_false with
              | None ->
                  fatal_from ~loc (Error.UnreconciliableTypes (t_true, t_false))
              | Some t -> t)
        in
        let ses = SES.union3 ses_cond ses_true ses_false in
        (t, E_Cond (e_cond', e_true', e_false') |> here, ses)
        |: TypingRule.ECond
    (* End *)
    | E_Tuple [ e ] -> annotate_expr env e
    (* Begin ETuple *)
    | E_Tuple li ->
        let ts, es, sess = List.map (annotate_expr env) li |> list_split3 in
        let ses = ses_non_conflicting_unions ~loc sess in
        (T_Tuple ts |> here, E_Tuple es |> here, ses) |: TypingRule.ETuple
    (* End *)
    | E_Array _ -> fatal_from ~loc UnrespectedParserInvariant
    (* Begin ERecord *)
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
        best_effort (ty, e, SES.empty) @@ fun _ ->
        let field_types =
          match (Types.make_anonymous env ty).desc with
          | T_Exception fields | T_Record fields -> fields
          | _ -> conflict ~loc [ T_Record [] ] ty
        in
        (* Rule DYQZ: A record expression shall assign every field of the record. *)
        let () =
          if
            List.for_all
              (fun (name, _) -> List.mem_assoc name fields)
              field_types
          then ()
          else fatal_from ~loc (Error.MissingField (List.map fst fields, ty))
          (* and whose fields have the values given in the field_assignment_list. *)
        in
        let+ () =
          match get_first_duplicate (List.map fst fields) with
          | None -> ok
          | Some x ->
              fun () -> fatal_from ~loc (Error.AlreadyDeclaredIdentifier x)
        in
        let annotate_field_init (name, e') =
          let t', e'', ses = annotate_expr env e' in
          let t_spec' =
            match List.assoc_opt name field_types with
            | None -> fatal_from ~loc (Error.BadField (name, ty))
            | Some t_spec' -> t_spec'
          in
          let+ () = check_type_satisfies ~loc env t' t_spec' in
          ((name, e''), ses) |: TypingRule.AnnotateFieldInit
        in
        let fields', sess = list_map_split annotate_field_init fields in
        let ses = ses_non_conflicting_unions ~loc sess in
        (ty, E_Record (ty, fields') |> here, ses) |: TypingRule.ERecord
    (* End *)
    (* Begin EArbitrary *)
    | E_Arbitrary ty ->
        let ty1, ses_ty = annotate_type ~loc env ty in
        let ty2 = Types.get_structure env ty1 in
        let ses = SES.add_non_determinism ses_ty in
        (ty1, E_Arbitrary ty2 |> here, ses) |: TypingRule.EArbitrary
    (* End *)
    | E_Slice (e', slices) -> (
        match e'.desc with
        | E_Var name
          when e'.version = V0
               && should_reduce_to_call env name ST_Getter
               && List.for_all slice_is_single slices ->
            let args =
              try List.map slice_as_single slices
              with Invalid_argument _ -> assert false
            in
            let call, ty, ses =
              annotate_call ~loc:(to_pos e) env
                { name; params = []; args; call_type = ST_Getter }
            in
            let ty = match ty with Some ty -> ty | None -> assert false in
            (ty, E_Call call |> here, ses)
        | _ -> (
            let t_e', e'', ses1 = annotate_expr env e' in
            let struct_t_e' = Types.make_anonymous env t_e' in
            match struct_t_e'.desc with
            (* Begin ESlice *)
            | T_Int _ | T_Bits _ ->
                let+ () =
                  check_true (not (list_is_empty slices)) @@ fun () ->
                  fatal_from ~loc Error.EmptySlice
                in
                (* TODO: check that:
                   - Rule SNQJ: An expression or subexpression which
                     may result in a zero-length bitvector must not be
                     side-effecting.
                *)
                let slices', ses2 =
                  best_effort (slices, SES.empty) (fun _ ->
                      annotate_slices env slices ~loc)
                in
                let w = slices_width env slices' in
                let ses = SES.union ses1 ses2 in
                (T_Bits (w, []) |> here, E_Slice (e'', slices') |> here, ses)
                |: TypingRule.ESlice
            (* End *)
            | T_Array (size, ty') when e'.version = V0 -> (
                match slices with
                | [ Slice_Single e_index ] ->
                    annotate_get_array ~loc env (size, ty') (e'', ses1, e_index)
                | _ -> conflict ~loc [ integer'; default_t_bits ] t_e')
            (* Begin ESliceError *)
            | _ ->
                conflict ~loc [ integer'; default_t_bits ] t_e'
                |: TypingRule.ESliceError
            (* End *)))
    | E_GetField (e1, field_name) -> (
        let reduced =
          if e1.version = V0 && C.use_field_getter_extension then
            reduce_getfields_to_slices env e1 [ field_name ]
          else None
        in
        match reduced with
        | Some (name, args) ->
            let call, ty, ses =
              annotate_call ~loc:(to_pos e) env
                { name; params = []; args; call_type = ST_Getter }
            in
            let ty = match ty with Some ty -> ty | None -> assert false in
            (ty, E_Call call |> here, ses)
        | None -> (
            let t_e2, e2, ses1 = annotate_expr env e1 in
            match (Types.make_anonymous env t_e2).desc with
            | T_Exception fields | T_Record fields -> (
                match List.assoc_opt field_name fields with
                (* Begin EGetBadRecordField *)
                | None ->
                    fatal_from ~loc (Error.BadField (field_name, t_e2))
                    |: TypingRule.EGetBadRecordField
                (* End *)
                (* Begin EGetRecordField *)
                | Some t ->
                    (t, E_GetField (e2, field_name) |> here, ses1)
                    |: TypingRule.EGetRecordField
                    (* End *))
            | T_Bits (_, bitfields) -> (
                match find_bitfield_opt field_name bitfields with
                (* Begin EGetBadBitField *)
                | None ->
                    fatal_from ~loc (Error.BadField (field_name, t_e2))
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
                    let t_e4, new_e, ses_new = annotate_expr env e3 in
                    let t_e5 =
                      match t_e4.desc with
                      | T_Bits (width, _bitfields) ->
                          T_Bits (width, bitfields') |> add_pos_from ~loc:t_e2
                      | _ -> assert false
                    in
                    (t_e5, new_e, ses_new) |: TypingRule.EGetBitFieldNested
                (* End *)
                (* Begin EGetBitFieldTyped *)
                | Some (BitField_Type (_field, slices, t)) ->
                    let e3 = E_Slice (e1, slices) |> here in
                    let t_e4, new_e, ses_new = annotate_expr env e3 in
                    let+ () = check_type_satisfies ~loc env t_e4 t in
                    (t, new_e, ses_new) |: TypingRule.EGetBitFieldTyped
                    (* End *))
            (* Begin EGetTupleItem *)
            | T_Tuple tys ->
                let index =
                  try Scanf.sscanf field_name "item%u" Fun.id
                  with Scanf.Scan_failure _ | Failure _ | End_of_file ->
                    fatal_from ~loc (Error.BadField (field_name, t_e2))
                in
                if 0 <= index && index < List.length tys then
                  ( List.nth tys index,
                    E_GetItem (e2, index) |> add_pos_from ~loc:e,
                    ses1 )
                else
                  fatal_from ~loc (Error.BadField (field_name, t_e2))
                  |: TypingRule.EGetTupleItem
            (* End *)
            (* Begin EGetBadField *)
            | _ ->
                fatal_from ~loc (Error.BadField (field_name, t_e2))
                |: TypingRule.EGetBadField)
        (* End *))
    (* Begin E_GetFields *)
    | E_GetFields (e_base, fields) -> (
        let reduced =
          if e_base.version = V0 && C.use_field_getter_extension then
            reduce_getfields_to_slices env e_base fields
          else None
        in
        match reduced with
        | Some (name, args) ->
            let call, ret_ty_opt, ses =
              annotate_call ~loc:(to_pos e) env
                { name; params = []; args; call_type = ST_Getter }
            in
            let ty =
              match ret_ty_opt with Some ty -> ty | None -> assert false
            in
            (ty, E_Call call |> here, ses)
        | None -> (
            let t_base_annot, e_base_annot, ses_base =
              annotate_expr env e_base
            in
            match (Types.make_anonymous env t_base_annot).desc with
            | T_Bits (_, bitfields) ->
                let one_field field =
                  match find_bitfields_slices_opt field bitfields with
                  | None ->
                      fatal_from ~loc (Error.BadField (field, t_base_annot))
                  | Some slices -> slices
                in
                E_Slice (e_base, list_concat_map one_field fields)
                |> here |> annotate_expr env |: TypingRule.EGetFields
            | T_Record base_fields ->
                let get_bitfield_width name =
                  match List.assoc_opt name base_fields with
                  | None ->
                      fatal_from ~loc (Error.BadField (name, t_base_annot))
                  | Some t -> get_bitvector_width ~loc env t
                in
                let widths = List.map get_bitfield_width fields in
                let e_slice_width =
                  let wh = List.hd widths and wts = List.tl widths in
                  List.fold_left (width_plus env) wh wts
                in
                ( T_Bits (e_slice_width, []) |> here,
                  E_GetFields (e_base_annot, fields) |> here,
                  ses_base )
                |: TypingRule.EGetFields
            | _ -> conflict ~loc [ default_t_bits ] t_base_annot))
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
        let t_e2, e2, ses_e = annotate_expr env e1 in
        let pat', ses_pat =
          best_effort (pat, SES.empty) (fun _ ->
              annotate_pattern ~loc env t_e2 pat)
        in
        let ses =
          SES.union ses_pat ses_e
          (* e1 is evaluated before the pattern, so they are not concurrent. *)
        in
        (T_Bool |> here, E_Pattern (e2, pat') |> here, ses)
        |: TypingRule.EPattern
    (* End *)
    (* Begin EGetArray *)
    | E_GetArray (e_base, e_index) -> (
        let t_base, e_base', ses_base = annotate_expr env e_base in
        let t_anon_base = Types.make_anonymous env t_base in
        match t_anon_base.desc with
        | T_Array (size, t_elem) ->
            annotate_get_array ~loc env (size, t_elem)
              (e_base', ses_base, e_index)
        | _ -> conflict ~loc [ default_array_ty ] t_base |: TypingRule.EGetArray
        )
    (* End *)
    | E_GetItem _ | E_EnumArray _ | E_GetEnumArray _ -> assert false

  (* Begin AnnotateGetArray *)
  and annotate_get_array ~loc env (size, t_elem) (e_base, ses_base, e_index) =
    let t_index', e_index', ses_index = annotate_expr env e_index in
    let wanted_t_index = type_of_array_length ~loc size in
    let+ () = check_type_satisfies ~loc env t_index' wanted_t_index in
    let ses = ses_non_conflicting_union ~loc ses_index ses_base in
    let new_e =
      match size with
      | ArrayLength_Enum _ -> E_GetEnumArray (e_base, e_index')
      | ArrayLength_Expr _ -> E_GetArray (e_base, e_index')
    in
    (t_elem, new_e |> add_pos_from ~loc, ses) |: TypingRule.AnnotateGetArray
  (* End *)

  (** For an expression of the form [e1.[f1,...,fn]], if [e1] represents a call
      to a getter then this function returns a list of slices needed to read
      the bitfields [f1...fn]. Otherwise, the result is [None].

      It is an ASLRef extension, guarded by [C.use_field_getter_extension].
  *)
  and reduce_getfields_to_slices env e1 fields =
    assert (e1.version = V0 && C.use_field_getter_extension);
    match e1.desc with
    | E_Var name when should_reduce_to_call env name ST_Getter ->
        let empty_getter = E_Slice (e1, []) |> add_pos_from ~loc:e1 in
        let ty, _, _ = annotate_expr env empty_getter in
        should_fields_reduce_to_call env name ty fields
    | _ -> None

  (* ListMinAbs( *)
  let list_min_abs_z =
    let min_abs acc z =
      match Z.compare (Z.abs acc) (Z.abs z) with
      | -1 -> acc
      | 1 -> z
      | 0 -> if Z.sign acc >= 0 then acc else z (* bias towards positive *)
      | _ -> assert false
    in
    function
    | [] -> raise (Invalid_argument "list_min_abs_z")
    | h :: t -> List.fold_left min_abs h t
  (* ListMinAbs) *)

  (* Begin BaseValue *)

  (** [base_value_v1 ~loc env t] returns an expression building the base value
      of the type [t] in [env].

      The base value expression can be used to initialize variables of type [t]
      in [env].
      This expression is side-effect free, and is a literal for singular types.
      If a base value cannot be statically determined (e.g. for parameterized
      integer types), a type error is thrown, at the ~location [loc].
  *)
  let rec base_value_v1 ~loc env t : expr =
    let here = add_pos_from ~loc in
    let lit v = here (E_Literal v) in
    let fatal_non_static e =
      fatal_from ~loc (Error.BaseValueNonStatic (t, e))
    in
    let fatal_is_empty () = fatal_from ~loc (Error.BaseValueEmptyType t) in
    let reduce_to_z e =
      match StaticModel.reduce_to_z_opt env e with
      | None -> fatal_non_static e
      | Some i -> i
    in
    match t.desc with
    | T_Bool -> L_Bool false |> lit
    | T_Bits (e, _) ->
        let length = reduce_to_z e |> Z.to_int in
        L_BitVector (Bitvector.zeros length) |> lit
    | T_Enum [] -> assert false
    | T_Enum (name :: _) -> lookup_constants env name |> lit
    | T_Int UnConstrained -> L_Int Z.zero |> lit
    | T_Int (Parameterized (_, id)) -> E_Var id |> here |> fatal_non_static
    | T_Int PendingConstrained -> assert false
    | T_Int (WellConstrained cs) ->
        let constraint_abs_min = function
          | Constraint_Exact e -> Some (reduce_to_z e)
          | Constraint_Range (e1, e2) ->
              let v1 = reduce_to_z e1 in
              let v2 = reduce_to_z e2 in
              if v1 <= v2 then
                match Z.(sign v1, sign v2) with
                | -1, -1 -> (* v1 <= v2 < 0 *) Some v2
                | -1, _ -> (* v1 < 0 <= v2 *) Some Z.zero
                | _, _ -> (* 0 <= v1 <= v2 *) Some v1
              else None
        in
        let z_min_list = List.filter_map constraint_abs_min cs in
        if list_is_empty z_min_list then fatal_is_empty ()
        else
          let z_min = list_min_abs_z z_min_list in
          L_Int z_min |> lit
    | T_Named _ -> Types.make_anonymous env t |> base_value_v1 ~loc env
    | T_Real -> L_Real Q.zero |> lit
    | T_Exception fields | T_Record fields ->
        let one_field (name, t_field) =
          (name, base_value_v1 ~loc env t_field)
        in
        E_Record (t, List.map one_field fields) |> here
    | T_String -> L_String "" |> lit
    | T_Tuple li ->
        let exprs = List.map (base_value_v1 ~loc env) li in
        E_Tuple exprs |> here
    | T_Array (index, ty) -> (
        let value = base_value_v1 ~loc env ty in
        match index with
        | ArrayLength_Enum (enum, labels) ->
            E_EnumArray { enum; labels; value } |> here
        | ArrayLength_Expr length -> E_Array { length; value } |> here)
  (* End *)

  let rec base_value_v0 ~loc env t : expr =
    assert (loc.version = V0);
    let here = add_pos_from ~loc in
    match t.desc with
    | T_Bool | T_Int UnConstrained | T_Real | T_String | T_Enum _ ->
        base_value_v1 ~loc env t
    | T_Bits (width, _) ->
        let e =
          E_Call
            {
              name = "Zeros";
              params = [];
              args = [ width ];
              call_type = ST_Function;
            }
        in
        let _, e', _ = annotate_expr env (here e) in
        e'
    | T_Int (Parameterized (_, id)) -> E_Var id |> here
    | T_Int (WellConstrained [] | PendingConstrained) -> assert false
    | T_Int
        (WellConstrained ((Constraint_Exact e | Constraint_Range (e, _)) :: _))
      ->
        e
    | T_Tuple li -> E_Tuple (List.map (base_value_v0 ~loc env) li) |> here
    | T_Exception fields | T_Record fields ->
        let fields =
          List.map
            (fun (name, t_field) -> (name, base_value_v0 ~loc env t_field))
            fields
        in
        E_Record (t, fields) |> here
    | T_Array (length, ty) -> (
        let value = base_value_v0 ~loc env ty in
        match length with
        | ArrayLength_Enum (enum, labels) ->
            E_EnumArray { enum; labels; value } |> here
        | ArrayLength_Expr length -> E_Array { length; value } |> here)
    | T_Named _id ->
        let t = Types.make_anonymous env t in
        base_value_v0 ~loc env t

  (** [base_value ~loc env e] is [base_value_v1 ~loc env e] if running for ASLv1,
      or [base_value_v0 ~loc env e] if running for ASLv0.
  *)
  let base_value ~loc env e =
    match loc.version with
    | V0 -> base_value_v0 ~loc env e
    | V1 -> base_value_v1 ~loc env e

  (* Begin AnnotateSetArray *)
  let annotate_set_array ~loc env (size, t_elem) rhs_ty
      (e_base, ses_base, e_index) =
    let+ () = check_type_satisfies ~loc env rhs_ty t_elem in
    let t_index', e_index', ses_index = annotate_expr env e_index in
    let wanted_t_index = type_of_array_length ~loc:e_base size in
    let+ () = check_type_satisfies ~loc env t_index' wanted_t_index in
    let ses = ses_non_conflicting_union ~loc ses_base ses_index in
    let new_le =
      match size with
      | ArrayLength_Enum _ -> LE_SetEnumArray (e_base, e_index')
      | ArrayLength_Expr _ -> LE_SetArray (e_base, e_index')
    in
    (new_le |> add_pos_from ~loc, ses) |: TypingRule.AnnotateSetArray
  (* End *)

  let rec annotate_lexpr env le t_e =
    let () =
      if false then
        Format.eprintf "Typing lexpr: @[%a@] to @[%a@]@." PP.pp_lexpr le
          PP.pp_ty t_e
    in
    let loc = to_pos le in
    let here x = add_pos_from ~loc x in
    match le.desc with
    (* Begin LEDiscard *)
    | LE_Discard -> (le, SES.empty) |: TypingRule.LEDiscard
    (* End *)
    (* Begin LEVar *)
    | LE_Var x ->
        let ty, ses =
          match IMap.find_opt x env.local.storage_types with
          | Some (ty, LDK_Var) -> (ty, SES.writes_local x)
          | Some _ -> fatal_from ~loc @@ Error.AssignToImmutable x
          | None -> (
              match IMap.find_opt x env.global.storage_types with
              | Some (ty, GDK_Var) -> (ty, SES.writes_global x)
              | Some _ -> fatal_from ~loc @@ Error.AssignToImmutable x
              | None -> undefined_identifier ~loc x)
        in
        let+ () = check_type_satisfies ~loc env t_e ty in
        (le, ses) |: TypingRule.LEVar
    (* End *)
    (* Begin LEDestructuring *)
    | LE_Destructuring les ->
        (match t_e.desc with
        | T_Tuple tys ->
            if List.compare_lengths tys les != 0 then
              Error.fatal_from le
                (Error.BadArity
                   (Static, "LEDestructuring", List.length tys, List.length les))
            else
              let les', sess =
                List.map2 (annotate_lexpr env) les tys |> List.split
              in
              let ses =
                (* TODO left-hand-side conflicting union *)
                SES.unions sess
              in
              (LE_Destructuring les' |> here, ses)
        | _ -> conflict ~loc [ T_Tuple [] ] t_e)
        |: TypingRule.LEDestructuring
    (* End *)
    | LE_Slice (le1, slices) -> (
        let t_le1, _, _ = expr_of_lexpr le1 |> annotate_expr env in
        let t_le1_anon = Types.make_anonymous env t_le1 in
        (* Begin LESlice *)
        match t_le1_anon.desc with
        | T_Bits _ ->
            let le2, ses1 = annotate_lexpr env le1 t_le1 in
            let+ () =
             fun () ->
              let width =
                slices_width env slices |> StaticModel.try_normalize env
              in
              let t = T_Bits (width, []) |> here in
              check_type_satisfies ~loc env t_e t ()
            in
            let slices2, ses2 =
              best_effort (slices, SES.empty) @@ fun _ ->
              annotate_slices env slices ~loc
            in
            let+ () = check_disjoint_slices ~loc env slices2 in
            let+ () =
              check_true (not (list_is_empty slices)) @@ fun () ->
              fatal_from ~loc Error.EmptySlice
            in
            let ses = ses_non_conflicting_union ~loc ses1 ses2 in
            (LE_Slice (le2, slices2) |> here, ses |: TypingRule.LESlice)
        | T_Array (size, t) when le.version = V0 -> (
            match slices with
            | [ Slice_Single e_index ] ->
                let le2, ses2 = annotate_lexpr env le1 t_le1 in
                annotate_set_array ~loc:le env (size, t) t_e (le2, ses2, e_index)
            | _ -> invalid_expr (expr_of_lexpr le1))
        | _ -> conflict ~loc:le1 [ default_t_bits ] t_le1
        (* End *))
    | LE_SetField (le1, field) ->
        (let t_le1, _, _ = expr_of_lexpr le1 |> annotate_expr env in
         let le2, ses = annotate_lexpr env le1 t_le1 in
         let t_le1_anon = Types.make_anonymous env t_le1 in
         match t_le1_anon.desc with
         (* Begin LESetStructuredField *)
         | T_Exception fields | T_Record fields ->
             let t =
               match List.assoc_opt field fields with
               | None -> fatal_from ~loc (Error.BadField (field, t_le1))
               | Some t -> t
             in
             let+ () = check_type_satisfies ~loc env t_e t in
             ( LE_SetField (le2, field) |> here,
               ses |: TypingRule.LESetStructuredField )
         (* End *)
         (* Begin LESetBitField *)
         | T_Bits (_, bitfields) ->
             let bits slices bitfields =
               T_Bits (slices_width env slices, bitfields) |> here
             in
             let t, slices =
               match find_bitfield_opt field bitfields with
               | None ->
                   fatal_from ~loc:le1 (Error.BadField (field, t_le1_anon))
               | Some (BitField_Simple (_field, slices)) ->
                   (bits slices [], slices)
               | Some (BitField_Nested (_field, slices, bitfields')) ->
                   (bits slices bitfields', slices)
               | Some (BitField_Type (_field, slices, t)) ->
                   let t' = bits slices [] in
                   let+ () = check_type_satisfies ~loc env t' t in
                   (t, slices)
             in
             let+ () = check_type_satisfies ~loc:le1 env t_e t in
             let le3 = LE_Slice (le1, slices) |> here in
             annotate_lexpr env le3 t_e |: TypingRule.LESetBitField
         (* End *)
         (* Begin LESetBadField *)
         | _ ->
             conflict ~loc:le1
               [ default_t_bits; T_Record []; T_Exception [] ]
               t_e)
        |: TypingRule.LESetBadField
    (* End *)
    (* Begin LESetFields *)
    | LE_SetFields (le_base, le_fields, []) -> (
        let t_base, _, _ = expr_of_lexpr le_base |> annotate_expr env in
        let le_base_annot, ses_base = annotate_lexpr env le_base t_base in
        let t_base_anon = Types.make_anonymous env t_base in
        match t_base_anon.desc with
        | T_Bits (_, bitfields) ->
            let slices_of_bitfield field =
              match find_bitfields_slices_opt field bitfields with
              | None -> fatal_from ~loc (Error.BadField (field, t_base_anon))
              | Some slices -> slices
            in
            let le_slice =
              LE_Slice
                (le_base_annot, list_concat_map slices_of_bitfield le_fields)
              |> here
            in
            annotate_lexpr env le_slice t_e |: TypingRule.LESetFields
        | T_Record base_fields ->
            let fold_bitvector_fields field (start, slices) =
              match List.assoc_opt field base_fields with
              | None -> fatal_from ~loc (Error.BadField (field, t_base_anon))
              | Some t_field ->
                  let field_width =
                    get_bitvector_const_width ~loc env t_field
                  in
                  (start + field_width, (start, field_width) :: slices)
            in
            let length, slices =
              List.fold_right fold_bitvector_fields le_fields (0, [])
            in
            let t_lhs = T_Bits (expr_of_int length, []) |> here in
            let+ () = check_type_satisfies ~loc env t_e t_lhs in
            (LE_SetFields (le_base_annot, le_fields, slices) |> here, ses_base)
        | _ -> conflict ~loc [ default_t_bits ] t_base |: TypingRule.LESetFields
        )
    (* End *)
    (* Begin LESetArray *)
    | LE_SetArray (e_base, e_index) -> (
        let t_base, _, _ = expr_of_lexpr e_base |> annotate_expr env in
        let t_anon_base = Types.make_anonymous env t_base in
        match t_anon_base.desc with
        | T_Array (size, t_elem) ->
            let e_base', ses_base = annotate_lexpr env e_base t_base in
            annotate_set_array ~loc env (size, t_elem) t_e
              (e_base', ses_base, e_index)
        | _ -> conflict ~loc [ default_array_ty ] t_base)
    (* End *)
    | LE_SetFields (_, _, _ :: _) | LE_SetEnumArray _ -> assert false

  (* Begin CheckCanBeInitializedWith *)
  let can_be_initialized_with env s t =
    (* Rules:
       - ZCVD: It is illegal for a storage element whose type has the
         structure of the parameterized integer to be initialized with a
         value whose type has the structure of the parameterized integer,
         unless the type is omitted from the declaration (and therefore the
         type can be unambiguously inferred) or the initialization expression
         is omitted (and therefore the type is not omitted from the
         declaration).
       - LXQZ: A storage element of type S, where S is
         any type that does not have the structure of
         the parameterized integer type, may only be
         assigned or initialized with a value of type T
         if T type-satisfies S)
    *)
    let s_struct = Types.get_structure env s in
    match s_struct.desc with
    | T_Int (Parameterized _) -> assert false
    | _ -> Types.type_satisfies env t s

  let check_can_be_initialized_with ~loc env s t () =
    if can_be_initialized_with env s t then () else conflict ~loc [ s.desc ] t
  (* End *)

  (* Begin ShouldRememberImmutableExpression *)
  let should_remember_immutable_expression ses =
    let ses_non_assert = SES.remove_assertions ses in
    SES.is_statically_evaluable ses_non_assert
    |: TypingRule.ShouldRememberImmutableExpression
  (* End *)

  (* Begin AddImmutableExpression *)
  let add_immutable_expression env ldk typed_e_opt x =
    match (ldk, typed_e_opt) with
    | (LDK_Constant | LDK_Let), Some (_, e, ses_e)
      when should_remember_immutable_expression ses_e -> (
        match StaticModel.normalize_opt env e with
        | Some e' ->
            add_local_immutable_expr x e' env
            |: TypingRule.AddImmutableExpression
        | None -> env)
    | _ -> env
  (* End *)

  let rec inherit_integer_constraints ~loc lhs_ty rhs_ty =
    match (lhs_ty.desc, rhs_ty.desc) with
    | T_Int PendingConstrained, T_Int (WellConstrained _) -> rhs_ty
    | T_Int PendingConstrained, _ ->
        fatal_from ~loc (Error.ConstrainedIntegerExpected rhs_ty)
    | T_Tuple lhs_tys, T_Tuple rhs_tys ->
        if List.compare_lengths lhs_tys rhs_tys != 0 then
          fatal_from ~loc
            (Error.BadArity
               ( Static,
                 "tuple initialization",
                 List.length rhs_tys,
                 List.length lhs_tys ))
        else
          let lhs_tys' =
            List.map2
              (inherit_integer_constraints ~loc:(to_pos lhs_ty))
              lhs_tys rhs_tys
          in
          T_Tuple lhs_tys' |> add_pos_from ~loc:lhs_ty
    | _ -> lhs_ty

  let annotate_local_decl_item ~loc (env : env) ty ldk ?e ldi =
    let () =
      if false then Format.eprintf "Annotating %a.@." PP.pp_local_decl_item ldi
    in
    match ldi with
    (* Begin LDVar *)
    | LDI_Var x ->
        (* Rule LCFD: A ~local declaration shall not declare an identifier
           which is already in scope at the point of declaration. *)
        let+ () = check_var_not_in_env ~loc env x in
        let env2 = add_local x ty ldk env in
        let new_env = add_immutable_expression env2 ldk e x in
        new_env |: TypingRule.LDVar
    (* End *)
    (* Begin LDTuple *)
    | LDI_Tuple names ->
        let tys =
          match (Types.make_anonymous env ty).desc with
          | T_Tuple tys when List.compare_lengths tys names = 0 -> tys
          | T_Tuple tys ->
              fatal_from ~loc
                (Error.BadArity
                   ( Static,
                     "tuple initialization",
                     List.length tys,
                     List.length names ))
          | _ -> conflict ~loc [ T_Tuple [] ] ty
        in
        let new_env =
          List.fold_right2
            (fun ty' name env' ->
              let+ () = check_var_not_in_env ~loc env' name in
              add_local name ty' ldk env')
            tys names env
        in
        new_env |: TypingRule.LDTuple
  (* End *)

  (* Begin DeclareLocalConstant *)
  let declare_local_constant env v = function
    | LDI_Var x -> add_local_constant x v env
    | LDI_Tuple _ -> (* Not yet implemented *) env
  (* End *)

  let rec annotate_stmt env s : stmt * env * SES.t =
    let () =
      if false then
        match s.desc with
        | S_Seq _ -> ()
        | _ -> Format.eprintf "@[<3>Annotating@ @[%a@]@]@." PP.pp_stmt s
    in
    let here x = add_pos_from ~loc:s x and loc = to_pos s in
    match s.desc with
    (* Begin SPass *)
    | S_Pass -> (s, env, SES.empty) |: TypingRule.SPass
    (* Begin SSeq *)
    | S_Seq (s1, s2) ->
        let new_s1, env1, ses1 = try_annotate_stmt env s1 in
        let new_s2, env2, ses2 = try_annotate_stmt env1 s2 in
        let ses = SES.union ses1 ses2 in
        (S_Seq (new_s1, new_s2) |> here, env2, ses) |: TypingRule.SSeq
    (* Begin SAssign *)
    | S_Assign (le, re) ->
        (let () =
           if false then
             Format.eprintf "@[<3>Annotating assignment@ @[%a@]@]@." PP.pp_stmt
               s
         in
         let ((t_re, re1, ses_re) as typed_re) = annotate_expr env re in
         match s.version with
         | V1 ->
             let le1, ses_le = annotate_lexpr env le t_re in
             let ses = SES.union ses_re ses_le in
             (S_Assign (le1, re1) |> here, env, ses)
         | V0 -> (
             let reduced = setter_should_reduce_to_call_s env le typed_re in
             match reduced with
             | Some (new_s, ses_s) -> (new_s, env, ses_s)
             | None ->
                 let env1 =
                   (*
                    * In version V0, variable declaration is optional,
                    * As a result typing will be partial and some
                    * function calls may lack extra parameters.
                    * Fix this by typing first assignments of
                    * undeclared variables as declarations.
                    *)
                   match ASTUtils.ldi_of_lexpr le with
                   | None -> env
                   | Some ldi ->
                       let undefined = function
                         | LDI_Var x -> is_undefined x env
                         | LDI_Tuple names ->
                             List.for_all (fun x -> is_undefined x env) names
                       in
                       if undefined ldi then
                         let () =
                           if false then
                             Format.eprintf
                               "@[<3>Assignment@ @[%a@] as declaration@]@."
                               PP.pp_stmt s
                         in
                         let ldk = LDK_Var in
                         let env2 =
                           annotate_local_decl_item ~loc env t_re ldk ldi
                         in
                         env2
                       else env
                 in
                 let le1, ses_le = annotate_lexpr env1 le t_re in
                 let ses = SES.union ses_re ses_le in
                 (S_Assign (le1, re1) |> here, env1, ses)))
        |: TypingRule.SAssign
    (* End *)
    (* Begin SCall *)
    | S_Call call ->
        let call, ty, ses = annotate_call ~loc env call in
        let () = assert (ty = None) in
        (S_Call call |> here, env, ses) |: TypingRule.SCall
    (* End *)
    (* Begin SReturn *)
    | S_Return e_opt ->
        (* Rule NYWH: A return statement appearing in a setter or procedure must
           have no return value expression. *)
        (* Rule PHNZ: A return statement appearing in a getter or function
           requires a return value expression that type-satisfies the return
           type of the subprogram. *)
        (match (env.local.return_type, e_opt) with
        | None, Some _ | Some _, None ->
            fatal_from ~loc (Error.BadReturnStmt env.local.return_type)
            |: TypingRule.SReturn
        | None, None ->
            (S_Return None |> here, env, SES.empty) |: TypingRule.SReturn
        | Some t, Some e ->
            let t_e', e', ses = annotate_expr env e in
            let () =
              if false then
                Format.eprintf
                  "Can I return %a(of type %a) when return_type = %a?@."
                  PP.pp_expr e PP.pp_ty t_e' PP.pp_ty t
            in
            let+ () = check_type_satisfies ~loc env t_e' t in
            (S_Return (Some e') |> here, env, ses))
        |: TypingRule.SReturn
    (* End *)
    (* Begin SCond *)
    | S_Cond (e, s1, s2) ->
        let t_cond, e_cond, ses_cond = annotate_expr env e in
        let+ () = check_type_satisfies ~loc:e_cond env t_cond boolean in
        let s1', ses1 = try_annotate_block env s1 in
        let s2', ses2 = try_annotate_block env s2 in
        let ses = SES.union3 ses_cond ses1 ses2 in
        (S_Cond (e_cond, s1', s2') |> here, env, ses) |: TypingRule.SCond
    (* End *)
    (* Begin SAssert *)
    | S_Assert e ->
        let t_e', e', ses_e = annotate_expr env e in
        let+ () = check_is_pure e ses_e in
        let+ () = check_type_satisfies ~loc env t_e' boolean in
        let ses = SES.add_assertion ses_e in
        (S_Assert e' |> here, env, ses) |: TypingRule.SAssert
    (* End *)
    (* Begin SWhile *)
    | S_While (e1, limit1, s1) ->
        let t, e2, ses_e = annotate_expr env e1 in
        let limit2, ses_limit = annotate_limit_expr ~loc env limit1 in
        let+ () = check_type_satisfies ~loc:e2 env t boolean in
        let s2, ses_block = try_annotate_block env s1 in
        let ses = SES.union3 ses_e ses_block ses_limit in
        (S_While (e2, limit2, s2) |> here, env, ses) |: TypingRule.SWhile
    (* End *)
    (* Begin SRepeat *)
    | S_Repeat (s1, e1, limit1) ->
        let s2, ses_block = try_annotate_block env s1 in
        let limit2, ses_limit = annotate_limit_expr ~loc env limit1 in
        let t, e2, ses_e = annotate_expr env e1 in
        let+ () = check_type_satisfies ~loc:e2 env t boolean in
        let ses = SES.union3 ses_block ses_e ses_limit in
        (S_Repeat (s2, e2, limit2) |> here, env, ses) |: TypingRule.SRepeat
    (* End *)
    (* Begin SFor *)
    | S_For { index_name; start_e; end_e; dir; body; limit } ->
        let start_t, start_e', ses_start = annotate_expr env start_e
        and end_t, end_e', ses_end = annotate_expr env end_e
        and limit', ses_limit =
          annotate_limit_expr ~warn:false ~loc env limit
        in
        let+ () = check_is_pure start_e ses_start in
        let+ () = check_is_deterministic start_e ses_start in
        let+ () = check_is_pure end_e ses_end in
        let+ () = check_is_deterministic end_e ses_end in
        let ses_cond = SES.union3 ses_start ses_end ses_limit in
        let start_struct = Types.make_anonymous env start_t
        and end_struct = Types.make_anonymous env end_t in
        (* TypingRule.ForConstraint( *)
        let cs =
          match (start_struct.desc, end_struct.desc) with
          | T_Int UnConstrained, T_Int _ | T_Int _, T_Int UnConstrained ->
              UnConstrained
          | T_Int _, T_Int _ ->
              let start_n = StaticModel.try_normalize env start_e'
              and end_n = StaticModel.try_normalize env end_e' in
              let e_bot, e_top =
                match dir with
                | Up -> (start_n, end_n)
                | Down -> (end_n, start_n)
              in
              WellConstrained [ Constraint_Range (e_bot, e_top) ]
          | T_Int _, _ -> conflict ~loc [ integer' ] end_t
          | _, _ -> conflict ~loc [ integer' ] start_t
          (* only happens in relaxed type-checking mode because of check_structure_integer earlier. *)
          (* TypingRule.ForConstraint) *)
        in
        let ty = T_Int cs |> here in
        let body', ses_block =
          let+ () = check_var_not_in_env ~loc env index_name in
          let env' = add_local index_name ty LDK_Let env in
          try_annotate_block env' body
        in
        let ses = ses_non_conflicting_union ~loc ses_cond ses_block in
        ( S_For
            {
              index_name;
              dir;
              start_e = start_e';
              end_e = end_e';
              body = body';
              limit = limit';
            }
          |> here,
          env,
          ses )
        |: TypingRule.SFor
    (* End *)
    (* Begin SDecl *)
    | S_Decl (ldk, ldi, ty_opt, e_opt) -> (
        match (ldk, e_opt) with
        (* SDecl.Some( *)
        | _, Some e ->
            let ((t_e, e', ses_e) as typed_e) = annotate_expr env e in
            let () =
              if false then
                Format.eprintf "Found rhs side-effects: %a@." SES.pp_print ses_e
            in
            let env1, ty_opt', ses_ldi =
              match ty_opt with
              | None ->
                  let env1 =
                    annotate_local_decl_item ~loc env t_e ldk ~e:typed_e ldi
                  in
                  (* When [print_typed] is specified, wrap untyped items with their inferred type. *)
                  let ty_opt = if C.print_typed then Some t_e else None in
                  (env1, ty_opt, SES.empty)
              | Some t ->
                  let t_e' = Types.get_structure env t_e in
                  let t = inherit_integer_constraints ~loc t t_e' in
                  let t', ses_t = annotate_type ~loc env t in
                  let+ () = check_can_be_initialized_with ~loc env t' t_e in
                  let env1 =
                    annotate_local_decl_item ~loc env t' ldk ~e:typed_e ldi
                  in
                  (env1, Some t', ses_t)
            in
            let ses = SES.union ses_e ses_ldi in
            let new_env =
              match ldk with
              | LDK_Let | LDK_Var -> env1
              | LDK_Constant -> (
                  let+ () = check_leq_constant_time ~loc:s typed_e in
                  try
                    let v = StaticInterpreter.static_eval env1 e in
                    declare_local_constant env1 v ldi
                  with Error.(ASLException _) -> env1)
            in
            (S_Decl (ldk, ldi, ty_opt', Some e') |> here, new_env, ses)
            |: TypingRule.SDecl
        (* SDecl.Some) *)
        (* SDecl.None( *)
        | LDK_Var, None ->
            (match ty_opt with
            | None ->
                fatal_from ~loc (Error.BadLDI ldi)
                |: TypingRule.LDUninitialisedVar
            | Some t ->
                let t', ses_t' = annotate_type ~loc env t in
                let e_init = base_value ~loc env t' in
                let new_env =
                  annotate_local_decl_item ~loc env t' LDK_Var ldi
                in
                ( S_Decl (LDK_Var, ldi, Some t', Some e_init) |> here,
                  new_env,
                  ses_t' )
                |: TypingRule.LDUninitialisedTyped)
            |: TypingRule.SDecl
        | (LDK_Constant | LDK_Let), None ->
            fatal_from ~loc UnrespectedParserInvariant)
    (* SDecl.None) *)
    (* End *)
    (* Begin SThrow *)
    | S_Throw (Some (e, _)) ->
        let t_e, e', ses1 = annotate_expr env e in
        let+ () = check_structure_exception ~loc env t_e in
        let exn_name =
          match t_e.desc with T_Named s -> s | _ -> assert false
        in
        let ses2 = SES.add_thrown_exception exn_name ses1 in
        (S_Throw (Some (e', Some t_e)) |> here, env, ses2) |: TypingRule.SThrow
    | S_Throw None ->
        (* TODO: verify that this is allowed? *)
        (s, env, SES.throws_exception "TODO") |: TypingRule.SThrow
        (* End *)
        (* Begin STry *)
    | S_Try (s', catchers, otherwise) ->
        let s'', ses1 = try_annotate_block env s' in
        let ses2, catchers_and_ses =
          list_fold_left_map (annotate_catcher ~loc env) ses1 catchers
        in
        let () =
          if false then
            Format.eprintf "After catchers, I have side effects:@ %a@."
              SES.pp_print ses2
        in
        let catchers', catchers_sess = List.split catchers_and_ses in
        let ses_catchers = SES.unions catchers_sess in
        let otherwise', ses3, ses_otherwise =
          match otherwise with
          | None -> (None, ses2, SES.empty)
          | Some block ->
              let block', ses_block = try_annotate_block env block in
              (Some block', SES.remove_thrown_exceptions ses2, ses_block)
        in
        let ses = SES.union3 ses3 ses_catchers ses_otherwise in
        (S_Try (s'', catchers', otherwise') |> here, env, ses)
        |: TypingRule.STry
    (* End *)
    (* Begin SPrint *)
    | S_Print { args; newline; debug } ->
        let args', sess =
          List.map
            (fun e ->
              let ty, annot_e, ses_e = annotate_expr env e in
              let+ () =
                check_true (Types.is_singular env ty) @@ fun () ->
                Error.fatal_from e (Error.BadPrintType ty)
              in
              (annot_e, ses_e))
            args
          |> List.split
        in
        let ses =
          SES.non_conflicting_unions sess
            ~fail:(conflicting_side_effects_error ~loc)
        in
        (S_Print { args = args'; newline; debug } |> here, env, ses)
        |: TypingRule.SPrint
    (* End *)
    (* Begin SPragma *)
    | S_Pragma (id, args) ->
        let () = warn_from ~loc (Error.PragmaUse id) in
        let _, _, sess = List.map (annotate_expr env) args |> list_split3 in
        let ses =
          SES.non_conflicting_unions sess
            ~fail:(conflicting_side_effects_error ~loc)
        in
        (S_Pass |> here, env, ses) |: TypingRule.SPragma
    (* End *)
    | S_Unreachable -> (s, env, SES.empty)

  (* Begin AnnotateLimitExpr *)
  and annotate_limit_expr ?(warn = true) ~loc env = function
    | None ->
        let () = if warn then warn_from ~loc Error.NoLoopLimit in
        (None, SES.empty)
    | Some limit ->
        let new_limit, ses =
          annotate_static_constrained_integer ~loc env limit
        in
        (Some new_limit, ses) |: TypingRule.AnnotateLimitExpr
  (* End *)

  (* Begin Catcher *)
  and annotate_catcher ~loc env ses_in (name_opt, ty, stmt) =
    let ty', ses_ty = annotate_type ~loc env ty in
    let+ () = check_structure_exception ~loc:ty' env ty' in
    let new_stmt, ses_block =
      let env' =
        match name_opt with
        | None -> env
        | Some name ->
            let+ () = check_var_not_in_env ~loc:stmt env name in
            add_local name ty LDK_Let env
      in
      try_annotate_block env' stmt
    and ses_filtered =
      let ty_name = match ty'.desc with T_Named s -> s | _ -> assert false in
      SES.filter_thrown_exceptions
        (fun s -> not (Types.subtypes_names env s ty_name))
        ses_in
    in
    let ses = SES.union ses_block ses_ty in
    (ses_filtered, ((name_opt, ty, new_stmt), ses)) |: TypingRule.Catcher
  (* End *)

  (* Begin Block *)
  and try_annotate_block env s =
    (*
        See rule JFRD:
           A ~local identifier declared with var, let or constant
           is in scope from the point immediately after its declaration
           until the end of the immediately enclosing block.

        From that follows that we can discard the environment at the end
        of an enclosing block.
    *)
    ( best_effort (s, SES.empty) @@ fun _ ->
      let s, _env, ses = annotate_stmt env s in
      (s, ses) )
    |: TypingRule.Block
  (* End *)

  and try_annotate_stmt env s =
    best_effort (s, env, SES.empty) (fun _ -> annotate_stmt env s)

  (* ASLRef extension that allows reduction of a form
     [MyGetter.[fieldA, fieldB] to [MyGetter([sliceA], [sliceB])]].

     It is guarded by [C.use_getter_field_extension]. *)
  and set_fields_should_reduce_to_call ~loc env x fields (t_e, e, ses_e) =
    (*
     * Field indices are extracted from the return type
     * of "associated" getter.
     *)
    assert (loc.version = V0 && C.use_field_getter_extension);
    let ( let* ) = Option.bind in
    let _, _, callee, _ =
      try Fn.try_subprogram_for_name ~loc env V0 x []
      with Error.ASLException _ -> assert false
    in
    let* ty = callee.return_type in
    let ty = Types.make_anonymous env ty in
    let* name, args = should_fields_reduce_to_call env x ty fields in
    let typed_args = (t_e, e, ses_e) :: List.map (annotate_expr env) args in
    let call, ret_ty, ses_call =
      annotate_call_v0 ~loc env name typed_args ST_Setter
    in
    let _, _, sess_args = list_split3 typed_args in
    let ses = SES.union ses_call @@ ses_non_conflicting_unions ~loc sess_args in
    let () = assert (ret_ty = None) in
    Some (S_Call call |> add_pos_from ~loc, ses)

  and setter_should_reduce_to_call_recurse ~loc env typed_e make_old_le sub_le =
    let () = assert (loc.version = V0) in
    let x = fresh_var "__setter_setfield" in
    let here le = add_pos_from ~loc le in
    let t_sub_re, sub_re, ses_sub_re =
      expr_of_lexpr sub_le |> annotate_expr env
    in
    let ldi_x = LDI_Var x in
    let env1 = annotate_local_decl_item ~loc env t_sub_re LDK_Var ldi_x in
    let s1, ses_s1 =
      (S_Decl (LDK_Var, ldi_x, None, Some sub_re) |> here, ses_sub_re)
    in
    let s2, ses_s2 =
      let t_e, e, ses_e = typed_e in
      let old_le = make_old_le (LE_Var x |> here) in
      let old_le', ses_old_le = annotate_lexpr env1 old_le t_e in
      (S_Assign (old_le', e) |> here, SES.union ses_old_le ses_e)
    in
    let typed_e_x = annotate_expr env1 (E_Var x |> here) in
    match setter_should_reduce_to_call_s env1 sub_le typed_e_x with
    | None -> None
    | Some (s, ses_s) ->
        Some (s_then (s_then s1 s2) s, SES.union3 ses_s1 ses_s2 ses_s)

  and setter_should_reduce_to_call_s env le typed_e : (stmt * SES.t) option =
    let () = assert (le.version = V0) in
    let t_e, e, ses_e = typed_e in
    let () =
      if false then
        Format.eprintf "@[<2>setter_..._s@ @[%a@]@ @[%a@]@]@." PP.pp_lexpr le
          PP.pp_expr e
    in
    let loc = to_pos le in
    let here d = add_pos_from ~loc d in
    (if false then (fun o ->
       let none f () = Format.fprintf f "no reduction." in
       let pp_fst pp f (x, _y) = pp f x in
       Format.eprintf "@[<2>Setter@ @[%a@ = %a@]@ gave @[%a@]@.@]" PP.pp_lexpr
         le PP.pp_expr e
         (Format.pp_print_option ~none (pp_fst PP.pp_stmt))
         o;
       o)
     else Fun.id)
    @@
    match le.desc with
    | LE_Discard -> None
    | LE_SetField (sub_le, field) -> (
        match sub_le.desc with
        | LE_Var x
          when C.use_field_getter_extension
               && should_reduce_to_call env x ST_Setter ->
            set_fields_should_reduce_to_call env ~loc x [ field ] typed_e
        | _ ->
            let old_le le' = LE_SetField (le', field) |> here in
            setter_should_reduce_to_call_recurse ~loc env typed_e old_le sub_le)
    | LE_SetFields (sub_le, fields, slices) -> (
        match sub_le.desc with
        | LE_Var x
          when C.use_field_getter_extension
               && should_reduce_to_call env x ST_Setter ->
            set_fields_should_reduce_to_call env ~loc x fields typed_e
        | _ ->
            let old_le le' = LE_SetFields (le', fields, slices) |> here in
            setter_should_reduce_to_call_recurse ~loc env typed_e old_le sub_le)
    | LE_Slice (sub_le, slices) -> (
        match sub_le.desc with
        | LE_Var x
          when should_reduce_to_call env x ST_Setter
               && List.for_all slice_is_single slices ->
            let args =
              try List.map slice_as_single slices
              with Invalid_argument _ -> assert false
            in
            let typed_args = typed_e :: List.map (annotate_expr env) args in
            let call, ret_ty, ses_call =
              annotate_call_v0 ~loc env x typed_args ST_Setter
            in
            let _, _, sess_args = list_split3 typed_args in
            let ses =
              SES.union ses_call @@ ses_non_conflicting_unions ~loc sess_args
            in
            let () = assert (ret_ty = None) in
            Some (S_Call call |> here, ses)
        | _ ->
            let old_le le' = LE_Slice (le', slices) |> here in
            setter_should_reduce_to_call_recurse ~loc env typed_e old_le sub_le)
    | LE_Destructuring les -> (
        match (Types.make_anonymous env t_e).desc with
        | T_Tuple t_es when List.compare_lengths les t_es = 0 ->
            let x = fresh_var "__setter_destructuring" in
            let ldi_x = LDI_Var x in
            let env1 =
              annotate_local_decl_item ~loc env t_e LDK_Let ~e:typed_e ldi_x
            in
            let sub_e i = E_GetItem (E_Var x |> here, i) |> here in
            let recurse_one i sub_le t_sub_e =
              setter_should_reduce_to_call_s env1 sub_le
                (t_sub_e, sub_e i, SES.empty)
            in
            let subs = list_mapi2 recurse_one 0 les t_es in
            if List.for_all Option.is_none subs then None
            else
              let s0 = S_Decl (LDK_Let, ldi_x, None, Some e) |> here in
              let produce_one i sub_le t_sub_e_i = function
                | None ->
                    let sub_le', sub_le_ses =
                      annotate_lexpr env sub_le t_sub_e_i
                    in
                    (S_Assign (sub_le', sub_e i) |> here, sub_le_ses)
                | Some (s, ses) -> (s, ses)
              in
              let stmts, sess =
                list_mapi3 produce_one 0 les t_es subs |> List.split
              in
              let s = stmt_from_list (s0 :: stmts)
              and ses = SES.unions (ses_e :: sess) in
              Some (s, ses)
        | _ -> None)
    | LE_Var x ->
        let st = ST_EmptySetter in
        if should_reduce_to_call env x st then
          let args = [ typed_e ] in
          let call, ret_ty, ses_call = annotate_call_v0 ~loc env x args st in
          let () = assert (ret_ty = None) in
          let ses = SES.union ses_call ses_e in
          Some (S_Call call |> here, ses)
        else None
    | LE_SetArray _ | LE_SetEnumArray _ -> assert false

  (** [func_sig_types f] returns a list of the types in the signature [f].
      The return type is first, followed by the argument types in order. *)
  let func_sig_types func_sig =
    let arg_types = List.map snd func_sig.args in
    let return_type =
      match func_sig.return_type with None -> [] | Some ty -> [ ty ]
    in
    return_type @ arg_types

  (** The parameters in a function signature, in order. *)
  let extract_parameters ~env func_sig =
    let rec parameters_of_expr ~env e =
      match e.desc with
      | E_Var x -> if is_undefined x env then [ x ] else []
      | E_Binop (_, e1, e2) ->
          parameters_of_expr ~env e1 @ parameters_of_expr ~env e2
      | E_Unop (_, e) -> parameters_of_expr ~env e
      | E_Literal _ -> []
      | _ -> Error.fatal_from (to_pos e) (Error.UnsupportedExpr (Static, e))
    in
    let parameters_of_constraint ~env c =
      match c with
      | Constraint_Exact e -> parameters_of_expr ~env e
      | Constraint_Range (e1, e2) ->
          parameters_of_expr ~env e1 @ parameters_of_expr ~env e2
    in
    let rec parameters_of_ty ~env ty =
      match ty.desc with
      | T_Bits (e, _) -> parameters_of_expr ~env e
      | T_Tuple tys -> list_concat_map (parameters_of_ty ~env) tys
      | T_Int (WellConstrained cs) ->
          list_concat_map (parameters_of_constraint ~env) cs
      | T_Int UnConstrained | T_Real | T_String | T_Bool | T_Array _ | T_Named _
        ->
          []
      | _ -> Error.fatal_from (to_pos ty) (Error.UnsupportedTy (Static, ty))
    in
    let types = func_sig_types func_sig in
    let all_parameters = list_concat_map (parameters_of_ty ~env) types in
    uniq all_parameters

  (** The set of variables which could define a parameter in a function signature. *)
  let extract_parameter_defining ~env f =
    let rec defining_of_ty ~env acc ty =
      match ty.desc with
      | T_Bits ({ desc = E_Var x; _ }, _) ->
          if is_undefined x env then ISet.add x acc else acc
      | T_Tuple tys -> List.fold_left (defining_of_ty ~env) acc tys
      | _ -> acc
    in
    let types = func_sig_types f in
    List.fold_left (defining_of_ty ~env)
      (ISet.of_list (List.map fst f.args))
      types

  let annotate_func_sig_v1 ~loc genv func_sig =
    let env = with_empty_local genv in
    (* Check recursion limit *)
    let recurse_limit, ses_recurse_limit =
      annotate_limit_expr ~warn:false ~loc env func_sig.recurse_limit
    in
    (* Annotate and declare parameters *)
    let (env_with_params, ses_with_params), parameters =
      let declare_parameter (new_env, new_ses) (x, ty_opt) =
        let ty, ses_ty =
          match ty_opt with
          | None | Some { desc = T_Int UnConstrained; _ } ->
              (Types.parameterized_ty x, SES.empty)
          | Some ty ->
              (* valid in environment which has no parameters declared *)
              annotate_type ~loc env ty
        in
        let+ () = check_constrained_integer ~loc env ty in
        let+ () = check_var_not_in_env ~loc new_env x in
        let new_env' = add_local x ty LDK_Let new_env
        and ses = SES.union new_ses ses_ty in
        ((new_env', ses), (x, Some ty))
      in
      list_fold_left_map declare_parameter (env, ses_recurse_limit)
        func_sig.parameters
    in
    (* Check parameters are declared correctly - in order and unique *)
    let+ () =
      let inferred_parameters = extract_parameters ~env func_sig in
      let declared_parameters = List.map fst func_sig.parameters in
      let all_parameters_declared =
        list_equal String.equal inferred_parameters declared_parameters
      in
      check_true all_parameters_declared @@ fun () ->
      fatal_from ~loc
        (BadParameterDecl
           (func_sig.name, inferred_parameters, declared_parameters))
    in
    (* Annotate and declare arguments *)
    let (env_with_args, ses_with_args), args =
      let declare_argument (new_env, new_ses) (x, ty) =
        (* valid in environment with only parameters declared *)
        let ty, ses_ty = annotate_type ~loc env_with_params ty in
        let+ () = check_var_not_in_env ~loc new_env x in
        let new_env = add_local x ty LDK_Let new_env
        and ses = SES.union new_ses ses_ty in
        ((new_env, ses), (x, ty))
      in
      list_fold_left_map declare_argument
        (env_with_params, ses_with_params)
        func_sig.args
    in
    (* Annotate return type *)
    let env_with_return, return_type, ses_with_return =
      match func_sig.return_type with
      | None -> (env_with_args, None, ses_with_args)
      | Some ty ->
          (* valid in environment with parameters declared *)
          let new_ty, ses_ty = annotate_type ~loc env_with_params ty in
          let return_type = Some new_ty in
          let local_env = { env_with_args.local with return_type } in
          let new_ses = SES.union ses_ty ses_with_args in
          ({ env_with_args with local = local_env }, return_type, new_ses)
    in
    let ses = SES.remove_locals ses_with_return in
    ( env_with_return,
      { func_sig with parameters; args; return_type; recurse_limit },
      ses )

  let annotate_func_sig_v0 ~loc genv func_sig =
    let env = with_empty_local genv in
    (* Check recursion limit *)
    let recurse_limit, ses_with_limit =
      annotate_limit_expr ~warn:false ~loc env func_sig.recurse_limit
    in
    (* Check parameters have defining arguments *)
    let inferred_parameters = extract_parameters ~env func_sig in
    let+ () =
      let defining = extract_parameter_defining ~env func_sig in
      let undefined_parameters =
        List.filter (fun x -> not (ISet.mem x defining)) inferred_parameters
      in
      check_true (list_is_empty undefined_parameters) @@ fun () ->
      fatal_from ~loc (ParameterWithoutDecl (List.hd undefined_parameters))
    in
    (* Annotate and declare parameters from arguments *)
    let (env_with_params, ses_with_params), typed_parameters =
      let declare_parameter (new_env, new_ses) x =
        let ty, ses_ty =
          match List.assoc_opt x func_sig.args with
          | None | Some { desc = T_Int UnConstrained } ->
              (Types.parameterized_ty x, SES.empty)
          | Some ty ->
              (* valid in environment which has no parameters declared *)
              annotate_type ~loc env ty
        in
        let+ () = check_constrained_integer ~loc env ty in
        let+ () = check_var_not_in_env ~loc new_env x in
        let new_ses = SES.union new_ses ses_ty
        and new_env = add_local x ty LDK_Let new_env in
        ((new_env, new_ses), (x, ty))
      in
      list_fold_left_map declare_parameter (env, ses_with_limit)
        inferred_parameters
    in
    let parameters = List.map (fun (x, ty) -> (x, Some ty)) typed_parameters in
    (* Annotate and declare remaining arguments *)
    let (env_with_args, ses_with_args), args =
      let declare_argument (new_env, new_ses) (x, ty) =
        match List.assoc_opt x typed_parameters with
        | Some ({ desc = T_Int (Parameterized _) } as loc) ->
            ((new_env, new_ses), (x, T_Int UnConstrained |> add_pos_from ~loc))
        | Some ty -> ((new_env, new_ses), (x, ty))
        | None ->
            let ty, ses_ty = annotate_type ~loc env_with_params ty in
            let+ () = check_var_not_in_env ~loc new_env x in
            let new_ses = SES.union new_ses ses_ty
            and new_env = add_local x ty LDK_Let new_env in
            ((new_env, new_ses), (x, ty))
      in
      list_fold_left_map declare_argument
        (env_with_params, ses_with_params)
        func_sig.args
    in
    (* Annotate return type *)
    let env_with_return, return_type, ses_with_return =
      match func_sig.return_type with
      | None -> (env_with_args, None, ses_with_args)
      | Some ty ->
          (* valid in environment with parameters declared *)
          let new_ty, ses_ty = annotate_type ~loc env_with_params ty in
          let return_type = Some new_ty in
          let local_env = { env_with_args.local with return_type } in
          let new_ses = SES.union ses_ty ses_with_args in
          ({ env_with_args with local = local_env }, return_type, new_ses)
    in
    ( env_with_return,
      { func_sig with parameters; args; return_type; recurse_limit },
      ses_with_return )

  let annotate_func_sig ~loc genv func_sig =
    match loc.version with
    | V0 -> annotate_func_sig_v0 ~loc genv func_sig
    | V1 -> annotate_func_sig_v1 ~loc genv func_sig

  module ControlFlow : sig
    val check_stmt_returns_or_throws : identifier -> stmt_desc annotated -> prop
    (** [check_stmt_interrupts name env body] checks that the function named
        [name] with the statement body [body] returns a value or throws an
        exception. *)
  end = struct
    (** Possible Control-Flow actions of a statement. *)
    type t =
      | Interrupt  (** Throwing an exception or returning a value. *)
      | AssertedNotInterrupt
          (** Assert that this control-flow path is unused. *)
      | MayNotInterrupt
          (** Among all control-flow path in a statement, there is one that
              will not throw an exception nor return a value. *)

    (* Begin ControlFlowSeq *)

    (** Sequencial combination of two control flows. *)
    let seq t1 t2 =
      if t1 = MayNotInterrupt then t2 else t1 |: TypingRule.ControlFlowSeq
    (* End *)

    (* Begin ControlFlowJoin *)

    (** [join t1 t2] corresponds to the parallel combination of [t1] and [t2].
    More precisely, it is the maximal element in the ordering
    AssertedNotInterrupt < Interrupt < MayNotInterrupt
    *)
    let join t1 t2 =
      match (t1, t2) with
      | MayNotInterrupt, _ | _, MayNotInterrupt ->
          MayNotInterrupt |: TypingRule.ControlFlowJoin
      | AssertedNotInterrupt, t | t, AssertedNotInterrupt ->
          t (* Assertion that the condition always holds *)
      | Interrupt, Interrupt -> Interrupt
    (* End *)

    (* Begin ControlFlowFromStmt *)

    (** [get_from_stmt env s] builds the control-flow analysis on [s] in [env].
    *)
    let rec from_stmt s =
      match s.desc with
      | S_Pass | S_Decl _ | S_Assign _ | S_Assert _ | S_Call _ | S_Print _
      | S_Pragma _ ->
          MayNotInterrupt |: TypingRule.ControlFlowFromStmt
      | S_Unreachable -> AssertedNotInterrupt
      | S_Return _ | S_Throw _ -> Interrupt
      | S_Seq (s1, s2) -> seq (from_stmt s1) (from_stmt s2)
      | S_Cond (_, s1, s2) -> join (from_stmt s1) (from_stmt s2)
      | S_Repeat (body, _, _) -> from_stmt body
      | S_While _ | S_For _ -> MayNotInterrupt
      | S_Try (body, catchers, otherwise) ->
          let res0 = from_stmt body in
          let res1 =
            match otherwise with
            | None -> res0
            | Some s -> join (from_stmt s) res0
          in
          List.fold_left
            (fun res (_, _, s) -> join res (from_stmt s))
            res1 catchers

    (* End *)

    (** [check_stmt_interrupts name env body] checks that the function named
        [name] with the statement body [body] returns a value or throws an
        exception. *)
    let check_stmt_returns_or_throws name s () =
      match from_stmt s with
      | AssertedNotInterrupt | Interrupt -> ()
      | MayNotInterrupt -> fatal_from ~loc:s (Error.NonReturningFunction name)
  end

  (* Begin Subprogram *)
  let annotate_subprogram (env : env) (f : AST.func) ses_func_sig :
      AST.func * SES.t =
    let () =
      if false then
        Format.eprintf "@[<hov>Annotating body in env:@ %a@]@." pp_env env
    in
    (* Annotate body *)
    let body =
      match f.body with SB_ASL body -> body | SB_Primitive _ -> assert false
    in
    let new_body, ses_body = try_annotate_block env body in
    let ses = SES.union ses_func_sig @@ SES.remove_locals ses_body in
    let () =
      if false then
        Format.eprintf "@[<v 2>For program %s, I got side-effects:@ %a@]@."
          f.name SES.pp_print ses
    in
    let+ () =
      match f.return_type with
      | None -> ok
      | Some _ -> ControlFlow.check_stmt_returns_or_throws f.name new_body
    in
    ({ f with body = SB_ASL new_body }, ses) |: TypingRule.Subprogram
  (* End *)

  let try_annotate_subprogram env f ses_func_sig =
    best_effort (f, ses_func_sig) @@ fun _ ->
    annotate_subprogram env f ses_func_sig

  (******************************************************************************)
  (*                                                                            *)
  (*                           Global env and funcs                             *)
  (*                                                                            *)
  (******************************************************************************)

  (* Begin CheckSetterHasGetter *)
  let check_setter_has_getter ~loc env (func_sig : AST.func) =
    let fail () =
      fatal_from ~loc (Error.SetterWithoutCorrespondingGetter func_sig)
    in
    let check_true thing = check_true thing fail in
    match func_sig.subprogram_type with
    | ST_Getter | ST_EmptyGetter | ST_Function | ST_Procedure -> ok
    | ST_EmptySetter | ST_Setter ->
        let ret_type, arg_types =
          match func_sig.args with
          | [] -> fatal_from ~loc Error.UnrespectedParserInvariant
          | (_, ret_type) :: args -> (ret_type, List.map snd args)
        in
        let _, _, func_sig', _ =
          try Fn.subprogram_for_name ~loc env V1 func_sig.name arg_types
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
  let declare_one_func ~loc (func_sig : AST.func) ses_func_sig env =
    let env1, name' =
      best_effort (env, func_sig.name) @@ fun _ ->
      Fn.add_new_func ~loc env func_sig.name func_sig.args
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
    let+ () = check_var_not_in_genv ~loc env1.global name' in
    let+ () = check_setter_has_getter ~loc env1 func_sig in
    let new_func_sig = { func_sig with name = name' } in
    let init_ses =
      match func_sig.body with
      | SB_ASL _ | SB_Primitive true ->
          SES.add_calls_recursive name' ses_func_sig
      | SB_Primitive false -> ses_func_sig
    in
    (add_subprogram name' new_func_sig init_ses env1, new_func_sig)
    |: TypingRule.DeclareOneFunc
  (* End *)

  (* Begin AnnotateAndDeclareFunc *)
  let annotate_and_declare_func ~loc func_sig genv =
    let env1, func_sig1, ses_f1 = annotate_func_sig ~loc genv func_sig in
    (declare_one_func ~loc func_sig1 ses_f1 env1, ses_f1)
    |: TypingRule.AnnotateAndDeclareFunc
  (* End *)

  (* Begin AddGlobalStorage *)
  let add_global_storage ~loc name keyword genv ty =
    if is_global_ignored name then genv
    else
      let+ () = check_var_not_in_genv ~loc genv name in
      add_global_storage name ty keyword genv |: TypingRule.AddGlobalStorage
  (* End *)

  (* Begin DeclareConst *)
  let declare_const ~loc name t v genv =
    add_global_storage ~loc name GDK_Constant genv t
    |> add_global_constant name v |: TypingRule.DeclareConst
  (* End *)

  (* Begin DeclareType *)
  let declare_type ~loc name ty s genv =
    let () =
      if false then Format.eprintf "Declaring type %s of %a@." name PP.pp_ty ty
    in
    let here x = add_pos_from ~loc:ty x in
    let+ () = check_var_not_in_genv ~loc genv name in
    let env = with_empty_local genv in
    let env1, t1 =
      match s with
      (* AnnotateExtraFields( *)
      | None -> (env, ty)
      | Some (super, extra_fields) ->
          let+ () =
           fun () ->
            if Types.subtype_satisfies env ty (T_Named super |> here) then ()
            else conflict ~loc [ T_Named super ] ty
          in
          let new_ty =
            if extra_fields = [] then ty
            else
              match IMap.find_opt super genv.declared_types with
              | Some ({ desc = T_Record fields; _ }, _) ->
                  T_Record (fields @ extra_fields) |> here
              | Some ({ desc = T_Exception fields; _ }, _) ->
                  T_Exception (fields @ extra_fields) |> here
              | Some _ -> conflict ~loc [ T_Record []; T_Exception [] ] ty
              | None -> undefined_identifier ~loc super
          and env = add_subtype name super env in
          (env, new_ty)
      (* AnnotateExtraFields) *)
    in
    let t2, ses_t = annotate_type ~decl:true ~loc env1 t1 in
    let time_frame = SES.max_time_frame ses_t in
    let env2 = add_type name t2 time_frame env1 in
    let new_tenv =
      match t2.desc with
      | T_Enum ids ->
          let t = T_Named name |> here in
          let declare_one env2 label =
            declare_const ~loc label t (L_Label label) env2
          in
          let genv3 = List.fold_left declare_one env2.global ids in
          { env2 with global = genv3 }
      | _ -> env2
    in
    let () = if false then Format.eprintf "Declared %s.@." name in
    new_tenv.global
  (* End *)

  (* Begin TryAddGlobalConstant *)
  let try_add_global_constant name env e =
    try
      let v = StaticInterpreter.static_eval env e in
      { env with global = add_global_constant name v env.global }
    with Error.(ASLException { desc = UnsupportedExpr _; _ }) -> env
  (* End *)

  (* Begin DeclareGlobalStorage *)
  let declare_global_storage ~loc gsd genv =
    let () = if false then Format.eprintf "Declaring %s@." gsd.name in
    best_effort (gsd, genv) @@ fun _ ->
    let here x = add_pos_from ~loc x in
    let { keyword; initial_value; ty = ty_opt; name } = gsd in
    let+ () = check_var_not_in_genv ~loc genv name in
    let env = with_empty_local genv in
    let target_time_frame = TimeFrame.of_gdk keyword in
    let typed_initial_value, ty_opt', declared_t =
      (* AnnotateTyOptInitialValue( *)
      match (ty_opt, initial_value) with
      | Some t, Some e ->
          let t', ses_t = annotate_type ~loc env t
          and ((t_e, _e', _vses_e) as typed_e) = annotate_expr env e in
          let+ () = check_type_satisfies ~loc env t_e t' in
          let+ () =
            let fake_e_for_error = E_ATC (e, t') |> here in
            check_is_time_frame ~loc target_time_frame
              (t', fake_e_for_error, ses_t)
          in
          (typed_e, Some t', t')
      | Some t, None ->
          let t', ses_t = annotate_type ~loc env t in
          let+ () =
            let fake_e_for_error = E_ATC (E_Var "-" |> here, t') |> here in
            check_is_time_frame ~loc target_time_frame
              (t', fake_e_for_error, ses_t)
          in
          let e' = base_value ~loc env t' in
          ((t', e', SES.empty), Some t', t')
      | None, Some e ->
          let ((t_e, _e', _ses_e) as typed_e) = annotate_expr env e in
          (typed_e, None, t_e)
      | None, None -> fatal_from ~loc UnrespectedParserInvariant
      (* AnnotateTyOptInitialValue) *)
    in
    let genv1 = add_global_storage ~loc name keyword genv declared_t in
    let env1 = with_empty_local genv1 in
    let _, initial_value', ses_initial_value = typed_initial_value in
    (* UpdateGlobalStorage( *)
    let env2 =
      match keyword with
      | GDK_Constant ->
          let+ () = check_leq_constant_time ~loc typed_initial_value in
          try_add_global_constant name env1 initial_value'
      | GDK_Let when should_remember_immutable_expression ses_initial_value -> (
          match StaticModel.normalize_opt env1 initial_value' with
          | Some e' -> add_global_immutable_expr name e' env1
          | None -> env1)
      | GDK_Config ->
          let+ () = check_leq_config_time ~loc typed_initial_value in
          env1
      | _ -> env1
      (* UpdateGlobalStorage) *)
    in
    let () = assert (env2.local == empty_local) in
    (* If C.print_typed is specified pass [declared_t] to make sure the storage element is type-annotated. *)
    let ty_opt' = if C.print_typed then Some declared_t else ty_opt' in
    ({ gsd with ty = ty_opt'; initial_value = Some initial_value' }, env2.global)
    |: TypingRule.DeclareGlobalStorage
  (* End *)

  (******************************************************************************)
  (*                                                                            *)
  (*                                Entry point                                 *)
  (*                                                                            *)
  (******************************************************************************)

  let type_check_decl d (acc, genv) =
    let here = add_pos_from_st d and loc = to_pos d in
    let () =
      if false then
        Format.eprintf "@[<v>Typing with %s in env:@ %a@]@." strictness_string
          pp_global genv
      else if false then Format.eprintf "@[Typing %a.@]@." PP.pp_t [ d ]
    in
    let new_d, new_genv =
      match d.desc with
      (* Begin TypecheckDecl *)
      | D_Func ({ body = SB_ASL _; _ } as f) ->
          let (env1, f1), ses_func_sig =
            annotate_and_declare_func ~loc f genv
          in
          let new_f, ses_f = try_annotate_subprogram env1 f1 ses_func_sig in
          let () =
            if
              ISet.mem f.name (SES.get_calls_recursives ses_f)
              && Option.is_none f.recurse_limit
            then warn_from ~loc Error.(NoRecursionLimit [ f.name ])
          in
          let ses_f = SES.remove_calls_recursives ses_f in
          let new_d = D_Func new_f |> here
          and new_env = StaticEnv.add_subprogram new_f.name new_f ses_f env1 in
          (new_d, new_env.global) |: TypingRule.TypecheckDecl
      | D_Func ({ body = SB_Primitive _; _ } as f) ->
          let (new_env, new_f), _ = annotate_and_declare_func ~loc f genv in
          let new_d = D_Func new_f |> here in
          (new_d, new_env.global)
      | D_GlobalStorage gsd ->
          let gsd', new_genv = declare_global_storage ~loc gsd genv in
          let new_d = D_GlobalStorage gsd' |> here in
          (new_d, new_genv) |: TypingRule.TypecheckDecl
      | D_TypeDecl (x, ty, s) ->
          let new_genv = declare_type ~loc x ty s genv in
          (d, new_genv) |: TypingRule.TypecheckDecl
      (* End *)
      | D_Pragma _ -> assert false
    in
    (new_d :: acc, new_genv)

  (* Being CheckGlobalPragma *)
  let check_global_pragma genv d =
    let loc = to_pos d in
    match d.desc with
    | D_Pragma (id, args) ->
        let () = warn_from ~loc (Error.PragmaUse id) in
        List.iter
          (fun e -> annotate_expr (with_empty_local genv) e |> ignore)
          args
        |: TypingRule.CheckGlobalPragma
    | _ -> assert false
  (* End *)

  (* Begin PropagateRecursiveCallsSess *)
  let propagate_recursive_calls_sess (sess : (func * SES.t) list) :
      (func * SES.t) list =
    let () =
      if false then
        let open Format in
        let pp_sep f () = fprintf f ";@ " in
        let pp f ((func_sig : func), ses) =
          fprintf f "@[<h 2>%s:@ %a@]" func_sig.name SES.pp_print ses
        in
        eprintf "@[<v 2>Propagating side-effects from:@ @[<v 2>[%a]@]@]@."
          (pp_print_list ~pp_sep pp) sess
    in
    let map0 =
      List.map (fun ((f : func), ses) -> (f.name, ses)) sess |> IMap.of_list
    in
    let call_graph = IMap.map (fun ses -> SES.get_calls_recursives ses) map0 in
    let transitive_call_graph = transitive_closure call_graph in
    let map0_without_recursive_calls =
      IMap.map (fun ses -> SES.remove_calls_recursives ses) map0
    in
    let res =
      List.map
        (fun ((func : func), ses) ->
          let callees =
            IMap.find func.name transitive_call_graph |> ISet.elements
          in
          let sess =
            List.map (fun x -> IMap.find x map0_without_recursive_calls) callees
          in
          (func, SES.unions (SES.remove_calls_recursives ses :: sess)))
        sess
    in
    let () =
      if false then
        let open Format in
        let pp_sep f () = fprintf f ";@ " in
        let pp f ((func_sig : func), ses) =
          fprintf f "@[<h 2>%s:@ %a@]" func_sig.name SES.pp_print ses
        in
        eprintf "@[<v 2>Propagating side-effects from:@ @[<v 2>[%a]@]@]@."
          (pp_print_list ~pp_sep pp) res
    in
    res
  (* End *)

  (** [check_recursive_limit_annotations locs sess] emits a warning if there a
      cycle in the call-graph described by [sess] without static annotations.

      The argument [locs] is only used for identifying an location in which to
      print the error.
  *)
  let check_recursive_limit_annotations locs sess =
    let call_graph_without_annotated_functions =
      List.filter_map
        (function
          | { recurse_limit = None; body = SB_ASL _; name }, ses ->
              Some (name, SES.get_calls_recursives ses)
          | _ -> None)
        sess
      |> IMap.of_list
    in
    match get_cycle call_graph_without_annotated_functions with
    | None -> ()
    | Some [] -> assert false
    | Some (x :: _ as cycle) ->
        let loc =
          List.find (fun d -> String.equal x (identifier_of_decl d)) locs
        in
        warn_from ~loc Error.(NoRecursionLimit cycle)

  (* Begin TypeCheckMutuallyRec *)
  let type_check_mutually_rec ds (acc, genv0) =
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
          let loc = to_pos d in
          match d.desc with
          | D_Func f ->
              let env', f, ses_f = annotate_func_sig ~loc genv0 f in
              (env'.local, f, ses_f, loc)
          | _ ->
              fatal_from ~loc
                (Error.BadRecursiveDecls
                   (List.map ASTUtils.identifier_of_decl ds)))
        ds
    in
    let env_and_fs1 =
      (* Setters last as they need getters declared. *)
      let setters, others =
        List.partition
          (fun (_, f, _, _) ->
            match f.subprogram_type with
            | ST_Setter | ST_EmptySetter -> true
            | _ -> false)
          env_and_fs
      in
      List.rev_append others setters
    in
    (* DeclareSubprograms( *)
    let genv2, env_and_fs2 =
      list_fold_left_map
        (fun genv (lenv, f, ses_f, loc) ->
          let env = { global = genv; local = lenv } in
          let env1, f1 = declare_one_func ~loc f ses_f env in
          (env1.global, (env1.local, f1, ses_f, loc)))
        genv0 env_and_fs1
      |: TypingRule.DeclareSubprograms
      (* DeclareSubprograms) *)
    in
    let ds, sess =
      list_map_split
        (fun (lenv2, f, ses_f, loc) ->
          let env2 = { local = lenv2; global = genv2 } in
          let here d = add_pos_from ~loc d in
          match f.body with
          | SB_ASL _ ->
              let () =
                if false then Format.eprintf "@[Analysing decl %s.@]@." f.name
              in
              let new_f, ses_f = try_annotate_subprogram env2 f ses_f in
              (D_Func new_f |> here, (new_f, ses_f))
          | SB_Primitive side_effecting ->
              let ses =
                if side_effecting then SES.calls_recursive f.name else SES.empty
              in
              (D_Func f |> here, (f, ses)))
        env_and_fs2
    in
    let () = check_recursive_limit_annotations ds sess in
    let env3 =
      let sess_prop = propagate_recursive_calls_sess sess in
      (* AddSubprogramDecls( *)
      List.fold_left
        (fun env2 ((new_f : func), ses_f) ->
          StaticEnv.add_subprogram new_f.name new_f ses_f env2)
        (StaticEnv.with_empty_local genv2)
        sess_prop
      (* AddSubprogramDecls) *)
    in
    (List.rev_append ds acc, env3.global) |: TypingRule.TypeCheckMutuallyRec
  (* End *)

  (* Begin TypeCheckAST *)
  let type_check_ast_in_env =
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
    fun env ast ->
      (* Type check D_Pragma declarations separately from the main AST
         We can do this because no other declaration depends on a pragma *)
      let is_pragma d = match d.desc with D_Pragma _ -> true | _ -> false in
      let pragmas, others = List.partition is_pragma ast in
      let ast_rev, env = fold_topo others ([], env) in
      let () = List.iter (check_global_pragma env) pragmas in
      (List.rev ast_rev, env)

  let type_check_ast ast = type_check_ast_in_env empty_global ast
end
(* End *)

module TypeCheckDefault = Annotate (struct
  let check = TypeCheck
  let output_format = Error.HumanReadable
  let print_typed = false
  let use_field_getter_extension = false
end)

let type_and_run ?instrumentation ast =
  let ast, static_env =
    Builder.with_stdlib ast
    |> Builder.with_primitives Native.DeterministicBackend.primitives
    |> TypeCheckDefault.type_check_ast
  in
  Native.interprete ?instrumentation static_env ast
