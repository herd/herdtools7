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

let fatal_from pos = Error.fatal_from pos

(* A bit more informative than assert false *)

let _warn = false
let _dbg = false

let rec subtypes_names env s1 s2 =
  if String.equal s1 s2 then true
  else
    match IMap.find_opt s1 StaticEnv.(env.global.subtypes) with
    | None -> false
    | Some s1' -> subtypes_names env s1' s2

let subtypes env t1 t2 =
  match (t1.desc, t2.desc) with
  | T_Named s1, T_Named s2 -> subtypes_names env s1 s2
  | _ -> false

module type S = sig
  module B : Backend.S
  module IEnv : Env.S with type v = B.value and module Scope = B.Scope

  type value_read_from = B.value * AST.identifier * B.Scope.t

  type 'a maybe_exception =
    | Normal of 'a
    | Throwing of (value_read_from * AST.ty) option * IEnv.env

  val eval_expr :
    IEnv.env -> AST.expr -> (B.value * IEnv.env) maybe_exception B.m

  val run_typed_env :
    (AST.identifier * B.value) list -> StaticEnv.global -> AST.t -> B.value B.m

  val run_typed : StaticEnv.global -> AST.t -> B.value B.m
end

module type Config = sig
  module Instr : Instrumentation.SEMINSTR

  val unroll : int
  val error_handling_time : Error.error_handling_time
  val log_nondet_choice : bool
end

module Make (B : Backend.S) (C : Config) = struct
  module B = B
  module SemanticsRule = Instrumentation.SemanticsRule

  type 'a m = 'a B.m

  module EnvConf = struct
    module Scope = B.Scope

    type v = B.value

    let unroll = C.unroll
  end

  module IEnv = Env.RunTime (EnvConf)

  type env = IEnv.env
  type value_read_from = B.value * identifier * B.Scope.t

  type 'a maybe_exception =
    | Normal of 'a
    | Throwing of (value_read_from * ty) option * env

  (** An intermediate result of a statement. *)
  type control_flow_state =
    | Returning of B.value list * IEnv.global
        (** Control flow interruption: skip to the end of the function. *)
    | Continuing of env  (** Normal behaviour: pass to next statement. *)

  type expr_eval_type = (B.value * env) maybe_exception m
  type stmt_eval_type = control_flow_state maybe_exception m
  type func_eval_type = (value_read_from list * IEnv.global) maybe_exception m

  let unsupported_expr e =
    fatal_from e Error.(UnsupportedExpr (C.error_handling_time, e))

  (*****************************************************************************)
  (*                                                                           *)
  (*                           Monadic operators                               *)
  (*                                                                           *)
  (*****************************************************************************)

  let one = B.v_of_int 1
  let true' = E_Literal (L_Bool true) |> add_dummy_annotation
  let false' = E_Literal (L_Bool false) |> add_dummy_annotation

  (* Return *)
  (* ------ *)
  let return = B.return
  let return_normal v = Normal v |> return
  let return_continue env : stmt_eval_type = Continuing env |> return_normal

  let return_return env vs : stmt_eval_type =
    Returning (vs, env.IEnv.global) |> return_normal

  (* Bind *)
  (* ---- *)

  (* Sequential bind *)
  let ( let*| ) = B.bind_seq

  (* Data bind *)
  let ( let* ) = B.bind_data
  let ( >>= ) = B.bind_data

  (* Control bind *)
  let ( let*= ) = B.bind_ctrl
  let ( >>*= ) = B.bind_ctrl

  (* Choice *)
  let choice ~pos m v1 v2 =
    if C.log_nondet_choice then
      let* v = m in
      let () =
        if B.is_undetermined v then
          Printf.eprintf "Non-deterministic choice done at position %s.\n%!"
            (Error.desc_to_string_inf PP.pp_pos pos)
      in
      B.choice (return v) (return v1) (return v2)
    else B.choice m (return v1) (return v2)

  (*
   * Choice with inserted branching (commit) effect/
   * [choice_with_branching_effect_msg m_cond msg v1 v2 kont]:
   *  [m_cond], evaluates to boolean condition,
   *  [msg], message to decorate the branching event,
   *  [v1 v2] alternative for choice.
   *  [kont] contitinuation, takes choosed [v1] or [v2] as
   *         input .
   *)

  let choice_with_branch_effect_msg ~pos m_cond msg v1 v2 kont =
    choice ~pos m_cond v1 v2 >>= fun v ->
    B.commit (Some msg) >>*= fun () -> kont v

  let choice_with_branch_effect m_cond e_cond v1 v2 kont =
    let pp_cond = Format.asprintf "%a@?" PP.pp_expr e_cond in
    choice_with_branch_effect_msg ~pos:e_cond m_cond pp_cond v1 v2 kont

  (* Exceptions *)
  let bind_exception binder m f =
    binder m (function Normal v -> f v | Throwing _ as res -> return res)

  let bind_exception_seq m f = bind_exception B.bind_seq m f
  let ( let**| ) = bind_exception_seq
  let bind_exception_data m f = bind_exception B.bind_data m f
  let ( let** ) = bind_exception_data

  (* Continue *)
  (* [bind_continue m f] executes [f] on [m] only if [m] is [Normal (Continuing _)] *)
  let bind_continue (m : stmt_eval_type) f : stmt_eval_type =
    bind_exception_seq m @@ function
    | Continuing env -> f env
    | Returning _ as res -> return_normal res

  let ( let*> ) = bind_continue

  (* Unroll *)
  (* [bind_unroll "while" m f] executes [f] on [m] after having ticked the
     unrolling stack of [m] only if [m] is [Normal (Continuing _)] *)
  let bind_unroll loop_name (m : stmt_eval_type) f : stmt_eval_type =
    bind_continue m @@ fun env ->
    let stop, env' = IEnv.tick_decr env in
    if stop then B.cutoffT (loop_name ^ " pruned") env >>= return_continue
    else f env'

  let bind_maybe_unroll loop_name undet =
    if undet then bind_unroll loop_name else bind_continue

  (* To give name to rules *)
  let ( |: ) = C.Instr.use_with

  (* Product parallel *)
  (* ---------------- *)
  let ( and* ) = B.prod_par

  (* Application *)
  (* ----------- *)
  let ( >=> ) m f = B.appl_data m f

  (*****************************************************************************)
  (*                                                                           *)
  (*                                Environments                               *)
  (*                                                                           *)
  (*****************************************************************************)

  (* Global env *)
  (* ---------- *)

  let eval_global_decl env0 eval_expr d env_m =
    let*| env = env_m in
    match d.desc with
    | D_GlobalStorage { initial_value; name; _ } -> (
        let scope = B.Scope.global ~init:true in
        match IMap.find_opt name env0 with
        | Some v -> IEnv.declare_global name v env |> return
        | None ->
            let init_expr =
              match initial_value with
              | Some e -> e
              | None -> fatal_from d TypeInferenceNeeded
            in
            let* eval_res = eval_expr env init_expr in
            let v, env2 =
              match eval_res with
              | Normal (v, env2) -> (v, env2)
              | Throwing (exc, _env2) ->
                  let ty =
                    match exc with
                    | Some (_, ty) -> ty
                    | None -> T_Named "implicit" |> add_pos_from d
                  in
                  fatal_from d (UnexpectedInitialisationThrow (ty, name))
            in
            let* () = B.on_write_identifier name scope v in
            let env3 = IEnv.declare_global name v env2 in
            return env3)
    | _ -> return env

  (* Begin EvalBuildGlobalEnv *)

  (** [build_genv penv static_env ast primitives] is the global environment before
      the start of the evaluation of [ast], with predefined values in [penv]. *)
  let build_genv env0 eval_expr (static_env : StaticEnv.global) (ast : AST.t) =
    let () =
      if _dbg then
        Format.eprintf "@[<v 2>Executing in env:@ %a@.]" StaticEnv.pp_global
          static_env
    in
    let env0 = IMap.of_list env0 in
    let global_decl_folder = function
      | TopoSort.ASTFold.Single d -> eval_global_decl env0 eval_expr d
      | TopoSort.ASTFold.Recursive ds ->
          List.fold_right (eval_global_decl env0 eval_expr) ds
    in
    let env =
      let open IEnv in
      let global = global_from_static static_env
      and local = local_empty_scoped (B.Scope.global ~init:true) in
      { global; local }
    in
    let*| env = TopoSort.ASTFold.fold global_decl_folder ast (return env) in
    return env.global |: SemanticsRule.BuildGlobalEnv
  (* End *)

  (* Bind Environment *)
  (* ---------------- *)
  let discard_exception m =
    B.bind_data m @@ function
    | Normal v -> return v
    | Throwing _ -> assert false

  let bind_env m f =
    B.delay m @@ fun res m ->
    match res with
    | Normal (_v, env) -> f (discard_exception m >=> fst, env)
    | Throwing _ as res ->
        (* Do not discard [m], otherwise events are lost *)
        m >>= fun _ -> return res

  let ( let*^ ) = bind_env

  (* Primitives handling *)
  (* ------------------- *)
  let primitive_runtimes =
    List.to_seq B.primitives
    |> Seq.map AST.(fun ({ name; subprogram_type = _; _ }, f) -> (name, f))
    |> Hashtbl.of_seq

  let primitive_decls =
    List.map (fun (f, _) -> D_Func f |> add_dummy_annotation) B.primitives

  let () =
    if false then
      Format.eprintf "@[<v 2>Primitives:@ %a@]@." PP.pp_t primitive_decls

  (*****************************************************************************)
  (*                                                                           *)
  (*                           Evaluation functions                            *)
  (*                                                                           *)
  (*****************************************************************************)

  (* Utils *)
  (* ----- *)
  let v_false = L_Bool false |> B.v_of_literal
  let v_true = L_Bool true |> B.v_of_literal
  let m_false = return v_false
  let m_true = return v_true

  let sync_list ms =
    let folder m vsm =
      let* v = m and* vs = vsm in
      return (v :: vs)
    in
    List.fold_right folder ms (return [])

  let fold_par2 fold1 fold2 acc e1 e2 =
    let*^ m1, acc = fold1 acc e1 in
    let*^ m2, acc = fold2 acc e2 in
    let* v1 = m1 and* v2 = m2 in
    return_normal ((v1, v2), acc)

  let rec fold_par_list fold acc es =
    match es with
    | [] -> return_normal ([], acc)
    | e :: es ->
        let** (v, vs), acc = fold_par2 fold (fold_par_list fold) acc e es in
        return_normal (v :: vs, acc)

  let rec fold_parm_list fold acc es =
    match es with
    | [] -> return_normal ([], acc)
    | e :: es ->
        let*^ m, acc = fold acc e in
        let** ms, acc = fold_parm_list fold acc es in
        return_normal (m :: ms, acc)

  let lexpr_is_var le =
    match le.desc with LE_Var _ | LE_Discard -> true | _ -> false

  let declare_local_identifier env name v =
    let* () = B.on_write_identifier name (IEnv.get_scope env) v in
    IEnv.declare_local name v env |> return

  let declare_local_identifier_m env x m = m >>= declare_local_identifier env x

  let declare_local_identifier_mm envm x m =
    let*| env = envm in
    declare_local_identifier_m env x m

  let assign_local_identifier env x v =
    let* () = B.on_write_identifier x (IEnv.get_scope env) v in
    IEnv.assign_local x v env |> return

  let return_identifier i = "return-" ^ string_of_int i
  let throw_identifier () = fresh_var "thrown"

  (* Begin EvalReadValueFrom *)
  let read_value_from ((v, name, scope) : value_read_from) =
    let* () = B.on_read_identifier name scope v in
    return v |: SemanticsRule.ReadValueFrom
  (* End *)

  let big_op default op =
    let folder m_acc m =
      let* acc = m_acc and* v = m in
      op acc v
    in
    function [] -> default | x :: li -> List.fold_left folder x li

  (** [check_non_overlapping_slices slices value_ranges] checks that the
      slice ranges [value_ranges] are non-overlapping.

      [value_ranges] are given by a list of couples [(start, length)].
      If they are overlapping, an error [OverlappingSlices (slices, Dynamic)]
      is thrown. *)
  let check_non_overlapping_slices ~pos slices value_ranges =
    let check_two_ranges_are_non_overlapping (s1, l1) m (s2, l2) =
      let* () = m in
      let cond =
        let* s1l1s2 =
          let* s1l1 = B.binop PLUS s1 l1 in
          B.binop LEQ s1l1 s2
        and* s2l2s1 =
          let* s2l2 = B.binop PLUS s2 l2 in
          B.binop LEQ s2l2 s1
        in
        B.binop BOR s1l1s2 s2l2s1
      in
      let* b = choice ~pos cond true false in
      if b then return ()
      else
        Error.(
          fatal_unknown_pos (OverlappingSlices (slices, C.error_handling_time)))
    in
    let check_range_does_not_overlap_previous_ranges past_ranges range =
      let* past_ranges = past_ranges in
      let* () =
        List.fold_left
          (check_two_ranges_are_non_overlapping range)
          (return ()) past_ranges
      in
      return (range :: past_ranges)
    in
    let* _ =
      List.fold_left check_range_does_not_overlap_previous_ranges (return [])
        value_ranges
    in
    return ()

  (* Evaluation of Expressions *)
  (* ------------------------- *)

  (** [eval_expr] specifies how to evaluate an expression [e] in an environment
      [env]. More precisely, [eval_expr env e] is the monadic evaluation  of
      [e] in [env]. *)
  let rec eval_expr (env : env) (e : expr) : expr_eval_type =
    if false then Format.eprintf "@[<3>Eval@ @[%a@]@]@." PP.pp_expr e;
    match e.desc with
    (* Begin EvalELit *)
    | E_Literal l -> return_normal (B.v_of_literal l, env) |: SemanticsRule.ELit
    (* End *)
    (* Begin EvalATC *)
    | E_ATC (e1, t) ->
        let** v, new_env = eval_expr env e1 in
        let* b = is_val_of_type e1 env v t in
        (if b then return_normal (v, new_env)
         else fatal_from e1 (Error.MismatchType (B.debug_value v, [ t.desc ])))
        |: SemanticsRule.ATC
    (* End *)
    (* Begin EvalEVar *)
    | E_Var x ->
        (match IEnv.find x env with
        | Local v ->
            let* () = B.on_read_identifier x (IEnv.get_scope env) v in
            return_normal (v, env)
        | Global v ->
            let* () = B.on_read_identifier x (B.Scope.global ~init:false) v in
            return_normal (v, env)
        | NotFound -> fatal_from e @@ Error.UndefinedIdentifier x)
        |: SemanticsRule.EVar
    (* End *)
    (* Begin EvalBinopAnd *)
    | E_Binop (BAND, e1, e2) ->
        (* if e1 then e2 else false *)
        E_Cond (e1, e2, false')
        |> add_pos_from e |> eval_expr env |: SemanticsRule.BinopAnd
    (* End *)
    (* Begin EvalBinopOr *)
    | E_Binop (BOR, e1, e2) ->
        (* if e1 then true else e2 *)
        E_Cond (e1, true', e2)
        |> add_pos_from e |> eval_expr env |: SemanticsRule.BinopOr
    (* End *)
    (* Begin EvalBinopImpl *)
    | E_Binop (IMPL, e1, e2) ->
        (* if e1 then e2 else true *)
        E_Cond (e1, e2, true')
        |> add_pos_from e |> eval_expr env |: SemanticsRule.BinopImpl
    (* End *)
    (* Begin EvalBinop *)
    | E_Binop (op, e1, e2) ->
        let*^ m1, env1 = eval_expr env e1 in
        let*^ m2, new_env = eval_expr env1 e2 in
        let* v1 = m1 and* v2 = m2 in
        let* v = B.binop op v1 v2 in
        return_normal (v, new_env) |: SemanticsRule.Binop
    (* End *)
    (* Begin EvalUnop *)
    | E_Unop (op, e1) ->
        let** v1, env1 = eval_expr env e1 in
        let* v = B.unop op v1 in
        return_normal (v, env1) |: SemanticsRule.Unop
    (* End *)
    (* Begin EvalECond *)
    | E_Cond (e_cond, e1, e2) ->
        let*^ m_cond, env1 = eval_expr env e_cond in
        if is_simple_expr e1 && is_simple_expr e2 then
          let*= v_cond = m_cond in
          let* v =
            (* The calls to [eval_expr_sef] are safe because [is_simple_expr]
               implies [is_pure]. *)
            B.ternary v_cond
              (fun () -> eval_expr_sef env1 e1)
              (fun () -> eval_expr_sef env1 e2)
          in
          return_normal (v, env) |: SemanticsRule.ECondSimple
        else
          choice_with_branch_effect m_cond e_cond e1 e2 (eval_expr env1)
          |: SemanticsRule.ECond
    (* End *)
    (* Begin EvalESlice *)
    | E_Slice (e_bv, slices) ->
        let*^ m_bv, env1 = eval_expr env e_bv in
        let*^ m_positions, new_env = eval_slices env1 slices in
        let* v_bv = m_bv and* positions = m_positions in
        let* v = B.read_from_bitvector positions v_bv in
        return_normal (v, new_env) |: SemanticsRule.ESlice
    (* End *)
    (* Begin EvalECall *)
    | E_Call { name; params; args } ->
        (* pass [params] and [args] as labelled arguments to avoid confusion *)
        let**| ms, new_env = eval_call (to_pos e) name env ~params ~args in
        let* v =
          match ms with
          | [ m ] -> m
          | _ ->
              let* vs = sync_list ms in
              B.create_vector vs
        in
        return_normal (v, new_env) |: SemanticsRule.ECall
    (* End *)
    (* Begin EvalEGetArray *)
    | E_GetArray (e_array, e_index) -> (
        let*^ m_array, env1 = eval_expr env e_array in
        let*^ m_index, new_env = eval_expr env1 e_index in
        let* v_array = m_array and* v_index = m_index in
        match B.v_to_int v_index with
        | None -> unsupported_expr e
        | Some i ->
            let* v = B.get_index i v_array in
            return_normal (v, new_env) |: SemanticsRule.EGetArray)
    (* End *)
    (* Begin EvalEGetTupleItem *)
    | E_GetItem (e_tuple, index) ->
        let** v_tuple, new_env = eval_expr env e_tuple in
        let* v = B.get_index index v_tuple in
        return_normal (v, new_env) |: SemanticsRule.EGetTupleItem
    (* End *)
    (* Begin EvalERecord *)
    | E_Record (_, e_fields) ->
        let names, fields = List.split e_fields in
        let** v_fields, new_env = eval_expr_list env fields in
        let* v = B.create_record (List.combine names v_fields) in
        return_normal (v, new_env) |: SemanticsRule.ERecord
    (* End *)
    (* Begin EvalEGetField *)
    | E_GetField (e_record, field_name) ->
        let** v_record, new_env = eval_expr env e_record in
        let* v = B.get_field field_name v_record in
        return_normal (v, new_env) |: SemanticsRule.EGetBitField
    (* End *)
    (* Begin EvalEGetFields *)
    | E_GetFields (e_record, field_names) ->
        let** v_record, new_env = eval_expr env e_record in
        let* v_list =
          List.map
            (fun field_name -> B.get_field field_name v_record)
            field_names
          |> sync_list
        in
        let* v = B.concat_bitvectors v_list in
        return_normal (v, new_env)
    (* End *)
    (* Begin EvalETuple *)
    | E_Tuple e_list ->
        let** v_list, new_env = eval_expr_list env e_list in
        let* v = B.create_vector v_list in
        return_normal (v, new_env) |: SemanticsRule.ETuple
    (* End *)
    (* Begin EvalEArray *)
    | E_Array { length = e_length; value = e_value } ->
        (let** v_value, new_env = eval_expr env e_value in
         (* The call to [eval_expr_sef] is safe because Typing.annotate_type
            checks that all expressions on which a type depends are statically
            evaluable, i.e. side-effect-free. *)
         let* v_length = eval_expr_sef env e_length in
         match B.v_to_int v_length with
         | Some n_length ->
             let* v =
               B.create_vector (List.init n_length (Fun.const v_value))
             in
             return_normal (v, new_env)
         | None -> unsupported_expr e_length)
        |: SemanticsRule.EArray
    (* End *)
    (* Begin EvalEArbitrary *)
    | E_Arbitrary t ->
        (* The call to [eval_expr_sef] is safe because Typing.annotate_type
           checks that all expressions on which a type depends are statically
           evaluable, i.e. side-effect-free. *)
        let* v = B.v_unknown_of_type ~eval_expr_sef:(eval_expr_sef env) t in
        return_normal (v, env) |: SemanticsRule.EArbitrary
    (* End *)
    (* Begin EvalEPattern *)
    | E_Pattern (e, p) ->
        let** v1, new_env = eval_expr env e in
        let* v = eval_pattern env e v1 p in
        return_normal (v, new_env) |: SemanticsRule.EPattern
  (* End *)

  (* Evaluation of Side-Effect-Free Expressions *)
  (* ------------------------------------------ *)

  (** [eval_expr_sef] specifies how to evaluate a side-effect-free expression
      [e] in an environment [env]. More precisely, [eval_expr_sef env e] is the
      [eval_expr env e], if e is side-effect-free. *)
  (* Begin EvalESideEffectFreeExpr *)
  and eval_expr_sef env e : B.value m =
    eval_expr env e >>= function
    | Normal (v, _env) -> return v
    | Throwing (None, _) ->
        let msg =
          Format.asprintf
            "@[<hov 2>An exception was@ thrown@ when@ evaluating@ %a@]@."
            PP.pp_expr e
        in
        fatal_from e (Error.UnexpectedSideEffect msg)
    | Throwing (Some (_, ty), _) ->
        let msg =
          Format.asprintf
            "@[<hov 2>An exception of type @[<hv>%a@]@ was@ thrown@ when@ \
             evaluating@ %a@]@."
            PP.pp_ty ty PP.pp_expr e
        in
        fatal_from e (Error.UnexpectedSideEffect msg)
  (* End *)

  (* Runtime checks *)
  (* -------------- *)

  (* Begin EvalValOfType *)
  and is_val_of_type loc env v ty : bool B.m =
    let big_or = big_op m_false (B.binop BOR) in
    let rec in_values v ty =
      match ty.desc with
      | T_Int UnConstrained -> m_true
      | T_Int (Parameterized _) ->
          (* This cannot happen, because:
             1. Forgetting now about named types, or any kind of compound types,
                you cannot ask: [expr as ty] if ty is the unconstrained integer
                because there is no syntax for it.
             2. You cannot construct a type that is an alias for the
                parameterized integer type.
             3. You cannot put the parameterized integer type in a compound
                type.
          *)
          fatal_from loc Error.UnrespectedParserInvariant
      | T_Bits (e, _) ->
          (* The call to [eval_expr_sef] is safe because Typing.annotate_type
             checks that all expressions on which a type depends are statically
             evaluable, i.e. side-effect-free. *)
          let* v' = eval_expr_sef env e and* v_length = B.bitvector_length v in
          B.binop EQ_OP v_length v'
      | T_Int (WellConstrained constraints) ->
          (* The calls to [eval_expr_sef] is safe because Typing.annotate_type
             checks that all expressions on which a type depends are statically
             evaluable, i.e. side-effect-free. *)
          let map_constraint = function
            | Constraint_Exact e ->
                let* v' = eval_expr_sef env e in
                B.binop EQ_OP v v'
            | Constraint_Range (e1, e2) ->
                let* v1 = eval_expr_sef env e1 and* v2 = eval_expr_sef env e2 in
                let* c1 = B.binop LEQ v1 v and* c2 = B.binop LEQ v v2 in
                B.binop BAND c1 c2
          in
          List.map map_constraint constraints |> big_or
      | T_Tuple tys ->
          let fold (i, prev) ty' =
            let m =
              let* v' = B.get_index i v in
              let* here = in_values v' ty' in
              prev >>= B.binop BAND here
            in
            (i + 1, m)
          in
          List.fold_left fold (0, m_true) tys |> snd
      | _ -> fatal_from loc TypeInferenceNeeded
    in
    choice ~pos:loc (in_values v ty) true false
  (* End *)

  (* Evaluation of Left-Hand-Side Expressions *)
  (* ---------------------------------------- *)

  (** [eval_lexpr version env le m] is [env[le --> m]]. *)
  and eval_lexpr ver le env m : env maybe_exception B.m =
    match le.desc with
    (* Begin EvalLEDiscard *)
    | LE_Discard -> return_normal env |: SemanticsRule.LEDiscard
    (* End *)
    | LE_Var x -> (
        let* v = m in
        match IEnv.assign x v env with
        (* Begin EvalLEVar *)
        | Local env ->
            let* () = B.on_write_identifier x (IEnv.get_scope env) v in
            return_normal env |: SemanticsRule.LEVar
        | Global env ->
            let* () = B.on_write_identifier x (B.Scope.global ~init:false) v in
            return_normal env |: SemanticsRule.LEVar
        (* End *)
        | NotFound -> (
            match ver with
            (* Begin EvalLEUndefIdentOne *)
            | V1 ->
                fatal_from le @@ Error.UndefinedIdentifier x
                |: SemanticsRule.LEUndefIdentV1
            (* End *)
            (* Begin EvalLEUndefIdentZero *)
            | V0 ->
                (* V0 first assignments promoted to local declarations *)
                declare_local_identifier env x v
                >>= return_normal |: SemanticsRule.LEUndefIdentV0))
    (* End *)
    (* Begin EvalLESlice *)
    | LE_Slice (e_bv, slices) ->
        let*^ m_bv, env1 = expr_of_lexpr e_bv |> eval_expr env in
        let*^ m_slice_ranges, env2 = eval_slices env1 slices in
        let new_m_bv =
          let* v = m and* slice_ranges = m_slice_ranges and* v_bv = m_bv in
          let* () = check_non_overlapping_slices ~pos:le slices slice_ranges in
          B.write_to_bitvector slice_ranges v v_bv
        in
        eval_lexpr ver e_bv env2 new_m_bv |: SemanticsRule.LESlice
    (* End *)
    (* Begin EvalLESetArray *)
    | LE_SetArray (re_array, e_index) ->
        let*^ rm_array, env1 = expr_of_lexpr re_array |> eval_expr env in
        let*^ m_index, env2 = eval_expr env1 e_index in
        let m1 =
          let* v = m and* v_index = m_index and* rv_array = rm_array in
          match B.v_to_int v_index with
          | None -> unsupported_expr e_index
          | Some i -> B.set_index i v rv_array
        in
        eval_lexpr ver re_array env2 m1 |: SemanticsRule.LESetArray
    (* End *)
    (* Begin EvalLESetField *)
    | LE_SetField (re_record, field_name) ->
        let*^ rm_record, env1 = expr_of_lexpr re_record |> eval_expr env in
        let m1 =
          let* v = m and* rv_record = rm_record in
          B.set_field field_name v rv_record
        in
        eval_lexpr ver re_record env1 m1 |: SemanticsRule.LESetField
    (* End *)
    (* Begin EvalLEDestructuring *)
    | LE_Destructuring le_list ->
        (* The index-out-of-bound on the vector are done either in typing,
           either in [B.get_index]. *)
        let n = List.length le_list in
        let nmonads = List.init n (fun i -> m >>= B.get_index i) in
        multi_assign ver env le_list nmonads |: SemanticsRule.LEDestructuring
    (* End *)
    (* Begin EvalLESetFields *)
    | LE_SetFields (le_record, fields, slices) ->
        let () =
          if List.compare_lengths fields slices != 0 then
            fatal_from le Error.TypeInferenceNeeded
        in
        let*^ rm_record, env1 = expr_of_lexpr le_record |> eval_expr env in
        let m2 =
          List.fold_left2
            (fun m1 field_name (i1, i2) ->
              let slice = [ (B.v_of_int i1, B.v_of_int i2) ] in
              let* v = m >>= B.read_from_bitvector slice and* rv_record = m1 in
              B.set_field field_name v rv_record)
            rm_record fields slices
        in
        eval_lexpr ver le_record env1 m2 |: SemanticsRule.LESetField
  (* End *)

  (* Evaluation of Expression Lists *)
  (* ------------------------------ *)
  (* Begin EvalEExprListM *)
  and eval_expr_list_m env es =
    fold_parm_list eval_expr env es |: SemanticsRule.EExprListM

  (* End *)
  (* Begin EvalEExprList *)
  and eval_expr_list env es =
    fold_par_list eval_expr env es |: SemanticsRule.EExprList
  (* End *)

  (* Evaluation of Slices *)
  (* -------------------- *)

  (** [eval_slices env slices] is the list of pair [(i_n, l_n)] that
      corresponds to the start (included) and the length of each slice in
      [slices]. *)
  and eval_slices env :
      slice list -> (B.value_range list * env) maybe_exception m =
    (* Begin EvalSlice *)
    let eval_slice env = function
      | Slice_Single e ->
          let** v_start, new_env = eval_expr env e in
          return_normal ((v_start, one), new_env) |: SemanticsRule.Slice
      | Slice_Length (e_start, e_length) ->
          let*^ m_start, env1 = eval_expr env e_start in
          let*^ m_length, new_env = eval_expr env1 e_length in
          let* v_start = m_start and* v_length = m_length in
          return_normal ((v_start, v_length), new_env) |: SemanticsRule.Slice
      | Slice_Range (e_top, e_start) ->
          let*^ m_top, env1 = eval_expr env e_top in
          let*^ m_start, new_env = eval_expr env1 e_start in
          let* v_top = m_top and* v_start = m_start in
          let* v_length = B.binop MINUS v_top v_start >>= B.binop PLUS one in
          return_normal ((v_start, v_length), new_env) |: SemanticsRule.Slice
      | Slice_Star (e_factor, e_length) ->
          let*^ m_factor, env1 = eval_expr env e_factor in
          let*^ m_length, new_env = eval_expr env1 e_length in
          let* v_factor = m_factor and* v_length = m_length in
          let* v_start = B.binop MUL v_factor v_length in
          return_normal ((v_start, v_length), new_env) |: SemanticsRule.Slice
      (* End *)
    in
    (* Begin EvalSlices *)
    fold_par_list eval_slice env |: SemanticsRule.Slices
  (* End *)

  (* Evaluation of Patterns *)
  (* ---------------------- *)

  (** [eval_pattern env pos v p] determines if [v] matches the pattern [p]. *)
  and eval_pattern env pos v : pattern -> B.value m =
    let true_ = B.v_of_literal (L_Bool true) |> return in
    let false_ = B.v_of_literal (L_Bool false) |> return in
    let disjunction = big_op false_ (B.binop BOR)
    and conjunction = big_op true_ (B.binop BAND) in
    (* The calls to [eval_expr_sef] are safe because Typing.annotate_pattern
       checks that all expressions on which a type depends are statically
       evaluable, i.e. side-effect-free. *)
    fun p ->
      match p.desc with
      (* Begin EvalPAll *)
      | Pattern_All -> true_ |: SemanticsRule.PAll
      (* End *)
      (* Begin EvalPAny *)
      | Pattern_Any ps ->
          let bs = List.map (eval_pattern env pos v) ps in
          disjunction bs |: SemanticsRule.PAny
      (* End *)
      (* Begin EvalPGeq *)
      | Pattern_Geq e ->
          let* v1 = eval_expr_sef env e in
          B.binop GEQ v v1 |: SemanticsRule.PGeq
      (* End *)
      (* Begin EvalPLeq *)
      | Pattern_Leq e ->
          let* v1 = eval_expr_sef env e in
          B.binop LEQ v v1 |: SemanticsRule.PLeq
      (* End *)
      (* Begin EvalPNot *)
      | Pattern_Not p1 ->
          let* b1 = eval_pattern env pos v p1 in
          B.unop BNOT b1 |: SemanticsRule.PNot
      (* End *)
      (* Begin EvalPRange *)
      | Pattern_Range (e1, e2) ->
          let* b1 =
            let* v1 = eval_expr_sef env e1 in
            B.binop GEQ v v1
          and* b2 =
            let* v2 = eval_expr_sef env e2 in
            B.binop LEQ v v2
          in
          B.binop BAND b1 b2 |: SemanticsRule.PRange
      (* End *)
      (* Begin EvalPSingle *)
      | Pattern_Single e ->
          let* v1 = eval_expr_sef env e in
          B.binop EQ_OP v v1 |: SemanticsRule.PSingle
      (* End *)
      (* Begin EvalPMask *)
      | Pattern_Mask m ->
          let bv bv = L_BitVector bv |> B.v_of_literal in
          let m_set = Bitvector.mask_set m
          and m_unset = Bitvector.mask_unset m in
          let m_specified = Bitvector.logor m_set m_unset in
          let* nv = B.unop NOT v in
          let* v_set = B.binop AND (bv m_set) v
          and* v_unset = B.binop AND (bv m_unset) nv in
          let* v_set_or_unset = B.binop OR v_set v_unset in
          B.binop EQ_OP v_set_or_unset (bv m_specified) |: SemanticsRule.PMask
      (* End *)
      (* Begin EvalPTuple *)
      | Pattern_Tuple ps ->
          let n = List.length ps in
          let* vs = List.init n (fun i -> B.get_index i v) |> sync_list in
          let bs = List.map2 (eval_pattern env pos) vs ps in
          conjunction bs |: SemanticsRule.PTuple

  (* End *)
  (* Evaluation of Local Declarations *)
  (* -------------------------------- *)
  and eval_local_decl ldi env m_init : env maybe_exception m =
    let () =
      if false then Format.eprintf "Evaluating %a.@." PP.pp_local_decl_item ldi
    in
    match ldi with
    (* Begin EvalLDVar *)
    | LDI_Var x ->
        m_init
        >>= declare_local_identifier env x
        >>= return_normal |: SemanticsRule.LDVar
    (* End *)
    (* Begin EvalLDTuple *)
    | LDI_Tuple ldis ->
        let n = List.length ldis in
        let* vm = m_init in
        let liv = List.init n (fun i -> B.return vm >>= B.get_index i) in
        let folder envm x vm =
          let**| env = envm in
          vm >>= declare_local_identifier env x >>= return_normal
        in
        List.fold_left2 folder (return_normal env) ldis liv
        |: SemanticsRule.LDTuple
  (* End *)

  (* Evaluation of Statements *)
  (* ------------------------ *)

  (** [eval_stmt env s] evaluates [s] in [env]. This is either an interruption
      [Returning vs] or a continuation [env], see [eval_res]. *)
  and eval_stmt (env : env) s : stmt_eval_type =
    (if false then
       match s.desc with
       | S_Seq _ -> ()
       | _ -> Format.eprintf "@[<3>Eval@ @[%a@]@]@." PP.pp_stmt s);
    match s.desc with
    (* Begin EvalSPass *)
    | S_Pass -> return_continue env |: SemanticsRule.SPass
    (* End *)
    (* Begin EvalSAssignCall *)
    | S_Assign
        ( { desc = LE_Destructuring les; _ },
          { desc = E_Call { name; params; args }; _ } )
      when List.for_all lexpr_is_var les ->
        (* pass [params] and [args] as labelled arguments to avoid confusion *)
        let**| vms, env1 = eval_call (to_pos s) name env ~params ~args in
        let**| new_env = protected_multi_assign s.version env1 s les vms in
        return_continue new_env |: SemanticsRule.SAssignCall
    (* End *)
    (* Begin EvalSAssign *)
    | S_Assign (le, re) ->
        let*^ m, env1 = eval_expr env re in
        let**| new_env = eval_lexpr s.version le env1 m in
        return_continue new_env |: SemanticsRule.SAssign
    (* End *)
    (* Begin EvalSReturn *)
    | S_Return (Some { desc = E_Tuple es; _ }) ->
        let**| ms, new_env = eval_expr_list_m env es in
        let scope = IEnv.get_scope new_env in
        let folder acc m =
          let*| i, vs = acc in
          let* v = m in
          let* () = B.on_write_identifier (return_identifier i) scope v in
          return (i + 1, v :: vs)
        in
        let*| _i, vs = List.fold_left folder (return (0, [])) ms in
        return_return new_env (List.rev vs) |: SemanticsRule.SReturn
    | S_Return (Some e) ->
        let** v, env1 = eval_expr env e in
        let* () =
          B.on_write_identifier (return_identifier 0) (IEnv.get_scope env1) v
        in
        return_return env1 [ v ] |: SemanticsRule.SReturn
    | S_Return None -> return_return env [] |: SemanticsRule.SReturn
    (* End *)
    (* Begin EvalSSeq *)
    | S_Seq (s1, s2) ->
        let*> env1 = eval_stmt env s1 in
        eval_stmt env1 s2 |: SemanticsRule.SSeq
    (* End *)
    (* Begin EvalSCall *)
    | S_Call { name; params; args } ->
        (* pass [params] and [args] as labelled arguments to avoid confusion *)
        let**| returned, env' = eval_call (to_pos s) name env ~params ~args in
        let () = assert (returned = []) in
        return_continue env' |: SemanticsRule.SCall
    (* End *)
    (* Begin EvalSCond *)
    | S_Cond (e, s1, s2) ->
        let*^ v, env' = eval_expr env e in
        choice_with_branch_effect v e s1 s2 (eval_block env')
        |: SemanticsRule.SCond
    | S_Case _ -> fatal_from s Error.TypeInferenceNeeded |: SemanticsRule.SCase
    (* Begin EvalSAssert *)
    | S_Assert e ->
        let*^ v, env1 = eval_expr env e in
        let*= b = choice ~pos:s v true false in
        if b then return_continue env1
        else fatal_from e @@ Error.AssertionFailed e |: SemanticsRule.SAssert
    (* End *)
    (* Begin EvalSWhile *)
    | S_While (e, e_limit_opt, body) ->
        let* limit_opt = eval_limit env e_limit_opt in
        let env = IEnv.tick_push env in
        eval_loop s true env limit_opt e body |: SemanticsRule.SWhile
    (* End *)
    (* Begin EvalSRepeat *)
    | S_Repeat (body, e, e_limit_opt) ->
        let* limit_opt1 = eval_limit env e_limit_opt in
        let* limit_opt2 = tick_loop_limit s limit_opt1 in
        let*> env1 = eval_block env body in
        let env2 = IEnv.tick_push_bis env1 1 in
        eval_loop s false env2 limit_opt2 e body |: SemanticsRule.SRepeat
    (* End *)
    (* Begin EvalSFor *)
    | S_For { index_name; start_e; dir; end_e; body; limit = e_limit_opt } ->
        (* The calls to [eval_expr_sef] are safe because Typing.annotate_stmt,
           S_For case, checks that the bounds are side-effect-free. *)
        let* start_v = eval_expr_sef env start_e
        and* end_v = eval_expr_sef env end_e
        and* limit_opt = eval_limit env e_limit_opt in
        (* By typing *)
        let undet = B.is_undetermined start_v || B.is_undetermined end_v in
        let*| env1 = declare_local_identifier env index_name start_v in
        let env2 = if undet then IEnv.tick_push_bis env1 1 else env1 in
        let loop_msg =
          if undet then Printf.sprintf "for %s" index_name
          else
            Printf.sprintf "for %s = %s %s %s" index_name
              (B.debug_value start_v) (PP.pp_for_direction dir)
              (B.debug_value end_v)
        in
        let*> env3 =
          eval_for loop_msg undet env2 index_name limit_opt start_v dir end_v
            body
        in
        let env4 = if undet then IEnv.tick_pop env3 else env3 in
        IEnv.remove_local index_name env4
        |> return_continue |: SemanticsRule.SFor
    (* End *)
    (* Begin EvalSThrow *)
    | S_Throw None -> return (Throwing (None, env)) |: SemanticsRule.SThrow
    | S_Throw (Some (e, Some t)) ->
        let** v, new_env = eval_expr env e in
        let name = throw_identifier () and scope = B.Scope.global ~init:false in
        let* () = B.on_write_identifier name scope v in
        return (Throwing (Some ((v, name, scope), t), new_env))
        |: SemanticsRule.SThrow
    | S_Throw (Some (_e, None)) -> fatal_from s Error.TypeInferenceNeeded
    (* End *)
    (* Begin EvalSTry *)
    | S_Try (s1, catchers, otherwise_opt) ->
        let s_m = eval_block env s1 in
        eval_catchers env catchers otherwise_opt s_m |: SemanticsRule.STry
    (* End *)
    (* Begin EvalSDecl *)
    | S_Decl (_ldk, ldi, _ty_opt, Some e_init) ->
        let*^ m_init, env1 = eval_expr env e_init in
        let**| new_env = eval_local_decl ldi env1 m_init in
        return_continue new_env |: SemanticsRule.SDecl
    | S_Decl (_ldk, _ldi, _ty_opt, None) -> fatal_from s TypeInferenceNeeded
    (* End *)
    (* Begin EvalSPrint *)
    | S_Print { args = e_list; newline; debug } ->
        let** v_list, new_env = eval_expr_list env e_list in
        let () =
          if debug then
            let open Format in
            let pp_value fmt v = B.debug_value v |> pp_print_string fmt in
            eprintf "@[@<2>%a:@ @[%a@]@ ->@ %a@]@." PP.pp_pos s
              (pp_print_list ~pp_sep:pp_print_space PP.pp_expr)
              e_list
              (pp_print_list ~pp_sep:pp_print_space pp_value)
              v_list
          else (
            List.map B.debug_value v_list |> String.concat "" |> print_string;
            if newline then print_newline () else ())
        in
        return_continue new_env |: SemanticsRule.SPrint
    (* End *)
    | S_Pragma _ -> assert false
    | S_Unreachable -> fatal_from s Error.UnreachableReached

  (* Evaluation of Blocks *)
  (* -------------------- *)
  (* Begin EvalBlock *)
  and eval_block env stm =
    let block_env = IEnv.push_scope env in
    let*> block_env1 = eval_stmt block_env stm in
    IEnv.pop_scope env block_env1 |> return_continue |: SemanticsRule.Block
  (* End *)

  (* Evaluation of while and repeat loops *)
  (* ------------------------------------ *)

  (* Evaluation of loop limits *)
  and eval_limit env e_limit_opt =
    match e_limit_opt with
    | None -> return None
    | Some e_limit -> (
        (* The call to [eval_expr_sef] is safe because
           [Typing.annotate_limit_expr] checks that the limit is statically
           evaluable. *)
        let* v_limit = eval_expr_sef env e_limit in
        match B.v_to_int v_limit with
        | Some limit -> return (Some limit)
        | None ->
            fatal_from e_limit
              (Error.MismatchType (B.debug_value v_limit, [ integer' ])))

  and check_recurse_limit pos name env e_limit_opt =
    let* limit_opt = eval_limit env e_limit_opt in
    match limit_opt with
    | None -> return ()
    | Some limit ->
        let stack_size = IEnv.get_stack_size name env in
        if limit < stack_size then fatal_from pos Error.RecursionLimitReached
        else return ()

  and tick_loop_limit loc limit_opt =
    match limit_opt with
    | None -> return None
    | Some limit ->
        if limit >= 1 then return (Some (limit - 1))
        else fatal_from loc Error.LoopLimitReached

  (* Begin EvalLoop *)
  and eval_loop loc is_while env limit_opt e_cond body : stmt_eval_type =
    (* Name for warn messages. *)
    let loop_name = if is_while then "While loop" else "Repeat loop" in
    (* Continuation in the positive case. *)
    let loop env =
      let* limit_opt' = tick_loop_limit loc limit_opt in
      let*> env1 = eval_block env body in
      eval_loop loc is_while env1 limit_opt' e_cond body
    in
    (* First we evaluate the condition *)
    let*^ cond_m, env = eval_expr env e_cond in
    (* Depending if we are in a while or a repeat,
       we invert that condition. *)
    let cond_m = if is_while then cond_m else cond_m >>= B.unop BNOT in
    (* If needs be, we tick the unrolling stack before looping. *)
    B.delay cond_m @@ fun cond cond_m ->
    let binder = bind_maybe_unroll loop_name (B.is_undetermined cond) in
    (* Real logic: if condition is validated, we loop,
       otherwise we continue to the next statement. *)
    choice_with_branch_effect cond_m e_cond loop return_continue
      (binder (return_continue env))
    |: SemanticsRule.Loop
  (* End *)

  (* Evaluation of for loops *)
  (* ----------------------- *)
  (* Begin EvalFor *)
  and eval_for loop_msg undet env index_name limit_opt v_start dir v_end body :
      stmt_eval_type =
    (* Evaluate the condition: "has the for loop terminated?" *)
    let* next_limit_opt = tick_loop_limit body limit_opt in
    let cond_m =
      let comp_for_dir = match dir with Up -> LT | Down -> GT in
      let* () = B.on_read_identifier index_name (IEnv.get_scope env) v_start in
      B.binop comp_for_dir v_end v_start
    in
    (* Increase the loop counter *)
    let step env index_name v_start dir =
      let op_for_dir = match dir with Up -> PLUS | Down -> MINUS in
      let* () = B.on_read_identifier index_name (IEnv.get_scope env) v_start in
      let* v_step = B.binop op_for_dir v_start one in
      let* new_env = assign_local_identifier env index_name v_step in
      return (v_step, new_env)
    in
    (* Continuation in the positive case. *)
    let loop env =
      bind_maybe_unroll "For loop" undet (eval_block env body) @@ fun env1 ->
      let*| v_step, env2 = step env1 index_name v_start dir in
      eval_for loop_msg undet env2 index_name next_limit_opt v_step dir v_end
        body
    in
    (* Real logic: if the condition holds, we continue to the next
       loop iteration, otherwise we loop. *)
    choice_with_branch_effect_msg ~pos:body cond_m loop_msg return_continue loop
      (fun kont -> kont env)
    |: SemanticsRule.For
  (* End *)

  (* Evaluation of Catchers *)
  (* ---------------------- *)
  and eval_catchers env catchers otherwise_opt s_m : stmt_eval_type =
    (* rethrow_implicit handles the implicit throwing logic, that is for
       statement like:
          try throw_my_exception ()
          catch
            when MyException => throw;
          end
       It edits the thrown value only in the case of an implicit throw and
       we have a explicitely thrown exception in the context. More formally:
       [rethrow_implicit to_throw m] is:
         - [m] if [m] is [Normal _]
         - [m] if [m] is [Throwing (Some _, _)]
         - [Throwing (Some to_throw, g)] if  [m] is [Throwing (None, g)] *)
    (* Begin EvalRethrowImplicit *)
    let rethrow_implicit (v, v_ty) s_m =
      B.bind_seq s_m @@ function
      | Throwing (None, env_throw1) ->
          Throwing (Some (v, v_ty), env_throw1) |> return
      | (Normal _ | Throwing (Some _, _)) as res ->
          return res |: SemanticsRule.RethrowImplicit
      (* End *)
    in
    (* [catcher_matches t c] returns true if the catcher [c] match the raised
       exception type [t]. *)
    (* Begin EvalFindCatcher *)
    let catcher_matches =
      let static_env = { StaticEnv.empty with global = env.global.static } in
      fun v_ty (_e_name, e_ty, _stmt) ->
        subtypes static_env v_ty e_ty |: SemanticsRule.FindCatcher
      (* End *)
    in
    (* Main logic: *)
    (* If an explicit throw has been made in the [try] block: *)
    B.bind_seq s_m @@ function
    (*  Begin CatchNoThrow *)
    | (Normal _ | Throwing (None, _)) as res ->
        return res |: SemanticsRule.CatchNoThrow
    (* End *)
    | Throwing (Some (v, v_ty), env_throw) -> (
        (* We compute the environment in which to compute the catch statements. *)
        match List.find_opt (catcher_matches v_ty) catchers with
        (* If any catcher matches the exception type: *)
        | Some catcher -> (
            (* Begin EvalCatch *)
            match catcher with
            | None, _e_ty, s ->
                eval_block env_throw s
                |> rethrow_implicit (v, v_ty)
                |: SemanticsRule.Catch
            (* Begin EvalCatchNamed *)
            | Some name, _e_ty, s ->
                (* If the exception is declared to be used in the
                   catcher, we update the environment before executing [s]. *)
                let*| env2 =
                  read_value_from v |> declare_local_identifier_m env_throw name
                in
                (let*> env3 = eval_block env2 s in
                 IEnv.remove_local name env3 |> return_continue)
                |> rethrow_implicit (v, v_ty)
                |: SemanticsRule.CatchNamed
                (* End *))
        | None -> (
            (* Otherwise we try to execute the otherwise statement, or we
               return the exception. *)
            match otherwise_opt with
            (* Begin EvalCatchOtherwise *)
            | Some s ->
                eval_block env_throw s
                |> rethrow_implicit (v, v_ty)
                |: SemanticsRule.CatchOtherwise
            (* Begin EvalCatchNone *)
            | None -> s_m |: SemanticsRule.CatchNone))
  (* End *)
  (* Evaluation of Function Calls *)
  (* ---------------------------- *)

  (** [eval_call pos name env ~params ~args] evaluates the call to function
      [name] with arguments [args] and parameters [params].
      The arguments/parameters are labelled to avoid confusion. *)
  (* Begin EvalCall *)
  and eval_call pos name env ~params ~args =
    let*^ vargs, env1 = eval_expr_list_m env args in
    let*^ vparams, env2 = eval_expr_list_m env1 params in
    let* vargs = vargs and* vparams = vparams in
    let genv = IEnv.incr_stack_size name env2.global in
    let res = eval_subprogram genv name pos ~params:vparams ~args:vargs in
    B.bind_seq res @@ function
    | Throwing (v, env_throw) ->
        let genv2 = IEnv.decr_stack_size name env_throw.global in
        let new_env = IEnv.{ local = env2.local; global = genv2 } in
        return (Throwing (v, new_env)) |: SemanticsRule.Call
    | Normal (ms, global) ->
        let ms2 = List.map read_value_from ms in
        let genv2 = IEnv.decr_stack_size name global in
        let new_env = IEnv.{ local = env2.local; global = genv2 } in
        return_normal (ms2, new_env) |: SemanticsRule.Call
  (* End *)

  (* Evaluation of Subprograms *)
  (* ----------------------- *)

  (** [eval_subprogram genv name pos ~params ~args] evaluates the function named [name]
      in the global environment [genv], with [args] the actual arguments, and
      [params] the positional parameters.
      The arguments/parmeters are labelled to avoid confusion. *)
  and eval_subprogram (genv : IEnv.global) name pos ~params
      ~(args : B.value m list) : func_eval_type =
    match IMap.find_opt name genv.static.subprograms with
    (* Begin EvalFUndefIdent *)
    | None ->
        fatal_from pos @@ Error.UndefinedIdentifier name
        |: SemanticsRule.FUndefIdent
    (* End *)
    (* Begin EvalFPrimitive *)
    | Some ({ body = SB_Primitive _; _ }, _) ->
        let scope = B.Scope.new_local name in
        let body = Hashtbl.find primitive_runtimes name in
        let* ms = body params args in
        let _, vsm =
          List.fold_right
            (fun m (i, acc) ->
              let x = return_identifier i in
              let m' =
                let*| v =
                  let* v = m in
                  let* () = B.on_write_identifier x scope v in
                  return (v, x, scope)
                and* vs = acc in
                return (v :: vs)
              in
              (i + 1, m'))
            ms
            (0, return [])
        in
        let*| vs = vsm in
        return_normal (vs, genv) |: SemanticsRule.FPrimitive
    (* End *)
    (* Begin EvalFBadArity *)
    | Some ({ args = arg_decls; _ }, _)
      when List.compare_lengths args arg_decls <> 0 ->
        fatal_from pos
        @@ Error.BadArity
             (Dynamic, name, List.length arg_decls, List.length args)
        |: SemanticsRule.FBadArity
    | Some ({ parameters = parameter_decls; _ }, _)
      when List.compare_lengths params parameter_decls <> 0 ->
        fatal_from pos
        @@ Error.BadParameterArity
             (Dynamic, V1, name, List.length parameter_decls, List.length params)
        |: SemanticsRule.FBadArity
    (* End *)
    (* Begin EvalFCall *)
    | Some
        ( {
            body = SB_ASL body;
            args = arg_decls;
            parameters = param_decls;
            recurse_limit;
            _;
          },
          _ ) ->
        (let () = if false then Format.eprintf "Evaluating %s.@." name in
         let scope = B.Scope.new_local name in
         let env1 = IEnv.{ global = genv; local = local_empty_scoped scope } in
         let* () = check_recurse_limit pos name env1 recurse_limit in
         let one_arg envm (x, _) m = declare_local_identifier_mm envm x m in
         let env2 = List.fold_left2 one_arg (return env1) arg_decls args in
         let*| env3 = List.fold_left2 one_arg env2 param_decls params in
         let**| res = eval_stmt env3 body in
         let () =
           if false then Format.eprintf "Finished evaluating %s.@." name
         in
         match res with
         | Continuing env4 -> return_normal ([], env4.global)
         | Returning (xs, ret_genv) ->
             let vs =
               List.mapi (fun i v -> (v, return_identifier i, scope)) xs
             in
             return_normal (vs, ret_genv))
        |: SemanticsRule.FCall
  (* End *)

  (** [multi_assign env [le_1; ... ; le_n] [m_1; ... ; m_n]] is
      [env[le_1 --> m_1] ... [le_n --> m_n]]. *)
  (* Begin EvalLEMultiAssign *)
  and multi_assign ver env les monads : env maybe_exception m =
    let folder envm le vm =
      let**| env = envm in
      eval_lexpr ver le env vm
    in
    List.fold_left2 folder (return_normal env) les monads
    |: SemanticsRule.LEMultiAssign
  (* End *)

  (** As [multi_assign], but checks that [les] and [monads] have the same
      length. *)
  and protected_multi_assign ver env pos les monads : env maybe_exception m =
    if List.compare_lengths les monads != 0 then
      fatal_from pos
      @@ Error.BadArity
           (Dynamic, "tuple construction", List.length les, List.length monads)
    else multi_assign ver env les monads

  (* Begin EvalSpec *)
  let run_typed_env env (static_env : StaticEnv.global) (ast : AST.t) :
      B.value m =
    let*| env = build_genv env eval_expr static_env ast in
    let*| res =
      eval_subprogram env "main" dummy_annotated ~params:[] ~args:[]
    in
    (match res with
    | Normal ([ v ], _genv) -> read_value_from v
    | Normal _ -> Error.(fatal_unknown_pos (MismatchedReturnValue "main"))
    | Throwing (v_opt, _genv) ->
        let msg =
          match v_opt with
          | None -> "implicitely thrown out of a try-catch."
          | Some ((v, _, _scope), ty) ->
              Format.asprintf "%a %s" PP.pp_ty ty (B.debug_value v)
        in
        Error.fatal_unknown_pos (Error.UncaughtException msg))
    |: SemanticsRule.Spec

  let run_typed env ast = run_typed_env [] env ast
  (* End *)
end
