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

let fail a msg = failwith (Printf.sprintf "%s: %s" (PP.pp_pos_str a) msg)

let fail_initialise a id =
  fail a (Printf.sprintf "Cannot initialise variable %s" id)

let _warn = false
let _dbg = false

module type S = sig
  module B : Backend.S

  val run_env : (AST.identifier * B.value) list -> B.ast -> B.value B.m
  val run : B.ast -> B.value B.m
  val run_typed : B.ast -> StaticEnv.env -> B.value B.m
end

module type Config = sig
  module Instr : Instrumentation.SEMINSTR

  val type_checking_strictness : Typing.strictness
  val unroll : int
end

module Make (B : Backend.S) (C : Config) = struct
  module B = B
  module SemanticsRule = Instrumentation.SemanticsRule

  type 'a m = 'a B.m

  module EnvConf = struct
    type v = B.value
    type primitive = B.primitive

    let unroll = C.unroll
  end

  module IEnv = Env.RunTime (EnvConf)

  type env = IEnv.env
  type value_read_from = B.value * identifier * scope

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

  type func_eval_type =
    (value_read_from list * IEnv.global) maybe_exception m

  (*****************************************************************************)
  (*                                                                           *)
  (*                           Monadic operators                               *)
  (*                                                                           *)
  (*****************************************************************************)

  let one = B.v_of_int 1
  let true' = E_Literal (L_Bool true) |> add_dummy_pos
  let false' = E_Literal (L_Bool false) |> add_dummy_pos

  (* Return *)
  (* ------ *)
  let return = B.return
  let return_normal v = Normal v |> return

  let return_continue env : stmt_eval_type =
    Continuing env |> return_normal

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
  let choice m v1 v2 = B.choice m (return v1) (return v2)

  (* Exceptions *)
  let bind_exception binder m f =
    binder m (function
      | Normal v -> f v
      | Throwing _ as res -> return res)

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
    if stop then
      B.warnT (loop_name ^ " unrolling reached limit") env
      >>= return_continue
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

  (* Functions *)
  (* --------- *)

  (** [build_funcs] initialize the unique calling reference for each function
      and builds the subprogram sub-env. *)
  let build_funcs ast (funcs : IEnv.func IMap.t) =
    List.to_seq ast
    |> Seq.filter_map (fun d ->
           match d.desc with
           | D_Func func -> Some (func.name, (ref 0, func))
           | _ -> None)
    |> Fun.flip IMap.add_seq funcs

  (* Global env *)
  (* ---------- *)

  let build_global_storage env0 eval_expr base_value =
    let names =
      List.fold_left (fun k (name, _) -> ISet.add name k) ISet.empty env0
    in
    let def d =
      match d.desc with
      | D_Func { name; _ }
      | D_GlobalStorage { name; _ }
      | D_TypeDecl (name, _, _) ->
          name
    in
    let use d = use_constant_decl ISet.empty d in
    let process_one_decl d =
      match d.desc with
      | D_GlobalStorage { initial_value; name; ty; _ } ->
          fun env_m ->
            if ISet.mem name names then env_m
            else
              let*| env = env_m in
              let* v =
                match (initial_value, ty) with
                | Some e, _ -> eval_expr env e
                | None, None -> fail_initialise d name
                | None, Some t -> base_value env t
              in
              let* () =
                match name with
                | "PSTATE" | "RESADDR" -> return ()
                | _ -> B.on_write_identifier name Scope_Global v
              in
              IEnv.declare_global name v env |> return
      | _ -> Fun.id
    in
    dag_fold def use process_one_decl

  (** [build_genv static_env ast primitives] is the global environment before
      the start of the evaluation of [ast]. *)
  let build_genv env0 eval_expr base_value (static_env : StaticEnv.env)
      (ast : B.primitive AST.t) =
    let funcs = IMap.empty |> build_funcs ast in
    let () =
      if _dbg then
        Format.eprintf "@[<v 2>Executing in env:@ %a@.]"
          StaticEnv.pp_global static_env.global
    in
    let env =
      let open IEnv in
      let global =
        {
          static = static_env.StaticEnv.global;
          storage = Storage.empty;
          funcs;
        }
      in
      { global; local = empty_scoped Scope_Global }
    in
    let env =
      List.fold_left
        (fun env (name, v) -> IEnv.declare_global name v env)
        env env0
    in
    let*| env =
      build_global_storage env0 eval_expr base_value ast (return env)
    in
    return env.global

  (* Bind Environment *)
  (* ---------------- *)
  let discard_exception m =
    B.bind_data m @@ function
    | Normal v -> return v
    | Throwing _ -> assert false

  let bind_env m f =
    B.delay m (function
      | Normal (_v, env) -> fun m -> f (discard_exception m >=> fst, env)
      | Throwing (v, g) -> Throwing (v, g) |> return |> Fun.const)

  let ( let*^ ) = bind_env

  (*****************************************************************************)
  (*                                                                           *)
  (*                           Evaluation functions                            *)
  (*                                                                           *)
  (*****************************************************************************)

  (* Utils *)
  (* ----- *)
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
        let** (v, vs), acc =
          fold_par2 fold (fold_par_list fold) acc e es
        in
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

  let declare_local_identifier env x v =
    let* () = B.on_write_identifier x (IEnv.get_scope env) v in
    IEnv.declare_local x v env |> return

  let declare_local_identifier_m env x m =
    m >>= declare_local_identifier env x

  let declare_local_identifier_mm envm x m =
    let*| env = envm in
    declare_local_identifier_m env x m

  let assign_local_identifier env x v =
    let* () = B.on_write_identifier x (IEnv.get_scope env) v in
    IEnv.assign_local x v env |> return

  let return_identifier i = "return-" ^ string_of_int i
  let throw_identifier () = fresh_var "thrown"

  let read_value_from ((v, name, scope) : value_read_from) =
    let* () = B.on_read_identifier name scope v in
    return v

  let big_op default op =
    let folder m_acc m =
      let* acc = m_acc and* v = m in
      op acc v
    in
    function [] -> default | x :: li -> List.fold_left folder x li

  (* Evaluation of Expressions *)
  (* ------------------------- *)

  (** [eval_expr] specifies how to evaluate an expression [e] in an environment
      [env]. More precisely, [eval_expr env e] is the monadic evaluation  of
      [e] in [env]. *)
  let rec eval_expr (env : env) (e : expr) : expr_eval_type =
    if false then Format.eprintf "@[<3>Eval@ @[%a@]@]@." PP.pp_expr e;
    match e.desc with
    (* Begin Lit *)
    | E_Literal v ->
        return_normal (B.v_of_literal v, env) |: SemanticsRule.Lit
    (* End *)
    (* Begin CTC *)
    | E_CTC (e, t) ->
        let** v, new_env = eval_expr env e in
        let* b = is_val_of_type e env v t in
        (if b then return_normal (v, new_env)
         else
           fatal_from e
             (Error.MismatchType (B.debug_value v, [ t.desc ])))
        |: SemanticsRule.CTC
    (* End *)
    | E_Var x -> (
        match IEnv.find x env with
        (* Begin ELocalVar *)
        | Local v ->
            let* () = B.on_read_identifier x (IEnv.get_scope env) v in
            return_normal (v, env) |: SemanticsRule.ELocalVar
        (* End *)
        (* Begin EGlobalVar *)
        | Global v ->
            let* () = B.on_read_identifier x Scope_Global v in
            return_normal (v, env) |: SemanticsRule.EGlobalVar
        (* End *)
        (* Begin EUndefIdent *)
        | NotFound ->
            fatal_from e @@ Error.UndefinedIdentifier x
            |: SemanticsRule.EUndefIdent)
    (* End *)
    (* Begin BinopAnd *)
    | E_Binop (BAND, e1, e2) ->
        (* if e1 then e2 else false *)
        E_Cond (e1, e2, false')
        |> add_pos_from e |> eval_expr env |: SemanticsRule.BinopAnd
    (* End *)
    (* Begin BinopOr *)
    | E_Binop (BOR, e1, e2) ->
        (* if e1 then true else e2 *)
        E_Cond (e1, true', e2)
        |> add_pos_from e |> eval_expr env |: SemanticsRule.BinopOr
    (* End *)
    (* Begin BinopImpl *)
    | E_Binop (IMPL, e1, e2) ->
        (* if e1 then e2 else true *)
        E_Cond (e1, e2, true')
        |> add_pos_from e |> eval_expr env |: SemanticsRule.BinopImpl
    (* End *)
    (* Begin Binop *)
    | E_Binop (op, e1, e2) ->
        let*^ m1, env' = eval_expr env e1 in
        let*^ m2, new_env = eval_expr env' e2 in
        let* v1 = m1 and* v2 = m2 in
        let* v = B.binop op v1 v2 in
        return_normal (v, new_env) |: SemanticsRule.Binop
    (* End *)
    (* Begin Unop *)
    | E_Unop (op, e') ->
        let** v', env' = eval_expr env e' in
        let* v = B.unop op v' in
        return_normal (v, env') |: SemanticsRule.Unop
    (* End *)
    (* Begin ECond *)
    | E_Cond (e_cond, e1, e2) ->
        let*^ m_cond, env1 = eval_expr env e_cond in
        if is_simple_expr e1 && is_simple_expr e2 then
          let*= v_cond = m_cond in
          let* v =
            B.ternary v_cond
              (fun () -> eval_expr_sef env1 e1)
              (fun () -> eval_expr_sef env1 e2)
          in
          return_normal (v, env) |: SemanticsRule.ECondSimple
        else
          choice m_cond e1 e2 >>*= eval_expr env1 |: SemanticsRule.ECond
    (* End *)
    (* Begin ESlice *)
    | E_Slice (e_bv, slices) ->
        let*^ m_bv, env1 = eval_expr env e_bv in
        let*^ m_positions, env' = eval_slices env1 slices in
        let* v_bv = m_bv and* positions = m_positions in
        let* v = B.read_from_bitvector positions v_bv in
        return_normal (v, env') |: SemanticsRule.ESlice
    (* End *)
    (* Begin ECall *)
    | E_Call (name, actual_args, params) ->
        let**| ms, env =
          eval_call (to_pos e) name env actual_args params
        in
        let* v =
          match ms with
          | [ m ] -> m
          | _ ->
              let* vs = sync_list ms in
              B.create_vector vs
        in
        return_normal (v, env) |: SemanticsRule.ECall
    (* End *)
    (* Begin EGetArray *)
    | E_GetArray (e_array, e_index) -> (
        let*^ m_array, env1 = eval_expr env e_array in
        let*^ m_index, env' = eval_expr env1 e_index in
        let* v_array = m_array and* v_index = m_index in
        match B.v_to_int v_index with
        | None ->
            (* TODO: create a proper runtime error for this.
               It should be caught at type-checking, but still. *)
            fatal_from e (Error.UnsupportedExpr e_index)
        | Some i ->
            let* v = B.get_index i v_array in
            return_normal (v, env') |: SemanticsRule.EGetArray)
    (* End *)
    (* Begin ERecord *)
    | E_Record (_, e_fields) ->
        let names, fields = List.split e_fields in
        let** v_fields, env' = eval_expr_list env fields in
        let* v = B.create_record (List.combine names v_fields) in
        return_normal (v, env') |: SemanticsRule.ERecord
    (* End *)
    (* Begin EGetField *)
    | E_GetField (e_record, field_name) ->
        let** v_record, env' = eval_expr env e_record in
        let* v = B.get_field field_name v_record in
        return_normal (v, env') |: SemanticsRule.EGetBitField
    (* End *)
    (* Begin EGetFields *)
    | E_GetFields _ ->
        fatal_from e Error.TypeInferenceNeeded
        |: SemanticsRule.EGetBitFields
    (* End *)
    (* Begin EConcat *)
    | E_Concat e_list ->
        let** v_list, env' = eval_expr_list env e_list in
        let* v = B.concat_bitvectors v_list in
        return_normal (v, env') |: SemanticsRule.EConcat
    (* End *)
    (* Begin ETuple *)
    | E_Tuple e_list ->
        let** v_list, env' = eval_expr_list env e_list in
        let* v = B.create_vector v_list in
        return_normal (v, env') |: SemanticsRule.ETuple
    (* End *)
    (* Begin EUnknown *)
    | E_Unknown t ->
        let v = B.v_unknown_of_type t in
        return_normal (v, env) |: SemanticsRule.EUnknown
    (* End *)
    (* Begin EPattern *)
    | E_Pattern (e, p) ->
        let** v, env' = eval_expr env e in
        let* v = eval_pattern env e v p in
        return_normal (v, env') |: SemanticsRule.EPattern
  (* End *)

  (* Evaluation of Side-Effect-Free Expressions *)
  (* ------------------------------------------ *)

  (** [eval_expr_sef] specifies how to evaluate a side-effect-free expression
      [e] in an environment [env]. More precisely, [eval_expr_sef env e] is the
      [eval_expr env e], if e is side-effect-free. *)
  (* Begin ESideEffectFreeExpr *)
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
            "@[<hov 2>An exception of type @[<hv>%a@]@ was@ thrown@ \
             when@ evaluating@ %a@]@."
            PP.pp_ty ty PP.pp_expr e
        in
        fatal_from e (Error.UnexpectedSideEffect msg)
  (* End *)

  (* Runtime checks *)
  (* -------------- *)

  (* Begin ValOfType *)
  and is_val_of_type loc env v ty : bool B.m =
    let m_true = L_Bool true |> B.v_of_literal |> return in
    let m_false = L_Bool false |> B.v_of_literal |> return in
    let and' prev here = prev >>= B.binop BAND here in
    let or' prev here = prev >>= B.binop BOR here in
    let rec in_values v ty =
      match (Types.get_structure (IEnv.to_static env) ty).desc with
      | T_Real | T_Bool | T_Enum _ | T_String | T_Int None -> m_true
      | T_Bits (e, _) ->
          let* v' = eval_expr_sef env e
          and* v_length = B.bitvector_length v in
          B.binop EQ_OP v_length v'
      | T_Int (Some constraints) ->
          let fold prev = function
            | Constraint_Exact e ->
                let* v' = eval_expr_sef env e in
                let* here = B.binop EQ_OP v v' in
                or' prev here
            | Constraint_Range (e1, e2) ->
                let* v1 = eval_expr_sef env e1
                and* v2 = eval_expr_sef env e2 in
                let* here =
                  let* c1 = B.binop LEQ v1 v
                  and* c2 = B.binop LEQ v v2 in
                  B.binop BAND c1 c2
                in
                or' prev here
          in
          List.fold_left fold m_false constraints
      | T_Record fields | T_Exception fields ->
          let fold prev (field_name, field_type) =
            let* v' = B.get_field field_name v in
            let* here = in_values v' field_type in
            and' prev here
          in
          List.fold_left fold m_true fields
      | T_Tuple tys ->
          let fold (i, prev) ty' =
            let m =
              let* v' = B.get_index i v in
              let* here = in_values v' ty' in
              and' prev here
            and i = i + 1 in
            (i, m)
          in
          List.fold_left fold (0, m_true) tys |> snd
      | T_Array (e, ty') ->
          let* v = eval_expr_sef env e in
          let n =
            match B.v_to_int v with
            | Some i -> i
            | None -> fatal_from loc @@ Error.UnsupportedExpr e
          in
          let rec loop i prev =
            if i = n then prev
            else
              let* v' = B.get_index i v in
              let* here = in_values v' ty' in
              loop (succ i) (and' prev here)
          in
          loop 0 m_true
      | T_Named _ -> assert false
    in
    choice (in_values v ty) true false
  (* End *)

  (* Evaluation of Left-Hand-Side Expressions *)
  (* ---------------------------------------- *)

  (** [eval_lexpr version env le m] is [env[le --> m]]. *)
  and eval_lexpr ver le env m : env maybe_exception B.m =
    match le.desc with
    (* Begin LEDiscard *)
    | LE_Discard -> return_normal env |: SemanticsRule.LEDiscard
    (* End *)
    | LE_Var x -> (
        let* v = m in
        match IEnv.assign x v env with
        (* Begin LELocalVar *)
        | Local env ->
            let* () = B.on_write_identifier x (IEnv.get_scope env) v in
            return_normal env |: SemanticsRule.LELocalVar
        (* End *)
        (* Begin LEGlobalVar *)
        | Global env ->
            let* () = B.on_write_identifier x Scope_Global v in
            return_normal env |: SemanticsRule.LEGlobalVar
        (* End *)
        | NotFound -> (
            match ver with
            (* Begin LEUndefIdentOne *)
            | V1 ->
                fatal_from le @@ Error.UndefinedIdentifier x
                |: SemanticsRule.LEUndefIdentV1
            (* End *)
            (* Begin LEUndefIdentZero *)
            | V0 ->
                (* V0 first assignments promoted to local declarations *)
                declare_local_identifier env x v
                >>= return_normal |: SemanticsRule.LEUndefIdentV0))
    (* End *)
    (* Begin LESlice *)
    | LE_Slice (re_bv, slices) ->
        let*^ rm_bv, env = expr_of_lexpr re_bv |> eval_expr env in
        let*^ m_positions, env = eval_slices env slices in
        let new_m_bv =
          let* v = m and* positions = m_positions and* rv_bv = rm_bv in
          B.write_to_bitvector positions v rv_bv
        in
        eval_lexpr ver re_bv env new_m_bv |: SemanticsRule.LESlice
    (* End *)
    (* Begin LESetArray *)
    | LE_SetArray (re_array, e_index) ->
        let*^ rm_array, env = expr_of_lexpr re_array |> eval_expr env in
        let*^ m_index, env = eval_expr env e_index in
        let m' =
          let* v = m and* v_index = m_index and* rv_array = rm_array in
          match B.v_to_int v_index with
          | None -> fatal_from le (Error.UnsupportedExpr e_index)
          | Some i -> B.set_index i v rv_array
        in
        eval_lexpr ver re_array env m' |: SemanticsRule.LESetArray
    (* End *)
    (* Begin LESetField *)
    | LE_SetField (re_record, field_name) ->
        let*^ rm_record, env =
          expr_of_lexpr re_record |> eval_expr env
        in
        let m' =
          let* v = m and* rv_record = rm_record in
          B.set_field field_name v rv_record
        in
        eval_lexpr ver re_record env m' |: SemanticsRule.LESetField
    (* End *)
    (* Begin LEDestructuring *)
    | LE_Destructuring le_list ->
        (* The index-out-of-bound on the vector are done either in typing,
           either in [B.get_index]. *)
        let n = List.length le_list in
        let nmonads = List.init n (fun i -> m >>= B.get_index i) in
        multi_assign ver env le_list nmonads
        |: SemanticsRule.LEDestructuring
    (* End *)
    (* Begin LEConcat *)
    | LE_Concat (les, Some widths) ->
        let extract_one width (ms, start) =
          let end_ = start + width in
          let v_width = B.v_of_int width
          and v_start = B.v_of_int start in
          let m' = m >>= B.read_from_bitvector [ (v_start, v_width) ] in
          (m' :: ms, end_)
        in
        let ms, _ = List.fold_right extract_one widths ([], 0) in
        multi_assign V1 env les ms
    (* End *)
    | LE_Concat (_, None) | LE_SetFields _ ->
        let* () =
          let* v = m in
          Format.eprintf "@[<2>Failing on @[%a@]@ <-@ %s@]@." PP.pp_lexpr
            le (B.debug_value v);
          B.return ()
        in
        fatal_from le Error.TypeInferenceNeeded

  (* Evaluation of Expression Lists *)
  (* ------------------------------ *)
  and eval_expr_list_m env es = fold_parm_list eval_expr env es
  and eval_expr_list env es = fold_par_list eval_expr env es

  (* Evaluation of Slices *)
  (* -------------------- *)

  (** [eval_slices env slices] is the list of pair [(i_n, l_n)] that
      corresponds to the start (included) and the length of each slice in
      [slices]. *)
  and eval_slices env :
      slice list -> ((B.value * B.value) list * env) maybe_exception m =
    let eval_one env = function
      (* Begin SliceSingle *)
      | Slice_Single e ->
          let** start, new_env = eval_expr env e in
          return_normal ((start, one), new_env)
          |: SemanticsRule.SliceSingle
      (* End *)
      (* Begin SliceLength *)
      | Slice_Length (e_start, e_length) ->
          let*^ start, env1 = eval_expr env e_start in
          let*^ length, new_env = eval_expr env1 e_length in
          let* start = start and* length = length in
          return_normal ((start, length), new_env)
          |: SemanticsRule.SliceLength
      (* End *)
      (* Begin SliceRange *)
      | Slice_Range (e_top, e_start) ->
          let*^ v_top, env1 = eval_expr env e_top in
          let*^ start, new_env = eval_expr env1 e_start in
          let* v_top = v_top and* start = start in
          let* length = B.binop MINUS v_top start >>= B.binop PLUS one in
          return_normal ((start, length), new_env)
          |: SemanticsRule.SliceRange
      (* End *)
      (* Begin SliceStar *)
      | Slice_Star (e_factor, e_length) ->
          let*^ v_factor, env1 = eval_expr env e_factor in
          let*^ length, new_env = eval_expr env1 e_length in
          let* v_factor = v_factor and* length = length in
          let* start = B.binop MUL v_factor length in
          return_normal ((start, length), new_env)
          |: SemanticsRule.SliceStar
      (* End *)
    in
    fold_par_list eval_one env

  (* Evaluation of Patterns *)
  (* ---------------------- *)

  (** [eval_pattern env pos v p] determines if [v] matches the pattern [p]. *)
  and eval_pattern env pos v : pattern -> B.value m =
    let true_ = B.v_of_literal (L_Bool true) |> return in
    let false_ = B.v_of_literal (L_Bool false) |> return in
    let disjunction = big_op false_ (B.binop BOR)
    and conjunction = big_op true_ (B.binop BAND) in
    function
    (* Begin PAll *)
    | Pattern_All -> true_ |: SemanticsRule.PAll
    (* End *)
    (* Begin PAny *)
    | Pattern_Any ps ->
        let bs = List.map (eval_pattern env pos v) ps in
        disjunction bs |: SemanticsRule.PAny
    (* End *)
    (* Begin PGeq *)
    | Pattern_Geq e ->
        let* v' = eval_expr_sef env e in
        B.binop GEQ v v' |: SemanticsRule.PGeq
    (* End *)
    (* Begin PLeq *)
    | Pattern_Leq e ->
        let* v' = eval_expr_sef env e in
        B.binop LEQ v v' |: SemanticsRule.PLeq
    (* End *)
    (* Begin PNot *)
    | Pattern_Not p' ->
        let* b' = eval_pattern env pos v p' in
        B.unop BNOT b' |: SemanticsRule.PNot
    (* End *)
    (* Begin PRange *)
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
    (* Begin PSingle *)
    | Pattern_Single e ->
        let* v' = eval_expr_sef env e in
        B.binop EQ_OP v v' |: SemanticsRule.PSingle
    (* End *)
    (* Begin PMask *)
    | Pattern_Mask m ->
        let bv bv = L_BitVector bv |> B.v_of_literal in
        let m_set = Bitvector.mask_set m
        and m_unset = Bitvector.mask_unset m in
        let m_specified = Bitvector.logor m_set m_unset in
        let* nv = B.unop NOT v in
        let* v_set = B.binop AND (bv m_set) v
        and* v_unset = B.binop AND (bv m_unset) nv in
        let* v_set_or_unset = B.binop OR v_set v_unset in
        B.binop EQ_OP v_set_or_unset (bv m_specified)
        |: SemanticsRule.PMask
    (* End *)
    (* Begin PTuple *)
    | Pattern_Tuple ps ->
        let n = List.length ps in
        let* vs = List.init n (fun i -> B.get_index i v) |> sync_list in
        let bs = List.map2 (eval_pattern env pos) vs ps in
        conjunction bs |: SemanticsRule.PTuple

  (* End *)
  (* Evaluation of Local Declarations *)
  (* -------------------------------- *)
  and eval_local_decl s ldi env m_init_opt : env maybe_exception m =
    match (ldi, m_init_opt) with
    (* Begin LDDiscard *)
    | LDI_Discard _ty, _ -> return_normal env |: SemanticsRule.LDDiscard
    (* End *)
    (* Begin LDVar *)
    | LDI_Var (x, _ty), Some m ->
        m
        >>= declare_local_identifier env x
        >>= return_normal |: SemanticsRule.LDVar
    (* End *)
    (* Begin LDTypedVar *)
    | LDI_Var (x, Some ty), None ->
        base_value env ty
        >>= declare_local_identifier env x
        >>= return_normal |: SemanticsRule.LDTypedVar
    (* End *)
    (* Begin LDUninitialisedVar *)
    | LDI_Var (x, None), None ->
        fail_initialise s x |: SemanticsRule.LDUninitialisedVar
    (* End *)
    (* Begin LDTuple *)
    | LDI_Tuple (ldis, _ty), Some m ->
        let n = List.length ldis in
        let liv = List.init n (fun i -> m >>= B.get_index i) in
        let folder envm ldi' vm =
          let**| env = envm in
          eval_local_decl s ldi' env (Some vm)
        in
        List.fold_left2 folder (return_normal env) ldis liv
        |: SemanticsRule.LDTuple
    (* End *)
    (* Begin LDTypedTuple *)
    | LDI_Tuple (_ldis, Some ty), None ->
        let m = base_value env ty in
        eval_local_decl s ldi env (Some m) |: SemanticsRule.LDTypedTuple
    (* End *)
    (* Begin LDUninitialisedTuple *)
    | LDI_Tuple (ldis, None), None ->
        let folder envm ldi' =
          let**| env = envm in
          eval_local_decl s ldi' env None
        in
        List.fold_left folder (return_normal env) ldis
        |: SemanticsRule.LDUninitialisedTuple
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
    (* Begin SPass *)
    | S_Pass -> return_continue env |: SemanticsRule.SPass
    (* End *)
    (* Begin SAssignCall *)
    | S_Assign
        ( { desc = LE_Destructuring les; _ },
          { desc = E_Call (name, args, named_args); _ },
          ver )
      when List.for_all lexpr_is_var les ->
        let**| vs, env' =
          eval_call (to_pos s) name env args named_args
        in
        let**| new_env = protected_multi_assign ver env' s les vs in
        return_continue new_env |: SemanticsRule.SAssignCall
    (* End *)
    (* Begin SAssignTuple *)
    | S_Assign
        ( { desc = LE_Destructuring les; _ },
          { desc = E_Tuple exprs; _ },
          ver )
      when List.for_all lexpr_is_var les ->
        let**| vs, env' = eval_expr_list_m env exprs in
        let**| new_env = protected_multi_assign ver env' s les vs in
        return_continue new_env |: SemanticsRule.SAssignTuple
    (* End *)
    (* Begin SAssign *)
    | S_Assign (le, re, ver) ->
        let*^ v, env' = eval_expr env re in
        let**| new_env = eval_lexpr ver le env' v in
        return_continue new_env |: SemanticsRule.SAssign
    (* End *)
    (* Begin SReturnSome *)
    | S_Return (Some { desc = E_Tuple es; _ }) ->
        let**| ms, env = eval_expr_list_m env es in
        let scope = IEnv.get_scope env in
        let folder acc m =
          let*| i, vs = acc in
          let* v = m in
          let* () =
            B.on_write_identifier (return_identifier i) scope v
          in
          return (i + 1, v :: vs)
        in
        let*| _i, vs = List.fold_left folder (return (0, [])) ms in
        return_return env (List.rev vs) |: SemanticsRule.SReturnSome
    (* End *)
    (* Begin SReturnOne *)
    | S_Return (Some e) ->
        let** v, env' = eval_expr env e in
        let* () =
          B.on_write_identifier (return_identifier 0)
            (IEnv.get_scope env') v
        in
        return_return env' [ v ] |: SemanticsRule.SReturnOne
    (* Begin SReturnNone *)
    | S_Return None -> return_return env [] |: SemanticsRule.SReturnNone
    (* End *)
    (* Begin SSeq *)
    | S_Seq (s1, s2) ->
        let*> env' = eval_stmt env s1 in
        eval_stmt env' s2 |: SemanticsRule.SSeq
    (* End *)
    (* Begin SCall *)
    | S_Call (name, args, named_args) ->
        let**| returned, env' =
          eval_call (to_pos s) name env args named_args
        in
        let () = assert (returned = []) in
        return_continue env' |: SemanticsRule.SCall
    (* End *)
    (* Begin SCond *)
    | S_Cond (e, s1, s2) ->
        let*^ v, env' = eval_expr env e in
        let*= s' = choice v s1 s2 in
        eval_block env' s' |: SemanticsRule.SCond
    (* Begin SCase *)
    | S_Case _ -> case_to_conds s |> eval_stmt env |: SemanticsRule.SCase
    (* End *)
    (* Begin SAssert *)
    | S_Assert e ->
        let*^ v, env' = eval_expr env e in
        let*= b = choice v true false in
        if b then return_continue env'
        else
          fatal_from e @@ Error.AssertionFailed e
          |: SemanticsRule.SAssert
    (* End *)
    (* Begin SWhile *)
    | S_While (e, body) ->
        let env = IEnv.tick_push env in
        eval_loop true env e body |: SemanticsRule.SWhile
    (* End *)
    (* Begin SRepeat *)
    | S_Repeat (body, e) ->
        let*> env = eval_block env body in
        let env = IEnv.tick_push_bis env in
        eval_loop false env e body |: SemanticsRule.SRepeat
    (* End *)
    (* Begin SFor *)
    | S_For (id, e1, dir, e2, s) ->
        let* v1 = eval_expr_sef env e1 and* v2 = eval_expr_sef env e2 in
        (* By typing *)
        let undet = B.is_undetermined v1 || B.is_undetermined v2 in
        let*| env = declare_local_identifier env id v1 in
        let env = if undet then IEnv.tick_push_bis env else env in
        let*> env = eval_for undet env id v1 dir v2 s in
        let env = if undet then IEnv.tick_pop env else env in
        IEnv.remove_local id env |> return_continue |: SemanticsRule.SFor
    (* End *)
    (* Begin SThrowNone *)
    | S_Throw None ->
        return (Throwing (None, env)) |: SemanticsRule.SThrowNone
    (* End *)
    (* Begin SThrowSomeTyped *)
    | S_Throw (Some (e, Some t)) ->
        let** v, new_env = eval_expr env e in
        let name = throw_identifier () and scope = Scope_Global in
        let* () = B.on_write_identifier name scope v in
        return (Throwing (Some ((v, name, scope), t), new_env))
        |: SemanticsRule.SThrowSomeTyped
    (* End *)
    (* Begin SThrowSome *)
    | S_Throw (Some (_e, None)) ->
        fatal_from s Error.TypeInferenceNeeded
        |: SemanticsRule.SThrowSome
    (* End *)
    (* Begin STry *)
    | S_Try (s, catchers, otherwise_opt) ->
        let s_m = eval_block env s in
        eval_catchers env catchers otherwise_opt s_m
        |: SemanticsRule.STry
    (* End *)
    (* Begin SDeclSome *)
    | S_Decl (_ldk, ldi, Some e) ->
        let*^ m, env1 = eval_expr env e in
        let**| env' = eval_local_decl s ldi env1 (Some m) in
        return_continue env' |: SemanticsRule.SDeclSome
    (* End *)
    (* Begin SDeclNone *)
    | S_Decl (_dlk, ldi, None) ->
        let**| env' = eval_local_decl s ldi env None in
        return_continue env' |: SemanticsRule.SDeclNone
    (* End *)
    | S_Debug e ->
        let* v = eval_expr_sef env e in
        let () =
          Format.eprintf "@[@<2>%a:@ @[%a@]@ ->@ %s@]@." PP.pp_pos e
            PP.pp_expr e (B.debug_value v)
        in
        return_continue env |: SemanticsRule.SDebug

  (* Evaluation of Blocks *)
  (* -------------------- *)
  (* Begin Block *)
  and eval_block env stm =
    let block_env = IEnv.push_scope env in
    let*> block_env' = eval_stmt block_env stm in
    IEnv.pop_scope env block_env'
    |> return_continue |: SemanticsRule.Block
  (* End *)

  (* Evaluation of while and repeat loops *)
  (* ------------------------------------ *)
  (* Begin Loop *)
  and eval_loop is_while env e_cond body : stmt_eval_type =
    (* Name for warn messages. *)
    let loop_name = if is_while then "While loop" else "Repeat loop" in
    (* Continuation in the positive case. *)
    let loop env =
      let*> env1 = eval_block env body in
      eval_loop is_while env1 e_cond body
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
    choice cond_m loop return_continue
    >>*= binder (return_continue env)
    |: SemanticsRule.Loop
  (* End *)

  (* Evaluation of for loops *)
  (* ----------------------- *)
  (* Begin For *)
  and eval_for undet (env : env) index_name v_start dir v_end body :
      stmt_eval_type =
    (* Evaluate the condition: "Is the for loop terminated?" *)
    let cond_m =
      let op = match dir with Up -> LT | Down -> GT in
      let* () =
        B.on_read_identifier index_name (IEnv.get_scope env) v_start
      in
      B.binop op v_end v_start
    in
    (* Increase the loop counter *)
    let step env index_name v_start dir =
      let op = match dir with Up -> PLUS | Down -> MINUS in
      let* () =
        B.on_read_identifier index_name (IEnv.get_scope env) v_start
      in
      let* v_step = B.binop op v_start one in
      let* env = assign_local_identifier env index_name v_step in
      return (v_step, env)
    in
    (* Continuation in the positive case. *)
    let loop env =
      bind_maybe_unroll "For loop" undet (eval_block env body)
      @@ fun env1 ->
      let*| v_step, env2 = step env1 index_name v_start dir in
      eval_for undet env2 index_name v_step dir v_end body
    in
    (* Real logic: if condition is validated, we continue to the next
       statement, otherwise we loop. *)
    choice cond_m return_continue loop >>*= fun kont ->
    kont env |: SemanticsRule.For
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
    let rethrow_implicit (v, v_ty) res =
      B.bind_seq res @@ function
      | Throwing (None, env_throw1) ->
          Throwing (Some (v, v_ty), env_throw1) |> return
      | _ -> res
    in
    (* [catcher_matches t c] returns true if the catcher [c] match the raised
       exception type [t]. *)
    let catcher_matches =
      let static_env =
        { StaticEnv.empty with global = env.global.static }
      in
      fun v_ty (_e_name, e_ty, _stmt) ->
        Types.type_satisfies static_env v_ty e_ty
    in
    (* Main logic: *)
    (* If an explicit throw has been made in the [try] block: *)
    B.bind_seq s_m @@ function
    (*  Begin CatchNoThrow *)
    | Normal _ | Throwing (None, _) -> s_m |: SemanticsRule.CatchNoThrow
    (* End *)
    | Throwing (Some (v, v_ty), env_throw) -> (
        (* We compute the environment in which to compute the catch statements. *)
        let env1 =
          if IEnv.same_scope env env_throw then env_throw
          else { local = env.local; global = env_throw.global }
        in
        match List.find_opt (catcher_matches v_ty) catchers with
        (* If any catcher matches the exception type: *)
        | Some catcher -> (
            (* Begin Catch *)
            match catcher with
            | None, _e_ty, s ->
                eval_block env1 s
                |> rethrow_implicit (v, v_ty)
                |: SemanticsRule.Catch
            (* Begin CatchNamed *)
            | Some name, _e_ty, s ->
                (* If the exception is declared to be used in the
                   catcher, we update the environment before executing [s]. *)
                let*| env2 =
                  read_value_from v
                  |> declare_local_identifier_m env1 name
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
            (* Begin CatchOtherwise *)
            | Some s ->
                eval_block env1 s
                |> rethrow_implicit (v, v_ty)
                |: SemanticsRule.CatchOtherwise
            (* Begin CatchNone *)
            | None -> s_m |: SemanticsRule.CatchNone))
  (* End *)
  (* Evaluation of Function Calls *)
  (* ---------------------------- *)

  (** [eval_call pos name env args named_args] evaluate the call to function
      [name] with arguments [args] and parameters [named_args] *)
  and eval_call pos name env args named_args =
    let names, nargs = List.split named_args in
    let*^ vargs, env1 = eval_expr_list_m env args in
    let*^ nargs, env2 = eval_expr_list_m env1 nargs in
    let* vargs = vargs and* nargs = nargs in
    let nargs = List.combine names nargs in
    let**| ms, global = eval_func env2.global name pos vargs nargs in
    let ms = List.map read_value_from ms
    and env' = { env2 with global } in
    return_normal (ms, env')

  (* Evaluation of Functions *)
  (* ----------------------- *)

  (** [eval_func genv name pos actual_args params] evaluate the function named [name]
      in the global environment [genv], with [actual_args] the actual arguments, and
      [params] the parameters deduced by type equality. *)
  and eval_func (genv : IEnv.global) name pos
      (actual_args : B.value m list) params : func_eval_type =
    match IMap.find_opt name genv.funcs with
    (* Begin FUndefIdent *)
    | None ->
        fatal_from pos @@ Error.UndefinedIdentifier name
        |: SemanticsRule.FUndefIdent
    (* End *)
    (* Begin FPrimitive *)
    | Some (r, { body = SB_Primitive body; _ }) ->
        let scope = Scope_Local (name, !r) in
        let () = incr r in
        let* ms = body actual_args in
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
    (* Begin FBadArity *)
    | Some (_, { args = arg_decls; _ })
      when List.compare_lengths actual_args arg_decls <> 0 ->
        fatal_from pos
        @@ Error.BadArity
             (name, List.length arg_decls, List.length actual_args)
        |: SemanticsRule.FBadArity
    (* End *)
    (* Begin FCall *)
    | Some (r, { body = SB_ASL body; args = arg_decls; _ }) ->
        (let () = if false then Format.eprintf "Evaluating %s.@." name in
         let scope = Scope_Local (name, !r) in
         let () = incr r in
         let env1 = IEnv.{ global = genv; local = empty_scoped scope } in
         let one_arg envm (x, _) m =
           declare_local_identifier_mm envm x m
         in
         let env2 =
           List.fold_left2 one_arg (return env1) arg_decls actual_args
         in
         let one_narg envm (x, m) =
           let*| env = envm in
           if IEnv.mem x env then return env
           else declare_local_identifier_m env x m
         in
         let*| env3 = List.fold_left one_narg env2 params in
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
  and multi_assign ver env les monads : env maybe_exception m =
    let folder envm le vm =
      let**| env = envm in
      eval_lexpr ver le env vm
    in
    List.fold_left2 folder (return_normal env) les monads

  (** As [multi_assign], but checks that [les] and [monads] have the same
      length. *)
  and protected_multi_assign ver env pos les monads :
      env maybe_exception m =
    if List.compare_lengths les monads != 0 then
      fatal_from pos
      @@ Error.BadArity
           ("tuple construction", List.length les, List.length monads)
    else multi_assign ver env les monads

  (* Base Value *)
  (* ---------- *)
  and base_value env t =
    let t_struct = Types.get_structure (IEnv.to_static env) t in
    let lit v = B.v_of_literal v |> return in
    match t_struct.desc with
    | T_Bool -> L_Bool true |> lit
    | T_Bits (e, _) ->
        let* v = eval_expr_sef env e in
        let length =
          match B.v_to_int v with
          | None -> fatal_from t (Error.UnsupportedExpr e)
          | Some i -> i
        in
        L_BitVector (Bitvector.zeros length) |> lit
    | T_Enum li ->
        IMap.find (List.hd li) env.global.static.constants_values |> lit
    | T_Int None | T_Int (Some []) -> L_Int Z.zero |> lit
    | T_Int (Some (Constraint_Exact e :: _))
    | T_Int (Some (Constraint_Range (e, _) :: _)) ->
        eval_expr_sef env e
    | T_Named _ -> assert false
    | T_Real -> L_Real Q.zero |> lit
    | T_Exception fields | T_Record fields ->
        List.map
          (fun (name, t_field) ->
            let* v = base_value env t_field in
            return (name, v))
          fields
        |> sync_list >>= B.create_record
    | T_String ->
        Error.fatal_from t
          (Error.NotYetImplemented "Base value of string types.")
    | T_Tuple li ->
        List.map (base_value env) li |> sync_list >>= B.create_vector
    | T_Array (e_length, ty) ->
        let* v = base_value env ty in
        let* length =
          match e_length.desc with
          | E_Var x when IMap.mem x env.global.static.declared_types ->
              IMap.find x env.global.static.constants_values
              |> B.v_of_literal |> return
          | _ -> eval_expr_sef env e_length
        in
        let length =
          match B.v_to_int length with
          | None -> Error.fatal_from t (Error.UnsupportedExpr e_length)
          | Some i -> i
        in
        List.init length (Fun.const v) |> B.create_vector

  (* Begin TopLevel *)
  let run_typed_env env (ast : B.ast) (static_env : StaticEnv.env) :
      B.value m =
    let*| env = build_genv env eval_expr_sef base_value static_env ast in
    let*| res = eval_func env "main" dummy_annotated [] [] in
    match res with
    | Normal ([ v ], _genv) -> read_value_from v
    | Normal _ ->
        Error.(fatal_unknown_pos (MismatchedReturnValue "main"))
    | Throwing (v_opt, _genv) ->
        let msg =
          match v_opt with
          | None -> "implicitely thrown out of a try-catch."
          | Some ((v, _, _scope), ty) ->
              Format.asprintf "%a %s" PP.pp_ty ty (B.debug_value v)
        in
        Error.fatal_unknown_pos (Error.UncaughtException msg)

  let run_typed ast env = run_typed_env [] ast env

  let run_env (env : (AST.identifier * B.value) list) (ast : B.ast) :
      B.value m =
    let ast = Builder.with_stdlib ast in
    let ast, static_env =
      Typing.type_check_ast C.type_checking_strictness ast
        StaticEnv.empty
    in
    let () =
      if false then
        Format.eprintf "@[<v 2>Typed AST:@ %a@]@." PP.pp_t ast
    in
    run_typed_env env ast static_env

  let run ast = run_env [] ast |: SemanticsRule.TopLevel
  (* End TopLevel *)
end
