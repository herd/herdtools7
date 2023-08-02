(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
(* Authors:                                                                 *)
(* Hadrien Renaud, University College London, UK.                           *)
(****************************************************************************)

open AST
open ASTUtils

let fatal_from pos = Error.fatal_from pos
let _warn = false
let _dbg = false

module type S = sig
  module B : Backend.S

  val run : B.ast -> B.value B.m
  val run_typed : B.ast -> StaticEnv.env -> B.value B.m
end

module type Config = sig
  module Instr : Instrumentation.INSTR

  val type_checking_strictness : Typing.strictness
  val unroll : int
end

module Make (B : Backend.S) (C : Config) = struct
  module B = B
  module Rule = Instrumentation.Rule

  type 'a m = 'a B.m

  module EnvConf = struct
    type v = B.value
    type primitive = B.primitive

    let unroll = C.unroll
  end

  module IEnv = Env.RunTime (EnvConf)

  type env = IEnv.env

  let one = B.v_of_int 1
  let return = B.return

  (* Binding operators, first with data *)
  let ( let* ) = B.bind_data
  let ( >>= ) = B.bind_data

  (* Then with po *)
  let ( let*| ) = B.bind_seq

  (* Parallel *)
  let ( and* ) = B.prod

  (* Applicative *)
  let ( >=> ) m f = m >>= fun v -> return (f v)

  (* To use instrumentation *)
  let ( |: ) = C.Instr.use_with

  let sync_list ms =
    let folder m vsm =
      let* v = m and* vs = vsm in
      return (v :: vs)
    in
    List.fold_right folder ms (return [])

  (*****************************************************************************)
  (*                                                                           *)
  (*                      Construction of the initial env                      *)
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

  let build_global_storage eval_expr base_value =
    let def d =
      match d.desc with
      | D_Func { name; _ }
      | D_GlobalStorage { name; _ }
      | D_TypeDecl (name, _, _) ->
          name
    in
    let use =
      let use_e e acc = use_e acc e in
      let use_ty _ty acc = acc (* TODO *) in
      fun d ->
        match d.desc with
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
    let process_one_decl d =
      match d.desc with
      | D_GlobalStorage { initial_value; name; ty; _ } ->
          fun env_m ->
            let*| env = env_m in
            let* v =
              match (initial_value, ty) with
              | Some e, _ -> eval_expr env e
              | None, None -> assert false
              | None, Some t -> base_value env t
            in
            IEnv.declare_global name v env |> return
      | _ -> Fun.id
    in
    dag_fold def use process_one_decl

  (** [build_genv static_env ast primitives] is the global environment before
      the start of the evaluation of [ast]. *)
  let build_genv eval_expr base_value (static_env : StaticEnv.env)
      (ast : B.primitive AST.t) =
    let funcs = IMap.empty |> build_funcs ast in
    let () =
      if _dbg then
        Format.eprintf "@[<v 2>Executing in env:@ %a@.]" StaticEnv.pp_global
          static_env.global
    in
    let env =
      let open IEnv in
      let global =
        { static = static_env.StaticEnv.global; storage = Storage.empty; funcs }
      in
      { global; local = empty_scoped Scope_Global }
    in
    let*| env = build_global_storage eval_expr base_value ast (return env) in
    return env.global

  (*****************************************************************************)
  (*                                                                           *)
  (*                       Main interpretation functions                       *)
  (*                                                                           *)
  (*****************************************************************************)

  type value_read_from = B.value * identifier * scope

  (** An intermediate result of a statement. *)
  type control_flow_state =
    | Returning of B.value list * IEnv.global
        (** Control flow interruption: skip to the end of the function. *)
    | Continuing of env  (** Normal behaviour: pass to next statement. *)

  type 'a maybe_exception =
    | Normal of 'a
    | Throwing of (value_read_from * ty) option * env

  type expr_eval_type = (B.value * env) maybe_exception m
  type stmt_eval_type = control_flow_state maybe_exception m
  type func_eval_type = (value_read_from list * IEnv.global) maybe_exception m

  let bind_exception binder m f =
    binder m (function Normal v -> f v | Throwing _ as res -> return res)

  let bind_exception_data m f = bind_exception B.bind_data m f
  let bind_exception_seq m f = bind_exception B.bind_seq m f
  let bind_exception_ctrl m f = bind_exception B.bind_ctrl m f
  let normal v = Normal v |> return
  let continue env : stmt_eval_type = Continuing env |> normal

  let returning env vs : stmt_eval_type =
    Returning (vs, env.IEnv.global) |> normal

  let choice_m m v1 v2 = B.choice m (return v1) (return v2)
  let choice v v1 v2 = choice_m (return v) v1 v2

  let discard_exception m =
    B.bind_data m @@ function
    | Normal v -> return v
    | Throwing _ -> assert false

  let bind_env m f =
    B.delay m (function
      | Normal (_v, env) -> fun m -> f (discard_exception m >=> fst, env)
      | Throwing (v, g) -> Throwing (v, g) |> return |> Fun.const)

  (* [bind_continue m f] executes [f] on [m] only if [m] is [Normal (Continuing _)] *)
  let bind_continue (m : stmt_eval_type) f : stmt_eval_type =
    bind_exception_seq m @@ function
    | Continuing env -> f env
    | Returning _ as res -> normal res

  (* [bind_unroll "while" m f] executes [f] on [m] after having ticked the
     unrolling stack of [m] only if [m] is [Normal (Continuing _)] *)
  let bind_unroll loop_name (m : stmt_eval_type) f : stmt_eval_type =
    bind_continue m @@ fun env ->
    let stop, env' = IEnv.tick_decr env in
    if stop then
      B.warnT (loop_name ^ " unrolling reached limit") env >>= continue
    else f env'

  let bind_maybe_unroll loop_name undet =
    if undet then bind_unroll loop_name else bind_continue

  (* [bind_thrown m f] executes [f] on [m] if [m] is [Throwing _] *)
  let bind_thrown (m : stmt_eval_type) f : stmt_eval_type =
    B.bind_seq m @@ function
    | Normal _ as res -> return res
    | Throwing (typed_value_opt, global) -> f typed_value_opt global

  (* [bind_explicit_thrown m f] triggers [f] only if [m] is [Throwing (Some v, g)] *)
  let bind_explicit_thrown (m : stmt_eval_type) f : stmt_eval_type =
    B.bind_seq m @@ function
    | Normal _ | Throwing (None, _) -> m
    | Throwing (Some typed_value, global) -> f typed_value global

  let ( let*^ ) = bind_env
  let ( let** ) = bind_exception_data
  let ( let**| ) = bind_exception_seq
  let ( let*> ) = bind_continue

  let fold_par2 fold1 fold2 acc e1 e2 =
    let*^ m1, acc = fold1 acc e1 in
    let*^ m2, acc = fold2 acc e2 in
    let* v1 = m1 and* v2 = m2 in
    normal ((v1, v2), acc)

  let fold_par fold acc e1 e2 = fold_par2 fold fold acc e1 e2

  let rec fold_par_list fold acc es =
    match es with
    | [] -> normal ([], acc)
    | e :: es ->
        let** (v, vs), acc = fold_par2 fold (fold_par_list fold) acc e es in
        normal (v :: vs, acc)

  let rec fold_parm_list fold acc es =
    match es with
    | [] -> normal ([], acc)
    | e :: es ->
        let*^ m, acc = fold acc e in
        let** ms, acc = fold_parm_list fold acc es in
        normal (m :: ms, acc)

  let lexpr_is_var le =
    match le.desc with LE_Var _ | LE_Ignore -> true | _ -> false

  let declare_local_identifier env x v =
    let* () = B.on_write_identifier x (IEnv.get_scope env) v in
    IEnv.declare_local x v env |> return

  let declare_local_identifier_m env x m = m >>= declare_local_identifier env x

  let declare_local_identifier_mm envm x m =
    let*| env = envm in
    declare_local_identifier_m env x m

  let assign_local_identifier env x v =
    let* () = B.on_write_identifier x (IEnv.get_scope env) v in
    IEnv.assign_local x v env |> return

  let read_value_from ((v, name, scope) : value_read_from) =
    let* () = B.on_read_identifier name scope v in
    return v

  let return_identifier i = "return-" ^ string_of_int i
  let throw_identifier () = fresh_var "thrown"

  (** [eval_expr env e] is the monadic evaluation  of [e] in [env]. *)
  let rec eval_expr (env : env) (e : expr) : expr_eval_type =
    if false then Format.eprintf "@[<3>Eval@ @[%a@]@]@." PP.pp_expr e;
    match e.desc with
    | E_Literal v -> normal (B.v_of_literal v, env) |: Rule.Lit
    | E_Typed (e, _t) -> eval_expr env e |: Rule.IgnoreTypedExpr
    | E_Var x -> (
        match IEnv.find x env with
        | Local v ->
            let* () = B.on_read_identifier x (IEnv.get_scope env) v in
            normal (v, env) |: Rule.ELocalVar
        | Global v ->
            let* () = B.on_read_identifier x Scope_Global v in
            normal (v, env) |: Rule.ELocalVar
        | NotFound -> fatal_from e @@ Error.UndefinedIdentifier x)
    | E_Binop (op, e1, e2) ->
        let** (v1, v2), env = fold_par eval_expr env e1 e2 in
        let* v = B.binop op v1 v2 in
        normal (v, env) |: Rule.Binop
    | E_Unop (op, e) ->
        let** v, env = eval_expr env e in
        let* v = B.unop op v |: Rule.Unop in
        normal (v, env)
    | E_Cond (econd, e2, e3) ->
        let*^ mcond, env = eval_expr env econd in
        if is_simple_expr e2 && is_simple_expr e3 then
          B.bind_ctrl mcond @@ fun cond ->
          let* v =
            B.ternary cond
              (fun () -> eval_expr_sef env e2)
              (fun () -> eval_expr_sef env e3)
          in
          normal (v, env) |: Rule.ECond
        else B.bind_ctrl (choice_m mcond e2 e3) @@ eval_expr env |: Rule.ECond
    | E_Slice (e', slices) ->
        let** (v, positions), env =
          fold_par2 eval_expr eval_slices env e' slices
        in
        let* v' = B.read_from_bitvector positions v in
        normal (v', env) |: Rule.ESlice
    | E_Call (name, args, named_args) ->
        let**| ms, env = eval_call (to_pos e) name env args named_args in
        let* v =
          match ms with
          | [ m ] -> m
          | _ ->
              let* vs = sync_list ms in
              B.create_vector vs
        in
        normal (v, env) |: Rule.ECall
    | E_GetArray (e_array, e_index) -> (
        let** (v_array, v_index), env =
          fold_par eval_expr env e_array e_index
        in
        match B.v_to_int v_index with
        | None -> fatal_from e (Error.UnsupportedExpr e_index)
        | Some i ->
            let* v = B.get_index i v_array in
            normal (v, env))
    | E_Record (_, li) ->
        let names, fields = List.split li in
        let** fields, env = eval_expr_list env fields in
        let* v = B.create_record (List.combine names fields) in
        normal (v, env) |: Rule.ERecord
    | E_GetField (e', x) ->
        let** vec, env = eval_expr env e' in
        let* v = B.get_field x vec in
        normal (v, env) |: Rule.GetRecordField
    | E_Concat es ->
        let** vs, env = eval_expr_list env es in
        let* v = B.concat_bitvectors vs in
        normal (v, env) |: Rule.EConcat
    | E_Tuple es ->
        let** vs, env = eval_expr_list env es in
        let* v = B.create_vector vs in
        normal (v, env)
    | E_Unknown t ->
        let v = B.v_unknown_of_type t in
        normal (v, env)
    | E_Pattern (e, p) ->
        let** v, env = eval_expr env e in
        let* v = eval_pattern env e v p in
        normal (v, env)
    | E_GetFields _ -> fatal_from e Error.TypeInferenceNeeded

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

  (** [eval_slices env slices] is the list of pair [(i_n, l_n)] that
      corresponds to the start (included) and the length of each slice in
      [slices]. *)
  and eval_slices env :
      slice list -> ((B.value * B.value) list * env) maybe_exception m =
    let eval_one env = function
      | Slice_Single e ->
          let** v, env = eval_expr env e in
          normal ((v, one), env)
      | Slice_Range (etop, ebot) ->
          let** (vtop, vbot), env = fold_par eval_expr env etop ebot in
          let* length = B.binop MINUS vtop vbot >>= B.binop PLUS one in
          normal ((vbot, length), env)
      | Slice_Star (efactor, elength) ->
          let ebot = binop MUL efactor elength in
          fold_par eval_expr env ebot elength
      | Slice_Length (ebot, elength) -> fold_par eval_expr env ebot elength
    in
    fold_par_list eval_one env

  (** [eval_pattern env pos v p] determines if [v] matches the pattern [p]. *)
  and eval_pattern env pos v : pattern -> B.value m =
    let true_ = B.v_of_literal (L_Bool true) |> return in
    let false_ = B.v_of_literal (L_Bool false) |> return in
    function
    | Pattern_All -> true_
    | Pattern_Any li ->
        let folder acc p =
          let* acc = acc and* b = eval_pattern env pos v p in
          B.binop BOR acc b
        in
        List.fold_left folder false_ li
    | Pattern_Geq e -> eval_expr_sef env e >>= B.binop GEQ v
    | Pattern_Leq e -> eval_expr_sef env e >>= B.binop LEQ v
    | Pattern_Not p -> eval_pattern env pos v p >>= B.unop BNOT
    | Pattern_Range (e1, e2) ->
        let* b1 = eval_expr_sef env e1 >>= B.binop GEQ v
        and* b2 = eval_expr_sef env e2 >>= B.binop LEQ v in
        B.binop BAND b1 b2
    | Pattern_Single e -> eval_expr_sef env e >>= B.binop EQ_OP v
    | Pattern_Mask m ->
        let bv bv = L_BitVector bv in
        let set = Bitvector.mask_set m |> bv |> B.v_of_literal
        and unset = Bitvector.mask_unset m |> bv |> B.v_of_literal
        and specified = Bitvector.mask_specified m |> bv |> B.v_of_literal in
        let* set = B.binop AND set v
        and* unset = B.unop NOT v >>= B.binop AND unset in
        B.binop OR set unset >>= B.binop EQ_OP specified
    | Pattern_Tuple li ->
        let folderi i acc p =
          let* acc = acc
          and* b =
            let* v' = B.get_index i v in
            eval_pattern env pos v' p
          in
          B.binop BAND acc b
        in
        let folder (acc, i) p = (folderi i acc p, succ i) in
        List.fold_left folder (true_, 0) li |> fst

  and base_value env t =
    let t_struct = Types.get_structure (IEnv.to_static env) t in
    let lit v = B.v_of_literal v |> return in
    match t_struct.desc with
    | T_Bool -> L_Bool true |> lit
    | T_Bits (BitWidth_Constraints (Constraint_Exact e :: _), _)
    | T_Bits (BitWidth_Constraints (Constraint_Range (e, _) :: _), _)
    | T_Bits (BitWidth_SingleExpr e, _) ->
        let* v = eval_expr_sef env e in
        let length =
          match B.v_to_int v with
          | None -> fatal_from t (Error.UnsupportedExpr e)
          | Some i -> i
        in
        L_BitVector (Bitvector.zeros length) |> lit
    | T_Bits (BitWidth_ConstrainedFormType _, _) ->
        Error.fatal_from t
          (Error.NotYetImplemented "Base value of type-constrained bitvectors.")
    | T_Bits (BitWidth_Constraints [], _) ->
        Error.fatal_from t
          (Error.NotYetImplemented "Base value of under-constrained bitvectors.")
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
        let* v = base_value env ty and* length = eval_expr_sef env e_length in
        let length =
          match B.v_to_int length with
          | None -> Error.fatal_from t (Error.UnsupportedExpr e_length)
          | Some i -> i
        in
        List.init length (Fun.const v) |> B.create_vector

  and eval_local_decl ldi env m : env maybe_exception m =
    match (ldi, m) with
    | LDI_Ignore _ty, _ -> normal env |: Rule.LEIgnore
    | LDI_Var (x, _ty), Some m ->
        let* env = declare_local_identifier_m env x m in
        normal env
    | LDI_Var (x, Some ty), None ->
        base_value env ty >>= declare_local_identifier env x >>= normal
    | LDI_Var (_, None), None -> assert false
    | LDI_Tuple (ldis, _ty), Some m ->
        let n = List.length ldis in
        let nmonads = List.init n (fun i -> m >>= B.get_index i) in
        let folder envm ldi' vm =
          let**| env = envm in
          eval_local_decl ldi' env (Some vm)
        in
        List.fold_left2 folder (normal env) ldis nmonads
    | LDI_Tuple (_ldis, Some ty), None ->
        let m = base_value env ty in
        eval_local_decl ldi env (Some m)
    | LDI_Tuple (ldis, None), None ->
        let folder envm ldi' =
          let**| env = envm in
          eval_local_decl ldi' env None
        in
        List.fold_left folder (normal env) ldis

  (** [eval_lexpr env le m] is [env[le --> m]]. *)
  and eval_lexpr le env m : env maybe_exception B.m =
    match le.desc with
    | LE_Ignore -> normal env |: Rule.LEIgnore
    | LE_Var x -> (
        let* v = m in
        match IEnv.assign x v env with
        | Local env ->
            let* () = B.on_write_identifier x (IEnv.get_scope env) v in
            normal env
        | Global env ->
            let* () = B.on_write_identifier x Scope_Global v in
            normal env
        | NotFound -> fatal_from le @@ Error.UndefinedIdentifier x)
    | LE_Slice (le', slices) ->
        let*^ bv_m, env = expr_of_lexpr le' |> eval_expr env in
        let*^ positions_m, env = eval_slices env slices in
        let m' =
          let* v = m and* positions = positions_m and* bv = bv_m in
          B.write_to_bitvector positions v bv
        in
        eval_lexpr le' env m' |: Rule.LESlice
    | LE_SetArray (le', e) ->
        let*^ array_m, env = expr_of_lexpr le' |> eval_expr env in
        let*^ index_m, env = eval_expr env e in
        let m' =
          let* v = m and* index_v = index_m and* array_v = array_m in
          match B.v_to_int index_v with
          | None -> fatal_from le (Error.UnsupportedExpr e)
          | Some i -> B.set_index i v array_v
        in
        eval_lexpr le' env m'
    | LE_SetField (le', x) ->
        let*^ vec_m, env = expr_of_lexpr le' |> eval_expr env in
        let m' =
          let* new_v = m and* vec = vec_m in
          B.set_field x new_v vec
        in
        eval_lexpr le' env m' |: Rule.LESetRecordField
    | LE_TupleUnpack les ->
        (* The index-out-of-bound on the vector are done either in typing,
           either in [B.get_index]. *)
        let n = List.length les in
        let nmonads = List.init n (fun i -> m >>= B.get_index i) in
        multi_assign env les nmonads
    | LE_SetFields _ -> fatal_from le Error.TypeInferenceNeeded

  (** [multi_assign env [le_1; ... ; le_n] [m_1; ... ; m_n]] is
      [env[le_1 --> m_1] ... [le_n --> m_n]]. *)
  and multi_assign env les monads : env maybe_exception m =
    let folder envm le vm =
      let**| env = envm in
      eval_lexpr le env vm
    in
    List.fold_left2 folder (normal env) les monads

  (** As [multi_assign], but checks that [les] and [monads] have the same
      length. *)
  and protected_multi_assign env pos les monads : env maybe_exception m =
    if List.compare_lengths les monads != 0 then
      fatal_from pos
      @@ Error.BadArity
           ("tuple construction", List.length les, List.length monads)
    else multi_assign env les monads

  (** [eval_stmt env s] evaluates [s] in [env]. This is either an interuption
      [Returning vs] or a continuation [env], see [eval_res]. *)
  and eval_stmt (env : env) s : stmt_eval_type =
    (if false then
       match s.desc with
       | S_Then _ -> ()
       | _ -> Format.eprintf "@[<3>Eval@ @[%a@]@]@." PP.pp_stmt s);
    match s.desc with
    | S_Pass -> continue env |: Rule.Pass
    | S_Assign
        ( { desc = LE_TupleUnpack les; _ },
          { desc = E_Call (name, args, named_args); _ } )
      when List.for_all lexpr_is_var les ->
        let**| ms, env = eval_call (to_pos s) name env args named_args in
        let**| env = protected_multi_assign env s les ms in
        continue env
    | S_Assign ({ desc = LE_TupleUnpack les; _ }, { desc = E_Tuple exprs; _ })
      when List.for_all lexpr_is_var les ->
        let**| ms, env = eval_expr_list_m env exprs in
        let**| env = protected_multi_assign env s les ms in
        continue env
    | S_Assign (le, e) ->
        let*^ m, env = eval_expr env e in
        let**| env = eval_lexpr le env m in
        continue env |: Rule.Assign
    | S_Return (Some { desc = E_Tuple es; _ }) ->
        let**| ms, env = eval_expr_list_m env es in
        let scope = IEnv.get_scope env in
        let folder acc m =
          let*| i, vs = acc in
          let* v = m in
          let* () = B.on_write_identifier (return_identifier i) scope v in
          return (i + 1, v :: vs)
        in
        let*| _i, vs = List.fold_left folder (return (0, [])) ms in
        returning env (List.rev vs)
    | S_Return (Some e) ->
        let** v, env = eval_expr env e in
        let* () =
          B.on_write_identifier (return_identifier 0) (IEnv.get_scope env) v
        in
        returning env [ v ] |: Rule.ReturnOne
    | S_Return None -> returning env [] |: Rule.ReturnNone
    | S_Then (s1, s2) ->
        let*> env = eval_stmt env s1 in
        eval_stmt env s2 |: Rule.Then
    | S_Call (name, args, named_args) ->
        let**| returned, env = eval_call (to_pos s) name env args named_args in
        let () = assert (returned = []) in
        continue env |: Rule.SCall
    | S_Cond (e, s1, s2) ->
        bind_exception_ctrl (eval_expr env e) @@ fun (v, env) ->
        choice v s1 s2 >>= eval_block env |: Rule.SCond
    | S_Case _ -> case_to_conds s |> eval_stmt env
    | S_Assert e ->
        bind_exception_ctrl (eval_expr env e) @@ fun (v, env) ->
        let* b = choice v true false in
        if b then continue env
        else fatal_from e @@ Error.AssertionFailed e |: Rule.Assert
    | S_While (e, body) ->
        let env = IEnv.tick_push env in
        eval_loop true env e body
    | S_Repeat (body, e) ->
        let*> env = eval_block env body in
        let env = IEnv.tick_push_bis env in
        eval_loop false env e body
    | S_For (id, e1, dir, e2, s) ->
        let* v1 = eval_expr_sef env e1 and* v2 = eval_expr_sef env e2 in
        (* By typing *)
        let undet = B.is_undetermined v1 || B.is_undetermined v2 in
        let*| env = declare_local_identifier env id v1 in
        let env = if undet then IEnv.tick_push_bis env else env in
        let*> env = eval_for undet env id v1 dir v2 s in
        let env = if undet then IEnv.tick_pop env else env in
        IEnv.remove_local id env |> continue
    | S_Throw None -> return (Throwing (None, env))
    | S_Throw (Some (e, Some t)) ->
        let** v, env = eval_expr env e in
        let name = throw_identifier () and scope = Scope_Global in
        let* () = B.on_write_identifier name scope v in
        return (Throwing (Some ((v, name, scope), t), env))
    | S_Throw (Some (_e, None)) -> fatal_from s Error.TypeInferenceNeeded
    | S_Try (s, catchers, otherwise_opt) ->
        let s_m = eval_block env s in
        eval_catchers env catchers otherwise_opt s_m
    | S_Decl (_ldk, ldi, Some e) ->
        let*^ m, env = eval_expr env e in
        let**| env = eval_local_decl ldi env (Some m) in
        continue env
    | S_Decl (_dlk, ldi, None) ->
        let**| env = eval_local_decl ldi env None in
        continue env

  and eval_block env stm =
    let block_env = IEnv.push_scope env in
    let*> block_env = eval_stmt block_env stm in
    IEnv.pop_scope env block_env |> continue

  and eval_loop is_while env e s : stmt_eval_type =
    (* Name for warn messages. *)
    let loop_name = if is_while then "While loop" else "Repeat loop" in
    (* Continuation in the positive case. *)
    let loop env =
      let*> env = eval_block env s in
      eval_loop is_while env e s
    in
    (* First we evaluate the condition *)
    let*^ cond_m, env = eval_expr env e in
    (* Depending if we are in a while or a repeat, we invert that condition. *)
    let cond_m = if is_while then cond_m else cond_m >>= B.unop BNOT in
    (* If needs be, we tick the unrolling stack before looping. *)
    B.delay cond_m @@ fun cond cond_m ->
    let binder = bind_maybe_unroll loop_name (B.is_undetermined cond) in
    (* Real logic: if condition is validated, we loop, otherwise we continue to
       the next statement. *)
    B.bind_ctrl (choice_m cond_m loop continue) @@ binder (continue env)

  and eval_for undet (env : env) id v dir v2 s : stmt_eval_type =
    (* Evaluate the condition: "Is the for loop terminated?" *)
    let cond_m =
      let op = match dir with Up -> LT | Down -> GT in
      let* () = B.on_read_identifier id (IEnv.get_scope env) v in
      B.binop op v2 v
    in
    (* Increase the loop counter *)
    let step env id v dir =
      let op = match dir with Up -> PLUS | Down -> MINUS in
      let* () = B.on_read_identifier id (IEnv.get_scope env) v in
      let* v = B.binop op v one in
      let* env = assign_local_identifier env id v in
      return (v, env)
    in
    (* Continuation in the positive case. *)
    let loop env =
      bind_maybe_unroll "For loop" undet (eval_block env s) @@ fun env ->
      let*| v, env = step env id v dir in
      eval_for undet env id v dir v2 s
    in
    (* Real logic: if condition is validated, we continue to the next
       statement, otherwise we loop. *)
    B.bind_ctrl (choice_m cond_m continue loop) @@ fun kont -> kont env

  and eval_catchers env catchers otherwise_opt s_m : stmt_eval_type =
    (* rethrow_implicit handles the implicit throwing logic, that is for
       statement like:
          try throw_my_exception ()
          catch
            when MyException => throw;
          end
       It edits the thrown value only it is an implicitly thrown and we have a
       explicitely thrown exception in the context. More formally:
       [rethrow_implicit to_throw m] is:
         - [m] if [m] is [Normal _]
         - [m] if [m] is [Throwing (Some _, _)]
         - [Throwing (Some to_throw, g)] if  [m] is [Throwing (None, g)] *)
    let rethrow_implicit to_throw thrown =
      bind_thrown thrown @@ fun typed_value_opt global ->
      match typed_value_opt with
      | None -> Throwing (Some to_throw, global) |> return
      | Some _ -> thrown
    in
    (* [catcher_matches t c] returns true if the catcher [c] match the raised
       exception type [t]. *)
    let catcher_matches =
      let static_env = { StaticEnv.empty with global = env.global.static } in
      fun v_ty (_e_name, e_ty, _stmt) ->
        Types.type_satisfies static_env v_ty e_ty
    in
    (* Main logic: *)
    (* If an explicit throw has been made in the [try] block: *)
    bind_explicit_thrown s_m @@ fun (v, v_ty) new_env ->
    (* We ensure that any implicit exception raised here will get replaced
       by its value. *)
    rethrow_implicit (v, v_ty)
    @@
    (* We compute the environment in which to compute the catch statements. *)
    let env =
      if IEnv.same_scope env new_env then new_env
      else { local = env.local; global = new_env.global }
    in
    match List.find_opt (catcher_matches v_ty) catchers with
    (* If any catcher matches the exception type: *)
    | Some (None, _e_ty, s) -> eval_block env s
    | Some (Some name, _e_ty, s) ->
        (* If the exception is declared to be used in the catcher, we
           update the environment before executing [s]. *)
        let*| env = read_value_from v |> declare_local_identifier_m env name in
        let*> env = eval_block env s in
        IEnv.remove_local name env |> continue
    | None -> (
        (* Otherwise we try to execute the otherwise statement, or we
           return the exception. *)
        match otherwise_opt with Some s -> eval_block env s | None -> s_m)

  (** [eval_call pos name env args named_args] evaluate the call to function
      [name] with arguments [args] and parameters [named_args] *)
  and eval_call pos name env args named_args =
    let names, nargs = List.split named_args in
    let** (vargs, nargs), env = fold_par eval_expr_list_m env args nargs in
    let nargs = List.combine names nargs in
    let**| ms, global = eval_func env.global name pos vargs nargs in
    let ms = List.map read_value_from ms and env = { env with global } in
    normal (ms, env)

  and eval_expr_list_m env es = fold_parm_list eval_expr env es
  and eval_expr_list env es = fold_par_list eval_expr env es

  (** [eval_func genv name pos args nargs] evaluate the function named [name]
      in the global environment [genv], with [args] the formal arguments, and
      [nargs] the arguments deduced by type equality. *)
  and eval_func (genv : IEnv.global) name pos (args : B.value m list) nargs :
      func_eval_type =
    match IMap.find_opt name genv.funcs with
    | None -> fatal_from pos @@ Error.UndefinedIdentifier name
    | Some (r, { body = SB_Primitive body; _ }) ->
        let scope = Scope_Local (name, !r) in
        let () = incr r in
        let* ms = body args in
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
        normal (vs, genv)
    | Some (_, { args = arg_decls; _ })
      when List.compare_lengths args arg_decls <> 0 ->
        fatal_from pos
        @@ Error.BadArity (name, List.length arg_decls, List.length args)
    | Some (r, { body = SB_ASL body; args = arg_decls; _ }) -> (
        let () = if false then Format.eprintf "Evaluating %s.@." name in
        let scope = Scope_Local (name, !r) in
        let () = incr r in
        let env = IEnv.{ global = genv; local = empty_scoped scope } in
        let one_arg envm (x, _) m = declare_local_identifier_mm envm x m in
        let envm = List.fold_left2 one_arg (return env) arg_decls args in
        let one_narg envm (x, m) =
          let*| env = envm in
          if IEnv.mem x env then return env
          else declare_local_identifier_m env x m
        in
        let*| env = List.fold_left one_narg envm nargs in
        let**| res = eval_stmt env body in
        let () =
          if false then Format.eprintf "Finished evaluating %s.@." name
        in
        match res with
        | Continuing env -> normal ([], env.global)
        | Returning (xs, genv) ->
            let vs =
              List.mapi (fun i v -> (v, return_identifier i, scope)) xs
            in
            normal (vs, genv))

  let run_typed (ast : B.ast) (static_env : StaticEnv.env) : B.value m =
    let*| env = build_genv eval_expr_sef base_value static_env ast in
    let*| res = eval_func env "main" dummy_annotated [] [] in
    match res with
    | Normal ([ v ], _genv) -> read_value_from v
    | Normal _ -> Error.(fatal_unknown_pos (MismatchedReturnValue "main"))
    | Throwing (v_opt, _genv) ->
        let msg =
          match v_opt with
          | None -> "implicitely thrown out of a try-catch."
          | Some ((v, _, _scope), ty) ->
              Format.asprintf "%a %s" PP.pp_ty ty (B.debug_value v)
        in
        Error.fatal_unknown_pos (Error.UncaughtException msg)

  (** Main entry point for the Interpreter, [run ast primitives] type-annotate
      [ast], build a global environment and then evaluate the "main" function
      in it. *)
  let run (ast : B.ast) : B.value m =
    let ast = Builder.with_stdlib ast in
    let ast, static_env =
      Typing.type_check_ast C.type_checking_strictness ast StaticEnv.empty
    in
    let () =
      if false then Format.eprintf "@[<v 2>Typed AST:@ %a@]@." PP.pp_t ast
    in
    run_typed ast static_env
end
