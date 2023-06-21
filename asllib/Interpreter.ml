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

let fatal_from pos = Error.fatal_from pos
let to_pos = ASTUtils.to_pos
let pair x y = (x, y)
let _warn = false
let _dbg = false

module type S = sig
  module B : Backend.S

  type body = B.value B.m list -> B.value B.m list B.m
  type primitive = body func_skeleton

  val run : t -> primitive list -> unit B.m
end

module type Config = sig
  module Instr : Instrumentation.INSTR

  val type_checking_strictness : Typing.strictness
  val unroll : int
end

module Make (B : Backend.S) (C : Config) = struct
  module B = B
  module IMap = ASTUtils.IMap
  module ISet = ASTUtils.ISet
  module Rule = Instrumentation.Rule

  type 'a m = 'a B.m
  type body = B.value m list -> B.value m list m

  module EnvConf = struct
    type v = B.value
    type primitive = body func_skeleton

    let unroll = C.unroll
  end

  module IEnv = Env.RunTime (EnvConf)
  open IEnv.Types

  type primitive = EnvConf.primitive

  let return = B.return

  (* Binding operators, first with data *)
  let ( let* ) = B.bind_data
  let ( >>= ) = B.bind_data

  (* Then without anything *)
  let ( let*| ) = B.bind_seq
  let ( >>=| ) = B.bind_seq

  (* Parallel *)
  let ( and* ) = B.prod
  let ( ||| ) = B.prod

  (* Applicative *)
  let ( >=> ) m f = m >>= fun v -> return (f v)

  (* To use instrumentation *)
  let ( |: ) = C.Instr.use_with
  let ( >|: ) f r m = m >>= fun v -> f (return v) |: r

  (** [prod_map] is a monadic parallel version of List.map. For example:
      [prod_map f [i1; i2; i3]] is {[f i1 ||| f i2 ||| f i3]] *)
  let prod_map f =
    let one acc elt =
      let* v = f elt and* acc = acc in
      return (v :: acc)
    in
    function
    | [] -> return [] | li -> List.fold_left one (return []) li >=> List.rev

  (** [list_index] returns the index of the first element that satisfies the
      predicate [f]. *)
  let list_index f =
    let rec aux i = function
      | [] -> None
      | h :: t -> if f h then Some i else aux (i + 1) t
    in
    aux 0

  (*****************************************************************************)
  (*                                                                           *)
  (*                             Records handling                              *)
  (*                                                                           *)
  (*****************************************************************************)

  let make_record pos ty fields =
    let ty_fields =
      match ty.desc with
      | T_Record ty_fields -> ASTUtils.canonical_fields ty_fields
      | _ -> assert false
    in
    let fields = ASTUtils.canonical_fields fields in
    let values = List.map snd fields in
    let eq_field (x, _) (y, _) = String.equal x y in
    if
      List.compare_lengths ty_fields fields == 0
      && List.for_all2 eq_field ty_fields fields
    then B.create_vector ty values
    else fatal_from pos @@ Error.BadFields (List.map fst fields, ty)

  let record_index_of_field pos x li ty =
    match list_index (fun (y, _) -> String.equal x y) li with
    | Some i -> i
    | None -> fatal_from pos @@ Error.BadField (x, ty)

  (*****************************************************************************)
  (*                                                                           *)
  (*                         Type annotations handling                         *)
  (*                                                                           *)
  (*****************************************************************************)

  (** Unpack a type-annotation. *)
  let type_of_ta pos = function
    | TA_None -> fatal_from pos Error.TypeInferenceNeeded
    | TA_InferredStructure ty -> ty

  (** Build type annotations on the given ast. *)
  let type_annotation ast sfuncs =
    (* There are three steps:
       1. Adding empty functions for each primitive;
       2. Typecheck with [Typing];
       3. Remove the added functions for the primitives.
    *)
    let add_fake_primitives =
      let fake_funcs =
        let one_sfunc { name; args; return_type; parameters; body = _ } =
          D_Primitive
            { name; args; body = ASTUtils.s_pass; return_type; parameters }
        in
        List.map one_sfunc sfuncs
      in
      List.rev_append fake_funcs
    in
    let remove_fake_primitives =
      let primitive_names =
        let one_sfunc ({ name; _ } : 'a func_skeleton) = name in
        sfuncs |> List.to_seq |> Seq.map one_sfunc |> ASTUtils.ISet.of_seq
      in
      let is_primitive = function
        | D_Primitive AST.{ name; _ } ->
            not (ASTUtils.ISet.mem name primitive_names)
        | _ -> true
      in
      List.filter is_primitive
    in
    let ast_with_fake_primitives = add_fake_primitives ast in
    let typed_ast, env =
      Typing.type_check_ast C.type_checking_strictness ast_with_fake_primitives
        Env.Static.empty
    in
    (remove_fake_primitives typed_ast, env)

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
    |> Seq.filter_map (function
         | D_Func func -> Some (func.name, Func (ref 0, func))
         | _ -> None)
    |> Fun.flip IMap.add_seq funcs

  (* Global env *)
  (* ---------- *)

  (** [add_primitives primitives funcs] augments [funcs] with [primitives]. *)
  let add_primitives (primitives : primitive list) funcs =
    let one_primitive (primitive : primitive) =
      (primitive.name, Primitive primitive)
    in
    primitives |> List.to_seq |> Seq.map one_primitive
    |> Fun.flip IMap.add_seq funcs

  let build_global_storage eval_expr =
    let def = function
      | D_Func { name; _ }
      | D_GlobalStorage { name; _ }
      | D_TypeDecl (name, _)
      | D_Primitive { name; _ } ->
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
        | D_TypeDecl (_, ty) -> use_ty ty ISet.empty
        | D_Func _ | D_Primitive _ ->
            ISet.empty (* TODO: pure functions that can be used in constants? *)
    in
    let process_one_decl = function
      | D_GlobalStorage { initial_value = Some e; name; _ } ->
          fun env_m ->
            let*| env = env_m in
            let* v = eval_expr env e in
            IEnv.add_global name v env |> return
      | _ -> Fun.id
    in
    ASTUtils.dag_fold def use process_one_decl

  (** [build_genv static_env ast primitives] is the global environment before
      the start of the evaluation of [ast]. *)
  let build_genv eval_expr (static_env : Env.Static.env) ast primitives =
    let funcs = IMap.empty |> build_funcs ast |> add_primitives primitives in
    let () =
      if _dbg then
        Format.eprintf "@[<v 2>Executing in env:@ %a@.]"
          Env.Static.PPEnv.pp_global static_env.global
    in
    let env =
      let open IEnv in
      let global = { empty_global with static = static_env.global; funcs } in
      { global; local = empty_scoped Scope_Global }
    in
    let*| env = build_global_storage eval_expr ast (return env) in
    return env.global

  (*****************************************************************************)
  (*                                                                           *)
  (*                       Main interpretation functions                       *)
  (*                                                                           *)
  (*****************************************************************************)

  (** An intermediate result of a statement. *)
  type eval_res =
    | Returning of B.value m list
        (** Control flow interruption: skip to the end of the function. *)
    | Continuing of env  (** Normal behaviour: pass to next statement. *)

  let continue (env : env) = return (Continuing env)

  let one_return_value pos name = function
    | [ m ] -> m
    | _ -> fatal_from pos @@ Error.MismatchedReturnValue name

  let lexpr_is_var le =
    match le.desc with LE_Var _ | LE_Ignore -> true | _ -> false

  (** [write_identifier env x m] is env' such that x -> v in env',
      with v being the value in m. *)
  let write_identifier (env : env) x m =
    let* v = m in
    let* () = B.on_write_identifier x env.local.scope v in
    IEnv.add_local x v env |> return

  let write_global_identifier env x m =
    let* v = m in
    let* () = B.on_write_identifier x Scope_Global v in
    IEnv.add_global x v env |> return

  let write_identifier_m env x m =
    B.bind_seq env (fun env -> write_identifier env x m)

  let is_defined env id =
    IMap.mem id env.global.storage || IMap.mem id env.local.storage

  (** [eval_expr env e] is the monadic evaluation  of [e] in [env]. *)
  let rec eval_expr (env : env) (e : expr) : B.value B.m =
    if false then Format.eprintf "@[<3>Eval@ @[%a@]@]@." PP.pp_expr e;
    match e.desc with
    | E_Literal v -> B.v_of_parsed_v v |> return |: Rule.Lit
    | E_Typed (e, _t) -> eval_expr env e |: Rule.IgnoreTypedExpr
    | E_Var x -> (
        match IMap.find_opt x env.global.storage with
        | Some v ->
            let* () = B.on_read_identifier x Scope_Global v in
            return v |: Rule.EGlobalVar
        | None -> (
            match IMap.find_opt x env.local.storage with
            | Some v ->
                let* () = B.on_read_identifier x env.local.scope v in
                return v |: Rule.ELocalVar
            | None -> fatal_from e @@ Error.UndefinedIdentifier x))
    | E_Binop (op, e1, e2) ->
        let* v1 = eval_expr env e1 and* v2 = eval_expr env e2 in
        B.binop op v1 v2 |: Rule.Binop
    | E_Unop (op, e) ->
        let* v = eval_expr env e in
        B.unop op v |: Rule.Unop
    | E_Cond (e1, e2, e3) ->
        B.bind_ctrl (eval_expr env e1) (fun v ->
            B.ternary v
              (fun () -> eval_expr env e2)
              (fun () -> eval_expr env e3))
        |: Rule.ECond
    | E_Slice (e', slices) ->
        let* positions = eval_slices env slices and* v = eval_expr env e' in
        B.read_from_bitvector positions v |: Rule.ESlice
    | E_Call (name, args, named_args) ->
        let vargs = List.map (eval_expr env) args
        and nargs =
          let one_narg (x, e) = (x, eval_expr env e) in
          List.map one_narg named_args
        in
        let*| returned = eval_func env.global name (to_pos e) vargs nargs in
        one_return_value e name returned |: Rule.ECall
    | E_Record (_, li, ta) ->
        let one_field (x, e) = eval_expr env e >=> pair x in
        let* fields = prod_map one_field li in
        make_record e (type_of_ta e ta) fields |: Rule.ERecord
    | E_GetField (e', x, ta) -> (
        let ty = type_of_ta e ta in
        match ty.desc with
        | T_Record li ->
            let i = record_index_of_field e x li ty in
            let* vec = eval_expr env e' in
            B.get_i i vec |: Rule.GetRecordField
        | _ -> fatal_from e @@ Error.BadField (x, ty))
    | E_GetFields (_, [], _) ->
        V_BitVector (Bitvector.of_string "") |> B.v_of_parsed_v |> return
    | E_GetFields (e', [ field ], ta) -> (
        let ty = type_of_ta e ta in
        match ty.desc with
        | T_Bits (_, fields) -> (
            match List.assoc_opt field fields with
            | Some slices ->
                E_Slice (e', slices)
                |> ASTUtils.add_pos_from e |> eval_expr env |: Rule.GetBitField
            | None -> fatal_from e @@ Error.BadField (field, ty))
        | _ -> fatal_from e @@ Error.BadField (field, ty))
    | E_GetFields (e', xs, ta) -> (
        let ty = type_of_ta e ta in
        match ty.desc with
        | T_Bits (_, fields) ->
            let one (x : string) =
              match List.assoc_opt x fields with
              | None -> fatal_from e @@ Error.BadField (x, ty)
              | Some slices ->
                  E_Slice (e', slices)
                  |> ASTUtils.add_pos_from e |> eval_expr env
            in
            prod_map one xs >>= B.concat_bitvectors |: Rule.GetBitFields
        | _ -> fatal_from e @@ Error.BadField (List.hd xs, ty))
    | E_Concat es ->
        prod_map (eval_expr env) es >>= B.concat_bitvectors |: Rule.EConcat
    | E_Tuple _ -> fatal_from e @@ Error.NotYetImplemented "tuple construction"
    | E_Unknown ty -> base_value_of_type env ty
    | E_Pattern (e, p) ->
        let* v = eval_expr env e in
        eval_pattern env e v p

  (** [eval_slices env slices] is the list of pair [(i_n, l_n)] that
      corresponds to the start (included) and the length of each slice in
      [slices]. *)
  and eval_slices env =
    let one = B.v_of_int 1 in
    let eval_one = function
      | Slice_Single e -> eval_expr env e >=> Fun.flip pair one
      | Slice_Range (etop, ebot) ->
          let* vtop = eval_expr env etop and* vbot = eval_expr env ebot in
          let* length =
            B.binop MINUS vtop vbot >>= B.binop PLUS (B.v_of_int 1)
          in
          return (vbot, length)
      | Slice_Length (ebot, elength) ->
          eval_expr env ebot ||| eval_expr env elength
    in
    prod_map eval_one

  (** [eval_pattern env pos v p] determines if [v] matches the pattern [p]. *)
  and eval_pattern env pos v = function
    | Pattern_All -> B.v_of_parsed_v (V_Bool true) |> return
    | Pattern_Any li ->
        let folder acc p =
          let* acc = acc and* b = eval_pattern env pos v p in
          B.binop BOR acc b
        in
        let init = B.v_of_parsed_v (V_Bool false) |> return in
        List.fold_left folder init li
    | Pattern_Geq e -> eval_expr env e >>= B.binop GEQ v
    | Pattern_Leq e -> eval_expr env e >>= B.binop LEQ v
    | Pattern_Mask _ ->
        fatal_from pos @@ Error.NotYetImplemented "Bitvector masks"
    | Pattern_Not p -> eval_pattern env pos v p >>= B.unop BNOT
    | Pattern_Range (e1, e2) ->
        let* b1 = eval_expr env e1 >>= B.binop GEQ v
        and* b2 = eval_expr env e2 >>= B.binop LEQ v in
        B.binop BAND b1 b2
    | Pattern_Single e -> eval_expr env e >>= B.binop EQ_OP v

  (** [base_value_of_type env ty] is the base value of the type [ty] in [env].
  *)
  and base_value_of_type env ty : B.value m =
    let return_lit v = B.v_of_parsed_v v |> return in
    let of_constraints _li = return 0 in
    match ty.desc with
    | T_Int None -> V_Int 0 |> return_lit
    | T_Int (Some cs) ->
        let* n = of_constraints cs in
        V_Int n |> return_lit
    | T_Bool -> V_Bool true |> return_lit
    | T_Bits (cs, _) ->
        let of_v = function
          | Some i -> i
          | None ->
              let e = E_Unknown ty |> ASTUtils.add_pos_from ty in
              fatal_from ty @@ Error.UnsupportedExpr e
        in
        let* n =
          match cs with
          | BitWidth_Constrained cs -> of_constraints cs
          | BitWidth_ConstrainedFormType t ->
              base_value_of_type env t >=> B.v_to_int >=> of_v
          | BitWidth_Determined e -> eval_expr env e >=> B.v_to_int >=> of_v
        in
        V_BitVector (Bitvector.zeros n) |> return_lit
    | T_Array (e, t) -> (
        let* n = eval_expr env e in
        match B.v_to_int n with
        | Some i ->
            let* v = base_value_of_type env t in
            B.create_vector ty (List.init i (Fun.const v))
        | None -> fatal_from ty @@ Error.UnsupportedExpr e)
    | T_Enum li ->
        IMap.find (List.hd li) env.global.static.Env.Static.constants_values
        |> B.v_of_parsed_v |> return
    | T_Named _ -> fatal_from ty @@ Error.TypeInferenceNeeded
    | T_Tuple li -> prod_map (base_value_of_type env) li >>= B.create_vector ty
    | T_Record li ->
        let one_field (_, t) = base_value_of_type env t in
        prod_map one_field li >>= B.create_vector ty
    | T_Exception li ->
        let one_field (_, t) = base_value_of_type env t in
        prod_map one_field li >>= B.create_vector ty
    | _ -> assert false

  (** [eval_lexpr env le m] is [env[le --> m]]. *)
  and eval_lexpr (env : env) le : B.value B.m -> env B.m =
    match le.desc with
    | LE_Ignore -> fun _ -> return env |: Rule.LEIgnore
    | LE_Var x ->
        if IMap.mem x env.global.storage then
          write_global_identifier env x >|: Rule.LEGlobalVar
        else write_identifier env x >|: Rule.LELocalVar
    | LE_Slice (le', slices) ->
        let setter = eval_lexpr env le' in
        fun m ->
          let* v = m
          and* positions = eval_slices env slices
          and* bv = ASTUtils.expr_of_lexpr le' |> eval_expr env in
          B.write_to_bitvector positions v bv |> setter |: Rule.LESlice
    | LE_SetField (le', x, ta) -> (
        let ty = type_of_ta le ta in
        match ty.desc with
        | T_Record li ->
            let setter = eval_lexpr env le' in
            let i = record_index_of_field le x li ty in
            fun m ->
              let* new_v = m
              and* vec = ASTUtils.expr_of_lexpr le' |> eval_expr env in
              B.set_i i new_v vec |> setter |: Rule.LESetRecordField
        | T_Bits _ ->
            LE_SetFields (le', [ x ], ta)
            |> ASTUtils.add_pos_from le |> eval_lexpr env |: Rule.LESetBitField
        | _ -> fatal_from le @@ Error.BadField (x, ty))
    | LE_SetFields (le', xs, ta) -> (
        let ty = type_of_ta le ta in
        match ty.desc with
        | T_Bits (_, fields) ->
            let folder prev_slices x =
              match List.assoc_opt x fields with
              | Some slices -> List.rev_append slices prev_slices
              | None -> fatal_from le @@ Error.BadField (x, ty)
            in
            let slices = List.fold_left folder [] xs |> List.rev in
            LE_Slice (le', slices)
            |> ASTUtils.add_pos_from le |> eval_lexpr env |: Rule.LESetBitFields
        | _ ->
            fatal_from le
            @@ Error.ConflictingTypes ([ ASTUtils.default_t_bits ], ty))
    | LE_TupleUnpack les ->
        (* The index-out-of-bound on the vector are done either in typing,
           either in [B.get_i]. *)
        let n = List.length les in
        fun m ->
          let nmonads = List.init n (fun i -> m >>= B.get_i i) in
          multi_assign env les nmonads

  (** [multi_assign env [le_1; ... ; le_n] [m_1; ... ; m_n]] is
      [env[le_1 --> m_1] ... [le_n --> m_n]]. *)
  and multi_assign env les monads =
    let folder envm le vm =
      let*| env = envm in
      eval_lexpr env le vm
    in
    List.fold_left2 folder (return env) les monads

  (** As [multi_assign], but checks that [les] and [monads] have the same
      length. *)
  and protected_multi_assign env pos les monads =
    if List.compare_lengths les monads != 0 then
      fatal_from pos
      @@ Error.BadArity
           ("tuple construction", List.length les, List.length monads)
    else multi_assign env les monads

  (** [eval_stmt env s] evaluates [s] in [env]. This is either an interuption
      [Returning vs] or a continuation [env], see [eval_res]. *)
  and eval_stmt (env : env) s =
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
        let vargs = List.map (eval_expr env) args
        and nargs = List.map (fun (x, e) -> (x, eval_expr env e)) named_args in
        eval_func env.global name (to_pos s) vargs nargs
        >>= protected_multi_assign env s les
        >>=| continue
    | S_Assign ({ desc = LE_TupleUnpack les; _ }, { desc = E_Tuple exprs; _ })
      when List.for_all lexpr_is_var les ->
        List.map (eval_expr env) exprs
        |> protected_multi_assign env s les
        >>=| continue
    | S_Assign (le, e) ->
        eval_expr env e |> eval_lexpr env le >>= continue |: Rule.Assign
    | S_Return (Some { desc = E_Tuple es; _ }) ->
        let ms = List.map (eval_expr env) es in
        return (Returning ms)
    | S_Return (Some e) ->
        let m = eval_expr env e in
        return (Returning [ m ]) |: Rule.ReturnOne
    | S_Return None -> return (Returning []) |: Rule.ReturnNone
    | S_Then (s1, s2) -> eval_seq env s1 s2 |: Rule.Then
    | S_Call (name, args, named_args) ->
        let vargs = List.map (eval_expr env) args
        and nargs = List.map (fun (x, e) -> (x, eval_expr env e)) named_args in
        let*| _ = eval_func env.global name (to_pos s) vargs nargs in
        continue env |: Rule.SCall
    | S_Cond (e, s1, s2) ->
        B.bind_ctrl
          (B.choice (eval_expr env e) (return s1) (return s2))
          (eval_stmt env)
        |: Rule.SCond
    | S_Case _ -> ASTUtils.case_to_conds s |> eval_stmt env
    | S_Assert e ->
        let v = eval_expr env e in
        B.bind_ctrl (B.choice v (return true) (return false)) @@ fun b ->
        if b then continue env
        else fatal_from e @@ Error.AssertionFailed e |: Rule.Assert
    | S_While (e, body) ->
        let env = IEnv.tick_push env in
        eval_loop true env e body
    | S_Repeat (body, e) ->
        eval_seq_kont env body (fun env ->
            let env = IEnv.tick_push_bis env in
            eval_loop false env e body)
    | S_For (id, e1, dir, e2, s) ->
        let* v1 = eval_expr env e1 and* v2 = eval_expr env e2 in
        (* It is an error to redefine an identifier *)
        assert (not (is_defined env id));
        (* By typing *)
        let undet = B.is_undetermined v1 || B.is_undetermined v2 in
        let* env = write_identifier env id (B.return v1) in
        let env = if undet then IEnv.tick_push_bis env else env in
        B.bind_seq (eval_for undet env id v1 dir v2 s) (fun r ->
            match r with
            | Returning _ -> return r
            | Continuing env ->
                (if undet then IEnv.tick_pop env else env)
                |> IEnv.remove_local id (* Destroy `id` binding *) |> continue)
    | S_Decl (_dlk, _dli, _e_opt) ->
        (* Type checking should change those into S_Assign. *)
        fatal_from s Error.TypeInferenceNeeded

  and eval_loop is_while env e s =
    B.delay
      (eval_expr env e >>= if is_while then return else B.unop BNOT)
      (fun b mb ->
        let stop, env =
          if B.is_undetermined b then IEnv.tick_decr env else (false, env)
        in
        if stop then
          B.bind_ctrl mb (fun _ ->
              B.warnT "Loop unrolling reached limit" (Continuing env))
        else
          B.bind_ctrl
            (B.choice mb
               (return (fun env ->
                    eval_seq_kont env s (fun env -> eval_loop is_while env e s)))
               (return continue))
            (fun f -> f env))

  and eval_for undet (env : env) id v dir v2 s =
    B.bind_ctrl
      (B.choice
         (let* () = B.on_read_identifier id env.local.scope v in
          let op = match dir with Up -> LT | Down -> GT in
          B.binop op v2 v)
         (return continue)
         (return (fun env ->
              (if undet then eval_unroll else eval_seq_kont) env s (fun env ->
                  let* v =
                    let* () = B.on_read_identifier id env.local.scope v in
                    let op = match dir with Up -> PLUS | Down -> MINUS in
                    B.binop op v (B.v_of_int 1)
                  in
                  let* env = write_identifier env id (return v) in
                  eval_for undet env id v dir v2 s))))
      (fun k -> k env)

  and eval_unroll env s k =
    let stop, env' = IEnv.tick_decr env in
    if stop then B.warnT "For loop unrolling reached limit" (Continuing env)
    else eval_seq_kont env' s k

  and eval_seq env (s1 : stmt) (s2 : stmt) =
    eval_seq_kont env s1 (fun env -> eval_stmt env s2)

  and eval_seq_kont env s1 k2 =
    B.bind_seq (eval_stmt env s1) (fun r1 ->
        match r1 with
        | Continuing env -> k2 env
        | Returning vs -> return (Returning vs))

  (** [eval_func genv name pos args nargs] evaluate the function named [name]
      in the global environment [genv], with [args] the formal arguments, and
      [nargs] the arguments deduced by type equality. *)
  and eval_func (genv : global) name pos (args : B.value m list) nargs :
      B.value m list m =
    match IMap.find_opt name genv.funcs with
    | None -> fatal_from pos @@ Error.UndefinedIdentifier name
    | Some (Primitive { body; _ }) -> body args
    | Some (Func (_, { args = arg_decls; _ }))
      when List.compare_lengths args arg_decls <> 0 ->
        fatal_from pos
        @@ Error.BadArity (name, List.length arg_decls, List.length args)
    | Some (Func (r, { args = arg_decls; body; _ })) -> (
        let scope = Scope_Local (name, !r) in
        let () = r := !r + 1 in
        let env = { global = genv; local = IEnv.empty_scoped scope } in
        let one_arg envm (x, _) m = write_identifier_m envm x m in
        let envm = List.fold_left2 one_arg (return env) arg_decls args in
        let one_narg envm (x, m) =
          let*| env = envm in
          if is_defined env x then return env else write_identifier env x m
        in
        let*| env = List.fold_left one_narg envm nargs in
        let*| res = eval_stmt env body in
        let () =
          if false then Format.eprintf "Finished evaluating %s.@." name
        in
        match res with Continuing _ -> return [] | Returning vs -> return vs)

  (** Main entry point for the Interpreter, [run ast primitives] type-annotate
      [ast], build a global environment and then evaluate the "main" function
      in it. *)
  let run (ast : t) primitives : unit m =
    let ast = List.rev_append (Lazy.force Builder.stdlib) ast in
    let ast, static_env = type_annotation ast primitives in
    let () =
      if false then Format.eprintf "@[<v 2>Typed AST:@ %a@]@." PP.pp_t ast
    in
    let*| genv = build_genv eval_expr static_env ast primitives in
    let*| _ = eval_func genv "main" ASTUtils.dummy_annotated [] [] in
    return ()
end
