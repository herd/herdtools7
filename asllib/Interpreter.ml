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
let _warn = false
let _dbg = false

module type S = sig
  module B : Backend.S

  type 'a m = 'a B.m
  type body = B.value m list -> B.value m list m
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

  (* To use instrumentation *)
  let ( |: ) = C.Instr.use_with
  let ( >|: ) f r m = m >>= fun v -> f (return v) |: r

  (** [prod_map] is a monadic parallel version of List.map. For example:
      [prod_map f [i1; i2; i3]] is {[f i1 ||| f i2 ||| f i3]] *)
  let prod_map f =
    let one elt acc =
      let* v = f elt and* acc = acc in
      return (v :: acc) in
    fun xs -> List.fold_right one xs (return [])


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
    then
      B.create_vector ty values
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
      | D_GlobalStorage { initial_value; name; ty; } ->
          fun env_m ->
            let*| env = env_m in
            let e =
              match initial_value with
              | Some e -> e
              | None -> (
                  match ty with
                  | None -> assert false
                  | Some t ->
                      let senv =
                        Env.Static.
                          { empty with global = env.IEnv.global.static }
                      in
                      Types.base_value t senv t)
            in
            let* v = eval_expr env e in
            IEnv.def_global name v env |> return
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
    | Returning of B.value m list * env
        (** Control flow interruption: skip to the end of the function. *)
    | Continuing of env  (** Normal behaviour: pass to next statement. *)

  let continue (env : env) = return (Continuing env)

  let one_return_value pos name = function
    | [ m ] -> m
    | _ -> fatal_from pos @@ Error.MismatchedReturnValue name

  let lexpr_is_var le =
    match le.desc with LE_Var _ | LE_Ignore -> true | _ -> false

  (** [write_identifier assign env x m] is env' extending [env]
      by the binding [x] -> v, v being the value in m
      The [assign] argument is [Decl] or [Assign] to identify
      declarations and actual assigments, which are compiled
      differently. In the first case a slot is allocated, while
      a slot is updated un the second case. *)

  let write_identifier le assign (env : env) x m =
    let* v = m in
    match IEnv.assign_stm assign x v env with
    | Failure ->
       fatal_from le @@ Error.UndefinedIdentifier x
    | Local env ->
       let* () = B.on_write_identifier x env.local.scope v in
       return env
    | Global env ->
       let* () = B.on_write_identifier x Scope_Global v in
       return env

  (* Used for formal argument initialisation in subprogram calls
     and for the index variable of for loops.
     Always a local declaration and cannot fail *)

  let decl_identifier env x v =
    let* () = B.on_write_identifier x env.local.scope v in
    IEnv.decl_local x v env |> return

  let update_identifier env x m =
    let* v = m in
    let* () = B.on_write_identifier x env.local.scope v in
    IEnv.assign_local x v env |> return

  let decl_identifier_m env x m =
    B.bind_seq env
      (fun env ->
        let* v = m in
        let* () = B.on_write_identifier x env.local.scope v in
        IEnv.decl_local x v env |> return)

  let is_defined env id =
    IMap.mem id env.global.storage.env
    ||  IMap.mem id env.local.storage.env

  let ret_sef env v = return (v, env)
  let discard_env m = B.bind_data m (fun (v, _env) -> return v)

  let eval_bind eval_expr env (x,e) =
    let* v,env = eval_expr env e in
    return ((x,v),env)

  (* Left to right ordering... *)
  let rec eval_list_m eval_expr env es =
    match es with
    | [] -> return ([],env)
    | e::es ->
       B.delay
         (eval_expr env e)
         (fun (_,env) mv ->
           let mv = mv >>= fun (v,_) -> return v in
           let*| mvs,env = eval_list_m eval_expr env es in
           return (mv::mvs,env))

  (* Left to right ordering... *)
  let rec eval_bind_list_m eval_expr env xes =
    match xes with
    | [] -> return ([],env)
    | (x,e)::xes ->
       B.delay
         (eval_expr env e)
         (fun (_,env) mv ->
           let mv = mv >>= fun (v,_) -> return v in
           let*| mvs,env = eval_bind_list_m eval_expr env xes in
           return ((x,mv)::mvs,env))

  (* Now parallel composition *)
  let do_par env f1 x1 f2 x2 =
    B.delay (f1 env x1)
      (fun (_,env) m1 ->
        B.delay (f2 env x2)
          (fun (_,env) m2 ->
            let* v1,_ = m1 and* v2,_ = m2 in
            return ((v1,v2),env)))

  let eval_par eval_expr env e1 e2 = do_par env eval_expr e1 eval_expr e2

  let rec eval_par_list f env = function
    | [] -> return ([],env)
    | x::xs ->
       let* (y,ys),env = do_par env f x (eval_par_list f) xs in
       return ((y::ys),env)

  (** [eval_expr env e] is the monadic evaluation  of [e] in [env]. *)
  let rec eval_expr (env : env) (e : expr) : (B.value * env) B.m =
    if false then Format.eprintf "@[<3>Eval@ @[%a@]@]@." PP.pp_expr e;
    match e.desc with
    | E_Literal v -> B.v_of_parsed_v v |> ret_sef env |: Rule.Lit
    | E_Typed (e, _t) -> eval_expr env e |: Rule.IgnoreTypedExpr
    | E_Var x ->
       begin
         let open IEnv in
         match find x env with
         | Local v ->
            let* () = B.on_read_identifier x env.local.scope v in
            return (v, env) |: Rule.ELocalVar
         | Global v ->
            let* () = B.on_read_identifier x Scope_Global v in
            return (v, env) |: Rule.EGlobalVar
         | Failure ->
            fatal_from e @@ Error.UndefinedIdentifier x
       end
    | E_Binop (op, e1, e2) ->
       let* (v1,v2),env = eval_par eval_expr env e1 e2 in
       B.binop op v1 v2 >>= ret_sef env |: Rule.Binop
    | E_Unop (op, e) ->
        let* v, env = eval_expr env e in
        B.unop op v >>= ret_sef env |: Rule.Unop
    | E_Cond (e1, e2, e3) ->
(* Cannot use the ternary 'if' any more, because of env result.
   Could still use it for side-effect free expressions e2 and e3 *)       
       if
         let open ASTUtils in
         simple_expr e2 && simple_expr e3
       then
         B.bind_ctrl
           (eval_expr env e1)
           (fun (c,env) ->
               B.ternary c
                 (fun () -> eval_expr_sef env e2)
                 (fun () -> eval_expr_sef env e3)
               >>= ret_sef env)
         |: Rule.ECond
       else
         B.bind_ctrl
           (eval_expr env e1)
           (fun (c, env) ->
             B.choice (return c) (eval_expr env e2) (eval_expr env e3))
         |: Rule.ECond
    | E_Slice (e', slices) ->
       do_slice env e' slices |: Rule.ESlice
    | E_Call (name, args, named_args) ->
        let* (vargs,nargs),env = eval_args env args named_args in
        let* returned, global =
          eval_func env.global name (to_pos e) vargs nargs
        in
        let* v = one_return_value e name returned in
        return (v, { env with global }) |: Rule.ECall
    | E_Record (_, li, ta) ->
       let* fields,env =
         eval_par_list (eval_bind eval_expr) env li in
        let* v =
          make_record e (type_of_ta e ta) fields  |: Rule.ERecord in
        return (v,env)
    | E_GetField (e', x, ta) -> (
        let ty = type_of_ta e ta in
        match ty.desc with
        | T_Record li ->
            let i = record_index_of_field e x li ty in
            let* v, env = eval_expr env e' in
            let* v = B.get_i i v in
            return (v, env) |: Rule.GetRecordField
        | _ -> fatal_from e @@ Error.BadField (x, ty))
    | E_GetFields (_, [], _) ->
        V_BitVector (Bitvector.of_string "") |> B.v_of_parsed_v |> ret_sef env
    | E_GetFields (e', [ field ], ta) ->
       begin
         let ty = type_of_ta e ta in
         match ty.desc with
         | T_Bits (_, fields) -> (
           match List.assoc_opt field fields with
           | Some slices ->
              do_slice env e' slices |: Rule.GetBitField
           | None -> fatal_from e @@ Error.BadField (field, ty))
         | _ -> fatal_from e @@ Error.BadField (field, ty)
       end
    | E_GetFields (e', xs, ta) ->
       begin
         let ty = type_of_ta e ta in
         match ty.desc with
         | T_Bits (_, fields) ->
            B.delay
              (eval_expr env e')
              (fun (v,env) m ->
                let one (x : string) =
                  match List.assoc_opt x fields with
                  | None -> fatal_from e @@ Error.BadField (x, ty)
                  | Some slices ->
                     let* positions = eval_slices env slices in
                     B.read_from_bitvector positions v in
                let* bvs = prod_map one xs
                and* _ = m in
                B.concat_bitvectors bvs >>= ret_sef env)
         | _ -> fatal_from e @@ Error.BadField (List.hd xs, ty)
       end
    | E_Concat es ->
        let* vs,env  = eval_par_list eval_expr env es in
        let* v = B.concat_bitvectors vs |: Rule.EConcat in
        return (v, env)
    | E_Tuple _ -> fatal_from e @@ Error.NotYetImplemented "tuple construction"
    | E_Unknown t ->
        let v = B.v_unknown_of_type t in
        return (v, env)
    | E_Pattern (e, p) ->
       B.delay 
         (eval_expr env e)
         (fun (v,env) m ->
           let* _ = m and* b = eval_pattern env e v p in
           return (b,env))

  and do_slice env e slices =
    B.delay
      (eval_expr env e)          
      (fun (v,env) m ->
        let* _ = m and* positions = eval_slices env slices in
        B.read_from_bitvector positions v
        >>= ret_sef env)

  and eval_expr_sef env e = eval_expr env e |> discard_env

  and eval_args env args named_args =
      do_par env
        (eval_list_m eval_expr) args
        (eval_bind_list_m eval_expr) named_args

(** [eval_slices env slices] is the list of pair [(i_n, l_n)] that
      corresponds to the start (included) and the length of each slice in
      [slices]. *)
  and eval_slices env =
    let one = B.v_of_int 1 in
    let eval_one = function
      | Slice_Single e ->
         let* i = eval_expr_sef env e in
         return (i,one)
      | Slice_Range (etop, ebot) ->         
          let* vbot = eval_expr_sef env ebot
          and* vtop = eval_expr_sef env etop in
          let* length =  B.binop MINUS vtop vbot >>= B.binop PLUS one in
          return (vbot, length)
      | Slice_Length (ebot, elength) ->
          let* vbot = eval_expr_sef env ebot
          and* length = eval_expr_sef env elength in
          return (vbot, length) in
    fun slices -> prod_map eval_one slices

  (** [eval_pattern env pos v p] determines if [v] matches the pattern [p]. *)
  and eval_pattern env pos v = function
    | Pattern_All ->
       B.v_of_parsed_v (V_Bool true) |> return
    | Pattern_Any li ->
       let rec eval_pats env = function
         | [] ->
            B.v_of_parsed_v (V_Bool false) |> return
         | p::li ->
            let* b = eval_pattern env pos v p
            and* c = eval_pats env li in
            B.binop BOR b c in
       eval_pats env li
    | Pattern_Geq e ->
       eval_expr_sef env e >>= B.binop GEQ v
    | Pattern_Leq e ->
       eval_expr_sef env e >>= B.binop LEQ v
    | Pattern_Mask _ ->
        fatal_from pos @@ Error.NotYetImplemented "Bitvector masks"
    | Pattern_Not p ->
       eval_pattern env pos v p >>= B.unop BNOT
    | Pattern_Range (e1, e2) ->
       let* b1 = eval_expr_sef env e1 >>= B.binop GEQ v
       and* b2 = eval_expr_sef env e2 >>= B.binop LEQ v in
       B.binop BAND b1 b2
    | Pattern_Single e ->
       eval_expr_sef env e >>= B.binop EQ_OP v

  (** [eval_lexpr env le m] is [env[le --> m]]. *)
  and eval_lexpr assign (env:env) le : B.value B.m -> env B.m =
    match le.desc with
    | LE_Ignore ->
       fun _ -> return env |: Rule.LEIgnore
    | LE_Var x ->
       write_identifier le assign env x >|: Rule.LELocalVar
    | LE_Slice (le', slices) ->
       let setter env = eval_lexpr assign env le' in
       fun m ->
          let* v = m
          and* bv,env = ASTUtils.expr_of_lexpr le' |> eval_expr env
          and* positions = eval_slices env slices in
          B.write_to_bitvector positions v bv |> setter env |: Rule.LESlice
    | LE_SetField (le', x, ta) -> (
        let ty = type_of_ta le ta in
        match ty.desc with
        | T_Record li ->
            let setter env = eval_lexpr assign env le' in
            let i = record_index_of_field le x li ty in
            fun m ->
              let* new_v = m
              and* vec,env =
                ASTUtils.expr_of_lexpr le' |> eval_expr env in
              B.set_i i new_v vec |> setter env |: Rule.LESetRecordField
        | T_Bits _ ->
            LE_SetFields (le', [ x ], ta)
            |> ASTUtils.add_pos_from le
            |> eval_lexpr assign env |: Rule.LESetBitField
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
            |> ASTUtils.add_pos_from le
            |> eval_lexpr assign env |: Rule.LESetBitFields
        | _ ->
            fatal_from le
            @@ Error.ConflictingTypes ([ ASTUtils.default_t_bits ], ty))
    | LE_TupleUnpack les ->
        (* The index-out-of-bound on the vector are done either in typing,
           either in [B.get_i]. *)
        let n = List.length les in
        fun m ->
          let nmonads = List.init n (fun i -> m >>= B.get_i i) in
          multi_assign assign env les nmonads

  (** [multi_assign env [le_1; ... ; le_n] [m_1; ... ; m_n]] is
      [env[le_1 --> m_1] ... [le_n --> m_n]]. *)
  and multi_assign assign env les monads =
    let folder envm le vm =
      let*| env = envm in
      eval_lexpr assign env le vm
    in
    List.fold_left2 folder (return env) les monads

  (** As [multi_assign], but checks that [les] and [monads] have the same
      length. *)
  and protected_multi_assign assign env pos les monads =
    if List.compare_lengths les monads != 0 then
      fatal_from pos
      @@ Error.BadArity
           ("tuple construction", List.length les, List.length monads)
    else multi_assign assign env les monads

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
        ( assign,
          { desc = LE_TupleUnpack les; _ },
          { desc = E_Call (name, args, named_args); _ } )
      when List.for_all lexpr_is_var les ->
        let* (vargs,nargs),env = eval_args env args named_args in
        let*| env =
          let* ms, global =
            eval_func env.global name (to_pos s) vargs nargs in
          let env = { env with global } in
          protected_multi_assign assign env s les ms
        in
        continue env
    | S_Assign
        (assign,
         { desc = LE_TupleUnpack les; _ },
         { desc = E_Tuple exprs; _ })
         when List.for_all lexpr_is_var les ->
       let* vs,env = eval_list_m eval_expr env exprs in
       protected_multi_assign assign env s les vs
        >>=| continue
    | S_Assign (assign, le, e) ->
       B.delay (eval_expr env e)
         (fun (_,env) mv ->
           let mv = mv >>= fun (v,_) -> return v in
           eval_lexpr assign env le mv)
       >>= continue |: Rule.Assign
    | S_Return (Some { desc = E_Tuple es; _ }) ->
        (* TODO: rework to allow side-effects there. *)
        let* ms,env = eval_list_m eval_expr env es in
        return (Returning (ms, env))
    | S_Return (Some e) ->
        let* m, env =
          B.delay (eval_expr env e)
            (fun (_,env) m ->
              let mv = m >>= fun (v,_) -> return v in
              return (mv,env)) in
        return (Returning ([ m ], env)) |: Rule.ReturnOne
    | S_Return None -> return (Returning ([], env)) |: Rule.ReturnNone
    | S_Then (s1, s2) ->
       eval_seq_kont
         (eval_stmt env s1)
         (fun env -> eval_stmt env s2) |: Rule.Then
    | S_Call (name, args, named_args) ->
        let* (vargs,nargs),env = eval_args env args named_args in
        let* returned, g =
          eval_func env.global name (to_pos s) vargs nargs in
        let () = assert (returned = []) in
        continue { env with global = g } |: Rule.SCall
    | S_Cond (e, s1, s2) ->
        B.bind_ctrl
          (let* c,env = eval_expr env e in
           B.choice
             (return c)
             (ret_sef env s1) (ret_sef env s2))
          (fun (stm,env) -> eval_block env stm)
        |: Rule.SCond
    | S_Case _ -> ASTUtils.case_to_conds s |> eval_stmt env
    | S_Assert e ->
        let* v, env = eval_expr env e in
        B.bind_ctrl (B.choice (return v) (return true) (return false))
        @@ fun b ->
        if b then continue env
        else fatal_from e @@ Error.AssertionFailed e |: Rule.Assert
    | S_While (e, body) ->
        let env = IEnv.tick_push env in
        eval_loop true env e body
    | S_Repeat (body, e) ->
        eval_block_kont env body
          (fun env ->
            let env = IEnv.tick_push_bis env in
            eval_loop false env e body)
    | S_For (id, e1, dir, e2, s) ->
         let* v1,env = eval_expr env e1 in
         let* v2,env = eval_expr env e2 in
        (* It is an error to redefine an identifier *)
        assert (not (is_defined env id));
        (* By typing *)
        let undet = B.is_undetermined v1 || B.is_undetermined v2 in
        let* env = decl_identifier env id v1 in
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

  and eval_block env stm =
    eval_stmt (IEnv.push_local env) stm  >>=|
      (function
       | Returning _ as r -> return r
       | Continuing kenv ->
          IEnv.pop_local env kenv |> continue)

  and eval_loop is_while env e s =
    let* cond, env = eval_expr env e in
    B.delay
      (if is_while then return cond else B.unop BNOT cond)
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
                    eval_block_kont env s
                      (fun env -> eval_loop is_while env e s)))
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
              (if undet then eval_unroll else eval_block_kont) env s (fun env ->
                  let* v =
                    let* () = B.on_read_identifier id env.local.scope v in
                    let op = match dir with Up -> PLUS | Down -> MINUS in
                    B.binop op v (B.v_of_int 1)
                  in
                  let* env = update_identifier env id (return v) in
                  eval_for undet env id v dir v2 s))))
      (fun k -> k env)

  and eval_unroll env s k =
    let stop, env' = IEnv.tick_decr env in
    if stop then B.warnT "For loop unrolling reached limit" (Continuing env)
    else eval_block_kont env' s k

  and eval_seq_kont m1 k2 =
    B.bind_seq m1
      (fun r1 ->
        match r1 with
        | Continuing env -> k2 env
        | Returning _ -> return r1)

  and eval_block_kont env s1 k2 =
    eval_seq_kont (eval_block env s1) k2

  (** [eval_func genv name pos args nargs] evaluate the function named [name]
      in the global environment [genv], with [args] the formal arguments, and
      [nargs] the arguments deduced by type equality. *)
  and eval_func (genv : global) name pos (args : B.value B.m list) nargs :
      (B.value m list * global) m =
    match IMap.find_opt name genv.funcs with
    | None -> fatal_from pos @@ Error.UndefinedIdentifier name
    | Some (Primitive { body; _ }) ->
       let* ms = body args in
       return (ms,genv)
    | Some (Func (_, { args = arg_decls; _ }))
      when List.compare_lengths args arg_decls <> 0 ->
        fatal_from pos
        @@ Error.BadArity (name, List.length arg_decls, List.length args)
    | Some (Func (r, { args = arg_decls; body; _ })) -> (
        let scope = Scope_Local (name, !r) in
        let () = r := !r + 1 in
        let env = return { global = genv; local = IEnv.empty_scoped scope } in
        let one_arg env (x, _) m = decl_identifier_m env x m in
        let env = List.fold_left2 one_arg env arg_decls args in
        let one_narg menv (x, m) =
          B.delay menv
            (fun env menv ->
              (* It may be that args define x *)
              if is_defined env x then menv
              else decl_identifier_m menv x m)
        in
        let*| env = List.fold_left one_narg env nargs in
        let*| res = eval_stmt env body in
        let () =
          if false then Format.eprintf "Finished evaluating %s.@." name
        in
        match res with
        | Continuing env -> return ([], env.global)
        | Returning (xs, env) -> return (xs, env.global))

  (** Main entry point for the Interpreter, [run ast primitives] type-annotate
      [ast], build a global environment and then evaluate the "main" function
      in it. *)
  let run (ast : t) primitives : unit m =
    let ast = List.rev_append (Lazy.force Builder.stdlib) ast in
    let ast, static_env = type_annotation ast primitives in
    let () =
      if false then Format.eprintf "@[<v 2>Typed AST:@ %a@]@." PP.pp_t ast
    in
    let*| env =
      build_genv
        (fun env e -> eval_expr env e |> discard_env)
        static_env ast primitives in
    let*| returned, _genv =
      eval_func env "main" ASTUtils.dummy_annotated [] []
    in
    assert (returned = []);
    return ()
end
