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

let fatal_from = Error.fatal_from
let to_pos = ASTUtils.to_pos

module type S = sig
  module B : Backend.S

  type body = B.value list -> B.value list B.m
  type primitive = (body, ty) func_skeleton

  val run : t -> primitive list -> B.value list B.m
end

module Make (B : Backend.S) = struct
  module B = B
  module IMap = ASTUtils.IMap
  module ISet = ASTUtils.ISet

  type 'a m = 'a B.m
  type body = B.value list -> B.value list m
  type primitive = (body, ty) func_skeleton

  let ( let* ) = B.bind_data
  let ( and* ) = B.prod
  let return = B.return

  let prod_map f =
    let one acc elt =
      let* v = f elt and* li = acc in
      return (v :: li)
    in
    fun li ->
      let* li = List.fold_left one (return []) li in
      return (List.rev li)

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
  (*                       Global constants environment                        *)
  (*                                                                           *)
  (*****************************************************************************)

  type func = Func of int ref * AST.func | Primitive of primitive
  type genv = { consts : value IMap.t; funcs : func IMap.t }
  type lenv = B.value IMap.t
  type env = genv * lenv

  let add_primitives primitives funcs =
    let one_primitive primitive = (primitive.name, Primitive primitive) in
    primitives |> List.to_seq |> Seq.map one_primitive
    |> Fun.flip IMap.add_seq funcs

  (*****************************************************************************)
  (*                                                                           *)
  (*                         Type annotations handling                         *)
  (*                                                                           *)
  (*****************************************************************************)

  let type_of_ta pos = function
    | TA_None -> fatal_from pos Error.TypeInferenceNeeded
    | TA_InferredStructure ty -> ty

  let type_annotation ast sfuncs =
    let add_fake_primitives =
      let fake_funcs =
        let one_sfunc { name; args; return_type; _ } =
          let one_arg i ty = ("arg" ^ string_of_int i, ty) in
          let args = List.mapi one_arg args in
          D_Func { name; args; body = ASTUtils.s_pass; return_type }
        in
        List.map one_sfunc sfuncs
      in
      List.rev_append fake_funcs
    in
    let remove_fake_primitives =
      let primitive_names =
        let one_sfunc { name; _ } = name in
        sfuncs |> List.to_seq |> Seq.map one_sfunc |> ASTUtils.ISet.of_seq
      in
      let is_primitive = function
        | D_Func AST.{ name; _ } -> not (ASTUtils.ISet.mem name primitive_names)
        | _ -> true
      in
      List.filter is_primitive
    in
    ast |> add_fake_primitives |> Typing.annotate_ast |> remove_fake_primitives

  (*****************************************************************************)
  (*                                                                           *)
  (*                      Construction of the initial env                      *)
  (*                                                                           *)
  (*****************************************************************************)

  let build_enums (ast : t) globals =
    let build_one (counter, globals) name =
      let globals = IMap.add name (V_Int counter) globals in
      (counter + 1, globals)
    in
    let build_decl acc = function
      | D_TypeDecl (_name, { desc = T_Enum ids; _ }) ->
          List.fold_left build_one acc ids
      | _ -> acc
    in
    let _, genv = List.fold_left build_decl (0, globals) ast in
    genv

  type build_status = NotYetEvaluated of expr | AlreadyEvaluated of value

  (* build every constant and make an global env *)
  let build_consts (ast : t) globals =
    (* In the following, acc is the current status of evaluation, i.e. it maps
       every global variable to either its build_status, that is its value if
       it has been evaluated, or its expression otherwise. This is why we have
       to use it every time we could use a variable. *)
    let acc =
      let one_decl = function
        | D_GlobalConst (name, _ty, e) -> Some (name, NotYetEvaluated e)
        | _ -> None
      in
      let add_decls =
        ast |> List.to_seq |> Seq.filter_map one_decl |> IMap.add_seq
      in
      let one_glob v = AlreadyEvaluated v in
      globals |> IMap.map one_glob |> add_decls |> ref
    in

    let rec env_lookup pos name =
      match IMap.find_opt name !acc with
      | Some (AlreadyEvaluated v) -> v
      | Some (NotYetEvaluated e) ->
          let v = eval_expr e in
          acc := IMap.add name (AlreadyEvaluated v) !acc;
          v
      | None -> fatal_from pos @@ Error.UndefinedIdentifier name
    and eval_expr e = StaticInterpreter.static_eval (env_lookup e) e in

    let one_decl = function
      | D_GlobalConst (name, _, pos) -> Some (name, env_lookup pos name)
      | _ -> None
    in
    ast |> List.to_seq |> Seq.filter_map one_decl |> IMap.of_seq

  let build_funcs ast funcs =
    List.to_seq ast
    |> Seq.filter_map (function
         | D_Func func -> Some (func.name, Func (ref 0, func))
         | _ -> None)
    |> Fun.flip IMap.add_seq funcs

  (*****************************************************************************)
  (*                                                                           *)
  (*                       Main interpretation functions                       *)
  (*                                                                           *)
  (*****************************************************************************)

  type eval_res = Returning of B.value list | Continuing of lenv

  let continue ((_genv, lenv) : env) = return (Continuing lenv)

  let one_return_value pos name = function
    | [ v ] -> return v
    | _ -> fatal_from pos @@ Error.MismatchedReturnValue name

  let lexpr_is_var le =
    match le.desc with LE_Var _ | LE_Ignore -> true | _ -> false

  let rec eval_expr (env : env) scope is_data e =
    let genv, lenv = env in
    match e.desc with
    | E_Literal v -> B.v_of_parsed_v v |> return
    | E_Var x -> (
        match IMap.find_opt x genv.consts with
        | Some v -> B.v_of_parsed_v v |> return
        | None -> (
            match IMap.find_opt x lenv with
            | Some v ->
                let* () = B.on_read_identifier x scope v in
                return v
            | None -> fatal_from e @@ Error.UndefinedIdentifier x))
    | E_Binop (op, e1, e2) ->
        let* v1 = eval_expr env scope is_data e1
        and* v2 = eval_expr env scope is_data e2 in
        B.binop op v1 v2
    | E_Unop (op, e) ->
        let* v = eval_expr env scope is_data e in
        B.unop op v
    | E_Cond (e1, e2, e3) ->
        let eval_ = eval_expr env scope is_data in
        B.choice (eval_ e1) (eval_ e2) (eval_ e3)
    | E_Slice (e', slices) ->
        let positions = eval_slices env (to_pos e) slices in
        let* v = eval_expr env scope is_data e' in
        B.read_from_bitvector positions v
    | E_Call (name, args) ->
        let vargs = List.map (eval_expr env scope is_data) args in
        let* returned = eval_func genv name (to_pos e) vargs in
        one_return_value e name returned
    | E_Record (_, li, ta) ->
        let one_field (x, e) =
          let* v = eval_expr env scope is_data e in
          return (x, v)
        in
        let* fields = prod_map one_field li in
        make_record e (type_of_ta e ta) fields
    | E_GetField (e', x, ta) -> (
        let ty = type_of_ta e ta in
        match ty.desc with
        | T_Record li ->
            let i = record_index_of_field e x li ty in
            let* vec = eval_expr env scope is_data e' in
            B.get_i i vec
        | T_Bits (_, Some fields) -> (
            match List.find_opt (fun (_, y) -> String.equal x y) fields with
            | Some (slices, _) ->
                E_Slice (e', slices)
                |> ASTUtils.add_pos_from e
                |> eval_expr env scope is_data
            | None -> fatal_from e @@ Error.BadField (x, ty))
        | _ -> fatal_from e @@ Error.BadField (x, ty))
    | E_Concat es ->
        let* values = prod_map (eval_expr env scope is_data) es in
        B.concat_bitvectors values
    | E_Tuple _ -> fatal_from e @@ Error.NotYetImplemented "tuple construction"

  and eval_slices (genv, _lenv) pos =
    let si_env s =
      match IMap.find_opt s genv.consts with
      | Some v -> v
      | None -> Error.fatal_from pos @@ Error.UndefinedIdentifier s
    in
    StaticInterpreter.slices_to_positions si_env

  and eval_lexpr (env : env) scope le =
    let genv, lenv = env in
    match le.desc with
    | LE_Ignore -> fun _ -> return env
    | LE_Var x ->
        fun v ->
          let* v = v in
          let* () = B.on_write_identifier x scope v in
          let lenv = IMap.add x v lenv in
          return (genv, lenv)
    | LE_Slice (le', slices) ->
        let setter = eval_lexpr env scope le' in
        let positions = eval_slices env (to_pos le) slices in
        fun m ->
          let* v = m
          and* bv =
            let e = ASTUtils.expr_of_lexpr le' in
            eval_expr env scope true e
          in
          B.write_to_bitvector positions v bv |> setter
    | LE_SetField (le', x, ta) -> (
        let ty = type_of_ta le ta in
        match ty.desc with
        | T_Record li ->
            let setter = eval_lexpr env scope le' in
            let i = record_index_of_field le x li ty in
            fun m ->
              let* new_v = m
              and* vec =
                let e = ASTUtils.expr_of_lexpr le' in
                eval_expr env scope true e
              in
              B.set_i i new_v vec |> setter
        | T_Bits (_, Some fields) -> (
            match List.find_opt (fun (_, y) -> String.equal x y) fields with
            | Some (slices, _) ->
                LE_Slice (le', slices)
                |> ASTUtils.add_pos_from le |> eval_lexpr env scope
            | None -> fatal_from le @@ Error.BadField (x, ty))
        | _ -> fatal_from le @@ Error.BadField (x, ty))
    | LE_TupleUnpack les ->
        fun v ->
          let* v = v in
          let mapper (i, le) =
            let setter = eval_lexpr env scope le in
            let w = B.get_i i v in
            setter w
          in
          let* envs = prod_map mapper (List.mapi (fun i le -> (i, le)) les) in
          let on_conflict _x v1 _v2 = Some v1 in
          (* TODO: handle union of genv *)
          let folder lenv (_genv, lenv2) = IMap.union on_conflict lenv lenv2 in
          let lenv = List.fold_left folder lenv envs in
          return (fst env, lenv)

  and multi_assign env scope pos les values =
    if List.compare_lengths les values != 0 then
      fatal_from pos
      @@ Error.BadArity
           ("tuple construction", List.length les, List.length values)
    else
      let mapper x v =
        let x =
          match x.desc with
          | LE_Var x -> x
          | LE_Ignore -> "-"
          | _ -> assert false
        in
        let* () = B.on_write_identifier x scope v in
        return (x, v)
      in
      let assignments = List.map2 mapper les values in
      let* assignments = prod_map Fun.id assignments in
      let add_to_lenv lenv (x, v) = IMap.add x v lenv in
      let lenv = List.fold_left add_to_lenv (snd env) assignments in
      continue (fst env, lenv)

  and eval_stmt (env : env) scope s =
    match s.desc with
    | S_Pass -> continue env
    | S_Assign
        ({ desc = LE_TupleUnpack les; _ }, { desc = E_Call (name, args); _ })
      when List.for_all lexpr_is_var les ->
        let vargs = List.map (eval_expr env scope true) args in
        let* returned_values = eval_func (fst env) name (to_pos s) vargs in
        multi_assign env scope s les returned_values
    | S_Assign ({ desc = LE_TupleUnpack les; _ }, { desc = E_Tuple exprs; _ })
      when List.for_all lexpr_is_var les ->
        let* values = prod_map (eval_expr env scope true) exprs in
        multi_assign env scope s les values
    | S_Assign (le, e) ->
        let v = eval_expr env scope true e
        and setter = eval_lexpr env scope le in
        let* env = setter v in
        continue env
    | S_Return (Some { desc = E_Tuple es; _ }) ->
        let* vs = prod_map (eval_expr env scope true) es in
        return (Returning vs)
    | S_Return (Some e) ->
        let* v = eval_expr env scope true e in
        return (Returning [ v ])
    | S_Return None -> return (Returning [])
    | S_Then (s1, s2) ->
        B.bind_seq (eval_stmt env scope s1) (fun r1 ->
            match r1 with
            | Continuing lenv -> eval_stmt (fst env, lenv) scope s2
            | Returning vs -> return (Returning vs))
    | S_Call (name, args) ->
        let vargs = List.map (eval_expr env scope true) args in
        let* _ = eval_func (fst env) name (to_pos s) vargs in
        continue env
    | S_Cond (e, s1, s2) ->
        let* s =
          B.choice (eval_expr env scope true e) (return s1) (return s2)
        in
        eval_stmt env scope s
    | S_Case _ -> ASTUtils.case_to_conds s |> eval_stmt env scope
    | S_Assert e ->
        let v = eval_expr env scope true e in
        let* b = B.choice v (return true) (return false) in
        if b then continue env else fatal_from e @@ Error.AssertionFailed e

  and eval_func (genv : genv) name pos (args : B.value m list) : B.value list m
      =
    match IMap.find_opt name genv.funcs with
    | None -> fatal_from pos @@ Error.UndefinedIdentifier name
    | Some (Primitive { body; _ }) ->
        let* args = prod_map Fun.id args in
        body args
    | Some (Func (_, { args = arg_decls; _ }))
      when List.compare_lengths args arg_decls <> 0 ->
        fatal_from pos
        @@ Error.BadArity (name, List.length arg_decls, List.length args)
    | Some (Func (r, { args = arg_decls; body; _ })) -> (
        let scope = (name, !r) in
        let () = r := !r + 1 in
        let one_arg (x, _type_desc) v =
          let* v = v in
          let* () = B.on_write_identifier x scope v in
          return (x, v)
        in
        let* bindings = List.map2 one_arg arg_decls args |> prod_map Fun.id in
        let lenv = List.to_seq bindings |> IMap.of_seq in
        let* res = eval_stmt (genv, lenv) scope body in
        match res with Continuing _ -> return [] | Returning vs -> return vs)

  let run (ast : t) primitives : B.value list m =
    let ast = type_annotation ast primitives in
    let funcs = IMap.empty |> build_funcs ast |> add_primitives primitives in
    let consts = IMap.empty |> build_enums ast |> build_consts ast in
    eval_func { consts; funcs } "main" ASTUtils.dummy_annotated []
end
