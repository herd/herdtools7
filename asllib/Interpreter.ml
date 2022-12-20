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

module type S = sig
  module B : Backend.S

  type ast = B.value AST.t
  type sfunc = B.value list -> B.value list B.m

  val run : ast -> (string * sfunc) list -> B.value list -> B.value list B.m
end

module Make (B : Backend.S) = struct
  module B = B
  open B
  module IMap = ASTUtils.IMap
  module ISet = ASTUtils.ISet

  type sfunc = value list -> value list m
  type ast = value AST.t

  let ( let* ) = B.bind_data
  let ( and* ) = B.prod

  let prod_map f =
    let one acc elt =
      let* v = f elt and* li = acc in
      return (v :: li)
    in
    fun li ->
      let* li = List.fold_left one (return []) li in
      return (List.rev li)

  (*****************************************************************************)
  (*                                                                           *)
  (*                       Global constants environment                        *)
  (*                                                                           *)
  (*****************************************************************************)

  module GEnv = struct
    include ASTUtils.IMap

    type elt =
      | Value of value
      | Func of int ref * value AST.func
      | SpecialFunc of sfunc

    type t = elt IMap.t

    let add_value name v = add name (Value v)
    let add_seq_value s = add_seq (Seq.map (fun (x, v) -> (x, Value v)) s)

    let add_seq_func s =
      add_seq (Seq.map (fun (x, f) -> (x, Func (ref 0, f))) s)

    let add_seq_special_func s =
      add_seq (Seq.map (fun (x, f) -> (x, SpecialFunc f)) s)

    let find_opt_value name env =
      match find_opt name env with Some (Value v) -> Some v | _ -> None
  end

  module LEnv = IMap

  type genv = GEnv.t
  type lenv = value LEnv.t
  type env = genv * lenv

  (*****************************************************************************)
  (*                                                                           *)
  (*                      Construction of the initial env                      *)
  (*                                                                           *)
  (*****************************************************************************)

  let build_enums (ast : ast) : genv =
    let build_one (counter, genv) name =
      let genv = GEnv.add_value name (v_of_int counter) genv in
      (counter + 1, genv)
    in
    let build_decl acc = function
      | AST.D_TypeDecl (_name, AST.T_Enum ids) ->
          List.fold_left build_one acc ids
      | _ -> acc
    in
    let _, genv = List.fold_left build_decl (0, IMap.empty) ast in
    genv

  type build_status =
    | NotYetEvaluated of value AST.expr
    | AlreadyEvaluated of value

  (* build every constant and make an global env *)
  let build_consts (ast : ast) genv : genv m =
    let rec eval_one acc name =
      match GEnv.find_opt_value name genv with
      | Some v -> return (v, acc)
      | None -> (
          match IMap.find_opt name acc with
          | Some (AlreadyEvaluated v) -> return (v, acc)
          | Some (NotYetEvaluated e) ->
              let* v, acc = eval_expr acc e in
              return (v, IMap.add name (AlreadyEvaluated v) acc)
          | _ -> fatal ("Unknown constant " ^ name))
    and eval_expr acc e =
      let open AST in
      match e with
      | E_Literal v -> return (v, acc)
      | E_Var x -> eval_one acc x
      | E_Unop (op, e') ->
          let* v', acc = eval_expr acc e' in
          let* v = B.unop op v' in
          return (v, acc)
      | E_Binop (op, e1, e2) ->
          let* v1, acc = eval_expr acc e1 in
          let* v2, acc = eval_expr acc e2 in
          let* v = B.binop op v1 v2 in
          return (v, acc)
      | E_Cond (e1, e2, e3) ->
          let* v, acc = eval_expr acc e1 in
          choice (return v) (eval_expr acc e2) (eval_expr acc e3)
      | E_Get _ | E_Call _ ->
          fatal "Function calling in constants is not yet implemented"
    in
    let init_acc =
      let one_decl acc = function
        | AST.D_GlobalConst (name, e) -> IMap.add name (NotYetEvaluated e) acc
        | _ -> acc
      in
      List.fold_left one_decl IMap.empty ast
    in
    let eval_all acc =
      let one_decl acc = function
        | AST.D_GlobalConst (name, _e) ->
            let* acc = acc in
            let* _, acc = eval_one acc name in
            return acc
        | _ -> acc
      in
      List.fold_left one_decl acc ast
    in
    let collect acc =
      let* acc = acc in
      let acc_items = IMap.to_seq acc in
      let one_item = function
        | name, AlreadyEvaluated v -> (name, v)
        | _ -> assert false
      in
      let new_items = Seq.map one_item acc_items in
      let genv = GEnv.add_seq_value new_items genv in
      return genv
    in
    collect (eval_all (return init_acc))

  let build_funcs ast genv =
    List.to_seq ast
    |> Seq.filter_map (function
         | AST.D_Func func -> Some (func.AST.name, func)
         | _ -> None)
    |> fun s -> GEnv.add_seq_func s genv

  type eval_res = Returning of value list | Continuing of lenv

  let continue ((_genv, lenv) : env) = return (Continuing lenv)

  (*****************************************************************************)
  (*                                                                           *)
  (*                       Main interpretation functions                       *)
  (*                                                                           *)
  (*****************************************************************************)

  let one_return_value name = function
    | [ v ] -> return v
    | _ -> fatal ("Return arrity error for function " ^ name)

  let rec eval_expr (env : env) scope is_data =
    let genv, lenv = env in
    let open AST in
    function
    | E_Literal v -> return v
    | E_Var x -> (
        match GEnv.find_opt x genv with
        | Some (GEnv.Value v) -> return v
        | Some _ ->
            let* vl = eval_func (fst env) x [] in
            one_return_value x vl
        | None -> (
            match LEnv.find_opt x lenv with
            | Some v ->
                let* () = on_read_identifier x scope v in
                return v
            | None -> fatal ("Unknown identifier: " ^ x)))
    | E_Binop (op, e1, e2) ->
        let* v1 = eval_expr env scope is_data e1
        and* v2 = eval_expr env scope is_data e2 in
        binop op v1 v2
    | E_Unop (op, e) ->
        let* v = eval_expr env scope is_data e in
        unop op v
    | E_Cond (e1, e2, e3) ->
        let eval_ = eval_expr env scope is_data in
        choice (eval_ e1) (eval_ e2) (eval_ e3)
    | E_Get (name, args) | E_Call (name, args) ->
        let* vargs = prod_map (eval_expr env scope is_data) args in
        let* returned = eval_func (fst env) name vargs in
        one_return_value name returned

  and eval_lexpr (env : env) scope =
    let genv, lenv = env in
    let open AST in
    function
    | LEVar x -> (
        match GEnv.find_opt x genv with
        | Some (GEnv.Value _) ->
            fatal
              ("Global variables are not supported yet. Cannot assign to " ^ x)
        | Some _ ->
            return (fun v ->
                let* _ = eval_func genv x [ v ] in
                continue env)
        | None ->
            return (fun v ->
                let* () = on_write_identifier x scope v in
                let lenv = LEnv.add x v lenv in
                continue (genv, lenv)))
    | LESet (x, args) ->
        let* vargs = prod_map (eval_expr env scope false) args in
        return (fun v ->
            let* _ = eval_func genv x (vargs @ [ v ]) in
            continue env)

  and eval_stmt (env : env) scope =
    let open AST in
    function
    | S_Pass -> continue env
    | S_Assign (le, e) ->
        let* v = eval_expr env scope true e
        and* setter = eval_lexpr env scope le in
        setter v
    | S_Return es ->
        let* vs = prod_map (eval_expr env scope true) es in
        return (Returning vs)
    | S_Then (s1, s2) ->
        bind_seq (eval_stmt env scope s1) (fun r1 ->
            match r1 with
            | Continuing lenv -> eval_stmt (fst env, lenv) scope s2
            | Returning vs -> return (Returning vs))
    | S_Call (name, args) ->
        let* vargs = prod_map (eval_expr env scope true) args in
        let* _ = eval_func (fst env) name vargs in
        continue env
    | S_Cond (e, s1, s2) ->
        choice
          (eval_expr env scope true e)
          (eval_stmt env scope s1) (eval_stmt env scope s2)

  and eval_func genv name args =
    match GEnv.find_opt name genv with
    | None -> fatal ("Unknown function: " ^ name)
    | Some (GEnv.Value _) -> fatal ("Cannot call value " ^ name)
    | Some (GEnv.SpecialFunc f) -> f args
    | Some (GEnv.Func (_, { AST.args = arg_decls; _ }))
      when List.compare_lengths args arg_decls <> 0 ->
        fatal ("Bad number of arguments for function " ^ name)
    | Some (GEnv.Func (r, { AST.args = arg_decls; body; _ })) -> (
        let scope = (name, !r) in
        let () = r := !r + 1 in
        let one_arg (x, _type_desc) v = AST.(S_Assign (LEVar x, E_Literal v)) in
        let body =
          AST.S_Then
            (ASTUtils.stmt_from_list (List.map2 one_arg arg_decls args), body)
        in
        let* res = eval_stmt (genv, LEnv.empty) scope body in
        match res with Continuing _ -> return [] | Returning vs -> return vs)

  let run (ast : ast) std_lib_extras (main_args : value list) : value list m =
    let genv = build_enums ast in
    let* genv = build_consts ast genv in
    let genv = build_funcs ast genv in
    let genv = GEnv.add_seq_special_func (List.to_seq std_lib_extras) genv in
    eval_func genv "main" main_args
end
