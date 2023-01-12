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

type 'body primitive = {
  name : string;
  args : AST.type_desc list;
  body : 'body;
  return_type : AST.type_desc option;
}

module type S = sig
  module B : Backend.S

  type body = B.value list -> B.value list B.m

  val run : AST.t -> body primitive list -> B.value list -> B.value list B.m
end

module Make (B : Backend.S) = struct
  module B = B
  open B
  module IMap = ASTUtils.IMap
  module ISet = ASTUtils.ISet

  type body = B.value list -> B.value list B.m

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
  (*                             Records handling                              *)
  (*                                                                           *)
  (*****************************************************************************)

  let make_record ty fields =
    let ty_fields =
      match ty with
      | AST.T_Record ty_fields -> ASTUtils.canonical_fields ty_fields
      | _ -> assert false
    in
    let fields = ASTUtils.canonical_fields fields in
    let values = List.map snd fields in
    let eq_field (x, _) (y, _) = String.equal x y in
    if List.for_all2 eq_field ty_fields fields then create_vector ty values
    else
      fatal
        ("Type error: bad fields passed for type " ^ PP.type_desc_to_string ty)

  let index_of_field_in_record x ty =
    let rec list_index i = function
      | [] -> None
      | (y, _) :: t -> if String.equal x y then Some i else list_index (i + 1) t
    in
    match ty with
    | AST.T_Record li -> (
        match list_index 0 li with
        | Some i -> i
        | None ->
            fatal
              (Format.asprintf "@[<hv>Cannot get field %s of type@ %a@]" x
                 PP.pp_type_desc ty))
    | _ ->
        fatal
          (Format.asprintf "@[<hv>Cannot index type@ %a@]" PP.pp_type_desc ty)

  (*****************************************************************************)
  (*                                                                           *)
  (*                       Global constants environment                        *)
  (*                                                                           *)
  (*****************************************************************************)

  type func = Func of int ref * AST.func | Primitive of body primitive
  type genv = { consts : AST.value IMap.t; funcs : func IMap.t }
  type lenv = value IMap.t
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

  let type_of_ta = function
    | AST.TA_None -> fatal "Interpreter error. Type inference step incomplete."
    | AST.TA_InferredStructure ty -> ty

  let type_annotation ast sfuncs =
    let add_fake_primitives =
      let fake_funcs =
        let one_sfunc { name; args; return_type; _ } =
          let one_arg i ty = ("arg" ^ string_of_int i, ty) in
          let args = List.mapi one_arg args in
          AST.(D_Func { name; args; body = S_Pass; return_type })
        in
        List.map one_sfunc sfuncs
      in
      List.rev_append fake_funcs
    in
    let annotate ast =
      let open Typing in
      try annotate_ast ast
      with TypingError err -> (
        fatal
        @@
        match err with
        | NotYetImplemented s -> "Typing -- Not yet implemented: " ^ s
        | UndefinedIdentifier s -> "Undefined identifier '" ^ s ^ "'."
        | TypeError s -> "Type error: " ^ s)
    in
    let remove_fake_primitives =
      let primitive_names =
        let one_sfunc { name; _ } = name in
        sfuncs |> List.to_seq |> Seq.map one_sfunc |> ASTUtils.ISet.of_seq
      in
      let is_primitive = function
        | AST.(D_Func { name; _ }) ->
            not (ASTUtils.ISet.mem name primitive_names)
        | _ -> true
      in
      List.filter is_primitive
    in
    ast |> add_fake_primitives |> annotate |> remove_fake_primitives

  (*****************************************************************************)
  (*                                                                           *)
  (*                      Construction of the initial env                      *)
  (*                                                                           *)
  (*****************************************************************************)

  let build_enums (ast : AST.t) globals =
    let build_one (counter, globals) name =
      let globals = IMap.add name (AST.V_Int counter) globals in
      (counter + 1, globals)
    in
    let build_decl acc = function
      | AST.D_TypeDecl (_name, AST.T_Enum ids) ->
          List.fold_left build_one acc ids
      | _ -> acc
    in
    let _, genv = List.fold_left build_decl (0, globals) ast in
    genv

  type build_status =
    | NotYetEvaluated of AST.expr
    | AlreadyEvaluated of AST.value

  (* build every constant and make an global env *)
  let build_consts (ast : AST.t) globals =
    (* In the following, acc is the current status of evaluation, i.e. it maps
       every global variable to either its build_status, that is its value if
       it has been evaluated, or its expression otherwise. This is why we have
       to use it every time we could use a variable. *)
    let acc =
      let one_decl = function
        | AST.D_GlobalConst (name, e) -> Some (name, NotYetEvaluated e)
        | _ -> None
      in
      let add_decls =
        ast |> List.to_seq |> Seq.filter_map one_decl |> IMap.add_seq
      in
      let one_glob v = AlreadyEvaluated v in
      globals |> IMap.map one_glob |> add_decls |> ref
    in

    let rec env_lookup name =
      match IMap.find_opt name !acc with
      | Some (AlreadyEvaluated v) -> v
      | Some (NotYetEvaluated e) ->
          let v = eval_expr e in
          acc := IMap.add name (AlreadyEvaluated v) !acc;
          v
      | None -> fatal ("Unknown constant " ^ name)
    and eval_expr e = StaticInterpreter.static_eval env_lookup e in

    let one_decl = function
      | AST.D_GlobalConst (name, _) -> Some (name, env_lookup name)
      | _ -> None
    in
    ast |> List.to_seq |> Seq.filter_map one_decl |> IMap.of_seq

  let build_funcs ast funcs =
    List.to_seq ast
    |> Seq.filter_map (function
         | AST.D_Func func -> Some (func.AST.name, Func (ref 0, func))
         | _ -> None)
    |> Fun.flip IMap.add_seq funcs

  (*****************************************************************************)
  (*                                                                           *)
  (*                       Main interpretation functions                       *)
  (*                                                                           *)
  (*****************************************************************************)

  type eval_res = Returning of value list | Continuing of lenv

  let continue ((_genv, lenv) : env) = return (Continuing lenv)

  let one_return_value name = function
    | [ v ] -> return v
    | _ -> fatal ("Return arrity error for function " ^ name)

  let slices_to_exprs : AST.slice list -> AST.expr list =
    let tr_one_slice = function
      | AST.Slice_Single e -> e
      | _ -> fatal "Cannot call getter with slices arguments."
    in
    List.map tr_one_slice

  let rec eval_expr (env : env) scope is_data =
    let genv, lenv = env in
    let open AST in
    function
    | E_Literal v -> v_of_parsed_v v |> return
    | E_Var x -> (
        match IMap.find_opt x genv.consts with
        | Some v -> return (v_of_parsed_v v)
        | None -> (
            if IMap.mem x genv.funcs then
              let* vl = eval_func genv x [] in
              one_return_value x vl
            else
              match IMap.find_opt x lenv with
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
    | E_Slice (E_Var x, slices) when IMap.mem x genv.funcs ->
        let args = slices_to_exprs slices in
        eval_expr env scope is_data (E_Call (x, args))
    | E_Slice (e, slices) ->
        let positions = eval_slices slices in
        let* v = eval_expr env scope is_data e in
        B.read_from_bitvector positions v
    | E_Call (name, args) ->
        let vargs = List.map (eval_expr env scope is_data) args in
        let* returned = eval_func genv name vargs in
        one_return_value name returned
    | E_Record (_, li, ta) ->
        let one_field (x, e) =
          let* v = eval_expr env scope is_data e in
          return (x, v)
        in
        let* fields = prod_map one_field li in
        make_record (type_of_ta ta) fields
    | E_GetField (e, x, ta) ->
        let i = type_of_ta ta |> index_of_field_in_record x in
        let* vec = eval_expr env scope is_data e in
        B.get_i i vec

  and eval_slices slices =
    let module SI = StaticInterpreter in
    let eval_expr e =
      SI.static_eval (fun _s -> assert false) e |> function
      | AST.V_Int i -> i
      | v -> raise (SI.StaticInterpreterError (SI.TypeError (v, "integer")))
    in
    let slice_to_positions =
      let interval bot len = List.init len (( + ) bot) in
      function
      | AST.Slice_Single e -> [ eval_expr e ]
      | AST.Slice_Range (etop, ebot) ->
          let pbot = eval_expr ebot and ptop = eval_expr etop in
          interval pbot (ptop - pbot + 1)
      | AST.Slice_Length (ebot, elength) ->
          let pbot = eval_expr ebot and plength = eval_expr elength in
          interval pbot plength
    in
    try slices |> List.map slice_to_positions |> List.concat
    with SI.StaticInterpreterError e ->
      fatal (SI.static_interpreter_error_to_string e)

  and eval_lexpr (env : env) scope =
    let open AST in
    let genv, lenv = env in
    function
    | LE_Var x -> (
        match IMap.find_opt x genv.funcs with
        | Some _ ->
            fun v ->
              let* _ = eval_func genv x [ v ] in
              continue env
        | None ->
            fun v ->
              let* v = v in
              let* () = on_write_identifier x scope v in
              let lenv = IMap.add x v lenv in
              continue (genv, lenv))
    | LE_Slice (LE_Var x, slices) ->
        let args =
          slices_to_exprs slices |> List.map (eval_expr env scope true)
        in
        fun m ->
          let* _ = eval_func genv x (m :: args) in
          continue env
    | LE_Slice (le, slices) ->
        let setter = eval_lexpr env scope le in
        let positions = eval_slices slices in
        fun m ->
          let* v = m
          and* bv =
            let e = ASTUtils.expr_of_lexpr le in
            eval_expr env scope true e
          in
          B.write_to_bitvector positions v bv |> setter
    | LE_SetField (le, x, ta) ->
        let setter = eval_lexpr env scope le
        and i = index_of_field_in_record x (type_of_ta ta) in
        fun m ->
          let* new_v = m
          and* vec =
            let e = ASTUtils.expr_of_lexpr le in
            eval_expr env scope true e
          in
          B.set_i i new_v vec |> setter

  and eval_stmt (env : env) scope =
    let open AST in
    function
    | S_Pass -> continue env
    | S_Assign (le, e) ->
        let v = eval_expr env scope true e
        and setter = eval_lexpr env scope le in
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
        let vargs = List.map (eval_expr env scope true) args in
        let* _ = eval_func (fst env) name vargs in
        continue env
    | S_Cond (e, s1, s2) ->
        choice
          (eval_expr env scope true e)
          (eval_stmt env scope s1) (eval_stmt env scope s2)
    | S_Case (e, cases) -> ASTUtils.case_to_conds e cases |> eval_stmt env scope

  and eval_func (genv : genv) name (args : value m list) : value list m =
    match IMap.find_opt name genv.funcs with
    | None -> fatal ("Unknown function: " ^ name)
    | Some (Primitive { body; _ }) ->
        let* args = prod_map Fun.id args in
        body args
    | Some (Func (_, { AST.args = arg_decls; _ }))
      when List.compare_lengths args arg_decls <> 0 ->
        fatal ("Bad number of arguments for function " ^ name)
    | Some (Func (r, { AST.args = arg_decls; body; _ })) -> (
        let scope = (name, !r) in
        let () = r := !r + 1 in
        let one_arg (x, _type_desc) v =
          let* v = v in
          let* () = on_write_identifier x scope v in
          return (x, v)
        in
        let* bindings = List.map2 one_arg arg_decls args |> prod_map Fun.id in
        let lenv = List.to_seq bindings |> IMap.of_seq in
        let* res = eval_stmt (genv, lenv) scope body in
        match res with Continuing _ -> return [] | Returning vs -> return vs)

  let run (ast : AST.t) primitives (main_args : value list) : value list m =
    let ast = type_annotation ast primitives in
    let funcs = IMap.empty |> build_funcs ast |> add_primitives primitives in
    let consts = IMap.empty |> build_enums ast |> build_consts ast in
    eval_func { consts; funcs } "main" (List.map return main_args)
end
