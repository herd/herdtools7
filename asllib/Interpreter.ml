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
let _warn = false
let _dbg = false

module type S = sig
  module B : Backend.S

  type body = B.value B.m list -> B.value B.m list B.m
  type primitive = body func_skeleton

  val run : t -> primitive list -> unit B.m
end

module Make (B : Backend.S) = struct
  module B = B
  module IMap = ASTUtils.IMap
  module ISet = ASTUtils.ISet

  type 'a m = 'a B.m
  type body = B.value m list -> B.value m list m
  type primitive = body func_skeleton

  let ( let* ) = B.bind_data
  let ( |||> ) = B.bind_data
  let ( let+ ) = B.bind_seq
  let ( and* ) = B.prod
  let ( ||| ) = B.prod
  let return = B.return
  let ( ||> ) m f = m |||> fun v -> return (f v)
  let pair x y = (x, y)

  let prod_map f =
    let one acc elt =
      let* v = f elt and* li = acc in
      return (v :: li)
    in
    function
    | [] -> return [] | li -> List.fold_left one (return []) li ||> List.rev

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

  type genv = {
    consts : value IMap.t;
        (** Union of the following fields of the ElabModel:
        - G_C, i.e. the global immutable compile-time constant storage (ie constant)
        - G_L, i.e. the global immutable execution-time constant storage (ie let)
        - G_P, i.e. the global immutable ~in between~ constant storage (ie config)
     *)
    funcs : func IMap.t;
        (** The field P of the ElabModel, i.e. the declared subprograms. *)
  }
  (** The global part of the ElabModel. *)

  type lenv = B.value IMap.t
  (** The local part of the ElabModel. *)

  type env = genv * lenv
  (** [env] shoud contain all the informations of the semantics elab model. *)

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
        let one_sfunc { name; args; return_type; parameters; body = _ } =
          D_Func { name; args; body = ASTUtils.s_pass; return_type; parameters }
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
          let v =
            try
              eval_expr e
            with Error.ASLException e ->
                  if _dbg || _warn then
                Format.eprintf "@[<2>Ignoring static evaluation error:@ %a@]@."
                  Error.pp_error e;
              V_Int 0
               | e ->
                  if _dbg then
                    Printf.eprintf
                      "Evaluating constant %s failed with %s!"
                      name (Printexc.to_string e) ;
                  raise e
          in
          acc := IMap.add name (AlreadyEvaluated v) !acc;
          v
      | None -> fatal_from pos @@ Error.UndefinedIdentifier name
    and eval_expr e = StaticInterpreter.static_eval (env_lookup e) e in

    let () =
      let one_decl = function
        | D_GlobalConst (name, _, pos) ->
            let _ = env_lookup pos name in
            ()
        | _ -> ()
      in
      List.iter one_decl ast
    in

    let one_glob = function AlreadyEvaluated v -> v | _ -> assert false in
    IMap.map one_glob !acc

  let build_funcs ast funcs =
    List.to_seq ast
    |> Seq.filter_map (function
         | D_Func func -> Some (func.name, Func (ref 0, func))
         | _ -> None)
    |> Fun.flip IMap.add_seq funcs

  let build_genv ast primitives =
    let funcs = IMap.empty |> build_funcs ast |> add_primitives primitives in
    let consts = IMap.empty |> build_enums ast |> build_consts ast in
    let () =
      if _dbg then (
        Format.eprintf "@[<v 2>Global const env:@ ";
        IMap.iter
          (fun name v -> Format.eprintf "@[<h>- %s: %a@]@ " name PP.pp_value v)
          consts;
        Format.eprintf "@]@.")
    in
    { consts; funcs }

  (*****************************************************************************)
  (*                                                                           *)
  (*                       Main interpretation functions                       *)
  (*                                                                           *)
  (*****************************************************************************)

  type eval_res = Returning of B.value m list | Continuing of lenv

  let continue ((_genv, lenv) : env) = return (Continuing lenv)

  let one_return_value pos name = function
    | [ m ] -> m
    | _ -> fatal_from pos @@ Error.MismatchedReturnValue name

  let lexpr_is_var le =
    match le.desc with LE_Var _ | LE_Ignore -> true | _ -> false

  let write_identifier lenv x scope m =
    let* v = m in
    let* () = B.on_write_identifier x scope v in
    let lenv' = IMap.add x v lenv in
    return lenv'

  let write_identifier_m lenvm x scope m =
    B.bind_seq lenvm (fun lenv -> write_identifier lenv x scope m)

  let rec eval_expr (env : env) scope e =
    let genv, lenv = env in
    match e.desc with
    | E_Literal v -> B.v_of_parsed_v v |> return
    | E_Typed (e, _t) -> eval_expr env scope e
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
        let* v1 = eval_expr env scope e1 and* v2 = eval_expr env scope e2 in
        B.binop op v1 v2
    | E_Unop (op, e) ->
        let* v = eval_expr env scope e in
        B.unop op v
    | E_Cond (e1, e2, e3) ->
        B.bind_ctrl
          (B.choice (eval_expr env scope e1) (return e2) (return e3))
          (eval_expr env scope)
    | E_Slice (e', slices) ->
        let* positions = eval_slices env (to_pos e) scope slices
        and* v = eval_expr env scope e' in
        B.read_from_bitvector positions v
    | E_Call (name, args, named_args) ->
        let vargs = List.map (eval_expr env scope) args
        and nargs =
          let one_narg (x, e) = (x, eval_expr env scope e) in
          List.map one_narg named_args
        in
        let+ returned = eval_func genv name (to_pos e) vargs nargs in
        one_return_value e name returned
    | E_Record (_, li, ta) ->
        let one_field (x, e) = eval_expr env scope e ||> pair x in
        let* fields = prod_map one_field li in
        make_record e (type_of_ta e ta) fields
    | E_GetField (e', x, ta) -> (
        let ty = type_of_ta e ta in
        match ty.desc with
        | T_Record li ->
            let i = record_index_of_field e x li ty in
            let* vec = eval_expr env scope e' in
            B.get_i i vec
        | _ -> fatal_from e @@ Error.BadField (x, ty))
    | E_GetFields (_, [], _) ->
        V_BitVector (Bitvector.of_string "") |> B.v_of_parsed_v |> return
    | E_GetFields (e', [ field ], ta) -> (
        let ty = type_of_ta e ta in
        match ty.desc with
        | T_Bits (_, Some fields) -> (
            match List.assoc_opt field fields with
            | Some slices ->
                E_Slice (e', slices)
                |> ASTUtils.add_pos_from e |> eval_expr env scope
            | None -> fatal_from e @@ Error.BadField (field, ty))
        | _ -> fatal_from e @@ Error.BadField (field, ty))
    | E_GetFields (e', xs, ta) -> (
        let ty = type_of_ta e ta in
        match ty.desc with
        | T_Bits (_, Some fields) ->
            let one (x : string) =
              match List.assoc_opt x fields with
              | None -> fatal_from e @@ Error.BadField (x, ty)
              | Some slices ->
                  E_Slice (e', slices)
                  |> ASTUtils.add_pos_from e |> eval_expr env scope
            in
            prod_map one xs |||> B.concat_bitvectors
        | _ -> fatal_from e @@ Error.BadField (List.hd xs, ty))
    | E_Concat es -> prod_map (eval_expr env scope) es |||> B.concat_bitvectors
    | E_Tuple _ -> fatal_from e @@ Error.NotYetImplemented "tuple construction"
    | E_Unknown ty -> base_value_of_type env scope ty
    | E_Pattern (e, p) ->
        let* v = eval_expr env scope e in
        eval_pattern env scope e v p

  and eval_slices env _pos scope =
    let one = B.v_of_int 1 in
    let eval_one = function
      | Slice_Single e -> eval_expr env scope e ||> Fun.flip pair one
      | Slice_Range (etop, ebot) ->
          let* vtop = eval_expr env scope etop
          and* vbot = eval_expr env scope ebot in
          let* length =
            B.binop MINUS vtop vbot |||> B.binop PLUS (B.v_of_int 1)
          in
          return (vbot, length)
      | Slice_Length (ebot, elength) ->
          eval_expr env scope ebot ||| eval_expr env scope elength
    in
    prod_map eval_one

  and eval_pattern env scope e v = function
    | Pattern_All -> B.v_of_parsed_v (V_Bool true) |> return
    | Pattern_Any li ->
        let folder acc p =
          let* acc = acc and* b = eval_pattern env scope e v p in
          B.binop BOR acc b
        in
        let init = B.v_of_parsed_v (V_Bool false) |> return in
        List.fold_left folder init li
    | Pattern_Geq e -> eval_expr env scope e |||> B.binop GEQ v
    | Pattern_Leq e -> eval_expr env scope e |||> B.binop LEQ v
    | Pattern_Mask _ ->
        fatal_from e @@ Error.NotYetImplemented "Bitvector masks"
    | Pattern_Not p -> eval_pattern env scope e v p |||> B.unop BNOT
    | Pattern_Range (e1, e2) ->
        let* b1 = eval_expr env scope e1 |||> B.binop GEQ v
        and* b2 = eval_expr env scope e2 |||> B.binop LEQ v in
        B.binop BAND b1 b2
    | Pattern_Single e -> eval_expr env scope e |||> B.binop EQ_OP v

  and base_value_of_type env scope ty : B.value m =
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
              base_value_of_type env scope t ||> B.v_to_int ||> of_v
          | BitWidth_Determined e ->
              eval_expr env scope e ||> B.v_to_int ||> of_v
        in
        V_BitVector (Bitvector.zeros n) |> return_lit
    | T_Array (e, t) -> (
        let* n = eval_expr env scope e in
        match B.v_to_int n with
        | Some i ->
            let* v = base_value_of_type env scope t in
            B.create_vector ty (List.init i (Fun.const v))
        | None -> fatal_from ty @@ Error.UnsupportedExpr e)
    | T_Enum li ->
        E_Var (List.hd li) |> ASTUtils.add_pos_from ty |> eval_expr env scope
    | T_Named _ -> fatal_from ty @@ Error.TypeInferenceNeeded
    | T_Tuple li ->
        prod_map (base_value_of_type env scope) li |||> B.create_vector ty
    | T_Record li ->
        let one_field (_, t) = base_value_of_type env scope t in
        prod_map one_field li |||> B.create_vector ty
    | T_Exception li ->
        let one_field (_, t) = base_value_of_type env scope t in
        prod_map one_field li |||> B.create_vector ty
    | _ -> assert false

  and eval_lexpr (env : env) scope le =
    let genv, lenv = env in
    match le.desc with
    | LE_Ignore -> fun _ -> return env
    | LE_Typed (le, _t) -> eval_lexpr env scope le
    | LE_Var x -> fun v -> write_identifier lenv x scope v ||> pair genv
    | LE_Slice (le', slices) ->
        let setter = eval_lexpr env scope le' in
        fun m ->
          let* v = m
          and* positions = eval_slices env (to_pos le) scope slices
          and* bv = ASTUtils.expr_of_lexpr le' |> eval_expr env scope in
          B.write_to_bitvector positions v bv |> setter
    | LE_SetField (le', x, ta) -> (
        let ty = type_of_ta le ta in
        match ty.desc with
        | T_Record li ->
            let setter = eval_lexpr env scope le' in
            let i = record_index_of_field le x li ty in
            fun m ->
              let* new_v = m
              and* vec = ASTUtils.expr_of_lexpr le' |> eval_expr env scope in
              B.set_i i new_v vec |> setter
        | T_Bits _ ->
            LE_SetFields (le', [ x ], ta)
            |> ASTUtils.add_pos_from le |> eval_lexpr env scope
        | _ -> fatal_from le @@ Error.BadField (x, ty))
    | LE_SetFields (le', xs, ta) -> (
        let ty = type_of_ta le ta in
        match ty.desc with
        | T_Bits (_, Some fields) ->
            let folder prev_slices x =
              match List.assoc_opt x fields with
              | Some slices -> List.rev_append slices prev_slices
              | None -> fatal_from le @@ Error.BadField (x, ty)
            in
            let slices = List.fold_left folder [] xs |> List.rev in
            LE_Slice (le', slices)
            |> ASTUtils.add_pos_from le |> eval_lexpr env scope
        | _ ->
            fatal_from le
            @@ Error.ConflictingTypes ([ ASTUtils.default_t_bits ], ty))
    | LE_TupleUnpack les ->
        let assign_each v =
          let nles = List.mapi pair les in
          let mapper (i, le) = B.get_i i v |> eval_lexpr env scope le in
          prod_map mapper nles
        in
        let big_union envs =
          let on_conflict _x v1 _v2 = Some v1 in
          (* TODO: handle union of genv *)
          let folder lenv (_genv, lenv2) = IMap.union on_conflict lenv lenv2 in
          List.fold_left folder lenv envs
        in
        fun m -> m |||> assign_each ||> big_union ||> pair (fst env)

  and multi_assign (genv, lenv) scope pos les monads =
    if List.compare_lengths les monads != 0 then
      fatal_from pos
      @@ Error.BadArity
           ("tuple construction", List.length les, List.length monads)
    else
      let folder acc le m =
        let x =
          match le.desc with
          | LE_Var x -> x
          | LE_Ignore -> "-"
          | _ -> assert false
        in
        write_identifier_m acc x scope m
      in
      List.fold_left2 folder (return lenv) les monads
      ||> pair genv |||> continue

  and eval_stmt (env : env) scope s =
    match s.desc with
    | S_Pass -> continue env
    | S_TypeDecl _ -> continue env
    | S_Assign
        ( { desc = LE_TupleUnpack les; _ },
          { desc = E_Call (name, args, named_args); _ } )
      when List.for_all lexpr_is_var les ->
        let vargs = List.map (eval_expr env scope) args
        and nargs =
          List.map (fun (x, e) -> (x, eval_expr env scope e)) named_args
        in
        eval_func (fst env) name (to_pos s) vargs nargs
        |||> multi_assign env scope s les
    | S_Assign ({ desc = LE_TupleUnpack les; _ }, { desc = E_Tuple exprs; _ })
      when List.for_all lexpr_is_var les ->
        List.map (eval_expr env scope) exprs |> multi_assign env scope s les
    | S_Assign (le, e) ->
        eval_expr env scope e |> eval_lexpr env scope le |||> continue
    | S_Return (Some { desc = E_Tuple es; _ }) ->
        let ms = List.map (eval_expr env scope) es in
        return (Returning ms)
    | S_Return (Some e) ->
        let m = eval_expr env scope e in
        return (Returning [ m ])
    | S_Return None -> return (Returning [])
    | S_Then (s1, s2) ->
        B.bind_seq (eval_stmt env scope s1) (fun r1 ->
            match r1 with
            | Continuing lenv -> eval_stmt (fst env, lenv) scope s2
            | Returning vs -> return (Returning vs))
    | S_Call (name, args, named_args) ->
        let vargs = List.map (eval_expr env scope) args
        and nargs =
          List.map (fun (x, e) -> (x, eval_expr env scope e)) named_args
        in
        let+ _ = eval_func (fst env) name (to_pos s) vargs nargs in
        continue env
    | S_Cond (e, s1, s2) ->
        B.bind_ctrl
          (B.choice (eval_expr env scope e) (return s1) (return s2))
          (eval_stmt env scope)
    | S_Case _ -> ASTUtils.case_to_conds s |> eval_stmt env scope
    | S_Assert e ->
        let v = eval_expr env scope e in
        B.bind_ctrl (B.choice v (return true) (return false)) @@ fun b ->
        if b then continue env else fatal_from e @@ Error.AssertionFailed e

  and eval_func (genv : genv) name pos (args : B.value m list) nargs :
      B.value m list m =
    match IMap.find_opt name genv.funcs with
    | None -> fatal_from pos @@ Error.UndefinedIdentifier name
    | Some (Primitive { body; _ }) -> body args
    | Some (Func (_, { args = arg_decls; _ }))
      when List.compare_lengths args arg_decls <> 0 ->
        fatal_from pos
        @@ Error.BadArity (name, List.length arg_decls, List.length args)
    | Some (Func (r, { args = arg_decls; body; _ })) -> (
        let scope = (name, !r) in
        let () = r := !r + 1 in
        let one_arg lenvm (x, _) m = write_identifier_m lenvm x scope m in
        let lenv = List.fold_left2 one_arg (return IMap.empty) arg_decls args in
        let one_narg lenvm (x, m) =
          let+ lenv = lenvm in
          if IMap.mem x lenv then return lenv
          else write_identifier lenv x scope m
        in
        let+ lenv = List.fold_left one_narg lenv nargs in
        let () =
          if false then (
            Format.eprintf "@[<v 2>Evaluating %S in initial local env:@ " name;
            IMap.iter
              (fun x v -> Format.eprintf "%S = %s@ " x (B.debug_value v))
              lenv;
            Format.eprintf "@]@.")
        in
        let+ res = eval_stmt (genv, lenv) scope body in
        let () =
          if false then Format.eprintf "Finished evaluating %s.@." name
        in
        match res with Continuing _ -> return [] | Returning vs -> return vs)

  let run (ast : t) primitives : unit m =
    let ast = List.rev_append (Lazy.force Builder.stdlib) ast in
    let ast = type_annotation ast primitives in
    let () =
      if false then Format.eprintf "@[<v 2>Typed AST:@ %a@]@." PP.pp_t ast
    in
    let genv = build_genv ast primitives in
    let+ _ = eval_func genv "main" ASTUtils.dummy_annotated [] [] in
    return ()
end
