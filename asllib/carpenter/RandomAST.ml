(****************************************************************************************************************)
(*  SPDX-FileCopyrightText: Copyright 2022-2024 Arm Limited and/or its affiliates <open-source-office@arm.com>  *)
(*  SPDX-License-Identifier: BSD-3-Clause                                                                       *)
(****************************************************************************************************************)

open Asllib
open AST
open ASTUtils
open QCheck2.Gen

let _dbg = false
let annot desc = ASTUtils.add_dummy_pos desc

type 'a gen = 'a QCheck2.Gen.t
type 'a sgen = 'a QCheck2.Gen.sized
type env = StaticEnv.env

let filter li = List.filter_map Fun.id li
let filter_oneof li = filter li |> oneof
let filter_oneofl li = filter li |> oneofl
let protected_oneofl = function [] -> None | li -> Some (oneofl li)
let protected_oneof = function [] -> None | li -> Some (oneof li)
let protected_filter_oneofl li = filter li |> protected_oneofl
let protected_filter_oneof li = filter li |> protected_oneof
let names = string_size ~gen:(char_range 'a' 'z') (1 -- 20)
let pay n = if n <= 0 then 0 else n - 1

let rec ensure_satisfies p t =
  let* v = t in
  if p v then return v else ensure_satisfies p t

let bind f t = t >>= f
let mapo f t = Option.map (map f) t
let ( >|== ) t f = mapo f t
let ( let** ) t f = Option.map (bind f) t

module Untyped (C : CConfig.S) = struct
  let unop : unop gen option =
    [
      (if C.Syntax.neg then Some NEG else None);
      (if C.Syntax.not then Some NOT else None);
      (if C.Syntax.bnot then Some BNOT else None);
    ]
    |> protected_filter_oneofl

  let binop : binop gen option =
    [
      (if C.Syntax.plus then Some PLUS else None);
      (if C.Syntax.and_ then Some AND else None);
      (if C.Syntax.band then Some BAND else None);
      (if C.Syntax.beq then Some BEQ else None);
      (if C.Syntax.bor then Some BOR else None);
      (if C.Syntax.div then Some DIV else None);
      (if C.Syntax.divrm then Some DIVRM else None);
      (if C.Syntax.eor then Some EOR else None);
      (if C.Syntax.eq_op then Some EQ_OP else None);
      (if C.Syntax.gt then Some GT else None);
      (if C.Syntax.geq then Some GEQ else None);
      (if C.Syntax.impl then Some IMPL else None);
      (if C.Syntax.lt then Some LT else None);
      (if C.Syntax.leq then Some LEQ else None);
      (if C.Syntax.mod_ then Some MOD else None);
      (if C.Syntax.minus then Some MINUS else None);
      (if C.Syntax.mul then Some MUL else None);
      (if C.Syntax.neq then Some NEQ else None);
      (if C.Syntax.or_ then Some OR else None);
      (if C.Syntax.pow then Some POW else None);
      (if C.Syntax.rdiv then Some RDIV else None);
      (if C.Syntax.shl then Some SHL else None);
      (if C.Syntax.shr then Some SHR else None);
    ]
    |> protected_filter_oneofl

  let literal : literal gen option =
    let l_int = small_nat >|= fun i -> L_Int (Z.of_int i)
    and l_bool = bool >|= fun b -> L_Bool b
    and l_real = float >|= fun r -> L_Real (Q.of_float r)
    and l_bv =
      let bit = oneofa [| '0'; '1' |] in
      string_of bit >|= fun s ->
      L_BitVector (Bitvector.of_string ("'" ^ s ^ "'"))
    and l_string = string_small_of printable >|= fun s -> L_String s in
    [
      (if C.Syntax.l_int then Some l_int else None);
      (if C.Syntax.l_bool then Some l_bool else None);
      (if C.Syntax.l_bitvector then Some l_bv else None);
      (if C.Syntax.l_real then Some l_real else None);
      (if C.Syntax.l_string then Some l_string else None);
    ]
    |> protected_filter_oneof

  let ty_basic =
    let t_bool = T_Bool |> annot |> pure
    and t_integer = T_Int UnConstrained |> annot |> pure
    and t_real = T_Real |> annot |> pure
    and t_string = T_String |> annot |> pure
    and t_named =
      let+ name = names in
      T_Named name |> annot
    in
    [
      (if C.Syntax.t_bool then Some t_bool else None);
      (if C.Syntax.t_int then Some t_integer else None);
      (if C.Syntax.t_real then Some t_real else None);
      (if C.Syntax.t_named then Some t_named else None);
      (if C.Syntax.t_string then Some t_string else None);
    ]
    |> protected_filter_oneof

  let slices expr =
    let slice_single n =
      let+ e = expr n in
      Slice_Single e
    and slice_range n =
      let* n1, n2 = Nat.split2 n in
      let+ e1 = expr n1 and+ e2 = expr n2 in
      Slice_Range (e1, e2)
    and slice_length n =
      let* n1, n2 = Nat.split2 n in
      let+ e1 = expr n1 and+ e2 = expr n2 in
      Slice_Length (e1, e2)
    and slice_star n =
      let* n1, n2 = Nat.split2 n in
      let+ e1 = expr n1 and+ e2 = expr n2 in
      Slice_Star (e1, e2)
    in
    let slice n =
      [
        (if C.Syntax.slice_single then Some (slice_single n) else None);
        (if C.Syntax.slice_range then Some (slice_range n) else None);
        (if C.Syntax.slice_length then Some (slice_length n) else None);
        (if C.Syntax.slice_star then Some (slice_star n) else None);
      ]
      |> filter_oneof
    in
    fun n ->
      if n >= 1 then Nat.list_sized slice (n - 1)
      else slice_single n >|= fun x -> [ x ]

  let expr : expr sgen =
    let e_literal = literal >|== fun l -> E_Literal l |> annot in
    let e_var = names >|= fun s -> E_Var s |> annot in
    let e_tuple expr n = Nat.list_sized expr n >|= fun li -> E_Tuple li |> annot
    and e_binop expr n =
      let** op = binop in
      let* n1, n2 = Nat.split2 n in
      let+ e1 = expr n1 and+ e2 = expr n2 in
      E_Binop (op, e1, e2) |> annot
    and e_unop expr n =
      let** op = unop in
      let+ e = expr n in
      E_Unop (op, e) |> annot
    and e_cond expr n =
      let* n1, n2, n3 = Nat.split3 n in
      let+ e1 = expr n1 and+ e2 = expr n2 and+ e3 = expr n3 in
      E_Cond (e1, e2, e3) |> annot
    and e_slice expr n =
      let* n1, n2 = Nat.split2 n in
      let+ e = expr n1 and+ slices = slices expr n2 in
      E_Slice (e, slices) |> annot
    in
    fix @@ fun expr n ->
    let n = pay n in
    [
      (if C.Syntax.e_literal then e_literal else None);
      (if C.Syntax.e_var then Some e_var else None);
      (if C.Syntax.e_unop && n >= 0 then e_unop expr n else None);
      (if C.Syntax.e_binop && n >= 2 then e_binop expr n else None);
      (if C.Syntax.e_tuple && n >= 2 then Some (e_tuple expr n) else None);
      (if C.Syntax.e_cond && n >= 3 then Some (e_cond expr n) else None);
      (if C.Syntax.e_slice && n >= 1 then Some (e_slice expr n) else None);
    ]
    |> filter
    |> function
    | [] -> failwith "cannot construct expressions"
    | li -> oneof li

  let slices n = slices expr n

  let ctnt : int_constraint sgen option =
    let exact n =
      let+ e = expr n in
      Constraint_Exact e
    and range n =
      let* n1, n2 = Nat.split2 n in
      let+ e1 = expr n1 and+ e2 = expr n2 in
      Constraint_Range (e1, e2)
    in
    [
      (if C.Syntax.constraint_exact then Some exact else None);
      (if C.Syntax.constraint_range then Some range else None);
    ]
    |> filter
    |> function
    | [] -> None
    | li ->
        Some
          (fun n ->
            let n = pay n in
            List.map (fun sgen -> sgen n) li |> oneof)

  let ty : ty sgen =
    let field ty n = pair names (ty n) in
    let fields ty n = Nat.list_sized (field ty) n in
    let t_tuple ty n =
      let+ li = Nat.list_sized ty n in
      T_Tuple li |> annot
    and t_record ty n =
      let+ li = fields ty n in
      T_Record li |> annot
    and t_exception ty n =
      let+ li = fields ty n in
      T_Exception li |> annot
    and t_enum n =
      let+ names = list_repeat n names in
      T_Enum names |> annot
    and t_int n =
      Fun.flip Option.map ctnt @@ fun ctnt ->
      let+ ctnts = Nat.list_sized ctnt n in
      T_Int (WellConstrained ctnts) |> annot
    and t_bits n =
      let+ width = expr n and+ bitfields = pure [] (* TODO *) in
      T_Bits (width, bitfields) |> annot
    in
    fix @@ fun ty n ->
    let n = pay n in
    [
      ty_basic;
      (if C.Syntax.t_int && n >= 1 then t_int n else None);
      (if C.Syntax.t_bits then Some (t_bits n) else None);
      (if C.Syntax.t_record && n >= 2 then Some (t_record ty n) else None);
      (if C.Syntax.t_enum && n >= 2 then Some (t_enum n) else None);
      (if C.Syntax.t_tuple && n >= 2 then Some (t_tuple ty n) else None);
      (if C.Syntax.t_exception && n >= 2 then Some (t_exception ty n) else None);
    ]
    |> function
    | [] -> failwith "cannot construct types"
    | li -> filter_oneof li

  let ldi : local_decl_item sgen =
    let ldi_ignore = return LDI_Discard
    and ldi_var =
      let+ name = names in
      LDI_Var name
    and ldi_typed ldis n =
      let* n1, n2 = Nat.split2 n in
      let+ ldi = ldis n1 and+ ty = ty n2 in
      LDI_Typed (ldi, ty)
    and ldi_tuple ldi n =
      let+ li = Nat.list_sized ldi n in
      LDI_Tuple li
    in
    fix @@ fun ldi n ->
    let n = pay n in
    [
      (if C.Syntax.ldi_var then Some ldi_var else None);
      (if C.Syntax.ldi_discard then Some ldi_ignore else None);
      (if C.Syntax.ldi_typed then Some (ldi_typed ldi n) else None);
      (if C.Syntax.ldi_tuple && n >= 2 then Some (ldi_tuple ldi n) else None);
    ]
    |> filter_oneof

  let ldk = oneofa [| LDK_Var; LDK_Constant; LDK_Let |]

  let lexpr : lexpr sgen =
    let le_ignore = LE_Discard |> annot |> pure
    and le_var = names >|= fun s -> LE_Var s |> annot
    and le_set_field lexpr n =
      let+ le = lexpr n and+ name = names in
      LE_SetField (le, name) |> annot
    and le_set_fields lexpr n =
      let* n1, n2 = Nat.split2 n in
      let+ le = lexpr n1 and+ names = list_repeat n2 names in
      LE_SetFields (le, names, []) |> annot
    and le_destructuring lexpr n =
      let+ les = Nat.list_sized lexpr n in
      LE_Destructuring les |> annot
    and le_slice lexpr n =
      let* n1, n2 = Nat.split2 n in
      let+ le = lexpr n1 and+ slices = slices n2 in
      LE_Slice (le, slices) |> annot
    and le_setarray lexpr n =
      let* n1, n2 = Nat.split2 n in
      let+ le = lexpr n1 and+ e = expr n2 in
      LE_SetArray (le, e) |> annot
    in
    fix @@ fun lexpr n ->
    let n = pay n in
    [
      (if C.Syntax.le_discard then Some le_ignore else None);
      (if C.Syntax.le_var then Some le_var else None);
      (if C.Syntax.le_setfield then Some (le_set_field lexpr n) else None);
      (if C.Syntax.le_setfields && n >= 2 then Some (le_set_fields lexpr n)
       else None);
      (if C.Syntax.le_destructuring && n >= 1 then
         Some (le_destructuring lexpr n)
       else None);
      (if C.Syntax.le_slice && n >= 1 then Some (le_slice lexpr n) else None);
      (if C.Syntax.le_setarray && n >= 1 then Some (le_setarray lexpr n)
       else None);
    ]
    |> filter_oneof

  let stmt : stmt sgen =
    let s_block stmt n = Nat.list_sized stmt n >|= ASTUtils.stmt_from_list in
    let s_cond stmt n =
      let* n1, n2, n3 = Nat.split3 n in
      let+ e1 = expr n1 and+ s2 = s_block stmt n2 and+ s3 = s_block stmt n3 in
      S_Cond (e1, s2, s3) |> annot
    in
    let s_decl n =
      let* n1, n2 = Nat.split2 n in
      let+ e = expr n1 |> option and+ ldi = ldi n2 and+ ldk = ldk in
      S_Decl (ldk, ldi, e) |> annot
    in
    let s_assert n =
      let+ e = expr n in
      S_Assert e |> annot
    in
    let s_assign n =
      let* n1, n2 = Nat.split2 n in
      let+ le = lexpr n1 and+ e = expr n2 in
      S_Assign (le, e, V1) |> annot
    in
    fix @@ fun stmt n ->
    let n = pay n in
    [
      (if C.Syntax.s_assert then Some (s_assert n) else None);
      (if C.Syntax.s_cond && n >= 2 then Some (s_cond stmt n) else None);
      (if C.Syntax.s_decl && n >= 3 then Some (s_decl n) else None);
      (if C.Syntax.s_assign && n >= 2 then Some (s_assign n) else None);
    ]
    |> filter_oneof

  let gdk = oneofa [| GDK_Constant; GDK_Config; GDK_Let; GDK_Var |]

  let subprogram_type =
    oneofa [| ST_Procedure; ST_Function; ST_Getter; ST_Setter |]

  let decl : decl sgen =
    let type_decl n =
      let+ ty = ty n and+ name = names in
      D_TypeDecl (name, ty, None) |> annot
    and global_decl n =
      let* n1, n2 = Nat.split2 n in
      let+ name = names
      and+ keyword = gdk
      and+ ty = ty n1 |> option
      and+ initial_value = expr n2 |> option in
      D_GlobalStorage { name; keyword; ty; initial_value } |> annot
    and func n =
      let* n1, n2, n3 = Nat.split3 n in
      let parameters = [] in
      let+ name = names
      and+ args = Nat.list_sized (fun n -> pair names (ty n)) n1
      and+ body = stmt n2 >|= fun s -> SB_ASL s
      and+ return_type = ty n3 |> option
      and+ subprogram_type = subprogram_type in
      D_Func { name; parameters; args; body; return_type; subprogram_type }
      |> annot
    in
    fun n ->
      [ type_decl n ]
      |> (if n >= 2 then List.cons (global_decl n) else Fun.id)
      |> (if n >= 3 then List.cons (func n) else Fun.id)
      |> oneof

  let ast : AST.t sgen = Nat.list_sized decl
end

module Typed (C : CConfig.S) = struct
  module Untyped = Untyped (C)

  let can_construct_literal env ty = Types.is_singular env ty
  let add_with f acc elt = acc + f elt

  let list_sum ?(init = 0) (f : 'a -> int) (li : 'a list) : int =
    List.fold_left (add_with f) init li

  let rec minimal_direct_fuel_ty env ty =
    let () =
      if _dbg && true then
        Format.eprintf "Getting minimal_direct_fuel of %a@." PP.pp_ty ty
    in
    match (Types.make_anonymous env ty).desc with
    | T_Int _ | T_Bool | T_Bits _ | T_Enum _ | T_Real | T_String -> 1
    | T_Named _ -> assert false
    | T_Array _ -> 1000000000
    | T_Tuple li -> list_sum ~init:2 (minimal_direct_fuel_ty env) li
    | T_Record fields | T_Exception fields ->
        list_sum ~init:2 (fun (_, ty) -> minimal_direct_fuel_ty env ty) fields

  let literal ty : literal gen =
    match ty.desc with
    | T_Named _ -> failwith "Not yet implemented: named types."
    | T_Int _ -> small_nat >|= fun i -> L_Int (Z.of_int i)
    | T_Bool -> bool >|= fun b -> L_Bool b
    | T_Real -> float >|= fun r -> L_Real (Q.of_float (Float.abs r))
    | T_String -> string_small_of printable >|= fun s -> L_String s
    | T_Bits _ ->
        let+ n = small_nat and+ i = nat in
        L_BitVector (Bitvector.of_int_sized n i)
    | _ -> failwith "Cannot construct a literal of this type"

  let fresh_name env =
    ensure_satisfies (fun s -> StaticEnv.is_undefined s env) names

  let t_bits : ty gen =
    let+ width = small_nat in
    T_Bits (E_Literal (L_Int (Z.of_int width)) |> annot, []) |> annot

  let slice expr env n : slice gen =
    let n = pay n in
    let* n1, n2 = Nat.split2 n in
    let int n = expr (env, integer, n) in
    [
      (if C.Syntax.slice_single then Some (int n >|= fun e -> Slice_Single e)
       else None);
      (if C.Syntax.slice_range then
         Some (pair (int n1) (int n2) >|= fun (e1, e2) -> Slice_Range (e1, e2))
       else None);
      (if C.Syntax.slice_range then
         Some (pair (int n1) (int n2) >|= fun (e1, e2) -> Slice_Length (e1, e2))
       else None);
      (if C.Syntax.slice_range then
         Some (pair (int n1) (int n2) >|= fun (e1, e2) -> Slice_Star (e1, e2))
       else None);
    ]
    |> filter_oneof

  let slices expr env = Nat.list_sized_non_empty (slice expr env)

  let expr : env -> ty -> expr sgen =
    let e_literal ty =
      match ty.desc with
      | T_Enum li -> oneofl li >|= fun s -> E_Var s |> annot
      | _ -> literal ty >|= fun l -> E_Literal l |> annot
    in
    let e_var env ty =
      let folder name (t, _) vars =
        if Types.structural_subtype_satisfies env t ty then
          (E_Var name |> annot) :: vars
        else vars
      in
      []
      |> ASTUtils.IMap.fold folder env.StaticEnv.local.storage_types
      |> ASTUtils.IMap.fold folder env.StaticEnv.global.storage_types
      |> protected_oneofl
    in
    let e_cond expr env ty n =
      let min = minimal_direct_fuel_ty env ty in
      if n <= min * 2 then None
      else
        Some
          (let* n1 = min -- n in
           let* n2 = min -- (n - min) in
           let+ e1 = expr (env, ty, n1)
           and+ e2 = expr (env, ty, n2)
           and+ e_cond = expr (env, boolean, n - n1 - n2) in
           E_Cond (e_cond, e1, e2) |> annot)
    in
    let e_ctc expr env ty ty_anon n =
      match ty.desc with
      | T_Int _ ->
          Some
            (let+ e' = expr (env, T_Int UnConstrained |> annot, n) in
             E_ATC (e', ty) |> annot)
      | T_Named _ when Types.is_singular env ty_anon ->
          Some (expr (env, ty_anon, n - 1))
      | _ -> None
    in
    let e_call expr env ty n =
      let funcs =
        ASTUtils.IMap.fold
          (fun name func_sig funcs ->
            match func_sig.return_type with
            | Some t ->
                if
                  Types.structural_subtype_satisfies env t ty
                  && List.length func_sig.args <= n
                then (name, func_sig) :: funcs
                else funcs
            | _ -> funcs)
          env.StaticEnv.global.subprograms []
      in
      let one_func (name, func_sig) =
        let* arg_sizes = Nat.split ~size:(List.length func_sig.args) n in
        let+ args =
          List.map2 (fun (_, ty) n -> expr (env, ty, n)) func_sig.args arg_sizes
          |> flatten_l
        in
        E_Call (name, args, []) |> annot
      in
      protected_oneofl funcs |> Option.map (bind one_func)
    in
    let can_construct_binop ty =
      match ty.desc with
      | T_Int _ | T_Real | T_Bits _ | T_Bool -> true
      | _ -> false
    in
    let e_binop expr env ty n =
      let* n1, n2 = Nat.split2 n in
      let* op, t1, t2 =
        match ty.desc with
        | T_Int _ ->
            [| PLUS; MINUS; MUL; DIV; DIVRM; MOD; SHL; SHR; POW |]
            |> oneofa
            |> map (fun op -> (op, integer, integer))
        | T_Bool ->
            [
              [| BAND; BOR; BEQ; IMPL |] |> oneofa
              |> map (fun op -> (op, boolean, boolean));
              (let+ op = [| EQ_OP; NEQ |] |> oneofa
               and+ t = [| integer; boolean; real |] |> oneofa in
               (op, t, t));
              (let+ op = [| LEQ; GEQ; GT; LT |] |> oneofa
               and+ t = [| integer; real |] |> oneofa in
               (op, t, t));
            ]
            |> oneof
        | T_Real ->
            [| PLUS; MINUS; MUL |] |> oneofa |> map (fun op -> (op, real, real))
        | T_Bits _ ->
            [
              [| AND; OR; EOR |] |> oneofa |> map (fun op -> (op, ty, ty));
              [| PLUS; MINUS |] |> oneofa |> map (fun op -> (op, ty, integer));
            ]
            |> oneof
        | _ -> assert false
      in
      let+ e1 = expr (env, t1, n1) and+ e2 = expr (env, t2, n2) in
      E_Binop (op, e1, e2) |> annot
    in
    let can_construct_unop ty =
      match ty.desc with
      | T_Int _ | T_Real | T_Bool | T_Bits _ -> true
      | _ -> false
    in
    let e_unop expr env ty n =
      let op =
        match ty.desc with
        | T_Int _ | T_Real -> NEG
        | T_Bits _ -> NOT
        | T_Bool -> BNOT
        | _ -> assert false
      in
      expr (env, ty, n) >|= fun e -> E_Unop (op, e) |> annot
    in
    let e_tuple expr env ty n =
      match ty.desc with
      | T_Tuple li ->
          let size = List.length li in
          if size > n then None
          else
            Some
              (let* sizes = Nat.split ~size n in
               List.map2 (fun ty n -> expr (env, ty, n)) li sizes |> flatten_l
               >|= fun li -> E_Tuple li |> annot)
      | _ -> None
    in
    let e_record expr env ty fields n =
      let* sizes = Nat.split ~size:(List.length fields) n in
      List.map2
        (fun (name, ty) n -> expr (env, ty, n) >|= fun e -> (name, e))
        fields sizes
      |> flatten_l
      >|= fun fields -> E_Record (ty, fields) |> annot
    in
    let e_get_array = (* TODO *) None in
    let e_get_fields = (* TODO *) None in
    let e_pattern = (* TODO *) None in
    let e_unknown ty = E_Unknown ty |> annot |> pure |> Option.some in
    let is_bits ty = match ty.desc with T_Bits _ -> true | _ -> false in
    let e_slices expr env n : expr gen =
      let* n2 = 1 -- n in
      let n1 = n - n2 in
      let+ e' =
        let* t = t_bits in
        expr (env, t, n1)
      and+ slices = slices expr env n2 in
      E_Slice (e', slices) |> annot
    in
    let e_get_field expr env ty n : expr gen option =
      let field_folder new_ty acc (field_name, field_ty) =
        if Types.structural_subtype_satisfies env field_ty ty then
          (let+ e' = expr (env, new_ty, n) in
           E_GetField (e', field_name) |> annot)
          :: acc
        else acc
      in
      let type_folder name ty' acc : expr gen list =
        let ty' = Types.make_anonymous env ty' in
        match ty'.desc with
        | T_Record fields | T_Exception fields ->
            let new_ty = T_Named name |> annot in
            if minimal_direct_fuel_ty env ty' <= n then
              List.fold_left (field_folder new_ty) acc fields
            else acc
        | _ -> acc
      in
      IMap.fold type_folder env.StaticEnv.global.declared_types [] |> function
      | [] -> None
      | li -> Some (oneof li)
    in
    let e_concat expr env n : expr gen =
      Nat.list_sized_non_empty (fun n -> t_bits >>= fun t -> expr (env, t, n)) n
      >|= fun li -> E_Concat li |> annot
    in
    let expr' =
      fix @@ fun expr (env, ty, n) ->
      let () =
        if _dbg then Printf.eprintf "Generating an expr of size %d\n%!" n
      in
      let n = pay n in
      let ty_anon = Types.make_anonymous env ty in
      [
        (if C.Syntax.e_literal && can_construct_literal env ty_anon then
           Some (e_literal ty_anon)
         else None);
        (if C.Syntax.e_unknown then e_unknown ty else None);
        (if C.Syntax.e_getarray then e_get_array else None);
        (if C.Syntax.e_getfields then e_get_fields else None);
        (if C.Syntax.e_pattern then e_pattern else None);
        (if C.Syntax.e_var then e_var env ty else None);
        (if C.Syntax.e_call then e_call expr env ty n else None);
        (if C.Syntax.e_binop && n >= 1 && can_construct_binop ty_anon then
           Some (e_binop expr env ty_anon n)
         else None);
        (if C.Syntax.e_unop && can_construct_unop ty_anon then
           Some (e_unop expr env ty_anon n)
         else None);
        (if C.Syntax.e_tuple && n >= 1 then e_tuple expr env ty_anon n else None);
        (if C.Syntax.e_slice && n >= 1 && is_bits ty_anon then
           Some (e_slices expr env n)
         else None);
        (if C.Syntax.e_concat && n >= 2 && is_bits ty_anon then
           Some (e_concat expr env n)
         else None);
        (if C.Syntax.e_ctc then e_ctc expr env ty ty_anon n else None);
        (if C.Syntax.e_record && n >= 2 then
           match (ty_anon.desc, ty.desc) with
           | (T_Record fields | T_Exception fields), T_Named _
             when List.length fields <= n ->
               Some (e_record expr env ty fields n)
           | _ -> None
         else None);
        (if C.Syntax.e_cond && n >= 3 then e_cond expr env ty n else None);
        (if C.Syntax.e_getfield && n >= 1 then e_get_field expr env ty n
         else None);
      ]
      |> filter
      |> function
      | [] ->
          if _dbg || false then
            Format.eprintf "@[<2>Cannot construct with fuel %d type@ %a@]@." n
              PP.pp_ty ty;
          expr (env, ty, minimal_direct_fuel_ty env ty)
      | li ->
          let res = oneof li in
          let () = if _dbg then Printf.eprintf "Generated expr.\n%!" in
          res
    in

    fun env ty n -> expr' (env, ty, n)

  let slices = slices (fun (env, ty, n) -> expr env ty n)

  let fields_maxed ty env ~max n =
    let* ns = Nat.list_sized_min_no_gen 1 n in
    let rec loop i max prevs = function
      | [] -> return prevs
      | n :: ns ->
          let* ty = ty (false, env, Some (max + i - n), n) and* name = string in
          let prevs = (name, ty) :: prevs
          and max = max - minimal_direct_fuel_ty env ty in
          loop (succ i) max prevs ns
    in
    loop 0 max [] ns

  let fields_unbounded ty env n =
    Nat.list_sized_non_empty (fun n -> pair names (ty (false, env, None, n))) n

  let fields env ty = function
    | Some max -> fields_maxed ty env ~max
    | None -> fields_unbounded ty env

  let ty : bool -> env -> ?max:int -> ty sgen =
    let t_bool = T_Bool |> annot |> pure
    and t_integer env max n : ty gen =
      let cnt_exact =
        if C.Syntax.constraint_exact then
          Option.some @@ fun n ->
          let+ e = expr env integer n in
          Constraint_Exact e
        else None
      and cnt_range =
        if C.Syntax.constraint_range then
          Option.some @@ fun n ->
          let* n1, n2 = Nat.split2 n in
          let+ e1 = expr env integer n1 and+ e2 = expr env integer n2 in
          Constraint_Range (e1, e2)
        else None
      in
      let cnt =
        [ cnt_exact; cnt_range ] |> filter |> function
        | [] -> None
        | li -> Some (fun n -> List.map (fun gen -> gen n) li |> oneof)
      in
      let cnts = Option.map Nat.list_sized_non_empty cnt in
      [
        Some (T_Int UnConstrained |> annot |> pure);
        (if n >= 1 && max = None then
           Fun.flip Option.map cnts @@ fun cnts ->
           let+ cnts = cnts n in
           T_Int (WellConstrained cnts) |> annot
         else None);
      ]
      |> filter_oneof
    and t_real = T_Real |> annot |> pure
    and t_tuple ty env max n =
      let+ fields = fields env ty max n in
      T_Tuple (List.map snd fields) |> annot
    and t_record ty env max n =
      let+ fields = fields env ty max n in
      T_Record fields |> annot
    and t_exception ty env max n =
      let+ fields = fields env ty max n in
      T_Exception fields |> annot
    and t_enum n =
      let+ names = list_repeat n names in
      T_Enum names |> annot
    and t_named env max =
      let folder name ty' prevs =
        let ty = T_Named name |> annot in
        match max with
        | None -> ty :: prevs
        | Some max ->
            if minimal_direct_fuel_ty env ty' <= max then ty :: prevs else prevs
      in
      IMap.fold folder env.StaticEnv.global.declared_types []
      |> protected_oneofl
    and t_array = None (* TODO *) in
    let ty' =
      fix @@ fun ty (is_decl, env, max, n) ->
      let () = if _dbg then Printf.eprintf "Generating ty of size %d\n%!" n in
      let n = pay n in
      [
        (if C.Syntax.t_bool then Some t_bool else None);
        (if C.Syntax.t_int then Some (t_integer env max n) else None);
        (if C.Syntax.t_real then Some t_real else None);
        (if C.Syntax.t_bits then Some t_bits else None);
        (if C.Syntax.t_array then t_array else None);
        (if C.Syntax.t_record && n >= 1 && is_decl then
           Some (t_record ty env max n)
         else None);
        (if C.Syntax.t_exception && n >= 1 && is_decl then
           Some (t_exception ty env max n)
         else None);
        (if C.Syntax.t_enum && n >= 1 && is_decl then Some (t_enum n) else None);
        (if C.Syntax.t_tuple && n > 2 && is_decl then
           Some (t_tuple ty env max n)
         else None);
        (if C.Syntax.t_named then t_named env max else None);
      ]
      |> filter_oneof
      |>
      if _dbg then (fun res ->
        Printf.eprintf "type generated.\n%!";
        res)
      else Fun.id
    in
    fun is_decl env ?max n -> ty' (is_decl, env, max, n)

  let lexpr : env -> ty -> lexpr sgen =
    let lexpr' =
      let le_discard = LE_Discard |> annot |> pure
      and le_var env ty =
        let folder name (t, _) vars =
          if Types.structural_subtype_satisfies env t ty then
            (LE_Var name |> annot) :: vars
          else vars
        in
        []
        |> ASTUtils.IMap.fold folder env.StaticEnv.local.storage_types
        |> ASTUtils.IMap.fold folder env.StaticEnv.global.storage_types
        |> protected_oneofl
      and le_tuple lexpr env ty n =
        match ty.desc with
        | T_Tuple li ->
            let size = List.length li in
            if n < size then None
            else
              Some
                (let* sizes = Nat.split ~size n in
                 List.map2 (fun ty n -> lexpr (env, ty, n)) li sizes
                 |> flatten_l
                 >|= fun li -> LE_Destructuring li |> annot)
        | _ -> None
      and le_set_field lexpr env ty n =
        let field_folder new_ty acc (field_name, field_ty) =
          if Types.structural_subtype_satisfies env field_ty ty then
            (let+ le' = lexpr (env, new_ty, n) in
             LE_SetField (le', field_name) |> annot)
            :: acc
          else acc
        in
        let type_folder name ty' acc : lexpr gen list =
          let ty' = Types.make_anonymous env ty' in
          match ty'.desc with
          | T_Record fields | T_Exception fields ->
              let new_ty = T_Named name |> annot in
              if minimal_direct_fuel_ty env ty' <= n then
                List.fold_left (field_folder new_ty) acc fields
              else acc
          | _ -> acc
        in
        IMap.fold type_folder env.StaticEnv.global.declared_types [] |> function
        | [] -> None
        | li -> Some (oneof li)
      and le_concat lexpr env ty n =
        match ty.desc with
        | T_Bits _ ->
            Some
              ( Nat.list_sized_non_empty
                  (fun n -> t_bits >>= fun t -> lexpr (env, t, n))
                  n
              >|= fun li -> LE_Concat (li, None) |> annot )
        | _ -> None
      and le_slices lexpr env ty n =
        match ty.desc with
        | T_Bits _ ->
            Some
              (let* n2 = 1 -- n in
               let n1 = n - n2 in
               let+ le' = lexpr (env, ty, n1) and+ slices = slices env n2 in
               LE_Slice (le', slices) |> annot)
        | _ -> None
      in
      fix @@ fun lexpr (env, ty, n) ->
      let () =
        if _dbg then Printf.eprintf "Generating lexpr of size %d\n%!" n
      in
      let n = pay n in
      let ty_anon = Types.make_anonymous env ty in
      [
        (if C.Syntax.le_discard then Some le_discard else None);
        (if C.Syntax.le_var then le_var env ty_anon else None);
        (if C.Syntax.le_concat then le_tuple lexpr env ty_anon n else None);
        (if C.Syntax.le_setfield && n >= 1 then le_set_field lexpr env ty_anon n
         else None);
        (if C.Syntax.le_concat && n >= 1 then le_concat lexpr env ty_anon n
         else None);
        (if C.Syntax.le_slice && n >= 1 then le_slices lexpr env ty_anon n
         else None);
      ]
      |> filter_oneof
    in
    fun env ty n -> lexpr' (env, ty, n)

  let stmt : env -> (stmt * env) sgen =
    let s_block stmt env n =
      if n <= 0 then pure (S_Pass |> annot, env)
      else
        let* n_elements = 1 -- n in
        let* sizes = Nat.split ~size:n_elements n in
        let* env, prevs =
          List.fold_left
            (fun m n ->
              let* env, prevs = m in
              let* s, env = stmt (env, n) in
              return (env, s :: prevs))
            (pure (env, []))
            sizes
        in
        let s = List.rev prevs |> stmt_from_list in
        return (s, env)
    in
    let s_assert env n =
      let+ e = expr env boolean n in
      (S_Assert e |> annot, env)
    in
    let s_assign env n =
      let* n1 = 1 -- (n / 3) in
      let* ty = ty false env ~max:n1 n1 in
      let min = minimal_direct_fuel_ty env ty in
      let* n2 = min -- (n - min) in
      let n3 = n - n1 - n2 in
      let+ le = lexpr env ty n2 and+ e = expr env ty n3 in
      (S_Assign (le, e, V1) |> annot, env)
    in
    let s_decl env n =
      let* n1, n2 = Nat.split2 n in
      let* ty = ty false env ~max:n2 n1 in
      let+ e = expr env ty n2
      and+ ldk = Untyped.ldk
      and+ name = fresh_name env in
      ( S_Decl (ldk, LDI_Typed (LDI_Var name, ty), Some e) |> annot,
        StaticEnv.add_local name ty ldk env )
    in
    let s_return env n =
      match env.StaticEnv.local.return_type with
      | Some t ->
          let+ e = expr env t n in
          (S_Return (Some e) |> annot, env)
      | None -> pure (S_Return None |> annot, env)
    in
    let s_cond stmt env n =
      let* n1, n2, n3 = Nat.split3 n in
      let+ e1 = expr env boolean n1
      and+ s2, _env2 = stmt (env, n2)
      and+ s3, _env3 = stmt (env, n3) in
      (S_Cond (e1, s2, s3) |> annot, env)
    in
    let stmt' =
      fix @@ fun stmt (env, n) ->
      let () =
        if _dbg then Printf.eprintf "Generating a stmt of size %d\n%!" n
      in
      let n = pay n in
      [
        (if C.Syntax.s_decl then Some (s_decl env n) else None);
        (if C.Syntax.s_assert then Some (s_assert env n) else None);
        (if C.Syntax.s_return then Some (s_return env n) else None);
        (if C.Syntax.s_pass then Some (s_block stmt env n) else None);
        (if C.Syntax.s_assign && n >= 3 then Some (s_assign env n) else None);
        (if C.Syntax.s_cond && n >= 3 then Some (s_cond stmt env n) else None);
      ]
      |> filter_oneof
    in
    fun env n -> stmt' (env, n)

  let decl : env -> (decl * env) sgen =
    let type_decl env n : (decl * env) gen =
      let+ ty = ty true env n and+ name = fresh_name env in
      (D_TypeDecl (name, ty, None) |> annot, StaticEnv.add_type name ty env)
    and global_decl env n : (decl * env) gen =
      let* n1, n2 = Nat.split2 n in
      let* ty = ty false env ~max:n2 n1 in
      let+ name = fresh_name env
      and+ keyword = Untyped.gdk
      and+ initial_value = expr env ty n2 |> option in
      ( D_GlobalStorage { name; keyword; ty = Some ty; initial_value } |> annot,
        StaticEnv.add_global_storage name ty keyword env )
    and func env n : (decl * env) gen =
      let* n2 = int_bound ((n / 2) + 1) in
      let* n3 = int_bound ((n / 4) + 1) in
      let n1 = n - n2 - n3 in
      let parameters = [] in
      let* return_type = ty false env n3 |> option in
      let env' =
        StaticEnv.
          { global = env.global; local = empty_local_return_type return_type }
      in
      let* args, env' =
        let* n_args = int_bound n2 in
        let* arg_sizes = Nat.split ~size:n_args n2 in
        List.fold_left
          (fun acc n ->
            let* prevs, env = acc in
            let+ name = fresh_name env and+ ty = ty false env n in
            ((name, ty) :: prevs, StaticEnv.add_local name ty LDK_Let env))
          (pure ([], env'))
          arg_sizes
      in
      let+ name = names
      and+ (body : subprogram_body) =
        stmt env' n1 >|= fun (s, _env') -> SB_ASL s
      and+ subprogram_type =
        match return_type with
        | Some _ -> oneofa [| ST_Function; ST_Getter |]
        | None ->
            if List.length args > 0 then oneofa [| ST_Procedure; ST_Setter |]
            else pure ST_Procedure
      in
      let func_sig =
        { name; parameters; args; body; return_type; subprogram_type }
      in
      (D_Func func_sig |> annot, StaticEnv.add_subprogram name func_sig env)
    in
    fun env n ->
      let () =
        if _dbg then Printf.eprintf "Generating a decl of size %d\n%!" n
      in
      let n = pay n in
      [ type_decl env n ]
      |> (if n >= 2 then List.cons (global_decl env n) else Fun.id)
      |> (if n >= 3 then List.cons (func env n) else Fun.id)
      |> oneof

  let main =
    let parameters = []
    and args = []
    and return_type = Some integer
    and subprogram_type = ST_Procedure
    and name = "main" in
    fun env n ->
      let n = pay n in
      let env' =
        StaticEnv.
          { global = env.global; local = empty_local_return_type return_type }
      in
      let+ body = stmt env' n >|= fun (s, _env') -> SB_ASL s in
      let func_sig =
        { name; parameters; args; body; return_type; subprogram_type }
      in
      D_Func func_sig |> annot

  let ast : AST.t sgen = function
    | 0 -> pure []
    | n ->
        let () =
          if _dbg then Printf.eprintf "Generating ast of size %d\n%!" n
        in
        let* n_main = 1 -- ((n + 4) / 5) in
        let n = n - n_main in
        let* n_decl = if n = 0 then pure 0 else 1 -- ((n + 4) / 5) in
        let* sizes = Nat.split ~size:n_decl n in
        let* decls, env =
          List.fold_left
            (fun acc n ->
              let* prevs, env = acc in
              let+ d, env = decl env n in
              (d :: prevs, env))
            (return ([], StaticEnv.empty))
            sizes
        in
        let+ main = main env n_main in
        let () = if _dbg then Printf.eprintf "Generated ast.\n%!" in
        List.rev (main :: decls)
end
