(****************************************************************************************************************)
(*  SPDX-FileCopyrightText: Copyright 2022-2024 Arm Limited and/or its affiliates <open-source-office@arm.com>  *)
(*  SPDX-License-Identifier: BSD-3-Clause                                                                       *)
(****************************************************************************************************************)

let filter_none li = List.filter_map Fun.id li

module Make (C : CConfig.S) = struct
  open Asllib
  open AST
  open Feat
  open Enum

  let annot desc = ASTUtils.add_dummy_pos desc

  (* Util not to forget to pay ==> make payment mandatory on recursion. *)
  let fix f = Fix.Memoize.Int.fix (fun blah -> f (pay blah))
  let enum_from_seq seq = seq |> Seq.map just |> Seq.fold_left ( ++ ) empty
  let rec payn n thing = if n <= 0 then thing else payn (n - 1) (pay thing)

  let scaled_finite li =
    let rec loop acc = function
      | [] -> acc
      | h :: t -> loop (just h ++ pay acc) t
    in
    match li with [] -> empty | h :: t -> loop (just h) t

  let unops : unop enum = scaled_finite [ NEG; BNOT; NOT ]

  let iterated (op : 'a -> 'a -> 'a) (li : 'a list) : 'a =
    let rec double_add acc = function
      | [] -> acc
      | [ x ] -> x :: acc
      | x1 :: x2 :: t -> double_add (op x1 x2 :: acc) t
    in
    let rec iterated = function
      | [] -> raise (Invalid_argument "iterated op")
      | [ x ] -> x
      | _ :: _ :: _ as li -> double_add [] li |> iterated
    in
    iterated li

  let oneof li = iterated sum li
  let option e = just None ++ pay (map Option.some e)
  let tuple3 x y z = x ** y ** z |> map (fun (x, (y, z)) -> (x, y, z))
  let nonempty_list x = x ** list x |> map (fun (x, li) -> x :: li)

  let binops : binop enum =
    [
      (if C.Syntax.plus then Some PLUS else None);
      (if C.Syntax.and_ then Some AND else None);
      (if C.Syntax.band then Some BAND else None);
      (if C.Syntax.beq then Some BEQ else None);
      (if C.Syntax.bor then Some BOR else None);
      (if C.Syntax.div then Some DIV else None);
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
      (if C.Syntax.rdiv then Some RDIV else None);
      (if C.Syntax.shl then Some SHL else None);
      (if C.Syntax.shr then Some SHR else None);
    ]
    |> filter_none |> scaled_finite

  let ints : int enum = fun i -> IFSeq.up (~-i + 1) i
  let uints : int enum = fun i -> IFSeq.up 0 i
  let nuints : int enum = fun i -> if i > 0 then IFSeq.up 1 i else IFSeq.empty

  let literals : literal enum =
    let l_ints = map (fun i -> L_Int (Z.of_int i)) ints
    and l_bools = finite [ L_Bool true; L_Bool false ]
    and l_reals = map (fun (i, j) -> L_Real Q.(i // j)) (uints ** nuints)
    and l_bvs =
      map
        (fun s -> L_BitVector (Bitvector.of_string ("'" ^ s ^ "'")))
        (fix (fun ss -> just "" ++ map (( ^ ) "0") ss ++ map (( ^ ) "1") ss))
    and l_strings =
      finite [ L_String "This is a string"; L_String "This is another string" ]
    in
    [
      (if C.Syntax.l_int then Some l_ints else None);
      (if C.Syntax.l_bool then Some l_bools else None);
      (if C.Syntax.l_real then Some l_reals else None);
      (if C.Syntax.l_bitvector then Some l_bvs else None);
      (if C.Syntax.l_string then Some l_strings else None);
    ]
    |> filter_none |> oneof

  let _make_vari i = "x" ^ string_of_int i
  let vars : identifier enum = finite [ "x"; "y" ] ++ pay (map _make_vari uints)
  let char_names = '_' :: List.init 26 Char.(fun i -> i + code 'a' |> chr)
  let list1 enum = enum ** list enum |> map (fun (x, xs) -> x :: xs) |> pay

  let list2 enum =
    (enum ** enum) ** list enum
    |> map (fun ((x, y), xs) -> x :: y :: xs)
    |> pay |> pay

  let names : identifier enum =
    let make li = li |> List.to_seq |> String.of_seq in
    list1 (scaled_finite char_names) |> map make

  let non_rec_ty =
    let t_string = just T_String
    and t_bool = just T_Bool
    and t_real = just T_Real in
    oneof [ t_string; t_bool; t_real ]

  let t_named s = T_Named s |> annot

  let slices exprs : slice list enum =
    let slice_single =
      let make_slice_single e = Slice_Single e in
      exprs |> map make_slice_single
    and slice_range =
      let make_slice_range (e1, e2) = Slice_Range (e1, e2) in
      exprs ** exprs |> map make_slice_range
    and slice_length =
      let make_slice_length (e1, e2) = Slice_Length (e1, e2) in
      exprs ** exprs |> map make_slice_length
    and slice_star =
      let make_slice_star (e1, e2) = Slice_Star (e1, e2) in
      exprs ** exprs |> map make_slice_star
    in
    [
      (if C.Syntax.slice_single then Some slice_single else None);
      (if C.Syntax.slice_range then Some slice_range else None);
      (if C.Syntax.slice_length then Some slice_length else None);
      (if C.Syntax.slice_star then Some slice_star else None);
    ]
    |> filter_none |> oneof |> list1

  let exprs : expr enum =
    fix @@ fun exprs ->
    let e_unops =
      let make_unop (op, expr) = E_Unop (op, expr) in
      unops ** exprs |> map make_unop
    and e_ctc =
      let make_e_ctc (e, s) = E_ATC (e, t_named s) in
      exprs ** names |> map make_e_ctc
    and e_binops =
      let make_binop (op, (e1, e2)) = E_Binop (op, e1, e2) in
      binops ** exprs ** exprs |> map make_binop
    and e_literals =
      let make_literal v = E_Literal v in
      literals |> map make_literal
    and e_vars =
      let make_var s = E_Var s in
      vars |> map make_var
    and e_conds =
      let make_cond (e1, (e2, e3)) = E_Cond (e1, e2, e3) in
      exprs ** exprs ** exprs |> map make_cond
    and e_slices =
      let make_slices (e, slices) = E_Slice (e, slices) in
      exprs ** slices exprs |> map make_slices
    and e_call =
      let make_e_call (name, args) = E_Call (name, args, []) in
      names ** list exprs |> map make_e_call
    and e_get_array =
      let make_e_get_array (e1, e2) = E_GetArray (e1, e2) in
      exprs ** exprs |> map make_e_get_array
    and e_get_field =
      let make_e_get_field (e, name) = E_GetField (e, name) in
      exprs ** names |> map make_e_get_field
    and e_get_fields =
      let make_e_get_fields (e, names) = E_GetFields (e, names) in
      exprs ** list2 names |> map make_e_get_fields
    and e_record =
      let make_record (name, fields) = E_Record (t_named name, fields) in
      names ** list (names ** exprs) |> map make_record
    and e_concat =
      let make_concat es = E_Concat es in
      list1 exprs |> map make_concat
    and e_tuple =
      let make_tuple es = E_Tuple es in
      list2 exprs |> map make_tuple
    and e_unknown =
      let make_unknown name = E_Unknown (t_named name) in
      names |> map make_unknown
    and e_pattern = empty (* TODO *) in
    [
      (if C.Syntax.e_unop then Some e_unops else None);
      (if C.Syntax.e_binop then Some e_binops else None);
      (if C.Syntax.e_literal then Some e_literals else None);
      (if C.Syntax.e_var then Some e_vars else None);
      (if C.Syntax.e_cond then Some e_conds else None);
      (if C.Syntax.e_slice then Some e_slices else None);
      (if C.Syntax.e_ctc then Some e_ctc else None);
      (if C.Syntax.e_call then Some e_call else None);
      (if C.Syntax.e_getarray then Some e_get_array else None);
      (if C.Syntax.e_getfield then Some e_get_field else None);
      (if C.Syntax.e_getfields then Some e_get_fields else None);
      (if C.Syntax.e_record then Some e_record else None);
      (if C.Syntax.e_concat then Some e_concat else None);
      (if C.Syntax.e_tuple then Some e_tuple else None);
      (if C.Syntax.e_unknown then Some e_unknown else None);
      (if C.Syntax.e_pattern then Some e_pattern else None);
    ]
    |> filter_none |> oneof |> map annot

  let slices = slices exprs

  let tys =
    fix @@ fun tys ->
    let t_string = just T_String
    and t_bool = just T_Bool
    and t_real = just T_Real
    and t_integer =
      let make_t_integer cs = T_Int (WellConstrained cs) in
      let cntt_range =
        let make_cntt_range (e1, e2) = Constraint_Range (e1, e2) in
        exprs ** exprs |> map make_cntt_range
      and cntt_single =
        let make_cntt_single e = Constraint_Exact e in
        exprs |> map make_cntt_single
      in
      just (T_Int UnConstrained)
      ++ (list1 (cntt_range ++ cntt_single) |> map make_t_integer)
    and t_tuple =
      let make_t_tuple li = T_Tuple li in
      list2 tys |> map make_t_tuple
    and t_record =
      let make_t_record li = T_Record li in
      names ** tys |> list |> map make_t_record
    and t_bits =
      let make_t_bits e = T_Bits (e, []) in
      exprs |> map make_t_bits |> pay
    and t_enum =
      let make_t_enum ss = T_Enum ss in
      nonempty_list names |> map make_t_enum
    and t_named =
      let make_t_named s = T_Named s in
      names |> map make_t_named
    in
    [
      (if C.Syntax.t_int then Some t_integer else None);
      (if C.Syntax.t_string then Some t_string else None);
      (if C.Syntax.t_bool then Some t_bool else None);
      (if C.Syntax.t_real then Some t_real else None);
      (if C.Syntax.t_bits then Some t_bits else None);
      (if C.Syntax.t_tuple then Some t_tuple else None);
      (if C.Syntax.t_record then Some t_record else None);
      (if C.Syntax.t_named then Some t_named else None);
      (if C.Syntax.t_enum then Some t_enum else None);
    ]
    |> filter_none |> oneof |> map annot

  let lexprs : lexpr enum =
    fix @@ fun lexprs ->
    let le_vars =
      let make_var s = LE_Var s in
      vars |> map make_var
    and le_ignore = just LE_Discard
    and le_concat =
      let make_le_concat les = LE_Concat (les, None) in
      list1 lexprs |> map make_le_concat
    and le_fields =
      let make_le_field (le, s) = LE_SetFields (le, s, []) in
      lexprs ** list2 names |> map make_le_field
    and le_field =
      let make_le_field (le, s) = LE_SetField (le, s) in
      lexprs ** names |> map make_le_field
    and le_destructuring =
      let make_destructuring les = LE_Destructuring les in
      list2 lexprs |> map make_destructuring
    and le_set_array =
      let make_le_set_array (le, e) = LE_SetArray (le, e) in
      lexprs ** exprs |> map make_le_set_array
    and le_slices =
      let make_le_slices (le, slices) = LE_Slice (le, slices) in
      lexprs ** slices |> map make_le_slices
    in
    [
      (if C.Syntax.le_var then Some le_vars else None);
      (if C.Syntax.le_discard then Some le_ignore else None);
      (if C.Syntax.le_concat then Some le_concat else None);
      (if C.Syntax.le_setfield then Some le_field else None);
      (if C.Syntax.le_setfields then Some le_fields else None);
      (if C.Syntax.le_destructuring then Some le_destructuring else None);
      (if C.Syntax.le_setarray then Some le_set_array else None);
      (if C.Syntax.le_slice then Some le_slices else None);
    ]
    |> filter_none |> oneof |> map annot

  let ldks = scaled_finite [ LDK_Constant; LDK_Var; LDK_Let ]

  let ldis =
    fix @@ fun ldis ->
    let ldi_discard = just LDI_Discard
    and ldi_var =
      let make_ldi_var s = LDI_Var s in
      vars |> map make_ldi_var
    and ldi_tuple =
      let make_ldi_tuple ldis = LDI_Tuple ldis in
      list2 ldis |> map make_ldi_tuple
    and ldi_typed =
      let make_ldi_typed (ldi, ty) = LDI_Typed (ldi, ty) in
      ldis ** tys |> map make_ldi_typed
    in
    [
      (if C.Syntax.ldi_discard then Some ldi_discard else None);
      (if C.Syntax.ldi_var then Some ldi_var else None);
      (if C.Syntax.ldi_typed then Some ldi_typed else None);
      (if C.Syntax.ldi_tuple then Some ldi_tuple else None);
    ]
    |> filter_none |> oneof

  let stmt_lists (stmts : stmt enum) : stmt enum =
    list stmts |> map ASTUtils.stmt_from_list

  let _make_mains_of_stmts =
    let make_main stmt =
      let name = "main"
      and args = []
      and parameters = []
      and subprogram_type = ST_Function
      and body = SB_ASL stmt
      and return_type = Some ASTUtils.integer in
      [
        D_Func { body; name; args; parameters; return_type; subprogram_type }
        |> annot;
      ]
    in
    map make_main

  let stmts : stmt enum =
    fix @@ fun stmts ->
    let block = stmt_lists stmts in
    let s_assigns =
      let make_assign (le, e) = S_Assign (le, e, V1) in
      lexprs ** exprs |> map make_assign
    and s_conds =
      let make_cond (e, (s1, s2)) = S_Cond (e, s1, s2) in
      exprs ** block ** block |> map make_cond
    and s_assert =
      let make_assert e = S_Assert e in
      exprs |> map make_assert
    and s_call =
      let make_s_call (name, args) = S_Call (name, args, []) in
      names ** list exprs |> map make_s_call
    and s_return =
      let make_s_return expr = S_Return expr in
      option exprs |> map make_s_return
    and s_decl =
      let make_s_decl (ldk, (ldi, expr_opt)) = S_Decl (ldk, ldi, expr_opt) in
      ldks ** ldis ** option exprs |> map make_s_decl
    and s_for =
      let make_s_for (name, (e1, (d, (e2, s)))) = S_For (name, e1, d, e2, s) in
      names ** exprs ** finite [ Up; Down ] ** exprs ** block |> map make_s_for
    and s_while =
      let make_s_while (e, s) = S_While (e, s) in
      exprs ** block |> map make_s_while |> pay
    and s_repeat =
      let make_s_repeat (s, e) = S_Repeat (s, e) in
      block ** exprs |> map make_s_repeat |> pay
    and s_throw =
      let make_s_throw opt = S_Throw opt in
      option (exprs ** option tys) |> map make_s_throw |> pay
    and s_try =
      let make_s_try (s, (catchers, s_opt)) = S_Try (s, catchers, s_opt) in
      let catcher = tuple3 (option names) tys block in
      block ** list catcher ** option block |> map make_s_try |> pay
    in
    [
      (if C.Syntax.s_assign then Some s_assigns else None);
      (if C.Syntax.s_cond then Some s_conds else None);
      (if C.Syntax.s_call then Some s_call else None);
      (if C.Syntax.s_assert then Some s_assert else None);
      (if C.Syntax.s_return then Some s_return else None);
      (if C.Syntax.s_decl then Some s_decl else None);
      (if C.Syntax.s_for then Some s_for else None);
      (if C.Syntax.s_while then Some s_while else None);
      (if C.Syntax.s_repeat then Some s_repeat else None);
      (if C.Syntax.s_throw then Some s_throw else None);
      (if C.Syntax.s_try then Some s_try else None);
    ]
    |> filter_none |> oneof |> map annot

  let mains : AST.t enum = _make_mains_of_stmts (stmt_lists stmts)

  let no_double_none a b =
    let a = pay (map Option.some a) and b = pay (map Option.some b) in
    (a ** b) ++ (a ** just None) ++ (just None ** b)

  let gdks = scaled_finite [ GDK_Config; GDK_Let; GDK_Constant; GDK_Var ]

  let decls =
    let d_func =
      let make_func (name, (body, (return_type, (args, subprogram_type)))) =
        let parameters = [] and body = SB_ASL body in
        D_Func { name; parameters; args; body; return_type; subprogram_type }
      in
      let args = vars ** tys in
      let subpgm_types =
        let setters = nonempty_list args ** just ST_Setter in
        let with_args typ = list args ** just typ in
        [
          payn 3 setters;
          payn 2 (with_args ST_Getter);
          pay (with_args ST_Procedure);
          with_args ST_Function;
        ]
        |> oneof
      in
      names ** stmt_lists stmts ** option tys ** subpgm_types |> map make_func
    and d_global_storage =
      let make_global_decl (keyword, (name, (ty, initial_value))) =
        D_GlobalStorage { keyword; name; ty; initial_value }
      in
      gdks ** vars ** no_double_none tys exprs |> map make_global_decl
    and d_type_decl =
      let make_type_decl (name, ty) = D_TypeDecl (name, ty, None) in
      names ** tys |> map make_type_decl
    in
    d_func ++ d_global_storage ++ d_type_decl |> map annot

  let asts = list decls
end
