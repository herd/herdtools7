open AST
module IMap = ASTUtils.IMap

let fatal_from = Error.fatal_from
let not_yet_implemented pos s = fatal_from pos (Error.NotYetImplemented s)
let undefined_identifier pos x = fatal_from pos (Error.UndefinedIdentifier x)
let bad_field pos x ty = fatal_from pos (Error.BadField (x, ty))

let conflict pos expected provided =
  fatal_from pos (Error.ConflictingTypes (expected, provided))

(******************************************************************************)
(*                                                                            *)
(*                               Environments                                 *)
(*                                                                            *)
(******************************************************************************)

type genv = ty IMap.t
(** Type environment for all globally declared identifiers.
    Note that this is shared between named_types and global variables, but not
    functions, getter, setters, and all subprograms.
    In asl-semantics, it is refered to as Γ.T : TypeModel.
*)

type func_sig = ty list * ty option
(** Type signature for functions, some kind of an arrow type. *)

type func_tr = (ty list * AST.identifier) list IMap.t
(** function renaming to get unique identifiers. *)

type tenv = { globals : genv; funcs : func_sig IMap.t; func_tr : func_tr }
(** The global type environment, with types for every globally available
    identifier, i.e. variables, named types or functions .*)

type lenv = ty IMap.t
(** The local type environment, with types for all locally declared variables. *)

let lookup_opt (tenv : tenv) (lenv : lenv) x =
  let open IMap in
  match find_opt x lenv with
  | Some ty -> Some ty
  | None -> (
      match find_opt x tenv.globals with
      | Some ty -> Some ty
      | None -> (
          match find_opt x tenv.funcs with
          | Some ([], Some ty) -> Some ty
          | _ -> None))

let lookup tenv lenv pos x =
  match lookup_opt tenv lenv x with
  | Some ty -> ty
  | None -> undefined_identifier pos x

let lookup_return_type (tenv : tenv) pos x =
  match IMap.find_opt x tenv.funcs with
  | Some (_, Some ty) -> ty
  | Some (_, None) -> fatal_from pos @@ Error.MismatchedReturnValue x
  | None -> undefined_identifier pos x

module FunctionRenaming = struct
  let has_arg_clash l1 l2 = List.compare_lengths l1 l2 = 0

  let add_new_func tr_table name arg_types =
    match IMap.find_opt name !tr_table with
    | None ->
        tr_table := IMap.add name [ (arg_types, name) ] !tr_table;
        name
    | Some assoc_list ->
        let name' = name ^ "-" ^ string_of_int (List.length assoc_list) in
        tr_table := IMap.add name ((arg_types, name') :: assoc_list) !tr_table;
        name'

  let find_name tr_table name arg_types =
    match IMap.find_opt name tr_table with
    | None -> name
    | Some assoc_list -> (
        let finder (arg_types', _) = has_arg_clash arg_types' arg_types in
        match List.find_opt finder assoc_list with
        | None -> name
        | Some (_, name') -> name')

  let new_tr_table () = ref IMap.empty
end

(******************************************************************************)
(*                                                                            *)
(*                         Type manipulation helpers                          *)
(*                                                                            *)
(******************************************************************************)

let expr_of_int i = ASTUtils.literal (V_Int i)
let add_dummy_pos = ASTUtils.add_dummy_pos
let add_pos_from = ASTUtils.add_pos_from_st
let get_desc { desc; _ } = desc

let slices_length =
  let open ASTUtils in
  let plus = binop PLUS in
  let minus = binop MINUS in
  let sum = function
    | [] -> expr_of_int 0
    | [ x ] -> x
    | h :: t -> List.fold_left plus h t
  in
  let slice_length = function
    | Slice_Single _ -> expr_of_int 1
    | Slice_Length (_, e) -> e
    | Slice_Range (e1, e2) -> minus e1 e2
  in
  fun li -> List.map slice_length li |> sum

let field_type pos x ty =
  match ty.desc with
  | T_Record li -> (
      match List.assoc_opt x li with
      | Some ty -> ty
      | None -> bad_field pos x ty)
  | T_Bits (_, Some fields) -> (
      match List.find_opt (fun (_, y) -> String.equal x y) fields with
      | Some (slices, _) ->
          T_Bits (BitWidth_Determined (slices_length slices), None)
          |> add_dummy_pos
      | None -> bad_field pos x ty)
  | _ -> bad_field pos x ty

let get_structure (genv : genv) : ty -> ty =
  (* TODO: rethink to have physical equality when structural equality? *)
  let rec get ty =
    let with_pos = add_pos_from ty in
    match ty.desc with
    | T_Named x -> (
        match IMap.find_opt x genv with
        | None -> undefined_identifier ty x
        | Some ty -> get ty)
    | T_Int _ | T_Real | T_String | T_Bool | T_Bits _ | T_Bit | T_Enum _ -> ty
    | T_Tuple subtypes -> T_Tuple (List.map get subtypes) |> with_pos
    | T_Array (e, t) -> T_Array (e, get t) |> with_pos
    | T_Record fields -> T_Record (get_fields fields) |> with_pos
    | T_Exception fields -> T_Exception (get_fields fields) |> with_pos
    | T_ZType ty -> T_ZType (get ty) |> with_pos
  and get_fields fields =
    let one_field (name, t) = (name, get t) in
    let fields = List.map one_field fields in
    ASTUtils.canonical_fields fields
  in
  get

(******************************************************************************)
(*                                                                            *)
(*                   Inference and type-checking helpers                      *)
(*                                                                            *)
(******************************************************************************)

let check_bitvector pos ty =
  match ty.desc with
  | T_Bits _ -> ty
  | _ -> conflict pos [ ASTUtils.default_t_bits ] ty

let check_integer pos ty =
  match ty.desc with T_Int _ -> ty | _ -> conflict pos [ T_Int None ] ty

let check_num pos ty =
  match ty.desc with
  | T_Int _ | T_Bits _ | T_Real -> ty
  | _ -> conflict pos [ T_Int None; ASTUtils.default_t_bits; T_Real ] ty

let t_bits_bitwidth e = T_Bits (BitWidth_Determined e, None)

let infer_values = function
  | V_Int i ->
      T_Int (Some [ Constraint_Exact (expr_of_int i) ]) |> add_dummy_pos
  | V_Bool _ -> T_Bool |> add_dummy_pos
  | V_Real _ -> T_Real |> add_dummy_pos
  | V_BitVector bv ->
      Bitvector.length bv |> expr_of_int |> t_bits_bitwidth |> add_dummy_pos
  | _ -> not_yet_implemented ASTUtils.dummy_annotated "static complex values"

let rec infer tenv lenv e =
  match e.desc with
  | E_Literal v -> infer_values v
  | E_Var n -> lookup tenv lenv e n
  | E_Binop (op, e1, e2) -> infer_op op e tenv lenv e1 e2
  | E_Unop (unop, e') -> infer_unop unop e tenv lenv e'
  | E_Call (name, _) -> lookup_return_type tenv e name
  | E_Slice ({ desc = E_Var name; _ }, _) when IMap.mem name tenv.funcs ->
      lookup_return_type tenv e name
  | E_Slice (e, slices) -> (
      let ty = infer tenv lenv e in
      match ty.desc with
      | T_Bits _ | T_Int _ ->
          slices_length slices |> t_bits_bitwidth |> add_dummy_pos
      | _ -> conflict e [ ASTUtils.default_t_bits; T_Int None ] ty)
  | E_Cond (_e1, e2, e3) -> (
      let ty2 = infer tenv lenv e2 in
      match ty2.desc with
      | T_Int None -> T_Int None |> add_dummy_pos
      | T_Int (Some c2) -> (
          let ty3 = infer tenv lenv e3 in
          match ty3.desc with
          | T_Int (Some c3) -> T_Int (Some (c2 @ c3)) |> add_dummy_pos
          | _ -> T_Int None |> add_dummy_pos)
      | _ -> ty2)
  | E_GetField (e, x, ta) -> (
      match ta with
      | TA_None -> infer tenv lenv e |> field_type e x
      | TA_InferredStructure ty -> field_type e x ty)
  | E_Record (ty, _, ta) -> (
      match ta with
      | TA_None -> get_structure tenv.globals ty
      | TA_InferredStructure ty -> ty)
  | E_Concat es ->
      let get_length acc e =
        let ty = infer tenv lenv e in
        match ty.desc with
        | T_Bits (BitWidth_Determined l, _) -> ASTUtils.binop PLUS acc l
        | T_Bits _ -> not_yet_implemented e "bitvector length inference"
        | _ -> conflict e [ ASTUtils.default_t_bits ] ty
      in
      let length = List.fold_left get_length (expr_of_int 0) es in
      t_bits_bitwidth length |> add_dummy_pos
  | E_Tuple es -> T_Tuple (List.map (infer tenv lenv) es) |> add_dummy_pos

and infer_op op =
  match op with
  | AND | EOR | OR -> bitwise_op
  | BAND | BEQ | BOR | IMPL | EQ_OP | NEQ | GT | GEQ | LT | LEQ -> bool_op
  | DIV | MOD | SHL | SHR -> int_int_op
  | MINUS | MUL | PLUS -> num_num_op
  | RDIV -> real_op

and bool_op _ _ _ _ _ = T_Bool |> add_dummy_pos
and bitwise_op pos tenv lenv e1 _e2 = infer tenv lenv e1 |> check_bitvector pos
and int_int_op pos tenv lenv e1 _e2 = infer tenv lenv e1 |> check_integer pos
and num_num_op pos tenv lenv e1 _e2 = infer tenv lenv e1 |> check_num pos
and real_op pos = not_yet_implemented pos "Real operations"

and infer_unop op pos tenv lenv e =
  match op with
  | BNOT -> T_Bool |> add_dummy_pos
  | NOT -> infer tenv lenv e |> check_bitvector pos
  | NEG -> infer tenv lenv e |> check_integer pos

let rec infer_lexpr tenv lenv le =
  match le.desc with
  | LE_Var x -> lookup tenv lenv le x
  | LE_Slice ({ desc = LE_Var x; _ }, _) when IMap.mem x tenv.funcs ->
      lookup_return_type tenv le x
  | LE_Slice (le', slices) -> (
      let ty = infer_lexpr tenv lenv le' in
      match ty.desc with
      | T_Bits _ -> slices_length slices |> t_bits_bitwidth |> add_dummy_pos
      | _ -> conflict le [ ASTUtils.default_t_bits ] ty)
  | LE_SetField (_, field, TA_InferredStructure ty) -> field_type le field ty
  | LE_SetField (le, field, TA_None) ->
      infer_lexpr tenv lenv le |> field_type le field
  | LE_Ignore -> not_yet_implemented le "Type inference of '-'"
  | LE_TupleUnpack les ->
      T_Tuple (List.map (infer_lexpr tenv lenv) les) |> add_dummy_pos

(******************************************************************************)
(*                                                                            *)
(*                               Annotate AST                                 *)
(*                                                                            *)
(******************************************************************************)

let should_reduce_to_call tenv name slices =
  let args =
    try Some (List.map ASTUtils.slice_as_single slices)
    with Invalid_argument _ -> None
  in
  match args with
  | None -> None
  | Some args -> (
      match IMap.find_opt name tenv.funcs with
      | None -> None
      | Some (arg_types, return_type) -> (
          match List.compare_lengths arg_types args with
          | 0 -> Some (name, args, return_type)
          | _ -> None))

let getter_should_reduce_to_call tenv x slices =
  let name = ASTUtils.getter_name x in
  match should_reduce_to_call tenv name slices with
  | Some (name, args, Some _) -> Some (name, args)
  | Some (_, _, None) | None -> None

let setter_should_reduce_to_call tenv x slices e =
  let name = ASTUtils.setter_name x and slices = Slice_Single e :: slices in
  match should_reduce_to_call tenv name slices with
  | Some (name, args, _) -> Some (name, args)
  | None -> None

let rec annotate_expr tenv lenv e : expr =
  let tr = annotate_expr tenv lenv in
  let tr_desc d = add_pos_from e d |> tr |> get_desc in
  add_pos_from e
  @@
  match e.desc with
  | E_Literal _ -> e.desc
  | E_Var x -> (
      match getter_should_reduce_to_call tenv x [] with
      | None -> e.desc
      | Some (name, args) -> E_Call (name, args) |> tr_desc)
  | E_Binop (op, e1, e2) -> E_Binop (op, tr e1, tr e2)
  | E_Unop (op, e) -> E_Unop (op, tr e)
  | E_Call (x, args) ->
      let args = List.map tr args in
      let arg_types = List.map (infer tenv lenv) args in
      let x' = FunctionRenaming.find_name tenv.func_tr x arg_types in
      E_Call (x', List.map tr args)
  | E_Slice (e', slices) -> (
      let reduced =
        match e'.desc with
        | E_Var x -> getter_should_reduce_to_call tenv x slices
        | _ -> None
      in
      match reduced with
      | Some (name, args) -> E_Call (name, args) |> tr_desc
      | None -> E_Slice (tr e', annotate_slices tenv lenv slices))
  | E_Cond (e1, e2, e3) -> E_Cond (tr e1, tr e2, tr e3)
  | E_GetField (e, field, _ta) ->
      let e = tr e in
      let ty = infer tenv lenv e in
      E_GetField (e, field, TA_InferredStructure ty)
  | E_Record (ty, fields, _ta) ->
      let ta = get_structure tenv.globals ty
      and fields =
        let one_field (name, e) = (name, tr e) in
        List.map one_field fields
      in
      E_Record (ty, fields, TA_InferredStructure ta)
  | E_Concat es -> E_Concat (List.map tr es)
  | E_Tuple es -> E_Tuple (List.map tr es)

and annotate_slices tenv lenv =
  let tr_one = function
    | Slice_Single e -> Slice_Single (annotate_expr tenv lenv e)
    | Slice_Range (e1, e2) ->
        Slice_Range (annotate_expr tenv lenv e1, annotate_expr tenv lenv e2)
    | Slice_Length (e1, e2) ->
        Slice_Length (annotate_expr tenv lenv e1, annotate_expr tenv lenv e2)
  in
  List.map tr_one

let rec annotate_lexpr tenv lenv le =
  add_pos_from le
  @@
  match le.desc with
  | LE_Var _ -> le.desc
  | LE_Slice (le, slices) ->
      LE_Slice (annotate_lexpr tenv lenv le, annotate_slices tenv lenv slices)
  | LE_SetField (le, field, _ta) ->
      let le = annotate_lexpr tenv lenv le in
      let ty = infer_lexpr tenv lenv le in
      LE_SetField (le, field, TA_InferredStructure ty)
  | LE_Ignore -> LE_Ignore
  | LE_TupleUnpack les ->
      LE_TupleUnpack (List.map (annotate_lexpr tenv lenv) les)

let rec annotate_stmt tenv lenv s =
  let tr_desc d =
    add_pos_from s d |> annotate_stmt tenv lenv |> fun ({ desc; _ }, lenv) ->
    (desc, lenv)
  in
  let add_pos (desc, lenv) = (add_pos_from s desc, lenv) in
  add_pos
  @@
  match s.desc with
  | S_Pass -> (S_Pass, lenv)
  | S_Then (s1, s2) ->
      let s1, lenv = annotate_stmt tenv lenv s1 in
      let s2, lenv = annotate_stmt tenv lenv s2 in
      (S_Then (s1, s2), lenv)
  | S_Assign (le, e) -> (
      let reduced =
        match le.desc with
        | LE_Var x -> setter_should_reduce_to_call tenv x [] e
        | LE_Slice ({ desc = LE_Var x; _ }, slices) ->
            setter_should_reduce_to_call tenv x slices e
        | _ -> None
      in
      match reduced with
      | Some (name, args) -> S_Call (name, args) |> tr_desc
      | None ->
          let e = annotate_expr tenv lenv e
          and le = annotate_lexpr tenv lenv le in
          let lenv =
            match le.desc with
            | LE_Var x -> (
                match lookup_opt tenv lenv x with
                | Some _ -> lenv (* Already declared *)
                | None ->
                    let ty = infer tenv lenv e in
                    IMap.add x ty lenv)
            | _ -> lenv
          in
          (S_Assign (le, e), lenv))
  | S_Call (x, args) ->
      (S_Call (x, List.map (annotate_expr tenv lenv) args), lenv)
  | S_Return (Some e) -> (S_Return (Some (annotate_expr tenv lenv e)), lenv)
  | S_Return None -> (S_Return None, lenv)
  | S_Cond (e, s1, s2) ->
      let e = annotate_expr tenv lenv e in
      let s1, lenv = try_annotate_stmt tenv lenv s1 in
      let s2, lenv = try_annotate_stmt tenv lenv s2 in
      (S_Cond (e, s1, s2), lenv)
  | S_Case (e, cases) ->
      let e = annotate_expr tenv lenv e in
      let annotate_case (acc, lenv) case =
        let es, s = case.desc in
        let es = List.map (annotate_expr tenv lenv) es in
        let s, lenv = try_annotate_stmt tenv lenv s in
        (add_pos_from case (es, s) :: acc, lenv)
      in
      let cases, lenv = List.fold_left annotate_case ([], lenv) cases in
      (S_Case (e, cases), lenv)
  | S_Assert e -> (S_Assert (annotate_expr tenv lenv e), lenv)

and try_annotate_stmt tenv lenv s =
  match Error.intercept (fun () -> annotate_stmt tenv lenv s) () with
  | Ok res -> res
  | Error e ->
      Format.eprintf "@[<hv 3>Ignoring type error:@ %a@]@." Error.pp_error e;
      (s, lenv)

let annotate_func (tenv : tenv) (f : AST.func) : AST.func =
  let lenv =
    let one_arg (name, ty) = (name, get_structure tenv.globals ty) in
    f.args |> List.to_seq |> Seq.map one_arg |> IMap.of_seq
  in
  let body, _lenv = try_annotate_stmt tenv lenv f.body in
  let name =
    FunctionRenaming.find_name tenv.func_tr f.name (List.map snd f.args)
  in
  { f with body; name }

(******************************************************************************)
(*                                                                            *)
(*                           Global env and funcs                             *)
(*                                                                            *)
(******************************************************************************)

let build_funcs genv : AST.t -> func_sig IMap.t * func_tr =
  let tr_table_ref = FunctionRenaming.new_tr_table () in
  let one_func = function
    | D_Func { name; args; return_type; body = _ } ->
        let args =
          let one_arg (_, ty) = get_structure genv ty in
          List.map one_arg args
        and return_type =
          match return_type with
          | None -> None
          | Some ty -> Some (get_structure genv ty)
        in
        let name' = FunctionRenaming.add_new_func tr_table_ref name args in
        Some (name', (args, return_type))
    | _ -> None
  in
  fun ast ->
    let funcs = List.to_seq ast |> Seq.filter_map one_func |> IMap.of_seq in
    (funcs, !tr_table_ref)

let reduce_genv : genv -> genv =
  let should_reduce genv = function
    | x, ({ desc = T_Named y; _ } as pos) -> (
        match IMap.find_opt y genv with
        | None -> undefined_identifier pos y
        | Some z -> Some (x, z))
    | _ -> None
  in
  let rec reduce genv =
    let edit_one (genv, _edited) (x, y) = (IMap.add x y genv, true) in
    let genv, edited =
      IMap.to_seq genv
      |> Seq.filter_map (should_reduce genv)
      |> Seq.fold_left edit_one (genv, false)
    in
    if edited then reduce genv else genv
  in
  reduce

let build_genv : AST.t -> genv =
  let one_decl = function
    | D_TypeDecl (name, ty) -> Some (name, ty)
    | D_GlobalConst (name, ty, _) -> Some (name, ty)
    | _ -> None
  in
  fun ast ->
    List.to_seq ast |> Seq.filter_map one_decl |> IMap.of_seq |> reduce_genv

(******************************************************************************)
(*                                                                            *)
(*                                Entry point                                 *)
(*                                                                            *)
(******************************************************************************)

let annotate_ast ast =
  let globals = build_genv ast in
  let funcs, func_tr = build_funcs globals ast in
  let annotate_func = annotate_func { globals; funcs; func_tr } in
  let one_decl = function D_Func f -> D_Func (annotate_func f) | d -> d in
  List.map one_decl ast
