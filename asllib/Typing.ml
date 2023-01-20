open AST
module IMap = ASTUtils.IMap

let fatal = Error.fatal
let not_yet_implemented s = fatal (Error.NotYetImplemented s)
let undefined_identifier x = fatal (Error.UndefinedIdentifier x)
let bad_field x ty = fatal (Error.BadField (x, ty))

let conflict expected provided =
  fatal (Error.ConflictingTypes (expected, provided))

(******************************************************************************)
(*                                                                            *)
(*                               Environments                                 *)
(*                                                                            *)
(******************************************************************************)

type genv = type_desc IMap.t
(** Type environment for all globally declared identifiers.
    Note that this is shared between named_types and global variables, but not
    functions, getter, setters, and all subprograms.
    In asl-semantics, it is refered to as Γ.T : TypeModel.
*)

type func_sig = type_desc list * type_desc option
(** Type signature for functions, some kind of an arrow type. *)

type func_tr = (type_desc list * AST.identifier) list IMap.t
(** function renaming to get unique identifiers. *)

type tenv = { globals : genv; funcs : func_sig IMap.t; func_tr : func_tr }
(** The global type environment, with types for every globally available
    identifier, i.e. variables, named types or functions .*)

type lenv = type_desc IMap.t
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

let lookup tenv lenv x =
  match lookup_opt tenv lenv x with
  | Some ty -> ty
  | None -> undefined_identifier x

let lookup_return_type (tenv : tenv) x =
  match IMap.find_opt x tenv.funcs with
  | Some (_, Some ty) -> ty
  | Some (_, None) -> fatal @@ Error.MismatchedReturnValue x
  | None -> undefined_identifier x

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

let expr_of_int i = E_Literal (V_Int i)

let slices_length =
  let plus e1 e2 = E_Binop (PLUS, e1, e2) in
  let minus e1 e2 = E_Binop (MINUS, e1, e2) in
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

let field_type x ty =
  match ty with
  | T_Record li -> (
      match List.assoc_opt x li with Some ty -> ty | None -> bad_field x ty)
  | T_Bits (_, Some fields) -> (
      match List.find_opt (fun (_, y) -> String.equal x y) fields with
      | Some (slices, _) ->
          T_Bits (BitWidth_Determined (slices_length slices), None)
      | None -> bad_field x ty)
  | _ -> bad_field x ty

let get_structure (genv : genv) : type_desc -> type_desc =
  let rec get = function
    | T_Named x -> (
        match IMap.find_opt x genv with
        | None -> undefined_identifier x
        | Some ty -> get ty)
    | (T_Int _ | T_Real | T_String | T_Bool | T_Bits _ | T_Bit | T_Enum _) as ty
      ->
        ty
    | T_Tuple subtypes -> T_Tuple (List.map get subtypes)
    | T_Array (e, t) -> T_Array (e, get t)
    | T_Record fields -> T_Record (get_fields fields)
    | T_Exception fields -> T_Exception (get_fields fields)
    | T_ZType ty -> T_ZType (get ty)
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

let check_bitvector = function
  | T_Bits _ as t -> t
  | ty -> conflict [ ASTUtils.default_t_bits ] ty

let check_integer = function
  | T_Int _ as t -> t
  | ty -> conflict [ T_Int None ] ty

let check_num = function
  | (T_Int _ | T_Bits _ | T_Real) as t -> t
  | ty -> conflict [ T_Int None; ASTUtils.default_t_bits; T_Real ] ty

let t_bits_bitwidth e = T_Bits (BitWidth_Determined e, None)

let infer_values = function
  | V_Int i -> T_Int (Some [ Constraint_Exact (expr_of_int i) ])
  | V_Bool _ -> T_Bool
  | V_Real _ -> T_Real
  | V_BitVector bv -> Bitvector.length bv |> expr_of_int |> t_bits_bitwidth
  | _ -> not_yet_implemented "static complex values"

let rec infer tenv lenv = function
  | E_Literal v -> infer_values v
  | E_Var n -> lookup tenv lenv n
  | E_Binop (op, e1, e2) -> infer_op op tenv lenv e1 e2
  | E_Unop (unop, e) -> infer_unop unop tenv lenv e
  | E_Call (name, _) -> lookup_return_type tenv name
  | E_Slice (E_Var name, _) when IMap.mem name tenv.funcs ->
      lookup_return_type tenv name
  | E_Slice (e, slices) -> (
      match infer tenv lenv e with
      | T_Bits _ | T_Int _ -> slices_length slices |> t_bits_bitwidth
      | t -> conflict [ ASTUtils.default_t_bits; T_Int None ] t)
  | E_Cond (_e1, e2, e3) -> (
      match infer tenv lenv e2 with
      | T_Int None -> T_Int None
      | T_Int (Some c2) -> (
          match infer tenv lenv e3 with
          | T_Int (Some c3) -> T_Int (Some (c2 @ c3))
          | _ -> T_Int None)
      | t -> t)
  | E_GetField (e, x, ta) -> (
      match ta with
      | TA_None -> infer tenv lenv e |> field_type x
      | TA_InferredStructure ty -> field_type x ty)
  | E_Record (ty, _, ta) -> (
      match ta with
      | TA_None -> get_structure tenv.globals ty
      | TA_InferredStructure ty -> ty)

and infer_op op =
  match op with
  | AND | EOR | OR -> bitwise_op
  | BAND | BEQ | BOR | IMPL | EQ_OP | NEQ | GT | GEQ | LT | LEQ -> bool_op
  | DIV | MOD | SHL | SHR -> int_int_op
  | MINUS | MUL | PLUS -> num_num_op
  | RDIV -> not_yet_implemented "Real operations"

and bool_op _ _ _ _ = T_Bool
and bitwise_op tenv lenv e1 _e2 = infer tenv lenv e1 |> check_bitvector
and int_int_op tenv lenv e1 _e2 = infer tenv lenv e1 |> check_integer
and num_num_op tenv lenv e1 _e2 = infer tenv lenv e1 |> check_num

and infer_unop op tenv lenv e =
  match op with
  | BNOT -> T_Bool
  | NOT -> infer tenv lenv e |> check_bitvector
  | NEG -> infer tenv lenv e |> check_integer

let rec infer_lexpr tenv lenv = function
  | LE_Var x -> lookup tenv lenv x
  | LE_Slice (LE_Var x, _) when IMap.mem x tenv.funcs ->
      lookup_return_type tenv x
  | LE_Slice (le, slices) -> (
      match infer_lexpr tenv lenv le with
      | T_Bits _ -> slices_length slices |> t_bits_bitwidth
      | t -> conflict [ ASTUtils.default_t_bits ] t)
  | LE_SetField (_, field, TA_InferredStructure ty) -> field_type field ty
  | LE_SetField (le, field, TA_None) ->
      infer_lexpr tenv lenv le |> field_type field

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

let rec annotate_expr tenv lenv e =
  let tr = annotate_expr tenv lenv in
  match e with
  | E_Literal _ as e -> e
  | E_Var x -> (
      match getter_should_reduce_to_call tenv x [] with
      | None -> E_Var x
      | Some (name, args) -> E_Call (name, args) |> tr)
  | E_Binop (op, e1, e2) -> E_Binop (op, tr e1, tr e2)
  | E_Unop (op, e) -> E_Unop (op, tr e)
  | E_Call (x, args) ->
      let args = List.map tr args in
      let arg_types = List.map (infer tenv lenv) args in
      let x' = FunctionRenaming.find_name tenv.func_tr x arg_types in
      E_Call (x', List.map tr args)
  | E_Slice (e, slices) -> (
      let reduced =
        match e with
        | E_Var x -> getter_should_reduce_to_call tenv x slices
        | _ -> None
      in
      match reduced with
      | Some (name, args) -> E_Call (name, args) |> tr
      | None -> E_Slice (tr e, annotate_slices tenv lenv slices))
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

and annotate_slices tenv lenv =
  let tr_one = function
    | Slice_Single e -> Slice_Single (annotate_expr tenv lenv e)
    | Slice_Range (e1, e2) ->
        Slice_Range (annotate_expr tenv lenv e1, annotate_expr tenv lenv e2)
    | Slice_Length (e1, e2) ->
        Slice_Length (annotate_expr tenv lenv e1, annotate_expr tenv lenv e2)
  in
  List.map tr_one

let rec annotate_lexpr tenv lenv = function
  | LE_Var x -> LE_Var x
  | LE_Slice (le, slices) ->
      LE_Slice (annotate_lexpr tenv lenv le, annotate_slices tenv lenv slices)
  | LE_SetField (le, field, _ta) ->
      let le = annotate_lexpr tenv lenv le in
      let ty = infer_lexpr tenv lenv le in
      LE_SetField (le, field, TA_InferredStructure ty)

let rec annotate_stmt tenv lenv s =
  match s with
  | S_Pass -> (S_Pass, lenv)
  | S_Then (s1, s2) ->
      let s1, lenv = annotate_stmt tenv lenv s1 in
      let s2, lenv = annotate_stmt tenv lenv s2 in
      (S_Then (s1, s2), lenv)
  | S_Assign (le, e) -> (
      let reduced =
        match le with
        | LE_Var x -> setter_should_reduce_to_call tenv x [] e
        | LE_Slice (LE_Var x, slices) ->
            setter_should_reduce_to_call tenv x slices e
        | _ -> None
      in
      match reduced with
      | Some (name, args) -> S_Call (name, args) |> annotate_stmt tenv lenv
      | None ->
          let e = annotate_expr tenv lenv e
          and le = annotate_lexpr tenv lenv le in
          let lenv =
            match le with
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
  | S_Return es -> (S_Return (List.map (annotate_expr tenv lenv) es), lenv)
  | S_Cond (e, s1, s2) ->
      let e = annotate_expr tenv lenv e in
      let s1, lenv = try_annotate_stmt tenv lenv s1 in
      let s2, lenv = try_annotate_stmt tenv lenv s2 in
      (S_Cond (e, s1, s2), lenv)
  | S_Case (e, cases) ->
      let e = annotate_expr tenv lenv e in
      let annotate_case (acc, lenv) (es, s) =
        let es = List.map (annotate_expr tenv lenv) es in
        let s, lenv = try_annotate_stmt tenv lenv s in
        ((es, s) :: acc, lenv)
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
    | x, T_Named y -> (
        match IMap.find_opt y genv with
        | None -> undefined_identifier y
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
