open AST
module IMap = ASTUtils.IMap
module ISet = ASTUtils.ISet

let fatal_from = Error.fatal_from
let not_yet_implemented pos s = fatal_from pos (Error.NotYetImplemented s)
let undefined_identifier pos x = fatal_from pos (Error.UndefinedIdentifier x)
let bad_field pos x ty = fatal_from pos (Error.BadField (x, ty))

let conflict pos expected provided =
  fatal_from pos (Error.ConflictingTypes (expected, provided))

let add_dummy_pos = ASTUtils.add_dummy_pos
let add_pos_from = ASTUtils.add_pos_from_st
let get_desc { desc; _ } = desc

(* Control Warning outputs. *)
let _warn = false

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

type func_sig = (identifier * ty) list * ty option
(** Type signature for functions, some kind of an arrow type. *)

type func_tr = ((identifier * ty) list * AST.identifier) list IMap.t
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

(* -------------------------------------------------------------------------

                                Structures

   ------------------------------------------------------------------------- *)

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

(* --------------------------------------------------------------------------

                              Subtyping

   --------------------------------------------------------------------------*)

let rec subtypes tenv t1 t2 =
  match (t1.desc, t2.desc) with
  | T_Bits (BitWidth_Determined e1, _), T_Bits (BitWidth_Determined e2, _) ->
      Some [ (e1, e2) ]
  | T_Int _, T_Int None -> Some []
  | T_Int (Some _), T_Int (Some _) ->
      Some [] (* TODO should we add equations here? *)
  | T_Real, T_Real | T_String, T_String | T_Bool, T_Bool -> Some []
  | T_Tuple li1, T_Tuple li2 when List.compare_lengths li1 li2 = 0 ->
      let rec on_tuples acc li1 li2 =
        match (li1, li2) with
        | [], [] -> Some acc
        | st1 :: h1, st2 :: h2 -> (
            match subtypes tenv st1 st2 with
            | None -> None
            | Some eqs -> on_tuples (List.rev_append eqs acc) h1 h2)
        | _ -> assert false
      in
      on_tuples [] li1 li2
  | T_Array (e1, st1), T_Array (e2, st2) -> (
      match subtypes tenv st1 st2 with
      | None -> None
      | Some eqs -> Some ((e1, e2) :: eqs))
  | T_Record li1, T_Record li2 when List.compare_lengths li1 li2 = 0 ->
      let rec on_fields acc li1 li2 =
        match (li1, li2) with
        | [], [] -> Some acc
        | (x1, _) :: _, (x2, _) :: _ when String.compare x1 x2 != 0 -> None
        | (_, st1) :: h1, (_, st2) :: h2 -> (
            match subtypes tenv st1 st2 with
            | None -> None
            | Some eqs -> on_fields (List.rev_append eqs acc) h1 h2)
        | _ -> assert false
      in
      on_fields []
        (ASTUtils.canonical_fields li1)
        (ASTUtils.canonical_fields li2)
  | T_Named x, T_Named y when String.compare x y = 0 -> Some []
  | T_Named _, _ | _, T_Named _ ->
      subtypes tenv
        (get_structure tenv.globals t1)
        (get_structure tenv.globals t2)
  | T_Enum l1, T_Enum l2 when List.compare_lengths l1 l2 = 0 ->
      if List.for_all2 String.equal l1 l2 then Some [] else None
  | _ -> None

let subtypes =
  if true then subtypes
  else fun tenv t1 t2 ->
    let res = subtypes tenv t1 t2 in
    let () =
      if false then
        Format.eprintf "Subtypes %a :< %a : %B.@," PP.pp_ty t1 PP.pp_ty t2
          (Option.is_some res)
    in
    res

let eqs_as_exprs =
  let one (e1, e2) =
    match e2.desc with
    | E_Var x -> Some (x, e1)
    | _ ->
        if false && e1.desc <> e2.desc then
          Format.eprintf "@[Unsupported type equation:@ %a@ == %a.@]@."
            PP.pp_expr e1 PP.pp_expr e2;
        None
  in
  List.filter_map one

(* -------------------------------------------------------------------------

                        Functionnal polymorphism

   ------------------------------------------------------------------------- *)

module FunctionRenaming = struct
  let has_arg_clash tenv caller callee =
    if List.compare_lengths caller callee != 0 then None
    else
      let rec on_each acc caller callee =
        match (caller, callee) with
        | [], [] -> Some acc
        | t1 :: caller, (_, t2) :: callee -> (
            match subtypes tenv t1 t2 with
            | None -> None
            | Some eqs -> on_each (List.rev_append eqs acc) caller callee)
        | _ -> None
      in
      on_each [] caller callee

  let add_new_func tr_table name arg_types =
    match IMap.find_opt name !tr_table with
    | None ->
        tr_table := IMap.add name [ (arg_types, name) ] !tr_table;
        name
    | Some assoc_list ->
        let name' = name ^ "-" ^ string_of_int (List.length assoc_list) in
        tr_table := IMap.add name ((arg_types, name') :: assoc_list) !tr_table;
        name'

  let find_name tenv name caller_arg_types =
    match IMap.find_opt name tenv.func_tr with
    | None ->
        let () =
          if _warn then Format.eprintf "No found function named %s.@." name
        in
        ([], name)
        (* Will trigger runtime exception *)
    | Some assoc_list -> (
        let finder (callee_arg_types, name') =
          match has_arg_clash tenv caller_arg_types callee_arg_types with
          | None -> None
          | Some eqs -> Some (eqs, name')
        in
        match List.filter_map finder assoc_list with
        | [] ->
            let () =
              if _warn then
                Format.eprintf "No found function %s with the right types.@."
                  name
            in
            ([], name)
            (* Will trigger runtime exception *)
        | [ (eqs, name') ] -> (eqs_as_exprs eqs, name')
        | (eqs, name') :: _ ->
            let () =
              if _warn then
                Format.eprintf
                  "Ambiguous call to %s. Many conflicting declared functions."
                  name
            in
            (eqs_as_exprs eqs, name')
        (* If ambiguous, I don't know what happens *))

  let new_tr_table () = ref IMap.empty
  let to_tr_table tr_table_ref = IMap.map List.rev !tr_table_ref
end

(******************************************************************************)
(*                                                                            *)
(*                         Type manipulation helpers                          *)
(*                                                                            *)
(******************************************************************************)

let expr_of_int i = ASTUtils.literal (V_Int i)

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

let rename_ty_eqs (eqs : (AST.identifier * AST.expr) list) ty =
  let mapping = IMap.of_list eqs in
  match ty.desc with
  | T_Bits (BitWidth_Determined ({ desc = E_Var callee_var; _ } as e), fields)
    when IMap.mem callee_var mapping ->
      let new_e = IMap.find callee_var mapping |> ASTUtils.with_pos_from e in
      T_Bits (BitWidth_Determined new_e, fields) |> add_pos_from ty
  | _ -> ty

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

let infer_value = function
  | V_Int i -> T_Int (Some [ Constraint_Exact (expr_of_int i) ])
  | V_Bool _ -> T_Bool
  | V_Real _ -> T_Real
  | V_BitVector bv -> Bitvector.length bv |> expr_of_int |> t_bits_bitwidth
  | _ -> not_yet_implemented ASTUtils.dummy_annotated "static complex values"

let rec infer tenv lenv e =
  match e.desc with
  | E_Literal v -> infer_value v |> add_dummy_pos
  | E_Var n -> lookup tenv lenv e n
  | E_Typed (_e, t) -> get_structure tenv.globals t
  | E_Binop (op, e1, e2) -> infer_op op e tenv lenv e1 e2
  | E_Unop (unop, e') -> infer_unop unop e tenv lenv e'
  | E_Call (name, args, eqs) -> (
      match IMap.find_opt name tenv.funcs with
      | None -> undefined_identifier e name
      | Some (_, None) -> fatal_from e @@ Error.MismatchedReturnValue name
      | Some (args_types, _return_type) ->
          let () =
            if List.compare_lengths args_types args != 0 then
              fatal_from e
              @@ Error.BadArity (name, List.length args_types, List.length args)
          in
          let eqs =
            let folder acc (x, _) e = (x, e) :: acc in
            List.fold_left2 folder eqs args_types args
          in
          lookup_return_type tenv e name |> rename_ty_eqs eqs)
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
  | E_Unknown ty -> ty
  | E_Pattern _ -> T_Bool |> ASTUtils.add_pos_from e

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
  | LE_Typed (_le, t) -> get_structure tenv.globals t
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
      | Some (_arg_types, return_type) -> Some (name, args, return_type))

let getter_should_reduce_to_call tenv x slices =
  let name = ASTUtils.getter_name x in
  match should_reduce_to_call tenv name slices with
  | Some (name, args, Some _) ->
      let () =
        if false then Format.eprintf "Reducing call from %s to %s.@." x name
      in
      Some (name, args)
  | Some (_, _, None) | None -> None

let setter_should_reduce_to_call tenv x slices e =
  let name = ASTUtils.setter_name x and slices = Slice_Single e :: slices in
  match should_reduce_to_call tenv name slices with
  | Some (name, args, _) ->
      let () =
        if false then Format.eprintf "Reducing call from %s to %s.@." x name
      in
      Some (name, args)
  | None -> None

let rec annotate_expr tenv lenv e : expr =
  let tr = annotate_expr tenv lenv in
  let tr_desc d = add_pos_from e d |> tr |> get_desc in
  add_pos_from e
  @@
  match e.desc with
  | E_Literal _ -> e.desc
  | E_Typed (e, t) -> E_Typed (tr e, t)
  | E_Var x -> (
      match getter_should_reduce_to_call tenv x [] with
      | None -> e.desc
      | Some (name, args) -> E_Call (name, args, []) |> tr_desc)
  | E_Binop (BAND, e1, e2) ->
      E_Cond (e1, e2, E_Literal (V_Bool false) |> ASTUtils.add_pos_from e)
      |> tr_desc
  | E_Binop (BOR, e1, e2) ->
      E_Cond (e1, E_Literal (V_Bool true) |> ASTUtils.add_pos_from e, e2)
      |> tr_desc
  | E_Binop (op, e1, e2) -> E_Binop (op, tr e1, tr e2)
  | E_Unop (op, e) -> E_Unop (op, tr e)
  | E_Call (x, args, named_args) ->
      let args = List.map tr args in
      let arg_types = List.map (infer tenv lenv) args in
      let extra_nargs, x' = FunctionRenaming.find_name tenv x arg_types in
      let () =
        if false then Format.eprintf "Renaming call from %s to %s.@." x x'
      in
      E_Call (x', args, List.rev_append named_args extra_nargs)
  | E_Slice (e', slices) -> (
      let reduced =
        match e'.desc with
        | E_Var x -> getter_should_reduce_to_call tenv x slices
        | _ -> None
      in
      match reduced with
      | Some (name, args) -> E_Call (name, args, []) |> tr_desc
      | None -> E_Slice (tr e', annotate_slices tenv lenv slices))
  | E_Cond (e1, e2, e3) ->
      let e2 = try_annotate_expr tenv lenv e2
      and e3 = try_annotate_expr tenv lenv e3 in
      E_Cond (tr e1, e2, e3)
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
  | E_Unknown t -> E_Unknown (get_structure tenv.globals t)
  | E_Pattern (e, p) -> E_Pattern (tr e, p)

and try_annotate_expr tenv lenv e =
  match Error.intercept (fun () -> annotate_expr tenv lenv e) () with
  | Ok res -> res
  | Error err ->
      if _warn then
        Format.eprintf
          "@[<v 2>Ignoring type error:@ %a@;<1 -2>in expr:@ @[%a@]@]@."
          Error.pp_error err PP.pp_expr e;
      e

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
  | LE_Typed (le, t) -> LE_Typed (annotate_lexpr tenv lenv le, t)
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
      | Some (name, args) -> S_Call (name, args, []) |> tr_desc
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
            | LE_Typed ({ desc = LE_Var x; _ }, ty) ->
                IMap.add x (get_structure tenv.globals ty) lenv
            | _ -> lenv
          in
          (S_Assign (le, e), lenv))
  | S_Call (x, args, named_args) ->
      let args = List.map (annotate_expr tenv lenv) args in
      let arg_types = List.map (infer tenv lenv) args in
      let extra_nargs, x' = FunctionRenaming.find_name tenv x arg_types in
      (S_Call (x', args, List.rev_append named_args extra_nargs), lenv)
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
        let p, s = case.desc in
        let s, lenv = try_annotate_stmt tenv lenv s in
        (add_pos_from case (p, s) :: acc, lenv)
      in
      let cases, lenv = List.fold_left annotate_case ([], lenv) cases in
      (S_Case (e, List.rev cases), lenv)
  | S_Assert e -> (S_Assert (annotate_expr tenv lenv e), lenv)
  | S_TypeDecl (x, t) -> (s.desc, IMap.add x (get_structure tenv.globals t) lenv)

and try_annotate_stmt tenv lenv s =
  match Error.intercept (fun () -> annotate_stmt tenv lenv s) () with
  | Ok res -> res
  | Error e ->
      if _warn then
        Format.eprintf
          "@[<v 2>Ignoring type error:@ %a@;<1 -2>in stmt:@ @[<v>%a@]@]@."
          Error.pp_error e PP.pp_stmt s;
      (s, lenv)

let annotate_func (tenv : tenv) (f : AST.func) : AST.func =
  let () = if false then Format.eprintf "Annotating %s.@." f.name in
  (* Build typing local environment. *)
  let lenv =
    let one_arg acc (x, ty) = IMap.add x (get_structure tenv.globals ty) acc in
    List.fold_left one_arg IMap.empty f.args
  in
  (* Add dependently typed identifiers. *)
  let add_dependently_typed_from_ty lenv ty =
    match ty.desc with
    | T_Bits (BitWidth_Determined { desc = E_Var x; _ }, _) ->
        if IMap.mem x lenv then lenv
        else IMap.add x (T_Int None |> add_dummy_pos) lenv
    | _ -> lenv
  in
  (* Resolve dependently typed identifiers in the arguments. *)
  let lenv =
    let one_arg acc (_, ty) = add_dependently_typed_from_ty acc ty in
    List.fold_left one_arg lenv f.args
  in
  (* Resolve dependently typed identifiers in the result type. *)
  let lenv =
    match f.return_type with
    | None -> lenv
    | Some t -> add_dependently_typed_from_ty lenv t
  in
  (* Annotate body *)
  let body, _lenv = try_annotate_stmt tenv lenv f.body in
  (* Optionnally rename the function if needs be *)
  let one_arg (_x, t) = get_structure tenv.globals t in
  let args = List.map one_arg f.args in
  let _, name = FunctionRenaming.find_name tenv f.name args in
  let () =
    if false then
      Format.eprintf "Renaming decl of %s (%a) to %s.@." f.name
        (Format.pp_print_list PP.pp_ty)
        args name
  in
  { f with body; name }

let try_annotate_func tenv f =
  match Error.intercept (fun () -> annotate_func tenv f) () with
  | Ok res -> res
  | Error e ->
      if _warn then
        Format.eprintf
          "@[<v 2>Ignoring type error:@ %a@;<1 -2>in func:@ @[<v>%a@]@]@."
          Error.pp_error e PP.pp_t [ D_Func f ];
      f

(******************************************************************************)
(*                                                                            *)
(*                           Global env and funcs                             *)
(*                                                                            *)
(******************************************************************************)

let build_funcs genv : AST.t -> func_sig IMap.t * func_tr =
  let one_func tr_table_ref = function
    | D_Func { name; args; return_type; body = _; parameters = _ } ->
        let args =
          let one_arg (x, ty) = (x, get_structure genv ty) in
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
  let one_func tr_table_ref f =
    match Error.intercept (fun () -> one_func tr_table_ref f) () with
    | Ok res -> res
    | Error e ->
        if _warn then
          Format.eprintf
            "@[<v 2>Ignoring type error:@ %a@;<1 -2>in func:@ @[<v>%a@]@]@."
            Error.pp_error e PP.pp_t [ f ];
        None
  in
  fun ast ->
    let tr_table_ref = FunctionRenaming.new_tr_table () in
    let funcs =
      List.to_seq ast |> Seq.filter_map (one_func tr_table_ref) |> IMap.of_seq
    in
    let tr_table = FunctionRenaming.to_tr_table tr_table_ref in
    let () =
      if false then (
        Format.eprintf "@[<v 2>Function env:@ ";
        IMap.iter
          (fun name li ->
            List.iter
              (fun (arg, name') ->
                Format.eprintf "- @[<hv 2>%s (-> %s):@ @[<hv>%a@]@]@ " name
                  name'
                  (Format.pp_print_list ~pp_sep:Format.pp_print_space
                     (fun f (_, t) -> PP.pp_ty f t))
                  arg)
              li)
          tr_table;
        Format.eprintf "@]@.")
    in
    (funcs, tr_table)

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
  let one_decl acc = function
    | D_TypeDecl (name, ({ desc = T_Enum ids; _ } as ty)) ->
        let acc = IMap.add name ty acc in
        let id_type = T_Named name |> ASTUtils.add_dummy_pos in
        let add_one_id acc x = IMap.add x id_type acc in
        List.fold_left add_one_id acc ids
    | D_TypeDecl (name, ty) | D_GlobalConst (name, ty, _) ->
        IMap.add name ty acc
    | _ -> acc
  in
  List.fold_left one_decl IMap.empty

(******************************************************************************)
(*                                                                            *)
(*                                Entry point                                 *)
(*                                                                            *)
(******************************************************************************)

let annotate_ast ast =
  let globals = build_genv ast |> reduce_genv in
  let funcs, func_tr = build_funcs globals ast in
  let annotate_func = try_annotate_func { globals; funcs; func_tr } in
  let one_decl = function D_Func f -> D_Func (annotate_func f) | d -> d in
  List.map one_decl ast
