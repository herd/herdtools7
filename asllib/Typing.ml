open AST
module IMap = ASTUtils.IMap

type typing_error =
  | NotYetImplemented of string
  | UndefinedIdentifier of string
  | TypeError of string

exception TypingError of typing_error

let not_yet_implemented s = raise (TypingError (NotYetImplemented s))
let type_error s = raise (TypingError (TypeError s))
let undefined_identifier x = raise (TypingError (UndefinedIdentifier x))

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

type tenv = { globals : genv; funcs : func_sig IMap.t }
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
  | Some (_, None) ->
      type_error @@ "Function " ^ x
      ^ " does not return anything, cannot be used in an expression."
  | None -> undefined_identifier x

(******************************************************************************)
(*                                                                            *)
(*                         Type manipulation helpers                          *)
(*                                                                            *)
(******************************************************************************)

let field_type x = function
  | T_Record li as tvec -> (
      match List.assoc_opt x li with
      | Some ty -> ty
      | None ->
          type_error
            (Format.asprintf "@[No such field as '%s' on type@ %a.@]" x
               PP.pp_type_desc tvec))
  | ty ->
      type_error
        (Format.asprintf "@[Cannot get field %s on type@ %a.@]" x
           PP.pp_type_desc ty)

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

let check_bitvector s = function T_Bits _ as t -> t | _ -> type_error s
let check_integer s = function T_Int _ as t -> t | _ -> type_error s

let check_num s = function
  | (T_Int _ | T_Bits _ | T_Real) as t -> t
  | _ -> type_error s

let slices_length =
  let plus e1 e2 = E_Binop (PLUS, e1, e2) in
  let minus e1 e2 = E_Binop (MINUS, e1, e2) in
  let e_of_int i = E_Literal (V_Int i) in
  let sum = function
    | [] -> e_of_int 0
    | [ x ] -> x
    | h :: t -> List.fold_left plus h t
  in
  let slice_length = function
    | Slice_Single _ -> e_of_int 1
    | Slice_Length (_, e) -> e
    | Slice_Range (e1, e2) -> minus e1 e2
  in
  fun li -> List.map slice_length li |> sum

let infer_values = function
  | V_Int i -> T_Int (Some [ Constraint_Exact (E_Literal (V_Int i)) ])
  | V_Bool _ -> T_Bool
  | V_Real _ -> T_Real
  | V_BitVector s ->
      T_Bits (BitWidth_Determined (E_Literal (V_Int (String.length s))))
  | _ -> assert false

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
      | T_Bits _c -> T_Bits (BitWidth_Determined (slices_length slices))
      | t -> type_error ("Cannot slice a " ^ PP.type_desc_to_string t))
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
  | AND | EOR | OR -> bitwise_op op
  | BAND | BEQ | BOR | IMPL | EQ_OP | NEQ | GT | GEQ | LT | LEQ -> bool_op op
  | DIV | MOD | SHL | SHR -> int_int_op op
  | MINUS | MUL | PLUS -> num_num_op op
  | RDIV -> not_yet_implemented "Real operations"

and bool_op _ _ _ _ _ = T_Bool

and bitwise_op op tenv lenv e1 _e2 =
  infer tenv lenv e1
  |> check_bitvector
       ("Operator " ^ PP.binop_to_string op ^ " works on bitvectors.")

and int_int_op op tenv lenv e1 _e2 =
  infer tenv lenv e1
  |> check_integer ("Operator " ^ PP.binop_to_string op ^ " works on integers.")

and num_num_op op tenv lenv e1 _e2 =
  infer tenv lenv e1
  |> check_num
       ("Operator " ^ PP.binop_to_string op
      ^ " works on either bitvectors, integers or reals.")

and infer_unop op tenv lenv e =
  match op with
  | BNOT -> T_Bool
  | NOT ->
      infer tenv lenv e
      |> check_bitvector
           ("Operator " ^ PP.unop_to_string op ^ " only work on bitvectors.")
  | NEG ->
      infer tenv lenv e
      |> check_integer
           ("Operator " ^ PP.unop_to_string op ^ " only work on integers.")

let rec infer_lexpr tenv lenv = function
  | LE_Var x -> lookup tenv lenv x
  | LE_Slice (LE_Var x, _) when IMap.mem x tenv.funcs ->
      lookup_return_type tenv x
  | LE_Slice (le, slices) -> (
      match infer_lexpr tenv lenv le with
      | T_Bits _ -> T_Bits (BitWidth_Determined (slices_length slices))
      | t ->
          type_error ("Cannot set slices of a type" ^ PP.type_desc_to_string t))
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
      | Some _ -> Some (name, args))

let getter_should_reduce_to_call tenv x slices =
  let name = ASTUtils.getter_name x in
  should_reduce_to_call tenv name slices

let setter_should_reduce_to_call tenv x slices e =
  let name = ASTUtils.setter_name x and slices = Slice_Single e :: slices in
  should_reduce_to_call tenv name slices

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
  | E_Call (x, args) -> E_Call (x, List.map tr args)
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
      let e = tr e and ty = infer tenv lenv e in
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
      let s1, lenv = annotate_stmt tenv lenv s1 in
      let s2, lenv = annotate_stmt tenv lenv s2 in
      (S_Cond (e, s1, s2), lenv)
  | S_Case (e, cases) ->
      let e = annotate_expr tenv lenv e in
      let annotate_case (acc, lenv) (es, s) =
        let es = List.map (annotate_expr tenv lenv) es in
        let s, lenv = annotate_stmt tenv lenv s in
        ((es, s) :: acc, lenv)
      in
      let cases, lenv = List.fold_left annotate_case ([], lenv) cases in
      (S_Case (e, cases), lenv)

let annotate_func (tenv : tenv) (f : AST.func) : AST.func =
  let lenv =
    let one_arg (name, ty) = (name, get_structure tenv.globals ty) in
    f.args |> List.to_seq |> Seq.map one_arg |> IMap.of_seq
  in
  let body, _lenv = annotate_stmt tenv lenv f.body in
  { f with body }

(******************************************************************************)
(*                                                                            *)
(*                           Global env and funcs                             *)
(*                                                                            *)
(******************************************************************************)

let build_funcs genv : AST.t -> func_sig IMap.t =
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
        Some (name, (args, return_type))
    | _ -> None
  in
  fun ast -> List.to_seq ast |> Seq.filter_map one_func |> IMap.of_seq

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
  let funcs = build_funcs globals ast in
  let one_decl = function
    | D_Func f -> D_Func (annotate_func { globals; funcs } f)
    | d -> d
  in
  List.map one_decl ast
