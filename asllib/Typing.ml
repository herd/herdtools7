open AST

type scope = identifier

module IMap = ASTUtils.IMap

type tenv = {
  genv : type_desc IMap.t;
  funcs : (type_desc list * type_desc option) IMap.t;
  lenvs : type_desc IMap.t IMap.t;
}

type typing_error =
  | NotYetImplemented of string
  | UndefinedIdentifier of string
  | TypeError of string
  | Internal_InvalidScope of string

exception TypingError of typing_error

let not_yet_implemented s = raise (TypingError (NotYetImplemented s))
let type_error s = raise (TypingError (TypeError s))
let undefined_identifier x = raise (TypingError (UndefinedIdentifier x))

let lookup { genv; funcs; lenvs } scope x =
  let open IMap in
  match find_opt scope lenvs with
  | None -> raise (TypingError (Internal_InvalidScope scope))
  | Some lenv -> (
      match find_opt x lenv with
      | Some ty -> Some ty
      | None -> (
          match find_opt x genv with
          | Some ty -> Some ty
          | None -> (
              match find_opt x funcs with
              | Some ([], Some ty) -> Some ty
              | _ -> None)))

let lookup_return_type { funcs; _ } x =
  match IMap.find_opt x funcs with
  | Some (_, Some ty) -> ty
  | Some (_, None) ->
      type_error @@ "Function " ^ x
      ^ " does not return anything, cannot be used in an expression."
  | None -> undefined_identifier x

let build_funcs =
  let one_func = function
    | D_Func { name; args; return_type; body = _ } ->
        Some (name, (List.map snd args, return_type))
    | _ -> None
  in
  fun ast -> List.to_seq ast |> Seq.filter_map one_func |> IMap.of_seq

let infer_values = function
  | V_Int i -> T_Int (Some [ Constraint_Exact (E_Literal (V_Int i)) ])
  | V_Bool _ -> T_Bool
  | V_Real _ -> T_Real
  | V_BitVector s ->
      T_Bits (BitWidth_Determined (E_Literal (V_Int (String.length s))))
  | _ -> assert false

let check_bitvector s = function T_Bits _ as t -> t | _ -> type_error s
let check_integer s = function T_Int _ as t -> t | _ -> type_error s

let check_num s = function
  | (T_Int _ | T_Bits _ | T_Real) as t -> t
  | _ -> type_error s

let rec infer tenv scope = function
  | E_Literal v -> infer_values v
  | E_Var n -> (
      match lookup tenv scope n with
      | Some ty -> ty
      | None -> undefined_identifier n)
  | E_Binop (op, e1, e2) -> infer_op op tenv scope e1 e2
  | E_Unop (unop, e) -> infer_unop unop tenv scope e
  | E_Call (name, _args) | E_Get (name, _args) -> lookup_return_type tenv name
  | E_Cond (_e1, e2, e3) -> (
      match infer tenv scope e2 with
      | T_Int None -> T_Int None
      | T_Int (Some c2) -> (
          match infer tenv scope e3 with
          | T_Int (Some c3) -> T_Int (Some (c2 @ c3))
          | _ -> T_Int None)
      | t -> t)

and infer_op op =
  match op with
  | AND | EOR | OR -> bitwise_op op
  | BAND | BEQ | BOR | IMPL | EQ_OP | NEQ | GT | GEQ | LT | LEQ -> bool_op op
  | DIV | MOD | SHL | SHR -> int_int_op op
  | MINUS | MUL | PLUS -> num_num_op op
  | RDIV -> not_yet_implemented "Real operations"

and bool_op _ _ _ _ _ = T_Bool

and bitwise_op op tenv scope e1 _e2 =
  infer tenv scope e1
  |> check_bitvector
       ("Operator " ^ PP.binop_to_string op ^ " works on bitvectors.")

and int_int_op op tenv scope e1 _e2 =
  infer tenv scope e1
  |> check_integer ("Operator " ^ PP.binop_to_string op ^ " works on integers.")

and num_num_op op tenv scope e1 _e2 =
  infer tenv scope e1
  |> check_num
       ("Operator " ^ PP.binop_to_string op
      ^ " works on either bitvectors, integers or reals.")

and infer_unop op tenv scope e =
  match op with
  | BNOT -> T_Bool
  | NOT ->
      infer tenv scope e
      |> check_bitvector
           ("Operator " ^ PP.unop_to_string op ^ " only work on bitvectors.")
  | NEG ->
      infer tenv scope e
      |> check_integer
           ("Operator " ^ PP.unop_to_string op ^ " only work on integers.")

let rec build_lenv tenv scope = function
  | S_Pass -> tenv
  | S_Then (s1, s2) ->
      let tenv = build_lenv tenv scope s1 in
      build_lenv tenv scope s2
  | S_Assign (LEVar x, e) ->
      let lenv = IMap.find scope tenv.lenvs in
      if IMap.mem x lenv then tenv
      else
        let t = infer tenv scope e in
        let lenv = IMap.add x t lenv in
        let lenvs = IMap.add scope lenv tenv.lenvs in
        { tenv with lenvs }
  | S_Assign (LESet _, _) -> tenv
  | S_Call _ -> tenv
  | S_Return _ -> tenv
  | S_Cond (_e, s1, s2) ->
      let tenv = build_lenv tenv scope s1 in
      build_lenv tenv scope s2

let build_lenvs =
  let one_func tenv { name; args; body; _ } =
    let scope = name in
    let lenv = List.to_seq args |> IMap.of_seq in
    let lenvs = IMap.add name lenv tenv.lenvs in
    let tenv = { tenv with lenvs } in
    build_lenv tenv scope body
  in
  let one_decl tenv = function
    | D_Func func -> one_func tenv func
    | _ -> tenv
  in
  List.fold_left one_decl

let reduce_genv =
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

let build_genv =
  let one_decl = function
    | D_TypeDecl (name, ty) -> Some (name, ty)
    | _ -> None
  in
  fun ast ->
    List.to_seq ast |> Seq.filter_map one_decl |> IMap.of_seq |> reduce_genv

let build_tenv ast =
  let tenv =
    { genv = build_genv ast; funcs = build_funcs ast; lenvs = IMap.empty }
  in
  build_lenvs tenv ast
