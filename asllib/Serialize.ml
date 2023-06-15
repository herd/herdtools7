open AST
open Printf

type buffer = Buffer.t
type 'a printer = buffer -> 'a -> unit

let addb buf s = Buffer.add_string buf s

let with_buf f =
  (* Same default value as Stdlib.Printf *)
  let b = Buffer.create 64 in
  let () = f b in
  Buffer.contents b

let pp_list pp_elt buf =
  let pp_elt_with_sep elt =
    addb buf "; ";
    pp_elt buf elt
  in
  function
  | [] -> addb buf "[]"
  | h :: t ->
      addb buf "[";
      pp_elt buf h;
      List.iter pp_elt_with_sep t;
      addb buf "]"

let pp_option pp_some buf = function
  | None -> addb buf "None"
  | Some elt -> bprintf buf "Some (%a)" pp_some elt

let pp_pair pp_left pp_right f (left, right) =
  bprintf f "(%a, %a)" pp_left left pp_right right

let pp_pair_list pp_left pp_right = pp_list (pp_pair pp_left pp_right)
let pp_string f = bprintf f "%S"
let pp_id_assoc pp_elt = pp_pair_list pp_string pp_elt
let pp_annotated f buf { desc; _ } = bprintf buf "annot (%a)" f desc

let pp_binop : binop -> string = function
  | AND -> "AND"
  | BAND -> "BAND"
  | BEQ -> "BEQ"
  | BOR -> "BOR"
  | DIV -> "DIV"
  | EOR -> "EOR"
  | EQ_OP -> "EQ_OP"
  | GT -> "GT"
  | GEQ -> "GEQ"
  | IMPL -> "IMPL"
  | LT -> "LT"
  | LEQ -> "LEQ"
  | MOD -> "MOD"
  | MINUS -> "MINUS"
  | MUL -> "MUL"
  | NEQ -> "NEQ"
  | OR -> "OR"
  | PLUS -> "PLUS"
  | RDIV -> "RDIV"
  | SHL -> "SHL"
  | SHR -> "SHR"

let pp_unop = function BNOT -> "BNOT" | NOT -> "NOT" | NEG -> "NEG"

let rec pp_value f = function
  | V_Int i -> bprintf f "V_Int %d" i
  | V_Bool b -> bprintf f "V_Bool %B" b
  | V_Real r -> bprintf f "V_Real %F" r
  | V_BitVector bv ->
      bprintf f "V_BitVector (Bitvector.of_string %S)" (Bitvector.to_string bv)
  | V_Tuple li ->
      addb f "V_Tuple ";
      pp_list pp_value f li
  | V_Record li ->
      addb f "V_Record ";
      pp_field_assoc f li
  | V_Exception li ->
      addb f "V_Exception ";
      pp_field_assoc f li

and pp_field_assoc f = pp_list (pp_pair pp_string pp_value) f

let rec pp_expr =
  let pp_desc f = function
    | E_Literal v -> bprintf f "E_Literal (%a)" pp_value v
    | E_Var x -> bprintf f "E_Var %S" x
    | E_Typed (e, t) -> bprintf f "E_Typed (%a, %a)" pp_expr e pp_ty t
    | E_Binop (op, e1, e2) ->
        bprintf f "E_Binop (%s, %a, %a)" (pp_binop op) pp_expr e1 pp_expr e2
    | E_Unop (op, e) -> bprintf f "E_Unop (%s, %a)" (pp_unop op) pp_expr e
    | E_Call (name, args, named_args) ->
        bprintf f "E_Call (%S, %a, %a)" name pp_expr_list args
          (pp_id_assoc pp_expr) named_args
    | E_Slice (e, args) ->
        bprintf f "E_Slice (%a, %a)" pp_expr e pp_slice_list args
    | E_Cond (e1, e2, e3) ->
        bprintf f "E_Cond (%a, %a, %a)" pp_expr e1 pp_expr e2 pp_expr e3
    | E_GetField (e, x, _ta) ->
        bprintf f "E_GetField (%a, %S, None)" pp_expr e x
    | E_GetFields (e, x, _ta) ->
        bprintf f "E_GetFields (%a, %a, None)" pp_expr e (pp_list pp_string) x
    | E_Record (ty, li, _ta) ->
        bprintf f "E_Record (%a, %a, None)" pp_ty ty (pp_id_assoc pp_expr) li
    | E_Concat es ->
        addb f "E_Concat ";
        pp_list pp_expr f es
    | E_Tuple es ->
        addb f "E_Tuple ";
        pp_expr_list f es
    | E_Unknown ty -> bprintf f "E_Unknown (%a)" pp_ty ty
    | E_Pattern (e, p) -> bprintf f "E_Pattern (%a, %a)" pp_expr e pp_pattern p
  in
  fun f e -> pp_annotated pp_desc f e

and pp_expr_list f = pp_list pp_expr f
and pp_slice_list f = pp_list pp_slice f

and pp_slice f = function
  | Slice_Single e -> bprintf f "Slice_Single (%a)" pp_expr e
  | Slice_Range (e1, e2) ->
      bprintf f "Slice_Range (%a, %a)" pp_expr e1 pp_expr e2
  | Slice_Length (e1, e2) ->
      bprintf f "Slice_Length (%a, %a)" pp_expr e1 pp_expr e2

and pp_pattern f = function
  | Pattern_All -> addb f "Pattern_All"
  | Pattern_Any li ->
      addb f "Pattern_Any ";
      pp_list pp_pattern f li
  | Pattern_Geq e -> bprintf f "Pattern_Geq (%a)" pp_expr e
  | Pattern_Leq e -> bprintf f "Pattern_Leq (%a)" pp_expr e
  | Pattern_Mask s -> bprintf f "Pattern_Mask %S" s
  | Pattern_Not p -> bprintf f "Pattern_Not (%a)" pp_pattern p
  | Pattern_Range (e1, e2) ->
      bprintf f "Pattern_Range (%a, %a)" pp_expr e1 pp_expr e2
  | Pattern_Single e -> bprintf f "Pattern_Single (%a)" pp_expr e

and pp_ty =
  let pp_desc f = function
    | T_Int constraint_opt ->
        bprintf f "T_Int (%a)" (pp_option pp_int_constraint) constraint_opt
    | T_Real -> addb f "T_Real"
    | T_String -> addb f "T_String"
    | T_Bool -> addb f "T_Bool"
    | T_Bits (bits_constraint, fields) ->
        let pp_fields =
          pp_option @@ pp_list @@ pp_pair pp_string pp_slice_list
        in
        bprintf f "T_Bits (%a, %a)" pp_bits_constraint bits_constraint pp_fields
          fields
    | T_Enum enum_type_desc ->
        addb f "T_Enum ";
        pp_list pp_string f enum_type_desc
    | T_Tuple li ->
        addb f "T_Tuple ";
        pp_list pp_ty f li
    | T_Array (e, elt_type) ->
        bprintf f "T_Array (%a, %a)" pp_expr e pp_ty elt_type
    | T_Record li ->
        addb f "T_Record ";
        pp_id_assoc pp_ty f li
    | T_Exception li ->
        addb f "T_Exception ";
        pp_id_assoc pp_ty f li
    | T_ZType type_desc -> bprintf f "T_ZType (%a)" pp_ty type_desc
    | T_Named identifier -> bprintf f "T_Named %S" identifier
  in
  fun f s -> pp_annotated pp_desc f s

and pp_int_constraint f =
  let pp_one f = function
    | Constraint_Exact e -> bprintf f "Constraint_Exact (%a)" pp_expr e
    | Constraint_Range (bot, top) ->
        bprintf f "Constraint_Range (%a, %a)" pp_expr bot pp_expr top
  in
  pp_list pp_one f

and pp_bits_constraint f = function
  | BitWidth_Determined i -> bprintf f "BitWidth_Determined (%a)" pp_expr i
  | BitWidth_ConstrainedFormType ty ->
      bprintf f "BitWidth_ConstrainedFormType (%a)" pp_ty ty
  | BitWidth_Constrained int_constraint ->
      bprintf f "BitWidth_Constrained (%a)" pp_int_constraint int_constraint

let pp_typed_identifier = pp_pair pp_string pp_ty

let rec pp_lexpr =
  let pp_desc f = function
    | LE_Var x -> bprintf f "LE_Var %S" x
    | LE_Typed (le, t) -> bprintf f "E_Typed (%a, %a)" pp_lexpr le pp_ty t
    | LE_Slice (le, args) ->
        bprintf f "LE_Slice (%a, %a)" pp_lexpr le pp_slice_list args
    | LE_SetField (le, x, _ta) ->
        bprintf f "LE_Set_Field (%a, %S, None)" pp_lexpr le x
    | LE_SetFields (le, x, _ta) ->
        bprintf f "LE_SetFields (%a, %a, None)" pp_lexpr le (pp_list pp_string)
          x
    | LE_Ignore -> addb f "LE_Ignore"
    | LE_TupleUnpack les ->
        addb f "LE_TupleUnpack ";
        pp_list pp_lexpr f les
  in
  fun f le -> pp_annotated pp_desc f le

let rec pp_stmt =
  let pp_desc f = function
    | S_Pass -> addb f "SPass"
    | S_Then (s1, s2) -> bprintf f "S_Then (%a, %a)" pp_stmt s1 pp_stmt s2
    | S_Assign (le, e) -> bprintf f "S_Assign (%a, %a)" pp_lexpr le pp_expr e
    | S_Call (name, args, named_args) ->
        bprintf f "S_Call (%S, %a, %a)" name pp_expr_list args
          (pp_id_assoc pp_expr) named_args
    | S_Cond (e, s1, s2) ->
        bprintf f "S_Cond (%a, %a, %a)" pp_expr e pp_stmt s1 pp_stmt s2
    | S_Return e -> bprintf f "S_Return (%a)" (pp_option pp_expr) e
    | S_Case (e, cases) ->
        bprintf f "S_Case (%a, %a)" pp_expr e
          (pp_list (pp_annotated (pp_pair pp_pattern pp_stmt)))
          cases
    | S_Assert e -> bprintf f "S_Assert (%a)" pp_expr e
    | S_TypeDecl (x, t) -> bprintf f "S_TypeDecl (%S, %a)" x pp_ty t
    | S_While (e,s) ->
        bprintf f "S_While(%a, %a)" pp_expr e pp_stmt s
    | S_Repeat (s,e) ->
        bprintf f "S_Repeat(%a, %a)" pp_stmt s pp_expr e
    | S_For (id,e1,dir,e2,s) ->
        bprintf f
          "S_For (%S, %a, %s, %a, %a)"
          id pp_expr e1
          (match dir with Up -> "Up" | Down -> "Down")
          pp_expr e2
          pp_stmt s
  in
  fun f s -> pp_annotated pp_desc f s

let pp_decl f = function
  | D_Func { name; args; body; return_type; parameters = _ } ->
      bprintf f
        "D_Func { name=%S; args=%a; body=%a; return_type=%a; parameters=[] }"
        name (pp_id_assoc pp_ty) args pp_stmt body (pp_option pp_ty) return_type
  | D_GlobalConst (x, ty, e) ->
      bprintf f "D_GlobalConst (%S, %a, %a)" x pp_ty ty pp_expr e
  | D_TypeDecl (name, type_desc) ->
      bprintf f "D_TypeDecl (%S, %a)" name pp_ty type_desc
  | D_Primitive { name; args; return_type; body = _; parameters = _ } ->
      bprintf f "D_Primitive { name=%S; args=%a; body=S_Pass; return_type=%a }"
        name (pp_id_assoc pp_ty) args (pp_option pp_ty) return_type

let pp_t f ast =
  addb f "let open AST in let annot = ASTUtils.add_dummy_pos in ";
  pp_list pp_decl f ast

let t_to_string ast = with_buf @@ fun b -> pp_t b ast
