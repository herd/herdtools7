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
let pp_int f i = addb f (string_of_int i)
let pp_float f = bprintf f "%F"
let pp_bool f = bprintf f "%B"

let pp_bitvector f s =
  addb f "'";
  addb f s;
  addb f "'"

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
  | V_Int i ->
      addb f "V_Int ";
      pp_int f i
  | V_Bool b ->
      addb f "V_Bool ";
      pp_bool f b
  | V_Real r ->
      addb f "V_Real ";
      pp_float f r
  | V_BitVector bv ->
      addb f "V_BitVector ";
      pp_bitvector f bv
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

let rec pp_expr f = function
  | E_Literal v -> bprintf f "E_Literal (%a)" pp_value v
  | E_Var x -> bprintf f "E_Var %S" x
  | E_Binop (op, e1, e2) ->
      bprintf f "E_Binop (%s, %a, %a)" (pp_binop op) pp_expr e1 pp_expr e2
  | E_Unop (op, e) -> bprintf f "E_Unop (%s, %a)" (pp_unop op) pp_expr e
  | E_Call (name, args) -> bprintf f "E_Call (%S, %a)" name pp_expr_list args
  | E_Slice (e, args) ->
      bprintf f "E_Slice (%a, %a)" pp_expr e pp_slice_list args
  | E_Cond (e1, e2, e3) ->
      bprintf f "E_Cond (%a, %a, %a)" pp_expr e1 pp_expr e2 pp_expr e3
  | E_GetField (e, x, _ta) -> bprintf f "E_GetField (%a, %S, None)" pp_expr e x
  | E_Record (ty, li, _ta) ->
      bprintf f "E_Record (%a, %a, None)" pp_type_desc ty
        (pp_pair_list pp_string pp_expr)
        li

and pp_expr_list f = pp_list pp_expr f
and pp_slice_list f = pp_list pp_slice f

and pp_slice f = function
  | Slice_Single e -> bprintf f "Slice_Single (%a)" pp_expr e
  | Slice_Range (e1, e2) ->
      bprintf f "Slice_Range (%a, %a)" pp_expr e1 pp_expr e2
  | Slice_Length (e1, e2) ->
      bprintf f "Slice_Length (%a, %a)" pp_expr e1 pp_expr e2

and pp_type_desc f = function
  | T_Int constraint_opt ->
      bprintf f "T_Int (%a)" (pp_option pp_int_constraint) constraint_opt
  | T_Real -> addb f "T_Real"
  | T_String -> addb f "T_String"
  | T_Bool -> addb f "T_Bool"
  | T_Bits (bits_constraint, fields) ->
      let pp_fields = pp_option @@ pp_list @@ pp_pair pp_slice_list pp_string in
      bprintf f "T_Bits (%a, %a)" pp_bits_constraint bits_constraint pp_fields
        fields
  | T_Bit -> addb f "T_Bit"
  | T_Enum enum_type_desc ->
      addb f "T_Enum ";
      pp_list pp_string f enum_type_desc
  | T_Tuple li ->
      addb f "T_Tuple ";
      pp_list pp_type_desc f li
  | T_Array (e, elt_type) ->
      bprintf f "T_Array (%a, %a)" pp_expr e pp_type_desc elt_type
  | T_Record li ->
      addb f "T_Record ";
      pp_pair_list pp_string pp_type_desc f li
  | T_Exception li ->
      addb f "T_Exception ";
      pp_pair_list pp_string pp_type_desc f li
  | T_ZType type_desc -> bprintf f "T_ZType (%a)" pp_type_desc type_desc
  | T_Named identifier -> bprintf f "T_Named %S" identifier

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
      bprintf f "BitWidth_ConstrainedFormType (%a)" pp_type_desc ty
  | BitWidth_Constrained int_constraint ->
      bprintf f "BitWidth_Constrained (%a)" pp_int_constraint int_constraint

let pp_typed_identifier = pp_pair pp_string pp_type_desc

let rec pp_lexpr f = function
  | LE_Var x -> bprintf f "LE_Var %S" x
  | LE_Slice (le, args) ->
      bprintf f "LE_Slice (%a, %a)" pp_lexpr le pp_slice_list args
  | LE_SetField (le, x, _ta) ->
      bprintf f "LE_SetField (%a, %S, None)" pp_lexpr le x

let rec pp_stmt f = function
  | S_Pass -> addb f "SPass"
  | S_Then (s1, s2) -> bprintf f "S_Then (%a, %a)" pp_stmt s1 pp_stmt s2
  | S_Assign (le, e) -> bprintf f "S_Assign (%a, %a)" pp_lexpr le pp_expr e
  | S_Call (name, args) -> bprintf f "S_Call (%S, %a)" name pp_expr_list args
  | S_Cond (e, s1, s2) ->
      bprintf f "S_Cond (%a, %a, %a)" pp_expr e pp_stmt s1 pp_stmt s2
  | S_Return el ->
      addb f "S_Return ";
      pp_expr_list f el
  | S_Case (e, cases) ->
      bprintf f "S_Case (%a, %a)" pp_expr e
        (pp_pair_list pp_expr_list pp_stmt)
        cases
  | S_Assert e -> bprintf f "S_Assert (%a)" pp_expr e

let pp_decl f = function
  | D_Func { name; args; body; return_type } ->
      bprintf f "D_Func { name=%S; args=%a; body=%a; return_type=%a }" name
        (pp_pair_list pp_string pp_type_desc)
        args pp_stmt body (pp_option pp_type_desc) return_type
  | D_GlobalConst (x, ty, e) ->
      bprintf f "D_GlobalConst (%S, %a, %a)" x pp_type_desc ty pp_expr e
  | D_TypeDecl (name, type_desc) ->
      bprintf f "D_TypeDecl (%S, %a)" name pp_type_desc type_desc
  | D_Primitive { name; args; return_type; body = _ } ->
      bprintf f "D_Primitive { name=%S; args=%a; body=S_Pass; return_type=%a }"
        name
        (pp_pair_list pp_string pp_type_desc)
        args (pp_option pp_type_desc) return_type

let pp_t f ast = bprintf f "Asllib.AST.(%a)" (pp_list pp_decl) ast
let t_to_string ast = with_buf @@ fun b -> pp_t b ast
