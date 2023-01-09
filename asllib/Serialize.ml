open AST

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
    addb buf " ";
    pp_elt buf elt
  in
  function
  | [] -> ()
  | h :: t ->
      pp_elt buf h;
      List.iter pp_elt_with_sep t

let pp_option none pp_some buf = function
  | None -> addb buf none
  | Some elt -> pp_some buf elt

let pp_pair pp_left pp_right f (left, right) =
  addb f "(";
  pp_left f left;
  addb f " ";
  pp_right f right;
  addb f ")"

let pp_string = addb
let pp_int f i = addb f (string_of_int i)
let pp_float f x = addb f (string_of_float x)
let pp_bool f b = addb f (if b then "true" else "false")

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
      addb f "(VInt ";
      pp_int f i;
      addb f ")"
  | V_Bool b ->
      addb f "(VBool ";
      pp_bool f b;
      addb f ")"
  | V_Real r ->
      addb f "(VReal ";
      pp_float f r;
      addb f ")"
  | V_BitVector bv ->
      addb f "(VBitVector ";
      pp_bitvector f bv;
      addb f ")"
  | V_Tuple li ->
      addb f "(VTuple ";
      pp_list pp_value f li;
      addb f ")"
  | V_Record li ->
      addb f "(VRecord ";
      pp_field_assoc f li;
      addb f ")"
  | V_Exception li ->
      addb f "(VException ";
      pp_field_assoc f li;
      addb f ")"

and pp_field_assoc f = pp_list (pp_pair pp_string pp_value) f

let rec pp_expr f = function
  | E_Literal v ->
      addb f "(ELiteral ";
      pp_value f v;
      addb f ")"
  | E_Var x ->
      addb f "(EVar ";
      addb f x;
      addb f ")"
  | E_Binop (op, e1, e2) ->
      addb f "(EBinop ";
      addb f (pp_binop op);
      addb f " ";
      pp_expr f e1;
      addb f " ";
      pp_expr f e2;
      addb f ")"
  | E_Unop (op, e) ->
      addb f "(EUnop ";
      addb f (pp_unop op);
      addb f " ";
      pp_expr f e;
      addb f ")"
  | E_Call (name, args) ->
      addb f "(ECall ";
      addb f name;
      addb f " ";
      pp_expr_list f args;
      addb f ")"
  | E_Slice (e, args) ->
      addb f "(ESlice ";
      pp_expr f e;
      addb f " ";
      pp_slice_list f args;
      addb f ")"
  | E_Cond (e1, e2, e3) ->
      addb f "(ECond ";
      pp_expr f e1;
      addb f " ";
      pp_expr f e2;
      addb f " ";
      pp_expr f e3;
      addb f ")"
  | E_GetField (e, x, _ta) ->
      addb f "(EGetField ";
      pp_expr f e;
      addb f " ";
      addb f x;
      addb f ")"
  | E_Record (ty, li, _ta) ->
      addb f "(ERecord ";
      pp_type_desc f ty;
      addb f " ";
      pp_list (pp_pair pp_string pp_expr) f li;
      addb f ")"

and pp_expr_list f = pp_list pp_expr f
and pp_slice_list f = pp_list pp_slice f

and pp_slice f = function
  | Slice_Single e -> pp_expr f e
  | Slice_Range (e1, e2) ->
      addb f "(SliceRange ";
      pp_expr f e1;
      addb f " ";
      pp_expr f e2;
      addb f ")"
  | Slice_Length (e1, e2) ->
      addb f "(SliceLength ";
      pp_expr f e1;
      addb f " ";
      pp_expr f e2;
      addb f ")"

and pp_fields_assoc pp_key pp_type_desc =
  let pp_one f (key, type_desc) =
    addb f "(";
    pp_key f key;
    addb f " ";
    pp_type_desc f type_desc;
    addb f ""
  in
  pp_list pp_one

and pp_type_desc f = function
  | T_Int constraint_opt ->
      addb f "(T_Int ";
      pp_option "unconstrainted" pp_int_constraint f constraint_opt;
      addb f ")"
  | T_Real -> addb f "T_Real"
  | T_String -> addb f "T_String"
  | T_Bool -> addb f "T_Bool"
  | T_Bits bits_constraint ->
      addb f "(T_bits ";
      pp_bits_constraint f bits_constraint;
      addb f ")"
  | T_Bit -> addb f "T_Bit"
  | T_Enum enum_type_desc ->
      addb f "(T_Enum ";
      pp_enum_type_desc f enum_type_desc;
      addb f ")"
  | T_Tuple li ->
      addb f "(T_Tuple ";
      pp_list pp_type_desc f li;
      addb f ")"
  | T_Array (e, elt_type) ->
      addb f "(T_Array ";
      pp_expr f e;
      addb f " ";
      pp_type_desc f elt_type;
      addb f ")"
  | T_Record record_type_desc ->
      addb f "(T_Record ";
      pp_record_type_desc f record_type_desc;
      addb f ")"
  | T_Exception record_type_desc ->
      addb f "(T_Exception ";
      pp_record_type_desc f record_type_desc;
      addb f ")"
  | T_ZType type_desc ->
      addb f "(T_ZType ";
      pp_type_desc f type_desc;
      addb f ")"
  | T_Named identifier ->
      addb f "(T_Named ";
      pp_string f identifier;
      addb f ")"

and pp_enum_type_desc = pp_list pp_string
and pp_record_type_desc f = pp_fields_assoc pp_string pp_type_desc f

and pp_int_constraint f =
  let pp_one f = function
    | Constraint_Exact v ->
        addb f "(Constraint_Exact ";
        pp_expr f v;
        addb f ")"
    | Constraint_Range (bot, top) ->
        addb f "(Constraint_Range ";
        pp_expr f bot;
        addb f " ";
        pp_expr f top;
        addb f ")"
  in
  pp_list pp_one f

and pp_bits_constraint f = function
  | BitWidth_Determined i ->
      addb f "(BitWidth_Determined ";
      pp_expr f i;
      addb f ")"
  | BitWidth_ConstrainedFormType ty ->
      addb f "(BitWidth_ConstrainedFormType ";
      pp_type_desc f ty;
      addb f ")"
  | BitWidth_Constrained int_constraint ->
      addb f "(BitWidth_Constrained ";
      pp_int_constraint f int_constraint;
      addb f ")"

let pp_typed_identifier f (identifier, type_desc) =
  addb f "(TypedIdentifier ";
  pp_string f identifier;
  addb f " ";
  pp_type_desc f type_desc;
  addb f ")"

let rec pp_lexpr f = function
  | LE_Var x ->
      addb f "(LEVar ";
      addb f x;
      addb f ")"
  | LE_Slice (le, args) ->
      addb f "(LESlice ";
      pp_lexpr f le;
      addb f " ";
      pp_slice_list f args;
      addb f ")"
  | LE_SetField (le, x, _ta) ->
      addb f "(LESetField ";
      pp_lexpr f le;
      addb f " ";
      addb f x;
      addb f ")"

let rec pp_stmt f = function
  | S_Pass -> addb f "SPass"
  | S_Then (s1, s2) ->
      addb f "(SThen ";
      pp_stmt f s1;
      addb f " ";
      pp_stmt f s2;
      addb f ")"
  | S_Assign (le, e) ->
      addb f "(SAssign ";
      pp_lexpr f le;
      addb f " ";
      pp_expr f e;
      addb f ")"
  | S_Call (name, args) ->
      addb f "(SCall ";
      addb f name;
      addb f " ";
      pp_expr_list f args;
      addb f ")"
  | S_Cond (e, s1, s2) ->
      addb f "(SCond ";
      pp_expr f e;
      addb f " ";
      pp_stmt f s1;
      addb f " ";
      pp_stmt f s2;
      addb f ")"
  | S_Return el ->
      addb f "(SReturn ";
      pp_expr_list f el;
      addb f ")"

let pp_decl f = function
  | D_Func { name; args; body; return_type } ->
      addb f "(Func ";
      addb f name;
      addb f " (";
      pp_list pp_typed_identifier f args;
      addb f ") ";
      (match return_type with
      | None -> ()
      | Some return_type ->
          pp_type_desc f return_type;
          addb f " ");
      pp_stmt f body;
      addb f ")"
  | D_GlobalConst (x, e) ->
      addb f "(GlobalConst ";
      addb f x;
      addb f " ";
      pp_expr f e;
      addb f ")"
  | D_TypeDecl (name, type_desc) ->
      addb f "(TypeDecl ";
      addb f name;
      addb f " ";
      pp_type_desc f type_desc;
      addb f ")"
  | D_Primitive { name; args; return_type; _ } ->
      addb f "(Primitive ";
      addb f name;
      addb f " (";
      pp_list pp_typed_identifier f args;
      addb f ")";
      (match return_type with
      | None -> ()
      | Some return_type ->
          addb f " ";
          pp_type_desc f return_type);
      addb f ")"

let pp_t f ast =
  addb f "(Asllib.AST ";
  pp_list pp_decl f ast;
  addb f ")"

let t_to_string ast = with_buf @@ fun b -> pp_t b ast
