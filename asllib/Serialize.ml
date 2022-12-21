open AST

type buffer = Buffer.t

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

let rec pp_value pp_int pp_bool pp_real pp_bv f =
  let pp_v = pp_value pp_int pp_bool pp_real pp_bv in
  let pp_field_assoc =
    let pp_one f (name, value) =
      addb f "(";
      addb f name;
      addb f " ";
      pp_v f value;
      addb f ")"
    in
    pp_list pp_one
  in
  function
  | V_Int i ->
      addb f "(VInt ";
      addb f (pp_int i);
      addb f ")"
  | V_Bool b ->
      addb f "(VBool ";
      addb f (pp_bool b);
      addb f ")"
  | V_Real r ->
      addb f "(VReal ";
      addb f (pp_real r);
      addb f ")"
  | V_BitVector bv ->
      addb f "(VBitVector ";
      addb f (pp_bv bv);
      addb f ")"
  | V_Tuple li ->
      addb f "(VTuple ";
      pp_list pp_v f li;
      addb f ")"
  | V_Record li ->
      addb f "(VRecord ";
      pp_field_assoc f li;
      addb f ")"
  | V_Exception li ->
      addb f "(VException ";
      pp_field_assoc f li;
      addb f ")"

let pp_string = addb
let pp_parsed_int = string_of_int
let pp_parsed_bool = string_of_bool
let pp_parsed_real = string_of_float
let pp_parsed_bitvector s = s

let pp_parsed_value =
  pp_value pp_parsed_int pp_parsed_bool pp_parsed_real pp_parsed_bitvector

let rec pp_expr pp_v f = function
  | E_Literal v ->
      addb f "(ELiteral ";
      pp_v f v;
      addb f ")"
  | E_Var x ->
      addb f "(EVar ";
      addb f x;
      addb f ")"
  | E_Binop (op, e1, e2) ->
      addb f "(EBinop ";
      addb f (pp_binop op);
      addb f " ";
      pp_expr pp_v f e1;
      addb f " ";
      pp_expr pp_v f e2;
      addb f ")"
  | E_Unop (op, e) ->
      addb f "(EUnop ";
      addb f (pp_unop op);
      addb f " ";
      pp_expr pp_v f e;
      addb f ")"
  | E_Call (name, args) ->
      addb f "(ECall ";
      addb f name;
      addb f " ";
      pp_expr_list pp_v f args;
      addb f ")"
  | E_Get (name, args) ->
      addb f "(EGet ";
      addb f name;
      addb f " ";
      pp_expr_list pp_v f args;
      addb f ")"
  | E_Cond (e1, e2, e3) ->
      addb f "(ECond ";
      pp_expr pp_v f e1;
      addb f " ";
      pp_expr pp_v f e2;
      addb f " ";
      pp_expr pp_v f e3;
      addb f ")"

and pp_expr_list pp_v = pp_list (pp_expr pp_v)

let pp_fields_assoc pp_key pp_type_desc =
  let pp_one f (key, type_desc) =
    addb f "(";
    pp_key f key;
    addb f " ";
    pp_type_desc f type_desc;
    addb f ""
  in
  pp_list pp_one

let rec pp_type_desc f = function
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
      pp_expr pp_parsed_value f e;
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

and pp_int_constraint =
  let pp_one f = function
    | Constraint_Exact v ->
        addb f "(Constraint_Exact ";
        pp_expr pp_parsed_value f v;
        addb f ")"
    | Constraint_Range (bot, top) ->
        addb f "(Constraint_Range ";
        pp_expr pp_parsed_value f bot;
        addb f " ";
        pp_expr pp_parsed_value f top;
        addb f ")"
  in
  pp_list pp_one

and pp_bits_constraint f = function
  | BitWidth_Determined i ->
      addb f "(BitWidth_Determined ";
      pp_expr pp_parsed_value f i;
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

let pp_lexpr pp_v f = function
  | LEVar x ->
      addb f "(LEVar ";
      addb f x;
      addb f ")"
  | LESet (name, args) ->
      addb f "(LESet ";
      addb f name;
      addb f " ";
      pp_expr_list pp_v f args;
      addb f ")"

let rec pp_stmt pp_v f = function
  | S_Pass -> addb f "SPass"
  | S_Then (s1, s2) ->
      addb f "(SThen ";
      pp_stmt pp_v f s1;
      addb f " ";
      pp_stmt pp_v f s2;
      addb f ")"
  | S_Assign (le, e) ->
      addb f "(SAssign ";
      pp_lexpr pp_v f le;
      addb f " ";
      pp_expr pp_v f e;
      addb f ")"
  | S_Call (name, args) ->
      addb f "(SCall ";
      addb f name;
      addb f " ";
      pp_expr_list pp_v f args;
      addb f ")"
  | S_Cond (e, s1, s2) ->
      addb f "(SCond ";
      pp_expr pp_v f e;
      addb f " ";
      pp_stmt pp_v f s1;
      addb f " ";
      pp_stmt pp_v f s2;
      addb f ")"
  | S_Return el ->
      addb f "(SReturn ";
      pp_expr_list pp_v f el;
      addb f ")"

let pp_decl pp_v f = function
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
      pp_stmt pp_v f body;
      addb f ")"
  | D_GlobalConst (x, e) ->
      addb f "(GlobalConst ";
      addb f x;
      addb f " ";
      pp_expr pp_v f e;
      addb f ")"
  | D_TypeDecl (name, type_desc) ->
      addb f "(TypeDecl ";
      addb f name;
      addb f " ";
      pp_type_desc f type_desc;
      addb f ")"

let pp_t pp_v f ast =
  addb f "(Asllib.AST ";
  pp_list (pp_decl pp_v) f ast;
  addb f ")"

let pp_parsed_t = pp_t pp_parsed_value

let t_to_string value_to_string ast =
  let pp_v f v = addb f (value_to_string v) in
  with_buf @@ fun b -> pp_t pp_v b ast

let parsed_t_to_string ast = with_buf @@ fun b -> pp_parsed_t b ast
