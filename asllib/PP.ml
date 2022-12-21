open Format
open AST

let pp_comma f () = fprintf f ",@ "

let binop_to_string : binop -> string = function
  | AND -> "AND"
  | BAND -> "&&"
  | BEQ -> "<->"
  | BOR -> "||"
  | DIV -> "DIV"
  | EOR -> "EOR"
  | EQ_OP -> "=="
  | GT -> ">"
  | GEQ -> ">="
  | IMPL -> "-->"
  | LT -> "<"
  | LEQ -> "<="
  | MOD -> "%"
  | MINUS -> "-"
  | MUL -> "*"
  | NEQ -> "!="
  | OR -> "OR"
  | PLUS -> "+"
  | RDIV -> "/"
  | SHL -> "<<"
  | SHR -> ">>"

let unop_to_string = function BNOT -> "!" | NEG -> "-" | NOT -> "NOT"

let rec pp_value pp_int pp_bool pp_real pp_bv f =
  let pp_v = pp_value pp_int pp_bool pp_real pp_bv in
  let pp_print_field_assoc f =
    let pp_one f (name, value) = fprintf f "@[%s = %a@]" name pp_v value in
    fprintf f "{ @[%a@] }" (pp_print_list ~pp_sep:pp_comma pp_one)
  in
  function
  | V_Int i -> pp_int f i
  | V_Bool b -> pp_bool f b
  | V_Real r -> pp_real f r
  | V_BitVector bv -> pp_bv f bv
  | V_Tuple li -> fprintf f "(@[%a@])" (pp_print_list ~pp_sep:pp_comma pp_v) li
  | V_Record li | V_Exception li -> pp_print_field_assoc f li

let pp_parsed_value =
  pp_value pp_print_int pp_print_bool pp_print_float pp_print_string

let rec pp_expr pp_v f = function
  | E_Literal v -> pp_v f v
  | E_Var x -> pp_print_string f x
  | E_Binop (b, e1, e2) ->
      fprintf f "(@[<hov 2>%a@ %s %a@])" (pp_expr pp_v) e1 (binop_to_string b)
        (pp_expr pp_v) e2
  | E_Unop (u, e) -> fprintf f "(%s %a)" (unop_to_string u) (pp_expr pp_v) e
  | E_Call (name, args) ->
      fprintf f "@[<hov 2>%s(%a)@]" name (pp_expr_list pp_v) args
  | E_Get (name, args) ->
      fprintf f "@[<hov 2>%s[%a]@]" name (pp_expr_list pp_v) args
  | E_Cond (e1, e2, e3) ->
      fprintf f "@[<hv>@[<h>if %a@ then@]@;<1 2>%a@ else@;<1 2>%a@ end@]"
        (pp_expr pp_v) e1 (pp_expr pp_v) e2 (pp_expr pp_v) e3

and pp_expr_list pp_v = pp_print_list ~pp_sep:pp_comma (pp_expr pp_v)

let pp_parsed_expr = pp_expr pp_parsed_value

let rec pp_type_desc f = function
  | T_Int None -> pp_print_string f "integer"
  | T_Int (Some int_constraint) ->
      fprintf f "@[integer {%a}@]" pp_int_constraints int_constraint
  | T_Real -> pp_print_string f "T_Real"
  | T_String -> pp_print_string f "T_String"
  | T_Bit -> pp_print_string f "T_Bit"
  | T_Bool -> pp_print_string f "T_Bool"
  | T_Bits bits_constraint ->
      fprintf f "@[bits(%a)@]" pp_bits_constraint bits_constraint
  | T_Enum enum_type_desc ->
      fprintf f "@[enumeration {%a}@]"
        (pp_print_list pp_print_string)
        enum_type_desc
  | T_Tuple type_desc_list ->
      fprintf f "@[(%a)@]" (pp_print_list pp_type_desc) type_desc_list
  | T_Array (e, elt_type) ->
      fprintf f "@[array [%a] of %a@]" pp_parsed_expr e pp_type_desc elt_type
  | T_Record record_type_desc ->
      fprintf f "@[record {%a}@]" pp_record_type_desc record_type_desc
  | T_Exception record_type_desc ->
      fprintf f "@[exception {%a}@]" pp_record_type_desc record_type_desc
  | T_ZType type_desc -> fprintf f "ZType(%a)" pp_type_desc type_desc
  | T_Named x -> pp_print_string f x

and pp_record_type_desc f =
  let pp_one f (field_name, field_type) =
    fprintf f "%s::%a" field_name pp_type_desc field_type
  in
  pp_print_list ~pp_sep:pp_comma pp_one f

and pp_int_constraint f = function
  | Constraint_Exact x -> pp_parsed_expr f x
  | Constraint_Range (x, y) ->
      fprintf f "%a..%a" pp_parsed_expr x pp_parsed_expr y

and pp_int_constraints f = pp_print_list ~pp_sep:pp_comma pp_int_constraint f

and pp_bits_constraint f = function
  | BitWidth_Determined i -> pp_expr pp_parsed_value f i
  | BitWidth_Constrained int_constraint -> pp_int_constraints f int_constraint
  | BitWidth_ConstrainedFormType type_desc -> pp_type_desc f type_desc

let pp_typed_identifier f (name, type_desc) =
  fprintf f "%s::%a" name pp_type_desc type_desc

let pp_lexpr pp_v f = function
  | LEVar x -> pp_print_string f x
  | LESet (x, args) -> fprintf f "%s[%a]" x (pp_expr_list pp_v) args

let rec pp_stmt pp_v f = function
  | S_Pass -> pp_print_string f "pass"
  | S_Then (s1, s2) -> fprintf f "%a ;@ %a" (pp_stmt pp_v) s1 (pp_stmt pp_v) s2
  | S_Assign (le, e) ->
      fprintf f "@[<h 2>%a =@ %a@]" (pp_lexpr pp_v) le (pp_expr pp_v) e
  | S_Call (name, args) ->
      fprintf f "@[<hov 2>%s(%a)@]" name (pp_expr_list pp_v) args
  | S_Return el -> fprintf f "return %a" (pp_expr_list pp_v) el
  | S_Cond (e, s1, s2) ->
      fprintf f
        "@[<hv>@[<h>if %a@ then@]@;\
         <1 2>@[<hv>%a@]@ else@;\
         <1 2>@[<hv>%a@]@ end@]" (pp_expr pp_v) e (pp_stmt pp_v) s1
        (pp_stmt pp_v) s2

let pp_decl pp_v f = function
  | D_Func { name; args; body; return_type } ->
      let pp_return_type_opt f = function
        | Some return_type -> fprintf f "@ => %a" pp_type_desc return_type
        | None -> ()
      in
      let pp_args = pp_print_list ~pp_sep:pp_comma pp_typed_identifier in
      fprintf f "@[<hv 2>@[<h>func %s(%a)%a@]@ @[<hv>%a@]@;<1 -2>end@]" name
        pp_args args pp_return_type_opt return_type (pp_stmt pp_v) body
  | D_GlobalConst (x, e) -> fprintf f "@[const %s@ = %a@]" x (pp_expr pp_v) e
  | D_TypeDecl (x, type_desc) ->
      fprintf f "@[type %s of %a@]" x pp_type_desc type_desc

let pp_t pp_v = pp_print_list ~pp_sep:pp_print_space (pp_decl pp_v)
let pp_parsed_t = pp_t pp_parsed_value
let t_to_string_pp pp_v = asprintf "%a" (pp_t pp_v)
let type_desc_to_string = asprintf "%a" pp_type_desc

let t_to_string v_to_string =
  let pp_v f v = pp_print_string f (v_to_string v) in
  t_to_string_pp pp_v

let parsed_t_to_string = t_to_string_pp pp_parsed_value
