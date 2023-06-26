open Format
open AST

(* Copied from stdlib 4.13 *)
let string_is_prefix ~prefix s =
  let open String in
  let len_s = length s and len_pre = length prefix in
  let rec aux i =
    if i = len_pre then true
    else if unsafe_get s i <> unsafe_get prefix i then false
    else aux (i + 1)
  in
  len_s >= len_pre && aux 0

let string_remove_prefix ~prefix s =
  String.sub s (String.length prefix) (String.length s - String.length prefix)

type 'a printer = Format.formatter -> 'a -> unit

(* Adapted from stdlib >= 4.12.0 *)
let pp_print_seq ?(pp_sep = pp_print_cut) pp_v ppf v =
  let is_first = ref true in
  let pp_v v =
    if !is_first then is_first := false else pp_sep ppf ();
    pp_v ppf v
  in
  Seq.iter pp_v v

let pp_comma f () = fprintf f ",@ "
let pp_comma_list pp_elt f = pp_print_list ~pp_sep:pp_comma pp_elt f

let pp_pos f { pos_start; pos_end; _ } =
  let open Lexing in
  let pp_char_num f { pos_cnum; pos_bol; _ } =
    pp_print_int f (pos_cnum - pos_bol)
  in
  if pos_start = dummy_pos || pos_end = dummy_pos then ()
  else (
    pp_open_hovbox f 2;
    fprintf f "File %s,@ " pos_start.pos_fname;
    if String.equal pos_start.pos_fname pos_end.pos_fname then
      if pos_start.pos_lnum = pos_end.pos_lnum then
        if pos_start.pos_cnum = pos_end.pos_cnum then
          fprintf f "line %d,@ character %a" pos_start.pos_lnum pp_char_num
            pos_start
        else
          fprintf f "line %d,@ characters %a to %a" pos_start.pos_lnum
            pp_char_num pos_start pp_char_num pos_end
      else
        fprintf f "line %d,@ character %a@ to@ line %2d,@ character %a"
          pos_start.pos_lnum pp_char_num pos_start pos_end.pos_lnum pp_char_num
          pos_end
    else
      fprintf f "line %d,@ character %a" pos_start.pos_lnum pp_char_num
        pos_start;
    pp_close_box f ())

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
  | MOD -> "MOD"
  | MINUS -> "-"
  | MUL -> "*"
  | NEQ -> "!="
  | OR -> "OR"
  | PLUS -> "+"
  | RDIV -> "/"
  | SHL -> "<<"
  | SHR -> ">>"

let unop_to_string = function BNOT -> "!" | NEG -> "-" | NOT -> "NOT"

let rec pp_value f =
  let pp_print_field_assoc f =
    let pp_one f (name, value) = fprintf f "@[%s = %a@]" name pp_value value in
    fprintf f "{ @[%a@] }" (pp_comma_list pp_one)
  in
  function
  | V_Int i -> pp_print_int f i
  | V_Bool true -> pp_print_string f "TRUE"
  | V_Bool false -> pp_print_string f "FALSE"
  | V_Real r -> pp_print_float f r
  | V_BitVector bv -> Bitvector.pp_t f bv
  | V_Tuple li -> fprintf f "(@[%a@])" (pp_comma_list pp_value) li
  | V_Record li | V_Exception li -> pp_print_field_assoc f li

let rec pp_expr f e =
  match e.desc with
  | E_Literal v -> pp_value f v
  | E_Var x -> pp_print_string f x
  | E_Typed (e, ty) -> fprintf f "@[%a@ as %a@]" pp_expr e pp_ty ty
  | E_Binop (b, e1, e2) ->
      fprintf f "(@[<hov 2>%a@ %s %a@])" pp_expr e1 (binop_to_string b) pp_expr
        e2
  | E_Unop (u, e) -> fprintf f "(%s %a)" (unop_to_string u) pp_expr e
  | E_Call (name, args, _) ->
      if string_is_prefix ~prefix:ASTUtils.getter_prefix name then
        let name = string_remove_prefix ~prefix:ASTUtils.getter_prefix name in
        fprintf f "@[<hov 2>%s[%a]@]" name pp_expr_list args
      else fprintf f "@[<hov 2>%s(%a)@]" name pp_expr_list args
  | E_Slice (e, args) ->
      fprintf f "@[<hov 2>%a[%a]@]" pp_expr e pp_slice_list args
  | E_Cond (e1, e2, e3) ->
      fprintf f "@[<hv>@[<h>if %a@ then@]@;<1 2>%a@ else@;<1 2>%a@]" pp_expr e1
        pp_expr e2 pp_expr e3
  | E_GetField (e, x, _ta) -> fprintf f "@[%a@,.%s@]" pp_expr e x
  | E_GetFields (e, xs, _ta) ->
      fprintf f "@[%a@,.[@[%a@]]@]" pp_expr e (pp_comma_list pp_print_string) xs
  | E_Record (ty, li, _ta) ->
      let pp_one f (x, e) = fprintf f "@[<h>%s =@ %a@]" x pp_expr e in
      fprintf f "@[<hv>%a {@;<1 2>%a@,}@]" pp_ty ty (pp_comma_list pp_one) li
  | E_Concat es -> fprintf f "@[<hv 2>[%a]@]" pp_expr_list es
  | E_Tuple es -> fprintf f "@[<hv 2>(%a)@]" pp_expr_list es
  | E_Unknown ty -> fprintf f "@[<h>UNKNOWN ::@ %a@]" pp_ty ty
  | E_Pattern (e, p) -> fprintf f "@[<hv 2>%a@ IN %a@]" pp_expr e pp_pattern p

and pp_expr_list f = pp_comma_list pp_expr f

and pp_slice f = function
  | Slice_Single e -> pp_expr f e
  | Slice_Range (e1, e2) -> fprintf f "@[<h>%a@,:%a@]" pp_expr e1 pp_expr e2
  | Slice_Length (e1, e2) -> fprintf f "@[<h>%a@,+:%a@]" pp_expr e1 pp_expr e2

and pp_pattern f = function
  | Pattern_All -> pp_print_string f "-"
  | Pattern_Any li -> fprintf f "@[{%a}@]" (pp_comma_list pp_pattern) li
  | Pattern_Geq e -> fprintf f "@[>= %a@]" pp_expr e
  | Pattern_Leq e -> fprintf f "@[<= %a@]" pp_expr e
  | Pattern_Mask s -> fprintf f "'%s'" s
  | Pattern_Not (Pattern_Any li) ->
      fprintf f "@[!{%a}@]" (pp_comma_list pp_pattern) li
  | Pattern_Not p -> fprintf f "@[!{%a}@]" pp_pattern p
  | Pattern_Range (e1, e2) -> fprintf f "@[%a .. %a@]" pp_expr e1 pp_expr e2
  | Pattern_Single e -> pp_expr f e

and pp_slice_list f = pp_comma_list pp_slice f

and pp_ty f t =
  match t.desc with
  | T_Int None -> pp_print_string f "integer"
  | T_Int (Some int_constraint) ->
      fprintf f "@[integer {%a}@]" pp_int_constraints int_constraint
  | T_Real -> pp_print_string f "real"
  | T_String -> pp_print_string f "string"
  | T_Bool -> pp_print_string f "boolean"
  | T_Bits (bits_constraint, []) ->
      fprintf f "@[bits(%a)@]" pp_bits_constraint bits_constraint
  | T_Bits (bits_constraint, fields) ->
      let pp_bitfield f (name, slices) =
        fprintf f "@[<h>[%a]@ %s@]" pp_slice_list slices name
      in
      fprintf f "bits (%a)@ {@[<hv 1>@,%a@]@,}" pp_bits_constraint
        bits_constraint
        (pp_comma_list pp_bitfield)
        fields
  | T_Enum enum_ty ->
      fprintf f "@[<hov 2>enumeration {@,%a@;<0 -2>}@]"
        (pp_comma_list pp_print_string)
        enum_ty
  | T_Tuple ty_list -> fprintf f "@[(%a)@]" (pp_comma_list pp_ty) ty_list
  | T_Array (e, elt_type) ->
      fprintf f "@[array [%a] of %a@]" pp_expr e pp_ty elt_type
  | T_Record record_ty ->
      fprintf f "@[<hv 2>record {@,%a@;<0 -2>}@]" pp_record_ty record_ty
  | T_Exception record_ty ->
      fprintf f "@[exception {%a}@]" pp_record_ty record_ty
  | T_Named x -> pp_print_string f x

and pp_record_ty f =
  let pp_one f (field_name, field_type) =
    fprintf f "%s::%a" field_name pp_ty field_type
  in
  pp_comma_list pp_one f

and pp_int_constraint f = function
  | Constraint_Exact x -> pp_expr f x
  | Constraint_Range (x, y) -> fprintf f "%a..%a" pp_expr x pp_expr y

and pp_int_constraints f = pp_comma_list pp_int_constraint f

and pp_bits_constraint f = function
  | BitWidth_Determined i -> pp_expr f i
  | BitWidth_Constrained int_constraint -> pp_int_constraints f int_constraint
  | BitWidth_ConstrainedFormType ty -> pp_ty f ty

let pp_typed_identifier f (name, ty) = fprintf f "%s::%a" name pp_ty ty

let rec pp_lexpr f le =
  match le.desc with
  | LE_Var x -> pp_print_string f x
  | LE_Slice (le, args) -> fprintf f "%a[%a]" pp_lexpr le pp_slice_list args
  | LE_SetField (le, x, _ta) -> fprintf f "@[%a@,.%s@]" pp_lexpr le x
  | LE_SetFields (le, li, _ta) ->
      fprintf f "@[%a@,.@[[%a]@]@]" pp_lexpr le
        (pp_comma_list pp_print_string)
        li
  | LE_Ignore -> pp_print_string f "-"
  | LE_TupleUnpack les -> fprintf f "@[( %a )@]" (pp_comma_list pp_lexpr) les

let pp_for_direction = function Up -> "to" | Down -> "downto"

let pp_local_decl_keyword f k =
  pp_print_string f
    (match k with
    | LDK_Var -> "var"
    | LDK_Constant -> "constant"
    | LDK_Let -> "let")

let rec pp_local_decl_item f =
  let pp_ty_opt f = function
    | Some ty -> fprintf f "@ :: @[%a@]" pp_ty ty
    | None -> ()
  in
  function
  | LDI_Ignore ty_opt -> fprintf f "@[-%a@]" pp_ty_opt ty_opt
  | LDI_Var (s, ty_opt) -> fprintf f "@[%s%a@]" s pp_ty_opt ty_opt
  | LDI_Tuple (ldis, ty_opt) ->
      fprintf f "@[(%a)%a@]"
        (pp_comma_list pp_local_decl_item)
        ldis pp_ty_opt ty_opt

let rec pp_stmt f s =
  match s.desc with
  | S_Pass -> pp_print_string f "pass;"
  | S_Then (s1, s2) -> fprintf f "%a@ %a" pp_stmt s1 pp_stmt s2
  | S_Assign (le, e) -> fprintf f "@[<h 2>%a =@ %a;@]" pp_lexpr le pp_expr e
  | S_Call (name, args, _) ->
      if string_is_prefix ~prefix:ASTUtils.setter_prefix name then
        let name = string_remove_prefix ~prefix:ASTUtils.setter_prefix name in
        let v, args = match args with [] -> assert false | h :: t -> (h, t) in
        fprintf f "@[<hov 2>%s[%a] = %a;@]" name pp_expr_list args pp_expr v
      else fprintf f "@[<hov 2>%s(%a);@]" name pp_expr_list args
  | S_Return (Some e) -> fprintf f "return %a;" pp_expr e
  | S_Return None -> fprintf f "return;"
  | S_Cond (e, s1, { desc = S_Pass; _ }) ->
      fprintf f "@[<hv>@[<h>if %a@ then@]@;<1 2>@[<hv>%a@]@ end@]" pp_expr e
        pp_stmt s1
  | S_Cond (e, s1, s2) ->
      fprintf f
        "@[<hv>@[<h>if %a@ then@]@;\
         <1 2>@[<hv>%a@]@ else@;\
         <1 2>@[<hv>%a@]@ end@]" pp_expr e pp_stmt s1 pp_stmt s2
  | S_Case (e, case_li) ->
      let pp_case_alt f { desc = p, s; _ } =
        match p with
        | Pattern_All -> fprintf f "@[<hv 2>otherwise:@ @[<hv>%a@]@]" pp_stmt s
        | Pattern_Any li ->
            fprintf f "@[<hv 2>when @[<h>%a@]:@ @[<hv>%a@]@]"
              (pp_comma_list pp_pattern) li pp_stmt s
        | _ -> fprintf f "@[<hv 2>when %a:@ @[<hv>%a@]@]" pp_pattern p pp_stmt s
      in
      fprintf f "@[<v 2>case %a of@ %a@;<1 -2>end@]" pp_expr e
        (pp_print_list ~pp_sep:pp_print_space pp_case_alt)
        case_li
  | S_Assert e -> fprintf f "@[<2>assert@ %a;@]" pp_expr e
  | S_While (e, s) ->
      fprintf f "@[<hv>@[<h>while %a@ do@]@;<1 2>@[<hv>%a@]@ end@]" pp_expr e
        pp_stmt s
  | S_Repeat (s, e) ->
      fprintf f "@[<hv 2>repeat@;<1 2>@[<hv>%a@]@;<1 0>@[<h>until@ %a;@]@]"
        pp_stmt s pp_expr e
  | S_For (id, e1, dir, e2, s) ->
      fprintf f "@[<hv 2>@[<h>for %a=%a %s %a@ do@]@;<1 2>@[<hv>%a@]@ end@]"
        pp_print_string id pp_expr e1 (pp_for_direction dir) pp_expr e2 pp_stmt
        s
  | S_Decl (ldk, ldi, None) ->
      fprintf f "@[<2>%a %a;@]" pp_local_decl_keyword ldk pp_local_decl_item ldi
  | S_Decl (ldk, ldi, Some e) ->
      fprintf f "@[<2>%a %a@ = %a;@]" pp_local_decl_keyword ldk
        pp_local_decl_item ldi pp_expr e

let pp_decl f =
  let pp_func_sig f { name; args; return_type; _ } =
    let pp_args = pp_comma_list pp_typed_identifier in
    let pp_return_type_opt f = function
      | Some return_type -> fprintf f "@;<1 -2>=> %a" pp_ty return_type
      | None -> ()
    in
    if string_is_prefix ~prefix:ASTUtils.getter_prefix name then
      let name = string_remove_prefix ~prefix:ASTUtils.getter_prefix name in
      fprintf f "@[<hv 4>getter %s [@,%a]%a@]" name pp_args args
        pp_return_type_opt return_type
    else if string_is_prefix ~prefix:ASTUtils.setter_prefix name then
      let name = string_remove_prefix ~prefix:ASTUtils.setter_prefix name in
      let new_v, args =
        match args with [] -> assert false | h :: t -> (h, t)
      in
      fprintf f "@[<hv 4>setter %s [@,%a]@ = %a@]" name pp_args args
        pp_typed_identifier new_v
    else
      fprintf f "@[<hv 4>func %s (@,%a)%a@]" name pp_args args
        pp_return_type_opt return_type
  in
  function
  | D_Func func ->
      fprintf f "@[<v>%a@ begin@;<1 2>@[<v>%a@]@ end@]" pp_func_sig func pp_stmt
        func.body
  | D_GlobalConst (x, ty, e) ->
      fprintf f "@[<2>constant %s@ :: %a@ = %a;@]" x pp_ty ty pp_expr e
  | D_TypeDecl (x, ty) -> fprintf f "@[<2>type %s of %a;@]" x pp_ty ty
  | D_Primitive func -> fprintf f "@[<h>%a ;@]" pp_func_sig func

let pp_t f =
  let pp_blank_line f () =
    pp_print_space f ();
    pp_print_cut f ()
  in
  fprintf f "@[<v>%a@]" (pp_print_list ~pp_sep:pp_blank_line pp_decl)

let ty_to_string = asprintf "%a" pp_ty
let t_to_string = asprintf "%a" pp_t
let value_to_string = asprintf "%a" pp_value

let pp_version f version =
  pp_print_string f
  @@ match version with `ASLv0 -> "ASLv0" | `ASLv1 -> "ASLv1" | `Any -> "any"
