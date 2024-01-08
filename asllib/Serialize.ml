(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

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
  | DIVRM -> "DIVRM"
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
  | POW -> "POW"

let pp_unop = function BNOT -> "BNOT" | NOT -> "NOT" | NEG -> "NEG"

let pp_literal f = function
  | L_Int i -> bprintf f "V_Int (Z.of_string \"%a\")" Z.bprint i
  | L_Bool b -> bprintf f "V_Bool %B" b
  | L_Real r -> bprintf f "V_Real (Q.of_string \"%a\")" Q.bprint r
  | L_BitVector bv ->
      bprintf f "V_BitVector (Bitvector.of_string %S)" (Bitvector.to_string bv)
  | L_String s -> bprintf f "V_String %S" s

let rec pp_expr =
  let pp_desc f = function
    | E_Literal v -> bprintf f "E_Literal (%a)" pp_literal v
    | E_Var x -> bprintf f "E_Var %S" x
    | E_CTC (e, t) -> bprintf f "E_CTC (%a, %a)" pp_expr e pp_ty t
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
    | E_GetArray (e1, e2) ->
        bprintf f "E_GetArray (%a, %a)" pp_expr e1 pp_expr e2
    | E_GetField (e, x) -> bprintf f "E_GetField (%a, %S)" pp_expr e x
    | E_GetFields (e, x) ->
        bprintf f "E_GetFields (%a, %a)" pp_expr e (pp_list pp_string) x
    | E_Record (ty, li) ->
        bprintf f "E_Record (%a, %a)" pp_ty ty (pp_id_assoc pp_expr) li
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
  | Slice_Star (e1, e2) -> bprintf f "Slice_Star (%a, %a)" pp_expr e1 pp_expr e2

and pp_pattern f = function
  | Pattern_All -> addb f "Pattern_All"
  | Pattern_Any li ->
      addb f "Pattern_Any ";
      pp_list pp_pattern f li
  | Pattern_Geq e -> bprintf f "Pattern_Geq (%a)" pp_expr e
  | Pattern_Leq e -> bprintf f "Pattern_Leq (%a)" pp_expr e
  | Pattern_Mask m ->
      bprintf f "Pattern_Mask (Bitvector.mask_of_string \"%S\")"
        (Bitvector.mask_to_canonical_string m)
  | Pattern_Not p -> bprintf f "Pattern_Not (%a)" pp_pattern p
  | Pattern_Range (e1, e2) ->
      bprintf f "Pattern_Range (%a, %a)" pp_expr e1 pp_expr e2
  | Pattern_Single e -> bprintf f "Pattern_Single (%a)" pp_expr e
  | Pattern_Tuple li ->
      addb f "Pattern_Tuple ";
      pp_list pp_pattern f li

and pp_ty =
  let pp_desc f = function
    | T_Int cs -> bprintf f "T_Int (%a)" pp_int_constraints cs
    | T_Real -> addb f "T_Real"
    | T_String -> addb f "T_String"
    | T_Bool -> addb f "T_Bool"
    | T_Bits (bits_constraint, fields) ->
        bprintf f "T_Bits (%a, %a)" pp_expr bits_constraint pp_bitfields fields
    | T_Enum enum_type_desc ->
        addb f "T_Enum ";
        pp_list pp_string f enum_type_desc
    | T_Tuple li ->
        addb f "T_Tuple ";
        pp_list pp_ty f li
    | T_Array (length, elt_type) ->
        bprintf f "T_Array (%a, %a)" pp_array_length length pp_ty elt_type
    | T_Record li ->
        addb f "T_Record ";
        pp_id_assoc pp_ty f li
    | T_Exception li ->
        addb f "T_Exception ";
        pp_id_assoc pp_ty f li
    | T_Named identifier -> bprintf f "T_Named %S" identifier
  in
  fun f s -> pp_annotated pp_desc f s

and pp_array_length f = function
  | ArrayLength_Expr e -> bprintf f "ArrayLength_Expr (%a)" pp_expr e
  | ArrayLength_Enum (s, i) -> bprintf f "ArrayLength_Enum (%S, %i)" s i

and pp_bitfield f = function
  | BitField_Simple (name, slices) ->
      bprintf f "BitField_Simple (%S, %a)" name pp_slice_list slices
  | BitField_Nested (name, slices, bitfields) ->
      bprintf f "BitField_Nested (%S, %a, %a)" name pp_slice_list slices
        pp_bitfields bitfields
  | BitField_Type (name, slices, ty) ->
      bprintf f "BitField_Type (%S, %a, %a)" name pp_slice_list slices pp_ty ty

and pp_bitfields f bitfields = pp_list pp_bitfield f bitfields

and pp_int_constraint f = function
  | Constraint_Exact e -> bprintf f "Constraint_Exact (%a)" pp_expr e
  | Constraint_Range (bot, top) ->
      bprintf f "Constraint_Range (%a, %a)" pp_expr bot pp_expr top

and pp_int_constraints f = function
  | UnConstrained -> addb f "UnConstrained"
  | WellConstrained cs ->
      addb f "WellConstrained ";
      pp_list pp_int_constraint f cs
  | UnderConstrained (i, x) -> bprintf f "UnderConstrained (%d, %S)" i x

let rec pp_lexpr =
  let pp_desc f = function
    | LE_Var x -> bprintf f "LE_Var %S" x
    | LE_Slice (le, args) ->
        bprintf f "LE_Slice (%a, %a)" pp_lexpr le pp_slice_list args
    | LE_SetArray (le, e) ->
        bprintf f "LE_SetArray (%a, %a)" pp_lexpr le pp_expr e
    | LE_SetField (le, x) -> bprintf f "LE_SetField (%a, %S)" pp_lexpr le x
    | LE_SetFields (le, x) ->
        bprintf f "LE_SetFields (%a, %a)" pp_lexpr le (pp_list pp_string) x
    | LE_Discard -> addb f "LE_Discard"
    | LE_Destructuring les ->
        addb f "LE_Destructuring ";
        pp_list pp_lexpr f les
    | LE_Concat (les, _) ->
        bprintf f "LE_Concat (%a, None)" (pp_list pp_lexpr) les
  in
  fun f le -> pp_annotated pp_desc f le

let pp_local_decl_keyboard f k =
  pp_string f
    (match k with
    | LDK_Var -> "LDK_Var"
    | LDK_Constant -> "LDK_Constant"
    | LDK_Let -> "LDK_Let")

let rec pp_local_decl_item f = function
  | LDI_Discard -> addb f "LDI_Discard"
  | LDI_Var s -> bprintf f "LDI_Var %S" s
  | LDI_Typed (ldi, t) ->
      bprintf f "LDI_Typed (%a, %a)" pp_local_decl_item ldi pp_ty t
  | LDI_Tuple ldis -> bprintf f "LDI_Tuple %a" (pp_list pp_local_decl_item) ldis

let rec pp_stmt =
  let pp_desc f = function
    | S_Pass -> addb f "SPass"
    | S_Seq (s1, s2) -> bprintf f "S_Seq (%a, %a)" pp_stmt s1 pp_stmt s2
    | S_Assign (le, e, _v) ->
        bprintf f "S_Assign (%a, %a)" pp_lexpr le pp_expr e
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
    | S_While (e, s) -> bprintf f "S_While(%a, %a)" pp_expr e pp_stmt s
    | S_Repeat (s, e) -> bprintf f "S_Repeat(%a, %a)" pp_stmt s pp_expr e
    | S_For (id, e1, dir, e2, s) ->
        bprintf f "S_For (%S, %a, %s, %a, %a)" id pp_expr e1
          (match dir with Up -> "Up" | Down -> "Down")
          pp_expr e2 pp_stmt s
    | S_Decl (ldk, ldi, e_opt) ->
        bprintf f "S_Decl (%a, %a, %a)" pp_local_decl_keyboard ldk
          pp_local_decl_item ldi (pp_option pp_expr) e_opt
    | S_Throw opt ->
        bprintf f "S_Throw (%a)"
          (pp_option (pp_pair pp_expr (pp_option pp_ty)))
          opt
    | S_Try (s, catchers, otherwise) ->
        bprintf f "S_Try (%a, %a, %a)" pp_stmt s (pp_list pp_catcher) catchers
          (pp_option pp_stmt) otherwise
    | S_Print { args; debug } ->
        bprintf f "S_Print { args = %a; debug = %B }" (pp_list pp_expr) args
          debug
  in
  fun f s -> pp_annotated pp_desc f s

and pp_catcher f (name, ty, s) =
  bprintf f "(%a, %a, %a)" (pp_option pp_string) name pp_ty ty pp_stmt s

let pp_gdk f gdk =
  addb f
  @@
  match gdk with
  | GDK_Config -> "GDK_Config"
  | GDK_Constant -> "GDK_Constant"
  | GDK_Let -> "GDK_Let"
  | GDK_Var -> "GDK_Var"

let pp_subprogram_type f st =
  addb f
    (match st with
    | ST_Function -> "ST_Function"
    | ST_Procedure -> "ST_Procedure"
    | ST_Setter -> "ST_Setter"
    | ST_Getter -> "ST_Getter")

let pp_body f = function
  | SB_ASL s -> bprintf f "SB_ASL (%a)" pp_stmt s
  | SB_Primitive -> addb f "SB_Primitive"

let pp_decl f d =
  match d.desc with
  | D_Func { name; args; body; return_type; parameters; subprogram_type } ->
      bprintf f
        "D_Func { name=%S; args=%a; body=%a; return_type=%a; parameters=%a; \
         subprogram_type=%a }"
        name (pp_id_assoc pp_ty) args pp_body body (pp_option pp_ty) return_type
        (pp_list (pp_pair pp_string (pp_option pp_ty)))
        parameters pp_subprogram_type subprogram_type
  | D_GlobalStorage { name; keyword; ty; initial_value } ->
      bprintf f "D_GlobalConst { name=%S; keyword=%a; ty=%a; initial_value=%a}"
        name pp_gdk keyword (pp_option pp_ty) ty (pp_option pp_expr)
        initial_value
  | D_TypeDecl (name, type_desc, subty_opt) ->
      bprintf f "D_TypeDecl (%S, %a, %a)" name pp_ty type_desc
        (pp_option (pp_pair pp_string (pp_id_assoc pp_ty)))
        subty_opt

let pp_t f ast =
  addb f "let open AST in let annot = ASTUtils.add_dummy_pos in ";
  pp_list pp_decl f ast

let t_to_string ast = with_buf @@ fun b -> pp_t b ast
