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
let pp_int f = bprintf f "%d"
let pp_string f = bprintf f "%S"
let pp_id_assoc pp_elt = pp_pair_list pp_string pp_elt
let pp_annotated f buf { desc; _ } = bprintf buf "annot (%a)" f desc

let pp_binop : binop -> string = function
  | `AND -> "AND"
  | `BAND -> "BAND"
  | `BEQ -> "BEQ"
  | `BOR -> "BOR"
  | `DIV -> "DIV"
  | `DIVRM -> "DIVRM"
  | `XOR -> "XOR"
  | `EQ -> "EQ"
  | `GT -> "GT"
  | `GE -> "GE"
  | `IMPL -> "IMPL"
  | `LT -> "LT"
  | `LE -> "LE"
  | `MOD -> "MOD"
  | `SUB -> "SUB"
  | `MUL -> "MUL"
  | `NE -> "NE"
  | `OR -> "OR"
  | `ADD -> "ADD"
  | `RDIV -> "RDIV"
  | `SHL -> "SHL"
  | `SHR -> "SHR"
  | `POW -> "POW"
  | `BV_CONCAT -> "BV_CONCAT"
  | `STR_CONCAT -> "STR_CONCAT"
  | `BIC -> "BIC"

let pp_unop = function BNOT -> "BNOT" | NOT -> "NOT" | NEG -> "NEG"

let pp_literal f = function
  | L_Int i -> bprintf f "L_Int (Z.of_string \"%a\")" Z.bprint i
  | L_Bool b -> bprintf f "L_Bool %B" b
  | L_Real r -> bprintf f "L_Real (Q.of_string \"%a\")" Q.bprint r
  | L_BitVector bv ->
      bprintf f "L_BitVector (Bitvector.of_string %S)" (Bitvector.to_string bv)
  | L_String s -> bprintf f "L_String %S" s
  | L_Label s -> bprintf f "L_Label %S" s

let subprogram_type_to_string = function
  | ST_Function -> "ST_Function"
  | ST_Procedure -> "ST_Procedure"
  | ST_Setter -> "ST_Setter"
  | ST_Getter -> "ST_Getter"
  | ST_EmptyGetter -> "ST_EmptyGetter"
  | ST_EmptySetter -> "ST_EmptySetter"

let pp_subprogram_type f st = addb f (subprogram_type_to_string st)

let rec pp_expr =
  let pp_desc f = function
    | E_Literal v -> bprintf f "E_Literal (%a)" pp_literal v
    | E_Var x -> bprintf f "E_Var %S" x
    | E_ATC (e, t) -> bprintf f "E_ATC (%a, %a)" pp_expr e pp_ty t
    | E_Binop (op, e1, e2) ->
        bprintf f "E_Binop (%s, %a, %a)" (pp_binop op) pp_expr e1 pp_expr e2
    | E_Unop (op, e) -> bprintf f "E_Unop (%s, %a)" (pp_unop op) pp_expr e
    | E_Call { name; args; params; call_type } ->
        bprintf f "E_Call {name=%S; args=%a; params=%a; call_type=%a}" name
          pp_expr_list args pp_expr_list params pp_subprogram_type call_type
    | E_Slice (e, args) ->
        bprintf f "E_Slice (%a, %a)" pp_expr e pp_slice_list args
    | E_Cond (e1, e2, e3) ->
        bprintf f "E_Cond (%a, %a, %a)" pp_expr e1 pp_expr e2 pp_expr e3
    | E_GetArray (e1, e2) ->
        bprintf f "E_GetArray (%a, %a)" pp_expr e1 pp_expr e2
    | E_GetEnumArray (e1, e2) ->
        bprintf f "E_GetEnumArray (%a, %a)" pp_expr e1 pp_expr e2
    | E_GetField (e, x) -> bprintf f "E_GetField (%a, %S)" pp_expr e x
    | E_GetFields (e, x) ->
        bprintf f "E_GetFields (%a, %a)" pp_expr e (pp_list pp_string) x
    | E_GetCollectionFields (x, fs) ->
        bprintf f "E_GetCollectionFields (%S, %a)" x (pp_list pp_string) fs
    | E_GetItem (e, i) -> bprintf f "E_GetItem (%a, %d)" pp_expr e i
    | E_Record (ty, li) ->
        bprintf f "E_Record (%a, %a)" pp_ty ty (pp_id_assoc pp_expr) li
    | E_Tuple es ->
        addb f "E_Tuple ";
        pp_expr_list f es
    | E_Array { length; value } ->
        bprintf f "E_Array { length=(%a); value=(%a) }" pp_expr length pp_expr
          value
    | E_EnumArray { enum; labels; value } ->
        bprintf f "E_EnumArray { enum=%S; labels=(%a); value=(%a) }" enum
          (pp_list pp_string) labels pp_expr value
    | E_Arbitrary ty -> bprintf f "E_Arbitrary (%a)" pp_ty ty
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

and pp_pattern =
  let pp_desc f = function
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
  in
  fun f p -> pp_annotated pp_desc f p

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
    | T_Collection li ->
        addb f "T_Collection ";
        pp_id_assoc pp_ty f li
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
  | ArrayLength_Enum (enum, labels) ->
      bprintf f "ArrayLength_Enum (%s, %a)" enum (pp_list pp_string) labels

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
  | WellConstrained (cs, precision_loss) ->
      bprintf f "WellConstrained (%a, %a)"
        (pp_list pp_int_constraint)
        cs pp_precision_loss precision_loss
  | PendingConstrained -> addb f "PendingConstrained"
  | Parameterized x -> bprintf f "Parameterized %S" x

and pp_precision_loss f = function
  | Precision_Full -> addb f "PrecisionFull"
  | Precision_Lost _ -> addb f "PrecisionLost []"

let rec pp_lexpr =
  let pp_desc f = function
    | LE_Var x -> bprintf f "LE_Var %S" x
    | LE_Slice (le, args) ->
        bprintf f "LE_Slice (%a, %a)" pp_lexpr le pp_slice_list args
    | LE_SetArray (le, e) ->
        bprintf f "LE_SetArray (%a, %a)" pp_lexpr le pp_expr e
    | LE_SetEnumArray (le, e) ->
        bprintf f "LE_SetEnumArray (%a, %a)" pp_lexpr le pp_expr e
    | LE_SetField (le, x) -> bprintf f "LE_SetField (%a, %S)" pp_lexpr le x
    | LE_SetFields (le, x, _) ->
        bprintf f "LE_SetFields (%a, %a)" pp_lexpr le (pp_list pp_string) x
    | LE_SetCollectionFields (le, fields, slices) ->
        bprintf f "LE_SetCollectionFields (%S, %a, %a)" le (pp_list pp_string)
          fields
          (pp_pair_list pp_int pp_int)
          slices
    | LE_Discard -> addb f "LE_Discard"
    | LE_Destructuring les ->
        addb f "LE_Destructuring ";
        pp_list pp_lexpr f les
  in
  fun f le -> pp_annotated pp_desc f le

let pp_local_decl_keyboard f k =
  pp_string f
    (match k with
    | LDK_Var -> "LDK_Var"
    | LDK_Constant -> "LDK_Constant"
    | LDK_Let -> "LDK_Let")

let pp_local_decl_item f = function
  | LDI_Var s -> bprintf f "LDI_Var %S" s
  | LDI_Tuple ldis -> bprintf f "LDI_Tuple %a" (pp_list pp_string) ldis

let rec pp_stmt =
  let pp_desc f = function
    | S_Pass -> addb f "SPass"
    | S_Seq (s1, s2) -> bprintf f "S_Seq (%a, %a)" pp_stmt s1 pp_stmt s2
    | S_Assign (le, e) -> bprintf f "S_Assign (%a, %a)" pp_lexpr le pp_expr e
    | S_Call { name; args; params; call_type } ->
        bprintf f "S_Call {name=%S; args=%a; params=%a; call_type=%a}" name
          pp_expr_list args pp_expr_list params pp_subprogram_type call_type
    | S_Cond (e, s1, s2) ->
        bprintf f "S_Cond (%a, %a, %a)" pp_expr e pp_stmt s1 pp_stmt s2
    | S_Return e -> bprintf f "S_Return (%a)" (pp_option pp_expr) e
    | S_Assert e -> bprintf f "S_Assert (%a)" pp_expr e
    | S_While (e, limit, s) ->
        bprintf f "S_While(%a, %a, %a)" pp_expr e (pp_option pp_expr) limit
          pp_stmt s
    | S_Repeat (s, e, limit) ->
        bprintf f "S_Repeat(%a, %a, %a)" pp_stmt s pp_expr e (pp_option pp_expr)
          limit
    | S_For { index_name; start_e; end_e; body; dir; limit } ->
        bprintf f
          "S_For { index_name=%S; start=%a; dir=%s; end_=%a; body=%a; limit=%a \
           }"
          index_name pp_expr start_e
          (match dir with Up -> "Up" | Down -> "Down")
          pp_expr end_e pp_stmt body (pp_option pp_expr) limit
    | S_Decl (ldk, ldi, ty_opt, e_opt) ->
        bprintf f "S_Decl (%a, %a, %a, %a)" pp_local_decl_keyboard ldk
          pp_local_decl_item ldi (pp_option pp_ty) ty_opt (pp_option pp_expr)
          e_opt
    | S_Throw opt ->
        bprintf f "S_Throw (%a)"
          (pp_option (pp_pair pp_expr (pp_option pp_ty)))
          opt
    | S_Try (s, catchers, otherwise) ->
        bprintf f "S_Try (%a, %a, %a)" pp_stmt s (pp_list pp_catcher) catchers
          (pp_option pp_stmt) otherwise
    | S_Print { args; newline; debug } ->
        bprintf f "S_Print { args = %a; newline = %B; debug = %B }"
          (pp_list pp_expr) args newline debug
    | S_Unreachable -> addb f "S_Unreachable"
    | S_Pragma (name, exprs) ->
        bprintf f "S_Pragma (%S, %a)" name (pp_list pp_expr) exprs
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

let pp_body f = function
  | SB_ASL s -> bprintf f "SB_ASL (%a)" pp_stmt s
  | SB_Primitive b -> bprintf f "SB_Primitive %B" b

let pp_override_info f override =
  addb f
  @@
  match override with Impdef -> "Impdef" | Implementation -> "Implementation"

let pp_decl f d =
  match d.desc with
  | D_Func
      { name; args; body; return_type; parameters; subprogram_type; override }
    ->
      bprintf f
        "D_Func { name=%S; args=%a; body=%a; return_type=%a; parameters=%a; \
         subprogram_type=%a; override=%a }"
        name (pp_id_assoc pp_ty) args pp_body body (pp_option pp_ty) return_type
        (pp_list (pp_pair pp_string (pp_option pp_ty)))
        parameters pp_subprogram_type subprogram_type
        (pp_option pp_override_info)
        override
  | D_GlobalStorage { name; keyword; ty; initial_value } ->
      bprintf f "D_GlobalConst { name=%S; keyword=%a; ty=%a; initial_value=%a}"
        name pp_gdk keyword (pp_option pp_ty) ty (pp_option pp_expr)
        initial_value
  | D_TypeDecl (name, type_desc, subty_opt) ->
      bprintf f "D_TypeDecl (%S, %a, %a)" name pp_ty type_desc
        (pp_option (pp_pair pp_string (pp_id_assoc pp_ty)))
        subty_opt
  | D_Pragma (name, exprs) ->
      bprintf f "D_Pragma (%S, %a)" name (pp_list pp_expr) exprs

let pp_t f ast =
  addb f "let open AST in let annot = ASTUtils.add_dummy_pos in ";
  pp_list pp_decl f ast

let t_to_string ast = with_buf @@ fun b -> pp_t b ast
