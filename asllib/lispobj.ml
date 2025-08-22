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

open Format

(** Type of Lisp objects *)
type obj =
  | Num of (Q.t * Q.t)  (** Complex rational *)
  | String of string
  | Symbol of (string * string)  (** Package, symbol *)
  | Char of char
  | Cons of (obj * obj)
  | Comment of (string * obj)
      (** Fake new object type to allow us to add single-line comments *)

let nil = Symbol ("COMMON-LISP", "NIL")
let t = Symbol ("COMMON-LISP", "T")
let quote = Symbol ("COMMON-LISP", "QUOTE")
let zero = Num (Q.zero, Q.zero)
let one = Num (Q.one, Q.zero)

let rec equal x y =
  match (x, y) with
  | Num (rx, ix), Num (ry, iy) -> Q.( = ) rx ry && Q.( = ) ix iy
  | String sx, String sy -> String.equal sx sy
  | Symbol (px, nx), Symbol (py, ny) -> String.equal nx ny && String.equal px py
  | Char cx, Char cy -> Char.equal cx cy
  | Cons (carx, cdrx), Cons (cary, cdry) -> equal carx cary && equal cdrx cdry
  | Comment (sx, ox), Comment (sy, oy) -> String.equal sx sy && equal ox oy
  | _ -> false

module type PrinterConf = sig
  val defaultpkg : string
  val downcase : bool
end

module MakePrinter (Conf : PrinterConf) = struct
  let defaultpkg = Conf.defaultpkg
  let downcase = Conf.downcase

  let symbol_part_str x =
    (* to get this actually right we'd need much better analysis of whether the symbol needs escaping.
       For now we don't expect to see bad characters in symbols much, but we might need mixed case *)
    if x = String.uppercase_ascii x then
      if downcase then String.lowercase_ascii x else x
    else "|" ^ x ^ "|"

  (* In strings we need to escape both quotes and backslashes, so that the Lisp reader will get the right string.
     We need four backslashes to represent each one backslash in both the regex input and output. *)
  let escape_backslashes_and_quotes s =
    let repl_char ch newstr str =
      String.concat newstr (String.split_on_char ch str)
    in
    repl_char '"' "\\\"" (repl_char '\\' "\\\\" s)

  let rec pp_obj f x =
    match x with
    | Num (r, i) ->
        if Q.( = ) i Q.zero then Q.pp_print f r
        else fprintf f "#C(@[<hov 0>%a@ %a@]" Q.pp_print r Q.pp_print i
    | String s -> fprintf f "\"%s\"" (escape_backslashes_and_quotes s)
    | Symbol (pkg, name) ->
        (* BOZO neither packages nor escaping are handled correctly *)
        if String.equal pkg "KEYWORD" then
          fprintf f ":%s" (symbol_part_str name)
        else if String.equal pkg defaultpkg || String.equal pkg "COMMON-LISP"
        then fprintf f "%s" (symbol_part_str name)
        else fprintf f "%s::%s" (symbol_part_str pkg) (symbol_part_str name)
    | Char c -> fprintf f "#\\%s" (Char.escaped c)
    | Cons (car, cdr) ->
        if equal car quote then
          match cdr with
          | Cons (cadr, cddr) ->
              if equal cddr nil then fprintf f "'%a" pp_obj cadr
              else pp_obj_as_list f x
          | _ -> pp_obj_as_list f x
        else pp_obj_as_list f x
    | Comment (str, obj) ->
        fprintf f "@[<v>%a@]%a" pp_comment_lines
          (String.split_on_char '\n' str)
          pp_obj obj

  and pp_comment_lines f lines =
    match lines with
    | line :: rest -> fprintf f ";; %s@;%a" line pp_comment_lines rest
    | _ -> fprintf f ""

  and pp_obj_as_list f x = fprintf f "(@[<hov 0>%a@])" pp_list x

  and pp_list f x =
    match x with
    | Cons (car, cdr) ->
        if equal cdr nil then pp_obj f car
        else fprintf f "%a@ %a" pp_obj car pp_list cdr
    | _ -> fprintf f ".@ %a" pp_obj x
end

let print_obj ?(pkg = "ACL2") f x =
  let module C = struct
    let defaultpkg = pkg
    let downcase = true
  end in
  let module P = MakePrinter (C) in
  P.pp_obj f x

let _test () =
  let foo = Symbol ("ACL2", "FOO") in
  let bar = Symbol ("ACL2", "BAR") in

  let rec make_sexpr n =
    if n <= 0 then
      match Random.int 8 with
      | 0 -> bar
      | 1 -> t
      | 2 -> zero
      | 3 -> one
      | 4 -> foo
      | _ -> nil
    else
      match Random.int 6 with
      | 0 ->
          Comment
            ( "hello this is a single-line comment",
              make_sexpr (n - Random.int (n + 4)) )
      | 1 ->
          Comment
            ( "hello this is a multi\n-line comment",
              make_sexpr (n - Random.int (n + 4)) )
      | _ ->
          Cons
            ( make_sexpr (n - Random.int (n + 4)),
              make_sexpr (n - Random.int (n + 4)) )
  in
  let obj = make_sexpr 100 in
  let _ = print_obj Format.std_formatter obj in
  let obj = make_sexpr 20 in
  let _ = print_obj Format.std_formatter obj in
  obj

open AST
(* -------------------------------------------------------------------------

   Utils

   ------------------------------------------------------------------------- *)

let of_list x = List.fold_right (fun fst rst -> Cons (fst, rst)) x nil
let of_list_map f x = of_list (List.map f x)
let of_seq x = of_list (List.of_seq x)
let of_seq_map f x = of_seq (Seq.map f x)
let rec nth_tl x n = match n with 0 -> x | _ -> nth_tl (List.tl x) (n - 1)

let rec tree_of_list_aux x n =
  match n with
  | 0 -> nil
  | 1 -> List.hd x
  | _ ->
      let half = n / 2 in
      Cons (tree_of_list_aux x half, tree_of_list_aux (nth_tl x half) (n - half))

let tree_of_list x = tree_of_list_aux x (List.length x)
let tagged_tree_of_list x = Cons (List.hd x, tree_of_list (List.tl x))
let tagged_list_of_list x = of_list x
let key str = Symbol ("KEYWORD", str)

let sym_alist pkg x =
  List.map (fun (k, v) -> Cons (Symbol (pkg, k), v)) x |> of_list

let kwd_alist x = sym_alist "KEYWORD" x
let aslsym_alist x = sym_alist "ASL" x
let of_q x = Num (x, Q.zero)
let of_int x = of_q (Q.of_int x)
let of_bigint x = of_q (Q.of_bigint x)
let of_bool x = if x then t else nil
let of_option f = function Some x -> f x | None -> nil
let of_version v = key (match v with V0 -> "V0" | V1 -> "V1")

let of_position x =
  let open Lexing in
  aslsym_alist
    [
      ("FNAME", String x.pos_fname);
      ("LNUM", of_int x.pos_lnum);
      ("BOL", of_int x.pos_bol);
      ("CNUM", of_int x.pos_cnum);
    ]

let of_annotated of_a x =
  if false then of_a x.desc
  else
    aslsym_alist
      [ ("DESC", of_a x.desc); ("POS_START", of_position x.pos_start) ]

let of_identifier x = String x
let of_uid x = of_int x

(* -------------------------------------------------------------------------

   Operations

   ------------------------------------------------------------------------- *)

let of_unop x =
  key (match x with BNOT -> "BNOT" | NEG -> "NEG" | NOT -> "NOT")

let of_binop (x : binop) =
  key
    (match x with
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
    | `POW -> "POW"
    | `RDIV -> "RDIV"
    | `SHL -> "SHL"
    | `SHR -> "SHR"
    | `BV_CONCAT -> "BV_CONCAT"
    | `STR_CONCAT -> "STR_CONCAT"
    | `BIC -> "BIC")

(* -------------------------------------------------------------------------

   Parsed values

   ------------------------------------------------------------------------- *)

let of_literal x =
  tagged_list_of_list
    (match x with
    | L_Int i -> [ key "L_INT"; of_bigint i ]
    | L_Bool b -> [ key "L_BOOL"; of_bool b ]
    | L_Real q -> [ key "L_REAL"; of_q q ]
    | L_BitVector v ->
        [
          key "L_BITVECTOR";
          of_int (Bitvector.length v);
          of_bigint (Bitvector.to_z_unsigned v);
        ]
    | L_String s -> [ key "L_STRING"; String s ]
    | L_Label s -> [ key "L_LABEL"; String s ])

(* -------------------------------------------------------------------------

   Expressions

   ------------------------------------------------------------------------- *)

let of_subprogram_type x =
  key
    (match x with
    | ST_Procedure -> "ST_PROCEDURE"
    | ST_Function -> "ST_FUNCTION"
    | ST_Getter -> "ST_GETTER"
    | ST_EmptyGetter -> "ST_EMPTYGETTER"
    | ST_Setter -> "ST_SETTER"
    | ST_EmptySetter -> "ST_EMPTYSETTER")

let of_bitvector_mask x =
  aslsym_alist
    [
      ("LENGTH", of_int (Bitvector.mask_length x));
      ("SET", of_bigint (Bitvector.to_z_unsigned (Bitvector.mask_set x)));
      ("UNSET", of_bigint (Bitvector.to_z_unsigned (Bitvector.mask_unset x)));
    ]

(* Note: This could be represented by the key alone, but since the
   precision_loss_flag type is a sum-of-products rather than just an
   enum (Precision_Lost contains a delayed_warning list) we're keeping
   it in the sum-of-products Lisp format for uniformity. *)
let of_precision_loss_flag x =
  tagged_list_of_list
    (match x with
    | Precision_Full -> [ key "PRECISION_FULL" ]
    | Precision_Lost _ -> [ key "PRECISION_LOST" ])

let rec of_expr_desc x =
  tagged_list_of_list
    (match x with
    | E_Literal l -> [ key "E_LITERAL"; of_literal l ]
    | E_Var v -> [ key "E_VAR"; of_identifier v ]
    | E_ATC (x, t) -> [ key "E_ATC"; of_expr x; of_ty t ]
    | E_Binop (op, x, y) -> [ key "E_BINOP"; of_binop op; of_expr x; of_expr y ]
    | E_Unop (op, x) -> [ key "E_UNOP"; of_unop op; of_expr x ]
    | E_Call c -> [ key "E_CALL"; of_call c ]
    | E_Slice (x, s) -> [ key "E_SLICE"; of_expr x; of_list_map of_slice s ]
    | E_Cond (x, y, z) -> [ key "E_COND"; of_expr x; of_expr y; of_expr z ]
    | E_GetArray (x, y) -> [ key "E_GETARRAY"; of_expr x; of_expr y ]
    | E_GetEnumArray (x, y) -> [ key "E_GETENUMARRAY"; of_expr x; of_expr y ]
    | E_GetField (x, i) -> [ key "E_GETFIELD"; of_expr x; of_identifier i ]
    | E_GetFields (x, i) ->
        [ key "E_GETFIELDS"; of_expr x; of_list_map of_identifier i ]
    | E_GetCollectionFields (i, ilst) ->
        [
          key "E_GETCOLLECTIONFIELDS";
          of_identifier i;
          of_list_map of_identifier ilst;
        ]
    | E_GetItem (x, i) -> [ key "E_GETITEM"; of_expr x; of_int i ]
    | E_Record (t, lst) ->
        [
          key "E_RECORD";
          of_ty t;
          of_list_map (fun (i, e) -> Cons (of_identifier i, of_expr e)) lst;
        ]
    | E_Tuple lst -> [ key "E_TUPLE"; of_list_map of_expr lst ]
    | E_Array { length; value } ->
        [ key "E_ARRAY"; of_expr length; of_expr value ]
    | E_EnumArray { enum; labels; value } ->
        [
          key "E_ENUMARRAY";
          of_identifier enum;
          of_list_map of_identifier labels;
          of_expr value;
        ]
    | E_Arbitrary t -> [ key "E_ARBITRARY"; of_ty t ]
    | E_Pattern (x, p) -> [ key "E_PATTERN"; of_expr x; of_pattern p ])

and of_expr x = of_annotated of_expr_desc x

and of_pattern_desc x =
  tagged_list_of_list
    (match x with
    | Pattern_All -> [ key "PATTERN_ALL" ]
    | Pattern_Any lst -> [ key "PATTERN_ANY"; of_list_map of_pattern lst ]
    | Pattern_Geq x -> [ key "PATTERN_GEQ"; of_expr x ]
    | Pattern_Leq x -> [ key "PATTERN_LEQ"; of_expr x ]
    | Pattern_Mask m -> [ key "PATTERN_MASK"; of_bitvector_mask m ]
    | Pattern_Not p -> [ key "PATTERN_NOT"; of_pattern p ]
    | Pattern_Range (x, y) -> [ key "PATTERN_RANGE"; of_expr x; of_expr y ]
    | Pattern_Single x -> [ key "PATTERN_SINGLE"; of_expr x ]
    | Pattern_Tuple lst -> [ key "PATTERN_TUPLE"; of_list_map of_pattern lst ])

and of_pattern x = of_annotated of_pattern_desc x

and of_slice x =
  tagged_list_of_list
    (match x with
    | Slice_Single x -> [ key "SLICE_SINGLE"; of_expr x ]
    | Slice_Range (x, y) -> [ key "SLICE_RANGE"; of_expr x; of_expr y ]
    | Slice_Length (x, y) -> [ key "SLICE_LENGTH"; of_expr x; of_expr y ]
    | Slice_Star (x, y) -> [ key "SLICE_STAR"; of_expr x; of_expr y ])

and of_call (x : call) =
  aslsym_alist
    [
      ("NAME", of_identifier x.name);
      ("PARAMS", of_list_map of_expr x.params);
      ("ARGS", of_list_map of_expr x.args);
      ("CALL_TYPE", of_subprogram_type x.call_type);
    ]

(* -------------------------------------------------------------------------

   Types

   ------------------------------------------------------------------------- *)

and of_type_desc x =
  tagged_list_of_list
    (match x with
    | T_Int c -> [ key "T_INT"; of_constraint_kind c ]
    | T_Bits (x, f) -> [ key "T_BITS"; of_expr x; of_list_map of_bitfield f ]
    | T_Real -> [ key "T_REAL" ]
    | T_String -> [ key "T_STRING" ]
    | T_Bool -> [ key "T_BOOL" ]
    | T_Enum i -> [ key "T_ENUM"; of_list_map of_identifier i ]
    | T_Tuple t -> [ key "T_TUPLE"; of_list_map of_ty t ]
    | T_Array (i, t) -> [ key "T_ARRAY"; of_array_index i; of_ty t ]
    | T_Record f -> [ key "T_RECORD"; of_list_map of_field f ]
    | T_Exception f -> [ key "T_EXCEPTION"; of_list_map of_field f ]
    | T_Collection f -> [ key "T_COLLECTION"; of_list_map of_field f ]
    | T_Named i -> [ key "T_NAMED"; of_identifier i ])

and of_ty x = of_annotated of_type_desc x

and of_int_constraint x =
  tagged_list_of_list
    (match x with
    | Constraint_Exact x -> [ key "CONSTRAINT_EXACT"; of_expr x ]
    | Constraint_Range (x, y) ->
        [ key "CONSTRAINT_RANGE"; of_expr x; of_expr y ])

and of_constraint_kind (x : constraint_kind) =
  tagged_list_of_list
    (match x with
    | UnConstrained -> [ key "UNCONSTRAINED" ]
    | WellConstrained (lst, flg) ->
        [
          key "WELLCONSTRAINED";
          of_list_map of_int_constraint lst;
          of_precision_loss_flag flg;
        ]
    | PendingConstrained -> [ key "PENDINGCONSTRAINED" ]
    | Parameterized i -> [ key "PARAMETRIZED"; of_identifier i ])

and of_bitfield x =
  tagged_list_of_list
    (match x with
    | BitField_Simple (i, s) ->
        [ key "BITFIELD_SIMPLE"; of_identifier i; of_list_map of_slice s ]
    | BitField_Nested (i, s, b) ->
        [
          key "BITFIELD_NESTED";
          of_identifier i;
          of_list_map of_slice s;
          of_list_map of_bitfield b;
        ]
    | BitField_Type (i, s, t) ->
        [
          key "BITFIELD_TYPE"; of_identifier i; of_list_map of_slice s; of_ty t;
        ])

and of_array_index x =
  tagged_list_of_list
    (match x with
    | ArrayLength_Expr x -> [ key "ARRAYLENGTH_EXPR"; of_expr x ]
    | ArrayLength_Enum (i, lst) ->
        [
          key "ARRAYLENGTH_ENUM"; of_identifier i; of_list_map of_identifier lst;
        ])

and of_field (id, t) = Cons (of_identifier id, of_ty t)
and of_typed_identifier (id, t) = Cons (of_identifier id, of_ty t)

(* -------------------------------------------------------------------------

   l-expressions and statements

   ------------------------------------------------------------------------- *)

let rec of_lexpr_desc x =
  tagged_list_of_list
    (match x with
    | LE_Discard -> [ key "LE_DISCARD" ]
    | LE_Var i -> [ key "LE_VAR"; of_identifier i ]
    | LE_Slice (lx, s) ->
        [ key "LE_SLICE"; of_lexpr lx; of_list_map of_slice s ]
    | LE_SetArray (lx, x) -> [ key "LE_SETARRAY"; of_lexpr lx; of_expr x ]
    | LE_SetEnumArray (lx, x) ->
        [ key "LE_SETENUMARRAY"; of_lexpr lx; of_expr x ]
    | LE_SetField (lx, i) -> [ key "LE_SETFIELD"; of_lexpr lx; of_identifier i ]
    | LE_SetFields (lx, i, pairs) ->
        [
          key "LE_SETFIELDS";
          of_lexpr lx;
          of_list_map of_identifier i;
          of_list_map (fun (x, y) -> Cons (of_int x, of_int y)) pairs;
        ]
    | LE_SetCollectionFields (i, ilst, pairs) ->
        [
          key "LE_SETCOLLECTIONFIELDS";
          of_identifier i;
          of_list_map of_identifier ilst;
          of_list_map (fun (x, y) -> Cons (of_int x, of_int y)) pairs;
        ]
    | LE_Destructuring lx -> [ key "LE_DESTRUCTURING"; of_list_map of_lexpr lx ])

and of_lexpr x = of_annotated of_lexpr_desc x

let of_local_decl_keyword x =
  key
    (match x with
    | LDK_Var -> "LDK_VAR"
    | LDK_Constant -> "LDK_CONSTANT"
    | LDK_Let -> "LDK_LET")

let of_local_decl_item x =
  tagged_list_of_list
    (match x with
    | LDI_Var i -> [ key "LDI_VAR"; of_identifier i ]
    | LDI_Tuple i -> [ key "LDI_TUPLE"; of_list_map of_identifier i ])

let of_for_direction x = key (match x with Up -> "UP" | Down -> "DOWN")

let rec of_stmt_desc x =
  tagged_list_of_list
    (match x with
    | S_Pass -> [ key "S_PASS" ]
    | S_Seq (s1, s2) -> [ key "S_SEQ"; of_stmt s1; of_stmt s2 ]
    | S_Decl (k, i, ty, x) ->
        [
          key "S_DECL";
          of_local_decl_keyword k;
          of_local_decl_item i;
          of_option of_ty ty;
          of_option of_expr x;
        ]
    | S_Assign (lx, x) -> [ key "S_ASSIGN"; of_lexpr lx; of_expr x ]
    | S_Call c -> [ key "S_CALL"; of_call c ]
    | S_Return x -> [ key "S_RETURN"; of_option of_expr x ]
    | S_Cond (x, s1, s2) -> [ key "S_COND"; of_expr x; of_stmt s1; of_stmt s2 ]
    | S_Assert x -> [ key "S_ASSERT"; of_expr x ]
    | S_For { index_name; start_e; dir; end_e; body; limit } ->
        [
          key "S_FOR";
          of_identifier index_name;
          of_expr start_e;
          of_for_direction dir;
          of_expr end_e;
          of_stmt body;
          of_option of_expr limit;
        ]
    | S_While (x, y, s) ->
        [ key "S_WHILE"; of_expr x; of_option of_expr y; of_stmt s ]
    | S_Repeat (s, x, y) ->
        [ key "S_REPEAT"; of_stmt s; of_expr x; of_option of_expr y ]
    | S_Throw opt ->
        [
          key "S_THROW";
          of_option (fun (x, t) -> of_list [ of_expr x; of_option of_ty t ]) opt;
        ]
    | S_Try (s1, c, s2) ->
        [
          key "S_TRY";
          of_stmt s1;
          of_list_map of_catcher c;
          of_option of_stmt s2;
        ]
    | S_Print { args; newline; debug } ->
        [
          key "S_PRINT";
          of_list_map of_expr args;
          of_bool newline;
          of_bool debug;
        ]
    | S_Unreachable -> [ key "S_UNREACHABLE" ]
    | S_Pragma (i, x) ->
        [ key "S_PRAGMA"; of_identifier i; of_list_map of_expr x ])

and of_stmt x = of_annotated of_stmt_desc x

and of_case_alt_desc x =
  aslsym_alist
    [
      ("PATTERN", of_pattern x.pattern);
      ("WHERE", of_option of_expr x.where);
      ("STMT", of_stmt x.stmt);
    ]

and of_case_alt x = of_annotated of_case_alt_desc x

and of_catcher (i, t, s) =
  of_list [ of_option of_identifier i; of_ty t; of_stmt s ]

(* -------------------------------------------------------------------------

   Functions and declarations

   ------------------------------------------------------------------------- *)

let of_subprogram_body x =
  tagged_list_of_list
    (match x with
    | SB_ASL s -> [ key "SB_ASL"; of_stmt s ]
    | SB_Primitive b -> [ key "SB_PRIMITIVE"; of_bool b ])

let of_func (x : func) =
  aslsym_alist
    [
      ("NAME", of_identifier x.name);
      ( "PARAMETERS",
        of_list_map
          (fun (i, t) -> Cons (of_identifier i, of_option of_ty t))
          x.parameters );
      ("ARGS", of_list_map of_typed_identifier x.args);
      ("BODY", of_subprogram_body x.body);
      ("RETURN_TYPE", of_option of_ty x.return_type);
      ("SUBPROGRAM_TYPE", of_subprogram_type x.subprogram_type);
      ("RECURSE_LIMIT", of_option of_expr x.recurse_limit);
      ("BUILTIN", of_bool x.builtin);
    ]

let of_global_decl_keyword x =
  key
    (match x with
    | GDK_Constant -> "GDK_CONSTANT"
    | GDK_Config -> "GDK_CONFIG"
    | GDK_Let -> "GDK_LET"
    | GDK_Var -> "GDK_VAR")

let of_global_decl x =
  aslsym_alist
    [
      ("KEYWORD", of_global_decl_keyword x.keyword);
      ("NAME", of_identifier x.name);
      ("TY", of_option of_ty x.ty);
      ("INITIAL_VALUE", of_option of_expr x.initial_value);
    ]

let of_decl_desc x =
  tagged_list_of_list
    (match x with
    | D_Func f -> [ key "D_FUNC"; of_func f ]
    | D_GlobalStorage d -> [ key "D_GLOBALSTORAGE"; of_global_decl d ]
    | D_TypeDecl (i, ty, opt) ->
        [
          key "D_TYPEDECL";
          of_identifier i;
          of_ty ty;
          of_option
            (fun (i, f) -> Cons (of_identifier i, of_list_map of_field f))
            opt;
        ]
    | D_Pragma (i, x) ->
        [ key "D_PRAGMA"; of_identifier i; of_list_map of_expr x ])

let of_decl x = of_annotated of_decl_desc x
let of_ast (x : AST.t) = of_list_map of_decl x

let of_imap of_a x =
  of_list_map (fun (k, v) -> Cons (String k, of_a v)) (ASTUtils.IMap.bindings x)

let of_iset x = of_list_map (fun x -> String x) (ASTUtils.ISet.elements x)

let of_timeframe (x : SideEffect.TimeFrame.t) =
  key (match x with Constant -> "CONSTANT" | Execution -> "EXECUTION")

let of_read (x : SideEffect.read) =
  aslsym_alist
    [
      ("NAME", of_identifier x.name);
      ("TIME_FRAME", of_timeframe x.time_frame);
      ("IMMUTABLE", of_bool x.immutable);
    ]

let of_side_effect (x : SideEffect.t) =
  tagged_list_of_list
    (match x with
    | ReadsLocal r -> [ key "READSLOCAL"; of_read r ]
    | WritesLocal w -> [ key "WRITESLOCAL"; of_identifier w ]
    | ReadsGlobal r -> [ key "READSGLOBAL"; of_read r ]
    | WritesGlobal w -> [ key "WRITESGLOBAL"; of_identifier w ]
    | ThrowsException i -> [ key "THROWSEXCEPTION"; of_identifier i ]
    | CallsRecursive i -> [ key "CALLSRECURSIVE"; of_identifier i ]
    | PerformsAssertions -> [ key "PERFORMSASSERTIONS" ]
    | NonDeterministic -> [ key "NONDETERMINISTIC" ]
    | Prints -> [ key "PRINTS" ])

let of_ses (x : SideEffect.SES.t) =
  of_list_map of_side_effect (SideEffect.SES.to_side_effect_list x)

let of_storage of_v (x : 'v Storage.t) =
  of_seq_map (fun (k, v) -> Cons (String k, of_v v)) (Storage.to_seq x)

let of_static_env_global (x : StaticEnv.global) =
  aslsym_alist
    [
      ( "DECLARED_TYPES",
        of_imap
          (fun (ty, tf) -> Cons (of_ty ty, of_timeframe tf))
          x.declared_types );
      ("CONSTANT_VALUES", of_storage of_literal x.constant_values);
      ( "STORAGE_TYPES",
        of_imap
          (fun (ty, kw) -> Cons (of_ty ty, of_global_decl_keyword kw))
          x.storage_types );
      ("SUBTYPES", of_imap of_identifier x.subtypes);
      ( "SUBPROGRAMS",
        of_imap
          (fun (func, ses) -> Cons (of_func func, of_ses ses))
          x.subprograms );
      ("OVERLOADED_SUBPROGRAMS", of_imap of_iset x.overloaded_subprograms);
      ("EXPR_EQUIV", of_imap of_expr x.expr_equiv);
    ]

let of_static_env_local (x : StaticEnv.local) =
  aslsym_alist
    [
      ("CONSTANT_VALUES", of_storage of_literal x.constant_values);
      ( "STORAGE_TYPES",
        of_imap
          (fun (ty, kw) -> Cons (of_ty ty, of_local_decl_keyword kw))
          x.storage_types );
      ("EXPR_EQUIV", of_imap of_expr x.expr_equiv);
      ("RETURN_TYPE", of_option of_ty x.return_type);
    ]

let of_static_env (x : StaticEnv.env) =
  aslsym_alist
    [
      ("GLOBAL", of_static_env_global x.global);
      ("LOCAL", of_static_env_local x.local);
    ]
