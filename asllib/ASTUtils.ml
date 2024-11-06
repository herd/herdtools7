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

module ISet = struct
  include Set.Make (String)

  let of_option = function None -> empty | Some s -> singleton s

  let pp_print f t =
    let open Format in
    let pp_comma f () = fprintf f ",@ " in
    fprintf f "@[{@,%a}@]"
      (pp_print_list ~pp_sep:pp_comma pp_print_string)
      (elements t)
end

module IMap = struct
  include Map.Make (String)

  let of_list li =
    List.fold_left (fun acc (key, value) -> add key value acc) empty li

  let pp_print pp_elt f t =
    let open Format in
    let pp_comma f () = fprintf f ",@ " in
    let pp_one f (name, v) = fprintf f "@[<h>%s:@ @[%a@]@]" name pp_elt v in
    fprintf f "{@[@,%a@]}" (pp_print_list ~pp_sep:pp_comma pp_one) (bindings t)
end

let dummy_pos = Lexing.dummy_pos
let desc v = v.desc
let annotated desc pos_start pos_end = { desc; pos_start; pos_end }
let add_dummy_pos desc = annotated desc dummy_pos dummy_pos
let dummy_annotated = add_dummy_pos ()
let to_pos pos = { pos with desc = () }

let add_pos_from_st pos desc =
  if pos.desc == desc then pos else { pos with desc }

let add_pos_from pos desc = { pos with desc }
let with_pos_from pos { desc; _ } = add_pos_from pos desc
let map_desc f thing = f thing |> add_pos_from thing
let map_desc_st' thing f = f thing.desc |> add_pos_from thing

let add_pos_from_pos_of ((fname, lnum, cnum, enum), desc) =
  let open Lexing in
  let common =
    { pos_fname = fname; pos_lnum = lnum; pos_bol = 0; pos_cnum = 0 }
  in
  {
    desc;
    pos_start = { common with pos_cnum = cnum };
    pos_end = { common with pos_cnum = enum };
  }

let list_equal equal li1 li2 =
  li1 == li2 || (List.compare_lengths li1 li2 = 0 && List.for_all2 equal li1 li2)

let rec list_compare cmp l1 l2 =
  (* List.compare available >= 4.12 *)
  match (l1, l2) with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | a1 :: l1, a2 :: l2 ->
      let c = cmp a1 a2 in
      if c <> 0 then c else list_compare cmp l1 l2

(* Straight out of stdlib v4.11 *)
let list_fold_left_map f accu l =
  let rec aux accu l_accu = function
    | [] -> (accu, List.rev l_accu)
    | x :: l ->
        let accu, x = f accu x in
        aux accu (x :: l_accu) l
  in
  aux accu [] l

(* Straigh out of stdlib v4.10 *)
let list_concat_map f l =
  let open List in
  let rec aux f acc = function
    | [] -> rev acc
    | x :: l ->
        let xs = f x in
        aux f (rev_append xs acc) l
  in
  aux f [] l

let list_is_empty = function [] -> true | _ -> false
let pair x y = (x, y)
let pair' y x = (x, y)
let pair_equal f g (x1, y1) (x2, y2) = f x1 x2 && g y1 y2

let map2_desc f thing1 thing2 =
  {
    desc = f thing1 thing2;
    pos_start = thing1.pos_start;
    pos_end = thing2.pos_end;
  }

let s_pass = add_dummy_pos S_Pass
let s_then = map2_desc (fun s1 s2 -> S_Seq (s1, s2))
let boolean = T_Bool |> add_dummy_pos
let integer' = T_Int UnConstrained
let integer = integer' |> add_dummy_pos
let integer_exact' e = T_Int (WellConstrained [ Constraint_Exact e ])
let integer_exact e = integer_exact' e |> add_dummy_pos
let string = T_String |> add_dummy_pos
let real = T_Real |> add_dummy_pos

let stmt_from_list : stmt list -> stmt =
  let is_not_s_pass = function { desc = S_Pass; _ } -> false | _ -> true in
  let rec one_step acc = function
    | [] -> List.rev acc
    | [ x ] -> List.rev (x :: acc)
    | s1 :: s2 :: t -> one_step (s_then s1 s2 :: acc) t
  in
  let rec aux = function
    | [] -> s_pass
    | [ x ] -> x
    | l -> aux @@ one_step [] l
  in
  fun l -> List.filter is_not_s_pass l |> aux

let mask_from_set_bits_positions size pos =
  let buf = Bytes.make size '0' in
  let set i = Bytes.set buf i '1' in
  let () = List.iter set pos in
  Bytes.to_string buf

let inv_mask =
  let one_char = function '0' -> '1' | '1' -> '0' | c -> c in
  String.map one_char

let slices_to_positions as_int =
  let one_slice (start, length) =
    let start = as_int start and length = as_int length in
    (* Reversed interval - recall that bitvectors are reversed. *)
    if length >= 0 then List.init length (( - ) (start + length - 1))
    else raise (Invalid_argument "slices_to_positions")
  in
  fun positions -> List.map one_slice positions |> List.flatten

let fold_named_list folder acc list =
  List.fold_left (fun acc (_, v) -> folder acc v) acc list

let ( $ ) f1 f2 acc = f1 acc |> f2
let use_option use_elt = function None -> Fun.id | Some elt -> use_elt elt
let use_list use_elt elts acc = List.fold_left (Fun.flip use_elt) acc elts

let use_named_list use_elt named_elts acc =
  fold_named_list (Fun.flip use_elt) acc named_elts

let rec use_e e =
  match e.desc with
  | E_Literal _ -> Fun.id
  | E_ATC (e, ty) -> use_ty ty $ use_e e
  | E_Var x -> ISet.add x
  | E_GetArray (e1, e2) | E_Binop (_, e1, e2) -> use_e e1 $ use_e e2
  | E_Unop (_op, e) -> use_e e
  | E_Call (x, args, named_args) ->
      ISet.add x $ use_fields named_args $ use_es args
  | E_Slice (e, slices) -> use_e e $ use_slices slices
  | E_Cond (e1, e2, e3) -> use_e e1 $ use_e e2 $ use_e e3
  | E_GetItem (e, _) -> use_e e
  | E_GetField (e, _) -> use_e e
  | E_GetFields (e, _) -> use_e e
  | E_Record (ty, li) -> use_ty ty $ use_fields li
  | E_Concat es -> use_es es
  | E_Tuple es -> use_es es
  | E_Array { length; value } -> use_e length $ use_e value
  | E_Unknown t -> use_ty t
  | E_Pattern (e, p) -> use_e e $ use_pattern p

and use_es es acc = use_list use_e es acc
and use_fields fields acc = use_named_list use_e fields acc

and use_pattern p =
  match p.desc with
  | Pattern_Mask _ | Pattern_All -> Fun.id
  | Pattern_Tuple li | Pattern_Any li -> use_list use_pattern li
  | Pattern_Single e | Pattern_Geq e | Pattern_Leq e -> use_e e
  | Pattern_Not p -> use_pattern p
  | Pattern_Range (e1, e2) -> use_e e1 $ use_e e2

and use_slice = function
  | Slice_Single e -> use_e e
  | Slice_Star (e1, e2) | Slice_Length (e1, e2) | Slice_Range (e1, e2) ->
      use_e e1 $ use_e e2

and use_slices slices = use_list use_slice slices

(** [use_ty t s] adds the identifiers that appear in [t] to the set of identifiers [s] *)
and use_ty t =
  match t.desc with
  | T_Named s -> ISet.add s
  | T_Int (UnConstrained | Parameterized _)
  | T_Enum _ | T_Bool | T_Real | T_String ->
      Fun.id
  | T_Int (WellConstrained cs) -> use_constraints cs
  | T_Tuple li -> use_list use_ty li
  | T_Record fields | T_Exception fields -> use_named_list use_ty fields
  | T_Array (ArrayLength_Expr e, t') -> use_e e $ use_ty t'
  | T_Array (ArrayLength_Enum (s, _), t') -> ISet.add s $ use_ty t'
  | T_Bits (e, bit_fields) -> use_e e $ use_bitfields bit_fields

and use_bitfields bitfields = use_list use_bitfield bitfields

and use_bitfield = function
  | BitField_Simple (_name, slices) -> use_slices slices
  | BitField_Nested (_name, slices, bitfields) ->
      use_bitfields bitfields $ use_slices slices
  | BitField_Type (_name, slices, ty) -> use_ty ty $ use_slices slices

and use_constraint = function
  | Constraint_Exact e -> use_e e
  | Constraint_Range (e1, e2) -> use_e e1 $ use_e e2

and use_constraints cs = use_list use_constraint cs

let rec use_s s =
  match s.desc with
  | S_Pass | S_Return None -> Fun.id
  | S_Seq (s1, s2) -> use_s s1 $ use_s s2
  | S_Assert e | S_Return (Some e) -> use_e e
  | S_Assign (le, e, _) -> use_e e $ use_le le
  | S_Call (x, args, named_args) ->
      ISet.add x $ use_fields named_args $ use_es args
  | S_Cond (e, s1, s2) -> use_s s1 $ use_s s2 $ use_e e
  | S_Case (e, cases) -> use_e e $ use_cases cases
  | S_For { start_e; end_e; body; index_name = _; dir = _; limit } ->
      use_option use_e limit $ use_e start_e $ use_e end_e $ use_s body
  | S_While (e, limit, s) | S_Repeat (s, e, limit) ->
      use_option use_e limit $ use_s s $ use_e e
  | S_Decl (_, ldi, e) -> use_option use_e e $ use_ldi ldi
  | S_Throw (Some (e, _)) -> use_e e
  | S_Throw None -> Fun.id
  | S_Try (s, catchers, s') ->
      use_s s $ use_option use_s s' $ use_catchers catchers
  | S_Print { args; debug = _ } -> use_es args
  | S_Unreachable -> Fun.id

and use_ldi = function
  | LDI_Discard | LDI_Var _ -> Fun.id
  | LDI_Typed (ldi, t) -> use_ty t $ use_ldi ldi
  | LDI_Tuple ldis -> List.fold_right use_ldi ldis

and use_case { desc = { pattern; where; stmt }; _ } =
  use_option use_e where $ use_pattern pattern $ use_s stmt

and use_cases cases = use_list use_case cases

and use_le le =
  match le.desc with
  | LE_Var x -> ISet.add x
  | LE_Destructuring les | LE_Concat (les, _) -> List.fold_right use_le les
  | LE_Discard -> Fun.id
  | LE_SetArray (le, e) -> use_le le $ use_e e
  | LE_SetField (le, _) | LE_SetFields (le, _, _) -> use_le le
  | LE_Slice (le, slices) -> use_slices slices $ use_le le

and use_catcher (_name, ty, s) = use_s s $ use_ty ty
and use_catchers catchers = use_list use_catcher catchers

and use_decl d =
  match d.desc with
  | D_TypeDecl (_name, ty, fields) -> use_ty ty $ use_option use_subtypes fields
  | D_GlobalStorage { initial_value; ty; name = _; keyword = _ } ->
      use_option use_e initial_value $ use_option use_ty ty
  | D_Func
      { body; name = _; args; return_type; parameters; subprogram_type = _ }
    -> (
      use_named_list use_ty args
      $ use_option use_ty return_type
      $ use_named_list (use_option use_ty) parameters
      $ match body with SB_ASL s -> use_s s | SB_Primitive -> Fun.id)

and use_subtypes (x, subfields) = ISet.add x $ use_named_list use_ty subfields

let used_identifiers ast = use_list use_decl ast ISet.empty
let used_identifiers_stmt s = use_s s ISet.empty

let canonical_fields li =
  let compare (x, _) (y, _) = String.compare x y in
  List.sort compare li

let literal_equal v1 v2 =
  v1 == v2
  ||
  match (v1, v2) with
  | L_Bool b1, L_Bool b2 -> b1 = b2
  | L_Bool _, _ -> false
  | L_Int i1, L_Int i2 -> i1 = i2
  | L_Int _, _ -> false
  | L_Real f1, L_Real f2 -> f1 = f2
  | L_Real _, _ -> false
  | L_BitVector bv1, L_BitVector bv2 -> Bitvector.equal bv1 bv2
  | L_BitVector _, _ -> false
  | L_String s1, L_String s2 -> String.equal s1 s2
  | L_String _, _ -> false

let rec expr_equal eq e1 e2 =
  e1 == e2 || eq e1 e2
  ||
  match (e1.desc, e2.desc) with
  | E_Binop (o1, e11, e21), E_Binop (o2, e12, e22) ->
      o1 = o2 && expr_equal eq e11 e12 && expr_equal eq e21 e22
  | E_Binop _, _ | _, E_Binop _ -> false
  | E_Call (x1, args1, _), E_Call (x2, args2, _) ->
      (* We can ignore parameters as they are deduced from arguments. *)
      String.equal x1 x2 && list_equal (expr_equal eq) args1 args2
  | E_Call _, _ | _, E_Call _ -> false
  | E_Concat li1, E_Concat li2 -> list_equal (expr_equal eq) li1 li2
  | E_Concat _, _ | _, E_Concat _ -> false
  | E_Cond (e11, e21, e31), E_Cond (e12, e22, e32) ->
      expr_equal eq e11 e12 && expr_equal eq e21 e22 && expr_equal eq e31 e32
  | E_Cond _, _ | _, E_Cond _ -> false
  | E_Slice (e1, slices1), E_Slice (e2, slices2) ->
      expr_equal eq e1 e2 && slices_equal eq slices1 slices2
  | E_Slice _, _ | _, E_Slice _ -> false
  | E_GetArray (e11, e21), E_GetArray (e12, e22) ->
      expr_equal eq e11 e12 && expr_equal eq e21 e22
  | E_GetArray _, _ | _, E_GetArray _ -> false
  | E_GetField (e1', f1), E_GetField (e2', f2) ->
      String.equal f1 f2 && expr_equal eq e1' e2'
  | E_GetField _, _ | _, E_GetField _ -> false
  | E_GetFields (e1', f1s), E_GetFields (e2', f2s) ->
      list_equal String.equal f1s f2s && expr_equal eq e1' e2'
  | E_GetFields _, _ | _, E_GetFields _ -> false
  | E_GetItem (e1', i1), E_GetItem (e2', i2) ->
      Int.equal i1 i2 && expr_equal eq e1' e2'
  | E_GetItem _, _ | _, E_GetItem _ -> false
  | E_Pattern _, _ | E_Record _, _ -> assert false
  | E_Literal v1, E_Literal v2 -> literal_equal v1 v2
  | E_Literal _, _ | _, E_Literal _ -> false
  | E_Tuple li1, E_Tuple li2 -> list_equal (expr_equal eq) li1 li2
  | E_Tuple _, _ | _, E_Tuple _ -> false
  | E_Array { length = l1; value = v1 }, E_Array { length = l2; value = v2 } ->
      expr_equal eq l1 l2 && expr_equal eq v1 v2
  | E_Array _, _ | _, E_Array _ -> false
  | E_ATC (e1, t1), E_ATC (e2, t2) -> expr_equal eq e1 e2 && type_equal eq t1 t2
  | E_ATC _, _ | _, E_ATC _ -> false
  | E_Unop (o1, e1), E_Unop (o2, e2) -> o1 = o2 && expr_equal eq e1 e2
  | E_Unop _, _ | _, E_Unop _ -> false
  | E_Unknown _, _ | _, E_Unknown _ -> false
  | E_Var s1, E_Var s2 -> String.equal s1 s2
  | E_Var _, _ (* | _, E_Var _ *) -> false

and slices_equal eq slices1 slices2 =
  list_equal (slice_equal eq) slices1 slices2

and slice_equal eq slice1 slice2 =
  slice1 == slice2
  ||
  match (slice1, slice2) with
  | Slice_Single e1, Slice_Single e2 -> expr_equal eq e1 e2
  | Slice_Range (e11, e21), Slice_Range (e12, e22)
  | Slice_Length (e11, e21), Slice_Length (e12, e22) ->
      expr_equal eq e11 e12 && expr_equal eq e21 e22
  | _ -> false

and constraint_equal eq c1 c2 =
  c1 == c2
  ||
  match (c1, c2) with
  | Constraint_Exact e1, Constraint_Exact e2 -> expr_equal eq e1 e2
  | Constraint_Range (e11, e21), Constraint_Range (e12, e22) ->
      expr_equal eq e11 e12 && expr_equal eq e21 e22
  | _ -> false

and constraints_equal eq cs1 cs2 =
  cs1 == cs2 || list_equal (constraint_equal eq) cs1 cs2

and array_length_equal eq l1 l2 =
  match (l1, l2) with
  | ArrayLength_Expr e1, ArrayLength_Expr e2 -> expr_equal eq e1 e2
  | ArrayLength_Enum (s1, _), ArrayLength_Enum (s2, _) -> String.equal s1 s2
  | ArrayLength_Enum (_, _), ArrayLength_Expr _
  | ArrayLength_Expr _, ArrayLength_Enum (_, _) ->
      false

and type_equal eq t1 t2 =
  t1.desc == t2.desc
  ||
  match (t1.desc, t2.desc) with
  | T_Bool, T_Bool
  | T_Real, T_Real
  | T_String, T_String
  | T_Int UnConstrained, T_Int UnConstrained ->
      true
  | T_Int (Parameterized (i1, _)), T_Int (Parameterized (i2, _)) -> i1 == i2
  | T_Int (WellConstrained c1), T_Int (WellConstrained c2) ->
      constraints_equal eq c1 c2
  | T_Bits (w1, bf1), T_Bits (w2, bf2) ->
      bitwidth_equal eq w1 w2 && bitfields_equal eq bf1 bf2
  | T_Array (l1, t1), T_Array (l2, t2) ->
      array_length_equal eq l1 l2 && type_equal eq t1 t2
  | T_Named s1, T_Named s2 -> String.equal s1 s2
  | T_Enum li1, T_Enum li2 ->
      (* TODO: order of fields? *) list_equal String.equal li1 li2
  | T_Exception f1, T_Exception f2 | T_Record f1, T_Record f2 ->
      list_equal
        (pair_equal String.equal (type_equal eq))
        (canonical_fields f1) (canonical_fields f2)
  | T_Tuple ts1, T_Tuple ts2 -> list_equal (type_equal eq) ts1 ts2
  | _ -> false

and bitwidth_equal eq w1 w2 = expr_equal eq w1 w2

and bitfields_equal eq bf1 bf2 =
  bf1 == bf2 || (list_equal (bitfield_equal eq)) bf1 bf2

and bitfield_equal eq bf1 bf2 =
  bf1 == bf2
  ||
  match (bf1, bf2) with
  | BitField_Simple (name1, slices1), BitField_Simple (name2, slices2) ->
      String.equal name1 name2 && slices_equal eq slices1 slices2
  | ( BitField_Nested (name1, slices1, bf1'),
      BitField_Nested (name2, slices2, bf2') ) ->
      String.equal name1 name2
      && slices_equal eq slices1 slices2
      && bitfields_equal eq bf1' bf2'
  | BitField_Type (name1, slices1, t1), BitField_Type (name2, slices2, t2) ->
      String.equal name1 name2
      && slices_equal eq slices1 slices2
      && type_equal eq t1 t2
  | BitField_Simple _, BitField_Nested _
  | BitField_Simple _, BitField_Type _
  | BitField_Nested _, BitField_Simple _
  | BitField_Nested _, BitField_Type _
  | BitField_Type _, BitField_Nested _
  | BitField_Type _, BitField_Simple _ ->
      false

let var_ x = E_Var x |> add_dummy_pos
let binop op = map2_desc (fun e1 e2 -> E_Binop (op, e1, e2))
let unop op = map_desc (fun e -> E_Unop (op, e))
let literal v = E_Literal v |> add_dummy_pos
let expr_of_int i = literal (L_Int (Z.of_int i))
let expr_of_z z = literal (L_Int z)
let zero_expr = expr_of_z Z.zero
let one_expr = expr_of_z Z.one

let expr_of_rational q =
  if Z.equal (Q.den q) Z.one then expr_of_z (Q.num q)
  else binop DIV (expr_of_z (Q.num q)) (expr_of_z (Q.den q))

let mul_expr e1 e2 =
  if expr_equal (fun _ _ -> false) e1 one_expr then e2
  else if expr_equal (fun _ _ -> false) e2 one_expr then e1
  else binop MUL e1 e2

let pow_expr e = function
  | 0 -> one_expr
  | 1 -> e
  | 2 -> mul_expr e e
  | p -> binop POW e (expr_of_int p)

let div_expr e z = if Z.equal z Z.one then e else binop DIV e (expr_of_z z)

let add_expr e1 (s, e2) =
  if s = 0 then e1 else if s > 0 then binop PLUS e1 e2 else binop MINUS e1 e2

let conj_expr e1 e2 =
  let lit_true = literal (L_Bool true) in
  if expr_equal (fun _ _ -> false) e1 lit_true then e2
  else if expr_equal (fun _ _ -> false) e2 lit_true then e1
  else binop BAND e1 e2

let cond_expr e1 e2 e3 = E_Cond (e1, e2, e3) |> add_pos_from dummy_annotated

module Infix = struct
  let ( ~$ ) i = L_Int (Z.of_int i)
  let ( !$ ) i = expr_of_int i
end

let lid_of_lexpr =
  let rec tr le =
    match le.desc with
    | LE_Discard -> LDI_Discard
    | LE_Var x -> LDI_Var x
    | LE_Destructuring les -> LDI_Tuple (List.map tr les)
    | _ -> raise Exit
  in
  fun le -> try Some (tr le) with Exit -> None

let expr_of_lexpr : lexpr -> expr =
  let rec aux le =
    match le.desc with
    | LE_Var x -> E_Var x
    | LE_Slice (le, args) -> E_Slice (map_desc aux le, args)
    | LE_SetArray (le, e) -> E_GetArray (map_desc aux le, e)
    | LE_SetField (le, x) -> E_GetField (map_desc aux le, x)
    | LE_SetFields (le, x, _) -> E_GetFields (map_desc aux le, x)
    | LE_Discard -> E_Var "-"
    | LE_Destructuring les -> E_Tuple (List.map (map_desc aux) les)
    | LE_Concat (les, _) -> E_Concat (List.map (map_desc aux) les)
  in
  map_desc aux

let fresh_var =
  let i = ref 0 in
  fun s ->
    let () = incr i in
    s ^ "-" ^ string_of_int !i

(* Straight out of stdlib 4.12 *)
let string_starts_with ~prefix s =
  let open String in
  let len_s = length s and len_pre = length prefix in
  let rec aux i =
    if i = len_pre then true
    else if unsafe_get s i <> unsafe_get prefix i then false
    else aux (i + 1)
  in
  len_s >= len_pre && aux 0

let global_ignored_prefix = "__global_ignored"
let global_ignored () = fresh_var global_ignored_prefix
let is_global_ignored s = string_starts_with ~prefix:global_ignored_prefix s

let case_to_conds : stmt -> stmt =
  let case_to_cond e0 case tail =
    let { pattern; where; stmt } = case.desc in
    let e_pattern = E_Pattern (e0, pattern) |> add_pos_from pattern in
    let cond =
      match where with
      | None -> e_pattern
      | Some e_where -> binop BAND e_pattern e_where
    in
    S_Cond (cond, stmt, tail) |> add_pos_from case
  in
  let cases_to_cond ~loc e0 cases =
    List.fold_right (case_to_cond e0) cases (add_pos_from loc S_Unreachable)
  in
  fun s ->
    match s.desc with
    | S_Case (({ desc = E_Var _; _ } as e0), cases) ->
        cases_to_cond ~loc:s e0 cases
    | S_Case (e, cases) ->
        let x = fresh_var "__case__linearisation" in
        let decl_x = S_Decl (LDK_Let, LDI_Var x, Some e) |> add_pos_from e in
        S_Seq (decl_x, cases_to_cond ~loc:s (var_ x) cases) |> add_pos_from s
    | _ -> raise (Invalid_argument "case_to_conds")

let slice_is_single = function Slice_Single _ -> true | _ -> false

let slice_as_single = function
  | Slice_Single e -> e
  | _ -> raise @@ Invalid_argument "slice_as_single"

let default_t_bits = T_Bits (E_Var "-" |> add_dummy_pos, [])

let identifier_of_decl d =
  match d.desc with
  | D_Func { name; _ } | D_GlobalStorage { name; _ } | D_TypeDecl (name, _, _)
    ->
      name

let patch ~src ~patches =
  (* Size considerations:
     - [src] is BIG.
     - [patches] is not that little. *)
  let to_remove =
    patches |> List.to_seq |> Seq.map identifier_of_decl |> ISet.of_seq
  in
  let filter d = not (ISet.mem (identifier_of_decl d) to_remove) in
  src |> List.filter filter |> List.rev_append patches

let list_cross f li1 li2 =
  List.fold_left
    (fun xys x -> List.fold_left (fun xys' y -> f x y :: xys') xys li2)
    [] li1
  |> List.rev

exception FailedConstraintOp

let is_left_increasing = function
  | MUL | DIV | DIVRM | MOD | SHL | SHR | POW | PLUS | MINUS -> true
  | AND | BAND | BEQ | BOR | EOR | EQ_OP | GT | GEQ | IMPL | LT | LEQ | NEQ | OR
  | RDIV ->
      raise FailedConstraintOp

let is_right_increasing = function
  | MUL | SHL | SHR | POW | PLUS -> true
  | DIV | DIVRM | MOD | MINUS -> false
  | AND | BAND | BEQ | BOR | EOR | EQ_OP | GT | GEQ | IMPL | LT | LEQ | NEQ | OR
  | RDIV ->
      raise FailedConstraintOp

let is_right_decreasing = function
  | MINUS -> true
  | DIV | DIVRM | MUL | SHL | SHR | POW | PLUS | MOD -> false
  | AND | BAND | BEQ | BOR | EOR | EQ_OP | GT | GEQ | IMPL | LT | LEQ | NEQ | OR
  | RDIV ->
      raise FailedConstraintOp

(* Begin ConstraintBinop *)
let constraint_binop op =
  let righ_inc = is_right_increasing op
  and righ_dec = is_right_decreasing op
  and left_inc = is_left_increasing op in
  let constraint_binop_pair c1 c2 =
    match (c1, c2) with
    | Constraint_Exact e1, Constraint_Exact e2 ->
        Constraint_Exact (binop op e1 e2)
    | Constraint_Exact e1, Constraint_Range (e21, e22) when righ_inc ->
        Constraint_Range (binop op e1 e21, binop op e1 e22)
    | Constraint_Exact e1, Constraint_Range (e21, e22) when righ_dec ->
        Constraint_Range (binop op e1 e22, binop op e1 e21)
    | Constraint_Range (e11, e12), Constraint_Exact e2 when left_inc ->
        Constraint_Range (binop op e11 e2, binop op e12 e2)
    | Constraint_Range (e11, e12), Constraint_Range (e21, e22)
      when left_inc && righ_inc ->
        Constraint_Range (binop op e11 e21, binop op e12 e22)
    | Constraint_Range (e11, e12), Constraint_Range (e21, e22)
      when left_inc && righ_dec ->
        Constraint_Range (binop op e11 e22, binop op e12 e21)
    | _ -> raise_notrace FailedConstraintOp
  in
  fun cs1 cs2 ->
    try WellConstrained (list_cross constraint_binop_pair cs1 cs2)
    with FailedConstraintOp -> UnConstrained
(* End *)

(* Begin SubstExpr *)
let rec subst_expr substs e =
  (* WARNING: only subst runtime vars. *)
  let tr e = subst_expr substs e in
  add_pos_from_st e
  @@
  match e.desc with
  | E_Var s -> (
      match List.assoc_opt s substs with None -> e.desc | Some e' -> e'.desc)
  | E_Binop (op, e1, e2) -> E_Binop (op, tr e1, tr e2)
  | E_Concat es -> E_Concat (List.map tr es)
  | E_Cond (e1, e2, e3) -> E_Cond (tr e1, tr e2, tr e3)
  | E_Call (x, args, param_args) -> E_Call (x, List.map tr args, param_args)
  | E_GetArray (e1, e2) -> E_GetArray (tr e1, tr e2)
  | E_GetField (e, x) -> E_GetField (tr e, x)
  | E_GetFields (e, fields) -> E_GetFields (tr e, fields)
  | E_GetItem (e, i) -> E_GetItem (tr e, i)
  | E_Literal _ -> e.desc
  | E_Pattern (e, ps) -> E_Pattern (tr e, ps)
  | E_Record (t, fields) ->
      E_Record (t, List.map (fun (x, e) -> (x, tr e)) fields)
  | E_Slice (e, slices) -> E_Slice (tr e, slices)
  | E_Tuple es -> E_Tuple (List.map tr es)
  | E_Array { length; value } ->
      E_Array { length = tr length; value = tr value }
  | E_ATC (e, t) -> E_ATC (tr e, t)
  | E_Unknown _ -> e.desc
  | E_Unop (op, e) -> E_Unop (op, tr e)
(* End *)

let scope_equal s1 s2 =
  match (s1, s2) with
  | Scope_Global _, Scope_Global _ -> true
  | Scope_Global _, _ | _, Scope_Global _ -> false
  | Scope_Local (n1, i1), Scope_Local (n2, i2) -> i1 == i2 && String.equal n1 n2

let scope_compare s1 s2 =
  match (s1, s2) with
  | Scope_Global _, Scope_Global _ -> 0
  | Scope_Global _, _ -> -1
  | _, Scope_Global _ -> 1
  | Scope_Local (n1, i1), Scope_Local (n2, i2) ->
      let n = Int.compare i1 i2 in
      if n != 0 then n else String.compare n1 n2

let rec is_simple_expr e =
  match e.desc with
  | E_Var _ | E_Literal _ | E_Unknown _ -> true
  | E_Array { length = e1; value = e2 }
  | E_GetArray (e1, e2)
  | E_Binop (_, e1, e2) ->
      is_simple_expr e1 && is_simple_expr e2
  | E_ATC (e, _)
  | E_GetFields (e, _)
  | E_GetField (e, _)
  | E_GetItem (e, _)
  | E_Unop (_, e)
  | E_Pattern (e, _) (* because pattern must be side-effect free. *) ->
      is_simple_expr e
  | E_Tuple es | E_Concat es -> List.for_all is_simple_expr es
  | E_Cond (e1, e2, e3) ->
      is_simple_expr e1 && is_simple_expr e2 && is_simple_expr e3
  | E_Record (_, fields) ->
      List.for_all (fun (_name, e) -> is_simple_expr e) fields
  | E_Call _ | E_Slice _ -> false

let bitfield_get_name = function
  | BitField_Simple (name, _)
  | BitField_Nested (name, _, _)
  | BitField_Type (name, _, _) ->
      name

let bitfield_get_slices = function
  | BitField_Simple (_, slices)
  | BitField_Nested (_, slices, _)
  | BitField_Type (_, slices, _) ->
      slices

let has_name name bf = bitfield_get_name bf |> String.equal name

(* Begin FindBitfieldOpt *)
let find_bitfield_opt name bitfields = List.find_opt (has_name name) bitfields
(* End *)

let find_bitfields_slices_opt name bitfields =
  try List.find (has_name name) bitfields |> bitfield_get_slices |> Option.some
  with Not_found -> None

let rename_locals map_name ast =
  let map_names li = List.map (fun (name, x) -> (map_name name, x)) li in
  let rec map_e e =
    map_desc_st' e @@ function
    | E_Literal _ -> e.desc
    | E_Unknown t -> E_Unknown (map_t t)
    | E_Var x -> E_Var (map_name x)
    | E_ATC (e', t) -> E_ATC (map_e e', map_t t)
    | E_Binop (op, e1, e2) -> E_Binop (op, map_e e1, map_e e2)
    | E_Unop (op, e') -> E_Unop (op, map_e e')
    | E_Call (name, args, nargs) -> E_Call (name, map_es args, map_names nargs)
    | E_Slice (e', slices) -> E_Slice (map_e e', map_slices slices)
    | E_Cond (e1, e2, e3) -> E_Cond (map_e e1, map_e e2, map_e e3)
    | E_GetArray (e1, e2) -> E_GetArray (map_e e1, map_e e2)
    | E_GetField (e', f) -> E_GetField (map_e e', f)
    | E_GetFields (e', li) -> E_GetFields (map_e e', li)
    | E_GetItem (e', i) -> E_GetItem (map_e e', i)
    | E_Record (t, li) -> E_Record (t, List.map (fun (f, e) -> (f, map_e e)) li)
    | E_Concat li -> E_Concat (map_es li)
    | E_Tuple li -> E_Tuple (map_es li)
    | E_Array { length; value } ->
        E_Array { length = map_e length; value = map_e value }
    | E_Pattern (_, _) -> failwith "Not yet implemented: obfuscate patterns"
  and map_es li = List.map map_e li
  and map_slices slices = List.map map_slice slices
  and map_slice = function
    | Slice_Length (e1, e2) -> Slice_Length (map_e e1, map_e e2)
    | Slice_Single e -> Slice_Single (map_e e)
    | Slice_Range (e1, e2) -> Slice_Range (map_e e1, map_e e2)
    | Slice_Star (e1, e2) -> Slice_Star (map_e e1, map_e e2)
  and map_t t =
    map_desc_st' t @@ function
    | T_Real | T_String | T_Bool | T_Enum _ | T_Named _ | T_Int UnConstrained ->
        t.desc
    | T_Int (Parameterized _) ->
        failwith "Not yet implemented: obfuscate parametrized types"
    | T_Int (WellConstrained cs) -> T_Int (WellConstrained (map_cs cs))
    | T_Bits (e, bitfields) -> T_Bits (map_e e, bitfields)
    | T_Tuple li -> T_Tuple (List.map map_t li)
    | T_Array (_, _) -> failwith "Not yet implemented: obfuscate array types"
    | T_Record li -> T_Record (List.map (fun (f, t) -> (f, map_t t)) li)
    | T_Exception li -> T_Exception (List.map (fun (f, t) -> (f, map_t t)) li)
  and map_cs cs = List.map map_c cs
  and map_c = function
    | Constraint_Exact e -> Constraint_Exact (map_e e)
    | Constraint_Range (e1, e2) -> Constraint_Range (map_e e1, map_e e2)
  and map_s s =
    map_desc_st' s @@ function
    | S_Pass -> s.desc
    | S_Seq (s1, s2) -> S_Seq (map_s s1, map_s s2)
    | S_Decl (ldk, ldi, e) -> S_Decl (ldk, map_ldi ldi, Option.map map_e e)
    | S_Assign (le, e, v) -> S_Assign (map_le le, map_e e, v)
    | S_Call (name, args, nargs) -> S_Call (name, map_es args, map_names nargs)
    | S_Return e -> S_Return (Option.map map_e e)
    | S_Cond (e, s1, s2) -> S_Cond (map_e e, map_s s1, map_s s2)
    | S_Case (_, _) -> failwith "Not yet implemented: obfuscate cases"
    | S_Assert e -> S_Assert (map_e e)
    | S_For { index_name; start_e; dir; end_e; body; limit } ->
        let index_name = map_name index_name
        and start_e = map_e start_e
        and end_e = map_e end_e
        and limit = Option.map map_e limit
        and body = map_s body in
        S_For { index_name; start_e; dir; end_e; body; limit }
    | S_While (e, limit, s) -> S_While (map_e e, Option.map map_e limit, map_s s)
    | S_Repeat (s, e, limit) ->
        S_Repeat (map_s s, map_e e, Option.map map_e limit)
    | S_Throw (Some (e, t)) -> S_Throw (Some (map_e e, Option.map map_t t))
    | S_Throw None -> s.desc
    | S_Try (_, _, _) -> failwith "Not yet implemented: obfuscate try"
    | S_Print { args; debug } -> S_Print { args = List.map map_e args; debug }
    | S_Unreachable -> S_Unreachable
  and map_le le =
    map_desc_st' le @@ function
    | LE_Discard -> le.desc
    | LE_Concat (les, t) -> LE_Concat (List.map map_le les, t)
    | LE_Var x -> LE_Var (map_name x)
    | LE_Slice (le, slices) -> LE_Slice (map_le le, map_slices slices)
    | LE_SetArray (le, i) -> LE_SetArray (map_le le, map_e i)
    | LE_SetField (le, f) -> LE_SetField (map_le le, f)
    | LE_SetFields (le, f, annot) -> LE_SetFields (map_le le, f, annot)
    | LE_Destructuring les -> LE_Destructuring (List.map map_le les)
  and map_ldi = function
    | LDI_Discard as ldi -> ldi
    | LDI_Var x -> LDI_Var (map_name x)
    | LDI_Typed (ldi, t) -> LDI_Typed (map_ldi ldi, map_t t)
    | LDI_Tuple ldis -> LDI_Tuple (List.map map_ldi ldis)
  and map_body = function
    | SB_Primitive as b -> b
    | SB_ASL s -> SB_ASL (map_s s)
  and map_func f =
    let map_args li = List.map (fun (name, t) -> (map_name name, map_t t)) li in
    let map_nargs li =
      List.map (fun (name, t) -> (map_name name, Option.map map_t t)) li
    in
    {
      f with
      parameters = map_nargs f.parameters;
      args = map_args f.args;
      body = map_body f.body;
      return_type = Option.map map_t f.return_type;
    }
  and map_decl d =
    map_desc_st' d @@ function D_Func f -> D_Func (map_func f) | d -> d
  in
  List.map map_decl ast
