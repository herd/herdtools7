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
    (* Reversed interval *)
    List.init length (( - ) (start + length - 1))
  in
  fun positions -> List.map one_slice positions |> List.flatten

let fold_named_list folder acc list =
  List.fold_left (fun acc (_, v) -> folder acc v) acc list

let rec use_e acc e =
  match e.desc with
  | E_Literal _ -> acc
  | E_CTC (e, ty) -> use_e (use_ty acc ty) e
  | E_Var x -> ISet.add x acc
  | E_GetArray (e1, e2) | E_Binop (_, e1, e2) -> use_e (use_e acc e2) e1
  | E_Unop (_op, e) -> use_e acc e
  | E_Call (x, args, named_args) ->
      let acc = ISet.add x acc in
      let acc = use_fields acc named_args in
      use_es acc args
  | E_Slice (e, slices) -> use_slices (use_e acc e) slices
  | E_Cond (e1, e2, e3) -> use_e (use_e (use_e acc e1) e3) e2
  | E_GetField (e, _) -> use_e acc e
  | E_GetFields (e, _) -> use_e acc e
  | E_Record (ty, li) -> use_fields (use_ty acc ty) li
  | E_Concat es -> use_es acc es
  | E_Tuple es -> use_es acc es
  | E_Unknown t -> use_ty acc t
  | E_Pattern (e, _p) -> use_e acc e

and use_es acc es = List.fold_left use_e acc es
and use_fields acc fields = fold_named_list use_e acc fields
and use_slices acc slices = List.fold_left use_slice acc slices

and use_slice acc = function
  | Slice_Single e -> use_e acc e
  | Slice_Star (e1, e2) | Slice_Length (e1, e2) | Slice_Range (e1, e2) ->
      use_e (use_e acc e1) e2

and use_ty acc t =
  match t.desc with
  | T_Named s -> ISet.add s acc
  | T_Int (UnConstrained | UnderConstrained _)
  | T_Enum _ | T_Bool | T_Real | T_String ->
      acc
  | T_Int (WellConstrained cs) -> use_constraints acc cs
  | T_Tuple li -> List.fold_left use_ty acc li
  | T_Record fields | T_Exception fields -> fold_named_list use_ty acc fields
  | T_Array (ArrayLength_Expr e, t') -> use_ty (use_e acc e) t'
  | T_Array (ArrayLength_Enum (s, _), t') -> use_ty (ISet.add s acc) t'
  | T_Bits (e, bit_fields) -> use_bitfields (use_e acc e) bit_fields

and use_bitfields acc bitfields = List.fold_left use_bitfield acc bitfields

and use_bitfield acc = function
  | BitField_Simple (_name, slices) -> use_slices acc slices
  | BitField_Nested (_name, slices, bitfields) ->
      let acc = use_bitfields acc bitfields in
      use_slices acc slices
  | BitField_Type (_name, slices, ty) ->
      let acc = use_ty acc ty in
      use_slices acc slices

and use_constraints acc cs = List.fold_left use_constraint acc cs

and use_constraint acc = function
  | Constraint_Exact e -> use_e acc e
  | Constraint_Range (e1, e2) -> use_e (use_e acc e1) e2

let rec use_s acc s =
  match s.desc with
  | S_Pass | S_Return None -> acc
  | S_Seq (s1, s2) -> use_s (use_s acc s1) s2
  | S_Assert e | S_Return (Some e) -> use_e acc e
  | S_Assign (le, e, _) -> use_le (use_e acc e) le
  | S_Call (x, args, named_args) ->
      let acc = ISet.add x acc in
      let acc = use_fields acc named_args in
      use_es acc args
  | S_Cond (e, s1, s2) -> use_s (use_s (use_e acc e) s2) s1
  | S_Case (e, cases) -> List.fold_left use_case (use_e acc e) cases
  | S_For (_, e1, _, e2, s) -> use_s (use_e (use_e acc e1) e2) s
  | S_While (e, s) | S_Repeat (s, e) -> use_s (use_e acc e) s
  | S_Decl (_, ldi, Some e) -> use_e (use_ldi acc ldi) e
  | S_Decl (_, ldi, None) -> use_ldi acc ldi
  | S_Throw (Some (e, _)) -> use_e acc e
  | S_Throw None -> acc
  | S_Try (s, catchers, None) -> use_catchers (use_s acc s) catchers
  | S_Try (s, catchers, Some s') ->
      use_catchers (use_s (use_s acc s') s) catchers
  | S_Print { args; debug = _ } -> List.fold_left use_e acc args

and use_ldi acc = function
  | LDI_Discard -> acc
  | LDI_Var _ -> acc
  | LDI_Typed (ldi, t) -> use_ldi (use_ty acc t) ldi
  | LDI_Tuple ldis -> List.fold_left use_ldi acc ldis

and use_case acc { desc = _p, stmt; _ } = use_s acc stmt

and use_le acc le =
  match le.desc with
  | LE_Var x -> ISet.add x acc
  | LE_Destructuring les | LE_Concat (les, _) -> List.fold_left use_le acc les
  | LE_Discard -> acc
  | LE_SetArray (le, e) -> use_le (use_e acc e) le
  | LE_SetField (le, _) | LE_SetFields (le, _) -> use_le acc le
  | LE_Slice (le, slices) -> use_slices (use_le acc le) slices

and use_catcher acc (_name, ty, s) = use_s (use_ty acc ty) s
and use_catchers acc = List.fold_left use_catcher acc

and use_decl acc d =
  match d.desc with
  | D_Func { body = SB_ASL s; _ } -> use_s acc s
  | D_GlobalStorage { initial_value = Some e; _ } -> use_e acc e
  | _ -> acc

let use_constant_decl acc d =
  match d.desc with
  | D_GlobalStorage { initial_value = Some e; ty = Some ty; _ } ->
      let acc = use_e acc e in
      use_ty acc ty
  | D_GlobalStorage { initial_value = None; ty = Some ty; _ } -> use_ty acc ty
  | D_GlobalStorage { initial_value = Some e; ty = None; _ } -> use_e acc e
  | D_GlobalStorage { initial_value = None; ty = None; _ } -> assert false
  | D_TypeDecl (_, ty, Some (s, fields)) ->
      let acc = ISet.add s acc in
      let acc = use_ty acc ty in
      let acc = fold_named_list use_ty acc fields in
      acc
  | D_TypeDecl (_, ty, None) -> use_ty acc ty
  | D_Func { body; args; return_type; _ } ->
      let acc =
        match body with SB_ASL s -> use_s acc s | SB_Primitive -> acc
      in
      let acc = match return_type with None -> acc | Some t -> use_ty acc t in
      fold_named_list use_ty acc args

let used_identifiers ast = List.fold_left use_decl ISet.empty ast
let used_identifiers_stmt = use_s ISet.empty

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
  | E_Pattern _, _ | E_Record _, _ -> assert false
  | E_Literal v1, E_Literal v2 -> literal_equal v1 v2
  | E_Literal _, _ | _, E_Literal _ -> false
  | E_Tuple li1, E_Tuple li2 -> list_equal (expr_equal eq) li1 li2
  | E_Tuple _, _ | _, E_Tuple _ -> false
  | E_CTC (e1, t1), E_CTC (e2, t2) -> expr_equal eq e1 e2 && type_equal eq t1 t2
  | E_CTC _, _ | _, E_CTC _ -> false
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
  | T_Int (UnderConstrained (i1, _)), T_Int (UnderConstrained (i2, _)) ->
      i1 == i2
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
  | _ -> false

let var_ x = E_Var x |> add_dummy_pos
let binop op = map2_desc (fun e1 e2 -> E_Binop (op, e1, e2))
let literal v = E_Literal v |> add_dummy_pos
let expr_of_int i = literal (L_Int (Z.of_int i))

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
    | LE_SetFields (le, x) -> E_GetFields (map_desc aux le, x)
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
  let rec cases_to_cond x = function
    | [] -> s_pass
    | case :: t -> map_desc (one_case x t) case
  and one_case x t case =
    let p, s = case.desc in
    S_Cond (E_Pattern (var_ x, p) |> add_pos_from case, s, cases_to_cond x t)
  in
  map_desc @@ fun s ->
  match s.desc with
  | S_Case ({ desc = E_Var y; _ }, cases) -> (cases_to_cond y cases).desc
  | S_Case (e, cases) ->
      let x = fresh_var "case" in
      let assign =
        let pos = e.pos_start in
        let le = LDI_Typed (LDI_Var x, integer) in
        annotated (S_Decl (LDK_Let, le, Some e)) pos e.pos_end
      in
      S_Seq (assign, cases_to_cond x cases)
  | _ -> raise (Invalid_argument "case_to_conds")

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

let constraint_binop op =
  let do_op c1 c2 =
    match (c1, c2, op) with
    | Constraint_Exact e1, Constraint_Exact e2, _ ->
        Constraint_Exact (binop op e1 e2)
    | Constraint_Exact e1, Constraint_Range (e21, e22), PLUS ->
        Constraint_Range (binop op e1 e21, binop op e1 e22)
    | Constraint_Exact e1, Constraint_Range (e21, e22), MINUS ->
        Constraint_Range (binop op e1 e22, binop op e1 e21)
    | Constraint_Range (e11, e12), Constraint_Exact e2, (PLUS | MINUS) ->
        Constraint_Range (binop op e11 e2, binop op e12 e2)
    | Constraint_Range (e11, e12), Constraint_Range (e21, e22), PLUS ->
        Constraint_Range (binop op e11 e21, binop op e12 e22)
    | Constraint_Range (e11, e12), Constraint_Range (e21, e22), MINUS ->
        Constraint_Range (binop op e11 e22, binop op e12 e21)
    | _ -> raise_notrace FailedConstraintOp
  in
  fun cs1 cs2 ->
    try WellConstrained (list_cross do_op cs1 cs2)
    with FailedConstraintOp -> UnConstrained

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
  | E_Call (x, args, ta) -> E_Call (x, List.map tr args, ta)
  | E_GetArray (e1, e2) -> E_GetArray (tr e1, tr e2)
  | E_GetField (e, x) -> E_GetField (tr e, x)
  | E_GetFields (e, fields) -> E_GetFields (tr e, fields)
  | E_Literal _ -> e.desc
  | E_Pattern (e, ps) -> E_Pattern (tr e, ps)
  | E_Record (t, fields) ->
      E_Record (t, List.map (fun (x, e) -> (x, tr e)) fields)
  | E_Slice (e, slices) -> E_Slice (tr e, slices)
  | E_Tuple es -> E_Tuple (List.map tr es)
  | E_CTC (e, t) -> E_CTC (tr e, t)
  | E_Unknown _ -> e.desc
  | E_Unop (op, e) -> E_Unop (op, tr e)

let scope_equal s1 s2 =
  match (s1, s2) with
  | Scope_Global, Scope_Global -> true
  | Scope_Global, _ | _, Scope_Global -> false
  | Scope_Local (n1, i1), Scope_Local (n2, i2) -> i1 == i2 && String.equal n1 n2

let scope_compare s1 s2 =
  match (s1, s2) with
  | Scope_Global, Scope_Global -> 0
  | Scope_Global, _ -> -1
  | _, Scope_Global -> 1
  | Scope_Local (n1, i1), Scope_Local (n2, i2) ->
      let n = Int.compare i1 i2 in
      if n != 0 then n else String.compare n1 n2

let rec is_simple_expr e =
  match e.desc with
  | E_Var _ | E_Literal _ | E_Unknown _ -> true
  | E_GetArray (e1, e2) | E_Binop (_, e1, e2) ->
      is_simple_expr e1 && is_simple_expr e2
  | E_CTC (e, _)
  | E_GetFields (e, _)
  | E_GetField (e, _)
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
let find_bitfield_opt name bitfields = List.find_opt (has_name name) bitfields

let find_bitfields_slices_opt name bitfields =
  try List.find (has_name name) bitfields |> bitfield_get_slices |> Option.some
  with Not_found -> None

let rename_locals map_name ast =
  let map_names li = List.map (fun (name, x) -> (map_name name, x)) li in
  let rec map_e e =
    map_desc_st' e @@ function
    | E_Literal _ | E_Unknown _ -> e.desc
    | E_Var x -> E_Var (map_name x)
    | E_CTC (e', t) -> E_CTC (map_e e', map_t t)
    | E_Binop (op, e1, e2) -> E_Binop (op, map_e e1, map_e e2)
    | E_Unop (op, e') -> E_Unop (op, map_e e')
    | E_Call (name, args, nargs) -> E_Call (name, map_es args, map_names nargs)
    | E_Slice (e', slices) -> E_Slice (map_e e', map_slices slices)
    | E_Cond (e1, e2, e3) -> E_Cond (map_e e1, map_e e2, map_e e3)
    | E_GetArray (e1, e2) -> E_GetArray (map_e e1, map_e e2)
    | E_GetField (e', f) -> E_GetField (map_e e', f)
    | E_GetFields (e', li) -> E_GetFields (map_e e', li)
    | E_Record (t, li) -> E_Record (t, List.map (fun (f, e) -> (f, map_e e)) li)
    | E_Concat li -> E_Concat (map_es li)
    | E_Tuple li -> E_Tuple (map_es li)
    | E_Pattern (_, _) -> failwith "Not yet implemented: offuscate patterns"
  and map_es li = List.map map_e li
  and map_slices slices = List.map map_slice slices
  and map_slice = function
    | Slice_Length (e1, e2) -> Slice_Length (map_e e1, map_e e2)
    | Slice_Single e -> Slice_Single (map_e e)
    | Slice_Range (e1, e2) -> Slice_Range (map_e e1, map_e e2)
    | Slice_Star (e1, e2) -> Slice_Star (map_e e1, map_e e2)
  and map_t t =
    map_desc_st' t @@ function
    | T_Real | T_String | T_Bool | T_Enum _ | T_Named _
    | T_Int (UnConstrained | UnderConstrained _) ->
        t.desc
    | T_Int (WellConstrained cs) -> T_Int (WellConstrained (map_cs cs))
    | T_Bits (e, bitfields) -> T_Bits (map_e e, bitfields)
    | T_Tuple li -> T_Tuple (List.map map_t li)
    | T_Array (_, _) -> failwith "Not yet implemented: offuscate array types"
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
    | S_Case (_, _) -> failwith "Not yet implemented: offuscate cases"
    | S_Assert e -> S_Assert (map_e e)
    | S_For (x, e1, d, e2, s) ->
        S_For (map_name x, map_e e1, d, map_e e2, map_s s)
    | S_While (e, s) -> S_While (map_e e, map_s s)
    | S_Repeat (s, e) -> S_Repeat (map_s s, map_e e)
    | S_Throw (Some (e, t)) -> S_Throw (Some (map_e e, Option.map map_t t))
    | S_Throw None -> s.desc
    | S_Try (_, _, _) -> failwith "Not yet implemented: offscate try"
    | S_Print { args; debug } -> S_Print { args = List.map map_e args; debug }
  and map_le le =
    map_desc_st' le @@ function
    | LE_Discard -> le.desc
    | LE_Concat (les, t) -> LE_Concat (List.map map_le les, t)
    | LE_Var x -> LE_Var (map_name x)
    | LE_Slice (le, slices) -> LE_Slice (map_le le, map_slices slices)
    | LE_SetArray (le, i) -> LE_SetArray (map_le le, map_e i)
    | LE_SetField (le, f) -> LE_SetField (map_le le, f)
    | LE_SetFields (le, f) -> LE_SetFields (map_le le, f)
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
