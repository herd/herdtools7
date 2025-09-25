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

let list_iterated_op ~empty op =
  let rec pairwise acc = function
    | [] -> acc
    | [ x ] -> x :: acc
    | x :: y :: t -> pairwise (op x y :: acc) t
  and iter = function
    | [] -> empty
    | [ x ] -> x
    | li -> pairwise [] li |> iter
  in
  iter

module ISet = struct
  include Set.Make (String)

  let of_option = function None -> empty | Some s -> singleton s

  let pp_print f t =
    let open Format in
    let pp_comma f () = fprintf f ",@ " in
    fprintf f "@[{@,%a}@]"
      (pp_print_list ~pp_sep:pp_comma pp_print_string)
      (elements t)

  let unions = list_iterated_op ~empty union
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
let default_version = V1
let desc v = v.desc

let annotated desc pos_start pos_end version =
  { desc; pos_start; pos_end; version }

let add_dummy_annotation ?(version = default_version) desc =
  annotated desc dummy_pos dummy_pos version

let dummy_annotated = add_dummy_annotation ()
let to_pos pos = { pos with desc = () }
let is_dummy_annotated x = x.pos_end == dummy_pos || x.pos_start == dummy_pos

let add_pos_from_st pos desc =
  if pos.desc == desc then pos else { pos with desc }

let add_pos_from pos desc = { pos with desc }

let add_pos_range_from pos_from pos_to desc =
  let () = assert (pos_from.version = pos_to.version) in
  {
    desc;
    pos_start = pos_from.pos_start;
    pos_end = pos_to.pos_end;
    version = pos_from.version;
  }

let map_desc f thing = f thing |> add_pos_from thing
let map_annotated thing f = f thing.desc |> add_pos_from thing

let add_maybe_loc ?loc thing =
  match loc with
  | None -> add_dummy_annotation thing
  | Some loc -> add_pos_from loc thing

let add_pos_from_pos_of ((fname, lnum, cnum, enum), desc) =
  let open Lexing in
  let common =
    { pos_fname = fname; pos_lnum = lnum; pos_bol = 0; pos_cnum = 0 }
  in
  {
    desc;
    pos_start = { common with pos_cnum = cnum };
    pos_end = { common with pos_cnum = enum };
    version = default_version (* used only in testing *);
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

let list_fold_lefti f accu l =
  List.fold_left (fun (i, accu) elt -> (i + 1, f i accu elt)) (0, accu) l |> snd

let list_coalesce_right f l =
  List.fold_right
    (fun e acc ->
      match acc with
      | [] -> [ e ]
      | acc_head :: acc_tail -> (
          match f e acc_head with
          | Some coalesced -> coalesced :: acc_tail
          | None -> e :: acc))
    l []

(* Straight out of stdlib v4.10 *)
let list_concat_map f l =
  let open List in
  let rec aux f acc = function
    | [] -> rev acc
    | x :: l ->
        let xs = f x in
        aux f (rev_append xs acc) l
  in
  aux f [] l

let list_take =
  let rec aux acc n li =
    match (li, n) with
    | [], _ | _, 0 -> List.rev acc
    | h :: t, n -> aux (h :: acc) (n - 1) t
  in
  fun n li ->
    if n < 0 then raise (Invalid_argument "list_take");
    aux [] n li

(** [list_take_while pred li] is the longest prefix of [li] where all items
    satisfy [pred]. *)
let list_take_while =
  let rec aux pred accu = function
    | [] -> List.rev accu
    | x :: xs -> if pred x then aux pred (x :: accu) xs else List.rev accu
  in
  fun pred li -> aux pred [] li

let uniq l =
  List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) [] l
  |> List.rev

let rec list_split3 = function
  | [] -> ([], [], [])
  | (x, y, z) :: l ->
      let xs, ys, zs = list_split3 l in
      (x :: xs, y :: ys, z :: zs)

let rec list_map_split f = function
  | [] -> ([], [])
  | [ a ] ->
      let x, y = f a in
      ([ x ], [ y ])
  | a1 :: a2 :: l ->
      let x1, y1 = f a1 in
      let x2, y2 = f a2 in
      let xs, ys = list_map_split f l in
      (x1 :: x2 :: xs, y1 :: y2 :: ys)

let get_first_duplicate li =
  let rec scan_for_dup = function
    | [] | [ _ ] -> None
    | x :: y :: rest ->
        if String.equal x y then Some x else scan_for_dup (y :: rest)
  in
  let sorted = List.sort String.compare li in
  scan_for_dup sorted

let list_is_empty = function [] -> true | _ -> false
let pair x y = (x, y)
let pair' y x = (x, y)
let pair_equal f g (x1, y1) (x2, y2) = f x1 x2 && g y1 y2

let map2_desc f thing1 thing2 =
  {
    desc = f thing1 thing2;
    pos_start = thing1.pos_start;
    pos_end = thing2.pos_end;
    version = thing1.version;
  }

let s_pass = add_dummy_annotation S_Pass
let s_then = map2_desc (fun s1 s2 -> S_Seq (s1, s2))
let boolean = T_Bool |> add_dummy_annotation
let string = T_String |> add_dummy_annotation
let real = T_Real |> add_dummy_annotation
let integer' = T_Int UnConstrained
let integer = integer' |> add_dummy_annotation

let well_constrained' ?(precision = Precision_Full) cs =
  T_Int (WellConstrained (cs, precision))

let well_constrained ?loc ?precision cs =
  well_constrained' ?precision cs |> add_maybe_loc ?loc

let integer_exact' e = well_constrained' [ Constraint_Exact e ]
let integer_exact ?loc e = integer_exact' e |> add_maybe_loc ?loc
let integer_range' e1 e2 = well_constrained' [ Constraint_Range (e1, e2) ]
let integer_range ?loc e1 e2 = integer_range' e1 e2 |> add_maybe_loc ?loc

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

let precision_join p1 p2 =
  match (p1, p2) with
  | Precision_Full, Precision_Full -> Precision_Full
  | Precision_Lost _, Precision_Full -> p1
  | Precision_Full, Precision_Lost _ -> p2
  | Precision_Lost l1, Precision_Lost l2 ->
      Precision_Lost (List.rev_append l1 l2)

let register_precision_loss p w =
  let ws = match p with Precision_Full -> [] | Precision_Lost l -> l in
  Precision_Lost (w :: ws)

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
  | L_Label l1, L_Label l2 -> String.equal l1 l2
  | L_Label _, _ -> false

let rec expr_equal eq e1 e2 =
  e1 == e2 || eq e1 e2
  ||
  match (e1.desc, e2.desc) with
  | E_Binop (o1, e11, e21), E_Binop (o2, e12, e22) ->
      o1 = o2 && expr_equal eq e11 e12 && expr_equal eq e21 e22
  | E_Binop _, _ | _, E_Binop _ -> false
  | ( E_Call { name = x1; params = params1; args = args1 },
      E_Call { name = x2; params = params2; args = args2 } ) ->
      if e1.version = V0 then
        (* We can ignore parameters as they are deduced from arguments. *)
        String.equal x1 x2 && list_equal (expr_equal eq) args1 args2
      else
        String.equal x1 x2
        && list_equal (expr_equal eq) params1 params2
        && list_equal (expr_equal eq) args1 args2
  | E_Call _, _ | _, E_Call _ -> false
  | E_Cond (e11, e21, e31), E_Cond (e12, e22, e32) ->
      expr_equal eq e11 e12 && expr_equal eq e21 e22 && expr_equal eq e31 e32
  | E_Cond _, _ | _, E_Cond _ -> false
  | E_Slice (e1, slices1), E_Slice (e2, slices2) ->
      expr_equal eq e1 e2 && slices_equal eq slices1 slices2
  | E_Slice _, _ | _, E_Slice _ -> false
  | E_GetArray (e11, e21), E_GetArray (e12, e22) ->
      expr_equal eq e11 e12 && expr_equal eq e21 e22
  | E_GetArray _, _ | _, E_GetArray _ -> false
  | E_GetEnumArray (e11, e21), E_GetEnumArray (e12, e22) ->
      expr_equal eq e11 e12 && expr_equal eq e21 e22
  | E_GetEnumArray _, _ | _, E_GetEnumArray _ -> false
  | E_GetField (e1', f1), E_GetField (e2', f2) ->
      String.equal f1 f2 && expr_equal eq e1' e2'
  | E_GetField _, _ | _, E_GetField _ -> false
  | E_GetFields (e1', f1s), E_GetFields (e2', f2s) ->
      list_equal String.equal f1s f2s && expr_equal eq e1' e2'
  | E_GetFields _, _ | _, E_GetFields _ -> false
  | E_GetCollectionFields (x1, f1s), E_GetCollectionFields (x2, f2s) ->
      String.equal x1 x2 && list_equal String.equal f1s f2s
  | E_GetCollectionFields _, _ | _, E_GetCollectionFields _ -> false
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
  | ( E_EnumArray { labels = l1; value = v1 },
      E_EnumArray { labels = l2; value = v2 } ) ->
      list_equal String.equal l1 l2 && expr_equal eq v1 v2
  | E_EnumArray _, _ | _, E_EnumArray _ -> false
  | E_ATC (e1, t1), E_ATC (e2, t2) -> expr_equal eq e1 e2 && type_equal eq t1 t2
  | E_ATC _, _ | _, E_ATC _ -> false
  | E_Unop (o1, e1), E_Unop (o2, e2) -> o1 = o2 && expr_equal eq e1 e2
  | E_Unop _, _ | _, E_Unop _ -> false
  | E_Arbitrary _, _ | _, E_Arbitrary _ -> false
  | E_Var s1, E_Var s2 -> String.equal s1 s2
  | E_Var _, _ (* | _, E_Var _ *) -> false

and slices_equal eq slices1 slices2 =
  list_equal (slice_equal eq) slices1 slices2

and slice_equal eq slice1 slice2 =
  slice1 == slice2
  ||
  match (slice1, slice2) with
  | Slice_Single e1, Slice_Single e2 -> expr_equal eq e1 e2
  | Slice_Length (e11, e21), Slice_Length (e12, e22)
  | Slice_Range (e11, e21), Slice_Range (e12, e22)
  | Slice_Star (e11, e21), Slice_Star (e12, e22) ->
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
  | ArrayLength_Enum (enum1, _), ArrayLength_Enum (enum2, _) ->
      String.equal enum1 enum2
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
  | T_Int (Parameterized x1), T_Int (Parameterized x2) -> String.equal x1 x2
  | T_Int (WellConstrained (c1, _)), T_Int (WellConstrained (c2, _)) ->
      constraints_equal eq c1 c2
  | T_Bits (w1, bf1), T_Bits (w2, bf2) ->
      bitwidth_equal eq w1 w2 && bitfields_equal eq bf1 bf2
  | T_Array (l1, t1), T_Array (l2, t2) ->
      array_length_equal eq l1 l2 && type_equal eq t1 t2
  | T_Named s1, T_Named s2 -> String.equal s1 s2
  | T_Enum li1, T_Enum li2 ->
      (* TODO: order of fields? *) list_equal String.equal li1 li2
  | T_Exception f1, T_Exception f2
  | T_Record f1, T_Record f2
  | T_Collection f1, T_Collection f2 ->
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

let qualifier_equal (q1 : func_qualifier option) q2 = Option.equal ( = ) q1 q2
let var_ x = E_Var x |> add_dummy_annotation
let binop op = map2_desc (fun e1 e2 -> E_Binop (op, e1, e2))
let unop op = map_desc (fun e -> E_Unop (op, e))
let literal v = E_Literal v |> add_dummy_annotation
let expr_of_int i = literal (L_Int (Z.of_int i))
let expr_of_z z = literal (L_Int z)
let e_true = literal (L_Bool true)
let e_false = literal (L_Bool false)
let expr_of_bool b = if b then e_true else e_false
let zero_expr = expr_of_z Z.zero
let one_expr = expr_of_z Z.one
let minus_one_expr = expr_of_z Z.minus_one

let expr_of_rational q =
  if Z.equal (Q.den q) Z.one then expr_of_z (Q.num q)
  else binop `DIV (expr_of_z (Q.num q)) (expr_of_z (Q.den q))

let mul_expr e1 e2 =
  if expr_equal (fun _ _ -> false) e1 one_expr then e2
  else if expr_equal (fun _ _ -> false) e2 one_expr then e1
  else binop `MUL e1 e2

let pow_expr e = function
  | 0 -> one_expr
  | 1 -> e
  | 2 -> mul_expr e e
  | p -> binop `POW e (expr_of_int p)

let div_expr e z = if Z.equal z Z.one then e else binop `DIV e (expr_of_z z)

let add_expr e1 (s, e2) =
  if s = 0 then e1 else if s > 0 then binop `ADD e1 e2 else binop `SUB e1 e2

let conj_expr e1 e2 =
  let lit_true = literal (L_Bool true) in
  if expr_equal (fun _ _ -> false) e1 lit_true then e2
  else if expr_equal (fun _ _ -> false) e2 lit_true then e1
  else binop `BAND e1 e2

let cond_expr e1 e2 e3 = E_Cond (e1, e2, e3) |> add_pos_from dummy_annotated

module Infix = struct
  let ( ~$ ) i = L_Int (Z.of_int i)
  let ( !$ ) i = expr_of_int i
end

let fresh_var =
  let i = ref 0 in
  fun s ->
    let () = incr i in
    s ^ "-" ^ string_of_int !i

let ldi_of_lexpr =
  let tr_tuple_var le = match le.desc with LE_Var x -> x | _ -> raise Exit in
  let tr le =
    match le.desc with
    | LE_Discard -> LDI_Var (fresh_var "__ldi_discard")
    | LE_Var x -> LDI_Var x
    | LE_Destructuring les -> LDI_Tuple (List.map tr_tuple_var les)
    | _ -> raise Exit
  in
  fun le -> try Some (tr le) with Exit -> None

let expr_of_lexpr : lexpr -> expr =
  let rec aux le =
    match le.desc with
    | LE_Var x -> E_Var x
    | LE_Slice (le, args) -> E_Slice (map_desc aux le, args)
    | LE_SetArray (le, e) -> E_GetArray (map_desc aux le, e)
    | LE_SetEnumArray (le, e) -> E_GetEnumArray (map_desc aux le, e)
    | LE_SetField (le, x) -> E_GetField (map_desc aux le, x)
    | LE_SetFields (le, x, _) -> E_GetFields (map_desc aux le, x)
    | LE_SetCollectionFields (x, fields, _) -> E_GetCollectionFields (x, fields)
    | LE_Discard -> E_Var "-"
    | LE_Destructuring les -> E_Tuple (List.map (map_desc aux) les)
  in
  map_desc aux

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
let local_ignored_prefix = "__ldi_discard"
let local_ignored () = fresh_var local_ignored_prefix
let is_local_ignored s = string_starts_with ~prefix:local_ignored_prefix s
let slice_is_single = function Slice_Single _ -> true | _ -> false

let is_noreturn (f : func) =
  match f.qualifier with Some Noreturn -> true | _ -> false

let slice_as_single = function
  | Slice_Single e -> e
  | _ -> raise @@ Invalid_argument "slice_as_single"

let default_t_bits = T_Bits (E_Var "-" |> add_dummy_annotation, [])

let default_array_ty =
  let len = ArrayLength_Expr (E_Var "-" |> add_dummy_annotation) in
  let ty = T_Named "-" |> add_dummy_annotation in
  T_Array (len, ty)

let identifier_of_decl d =
  match d.desc with
  | D_Func { name; _ } | D_GlobalStorage { name; _ } | D_TypeDecl (name, _, _)
    ->
      name
  | D_Pragma _ -> assert false

let patch ~src ~patches =
  (* Size considerations:
     - [src] is BIG.
     - [patches] is not that little. *)
  let to_remove =
    patches |> List.to_seq |> Seq.map identifier_of_decl |> ISet.of_seq
  in
  let filter d =
    match d.desc with
    | D_Pragma _ -> true
    | _ -> not (ISet.mem (identifier_of_decl d) to_remove)
  in
  src |> List.filter filter |> List.rev_append patches

let list_cross f li1 li2 =
  List.fold_left
    (fun xys x -> List.fold_left (fun xys' y -> f x y :: xys') xys li2)
    [] li1
  |> List.rev

let list_flat_cross f li1 li2 =
  List.fold_left
    (fun xys x ->
      List.fold_left (fun xys' y -> List.rev_append (f x y) xys') xys li2)
    [] li1
  |> List.rev

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
  | E_Cond (e1, e2, e3) -> E_Cond (tr e1, tr e2, tr e3)
  | E_Call { name; args; params; call_type } ->
      E_Call
        {
          name;
          args = List.map tr args;
          params = List.map tr params;
          call_type;
        }
  | E_GetArray (e1, e2) -> E_GetArray (tr e1, tr e2)
  | E_GetEnumArray (e1, e2) -> E_GetEnumArray (tr e1, tr e2)
  | E_GetField (e, x) -> E_GetField (tr e, x)
  | E_GetFields (e, fields) -> E_GetFields (tr e, fields)
  | E_GetCollectionFields _ -> failwith "No collection should be used here"
  | E_GetItem (e, i) -> E_GetItem (tr e, i)
  | E_Literal _ -> e.desc
  | E_Pattern (e, ps) -> E_Pattern (tr e, ps)
  | E_Record (t, fields) ->
      E_Record (t, List.map (fun (x, e) -> (x, tr e)) fields)
  | E_Slice (e, slices) -> E_Slice (tr e, slices)
  | E_Tuple es -> E_Tuple (List.map tr es)
  | E_Array { length; value } ->
      E_Array { length = tr length; value = tr value }
  | E_EnumArray { enum; labels; value } ->
      E_EnumArray { enum; labels; value = tr value }
  | E_ATC (e, t) -> E_ATC (tr e, t)
  | E_Arbitrary _ -> e.desc
  | E_Unop (op, e) -> E_Unop (op, tr e)
(* End *)

let rec is_simple_expr e =
  match e.desc with
  | E_Var _ | E_Literal _ | E_Arbitrary _ | E_GetCollectionFields _ -> true
  | E_Array { length = e1; value = e2 }
  | E_GetArray (e1, e2)
  | E_GetEnumArray (e1, e2)
  | E_Binop (_, e1, e2) ->
      is_simple_expr e1 && is_simple_expr e2
  | E_EnumArray { value = e }
  | E_ATC (e, _)
  | E_GetFields (e, _)
  | E_GetField (e, _)
  | E_GetItem (e, _)
  | E_Unop (_, e)
  | E_Pattern (e, _) (* because pattern must be side-effect free. *) ->
      is_simple_expr e
  | E_Tuple es -> List.for_all is_simple_expr es
  | E_Cond (e1, e2, e3) ->
      is_simple_expr e1 && is_simple_expr e2 && is_simple_expr e3
  | E_Slice (e, slices) ->
      is_simple_expr e && List.for_all is_simple_slice slices
  | E_Record (_, fields) ->
      List.for_all (fun (_name, e) -> is_simple_expr e) fields
  | E_Call _ -> false

and is_simple_slice = function
  | Slice_Length (e1, e2) | Slice_Range (e1, e2) | Slice_Star (e1, e2) ->
      is_simple_expr e1 && is_simple_expr e2
  | Slice_Single e -> is_simple_expr e

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

let bitfield_get_nested = function
  | BitField_Simple _ | BitField_Type _ -> []
  | BitField_Nested (_, _, nested_fields) -> nested_fields

let has_name name bf = bitfield_get_name bf |> String.equal name

(* Begin FindBitfieldOpt *)
let find_bitfield_opt name bitfields = List.find_opt (has_name name) bitfields
(* End *)

(* Begin FindBitFieldsSlices *)
let find_bitfields_slices_opt name bitfields =
  try List.find (has_name name) bitfields |> bitfield_get_slices |> Option.some
  with Not_found -> None
(* End *)

let rename_locals map_name ast =
  (* Begin RenameLocalsExpr *)
  let rec map_e e =
    map_annotated e @@ function
    | E_Literal _ -> e.desc
    | E_Arbitrary t -> E_Arbitrary (map_t t)
    | E_Var x -> E_Var (map_name x)
    | E_ATC (e1, t) -> E_ATC (map_e e1, map_t t)
    | E_Binop (op, e1, e2) -> E_Binop (op, map_e e1, map_e e2)
    | E_Unop (op, e1) -> E_Unop (op, map_e e1)
    | E_Call { name; args; params; call_type } ->
        E_Call { name; args = map_es args; params = map_es params; call_type }
    | E_Slice (e1, slices) -> E_Slice (map_e e1, map_slices slices)
    | E_Cond (e1, e2, e3) -> E_Cond (map_e e1, map_e e2, map_e e3)
    | E_GetArray (e1, e2) -> E_GetArray (map_e e1, map_e e2)
    | E_GetEnumArray (e1, e2) -> E_GetEnumArray (map_e e1, map_e e2)
    | E_GetField (e1, f) -> E_GetField (map_e e1, f)
    | E_GetFields (e1, li) -> E_GetFields (map_e e1, li)
    | E_GetCollectionFields (x, li) -> E_GetCollectionFields (map_name x, li)
    | E_GetItem (e1, i) -> E_GetItem (map_e e1, i)
    | E_Record (t, li) -> E_Record (t, List.map (fun (f, e) -> (f, map_e e)) li)
    | E_Tuple li -> E_Tuple (map_es li)
    | E_Array { length; value } ->
        E_Array { length = map_e length; value = map_e value }
    | E_EnumArray { enum; labels; value } ->
        E_EnumArray { enum; labels; value = map_e value }
    | E_Pattern (e1, p) -> E_Pattern (map_e e1, map_pattern p)
  (* End *)
  and map_es li = List.map map_e li
  and map_slices slices = List.map map_slice slices
  (* Begin RenameLocalsSlice *)
  and map_slice = function
    | Slice_Length (e1, e2) -> Slice_Length (map_e e1, map_e e2)
    | Slice_Single e -> Slice_Single (map_e e)
    | Slice_Range (e1, e2) -> Slice_Range (map_e e1, map_e e2)
    | Slice_Star (e1, e2) -> Slice_Star (map_e e1, map_e e2)
  (* End *)
  (* Begin RenameLocalsType *)
  and map_t t =
    map_annotated t @@ function
    | T_Real | T_String | T_Bool | T_Enum _ | T_Named _
    | T_Int (UnConstrained | PendingConstrained) ->
        t.desc
    | T_Int (Parameterized param_name) ->
        T_Int (Parameterized (map_name param_name))
    | T_Int (WellConstrained (cs, p)) -> T_Int (WellConstrained (map_cs cs, p))
    | T_Bits (e, bitfields) -> T_Bits (map_e e, bitfields)
    | T_Tuple li -> T_Tuple (List.map map_t li)
    | T_Array (index, elem_ty) -> T_Array (map_array_index index, map_t elem_ty)
    | T_Collection li -> T_Collection (List.map (fun (f, t) -> (f, map_t t)) li)
    | T_Record li -> T_Record (List.map (fun (f, t) -> (f, map_t t)) li)
    | T_Exception li -> T_Exception (List.map (fun (f, t) -> (f, map_t t)) li)
  (* End *)
  and map_cs cs = List.map map_c cs
  (* Begin RenameLocalsConstraint *)
  and map_c = function
    | Constraint_Exact e -> Constraint_Exact (map_e e)
    | Constraint_Range (e1, e2) -> Constraint_Range (map_e e1, map_e e2)
  (* End *)
  (* Begin RenameLocalsStmt *)
  and map_s s =
    map_annotated s @@ function
    | S_Pass -> s.desc
    | S_Seq (s1, s2) -> S_Seq (map_s s1, map_s s2)
    | S_Decl (ldk, ldi, ty, e) ->
        S_Decl (ldk, map_ldi ldi, Option.map map_t ty, Option.map map_e e)
    | S_Assign (le, e) -> S_Assign (map_le le, map_e e)
    | S_Call { name; args; params; call_type } ->
        S_Call { name; args = map_es args; params = map_es params; call_type }
    | S_Return e -> S_Return (Option.map map_e e)
    | S_Cond (e, s1, s2) -> S_Cond (map_e e, map_s s1, map_s s2)
    | S_Assert e -> S_Assert (map_e e)
    | S_For { index_name; start_e; dir; end_e; body; limit } ->
        let index_name = map_name index_name
        and start_e = map_e start_e
        and end_e = map_e end_e
        and limit = Option.map map_e limit
        and body = map_s body in
        S_For { index_name; start_e; dir; end_e; body; limit }
    | S_While (e, limit, body) ->
        S_While (map_e e, Option.map map_e limit, map_s body)
    | S_Repeat (s, e, limit) ->
        S_Repeat (map_s s, map_e e, Option.map map_e limit)
    | S_Throw (e, t) -> S_Throw (map_e e, Option.map map_t t)
    | S_Try (s1, catchers, otherwise_opt) ->
        S_Try
          ( map_s s1,
            List.map map_catcher catchers,
            Option.map map_s otherwise_opt )
    | S_Print { args; newline; debug } ->
        S_Print { args = List.map map_e args; newline; debug }
    | S_Unreachable -> S_Unreachable
    | S_Pragma (name, args) ->
        let args = map_es args in
        S_Pragma (name, args)
  (* End *)
  (* Begin RenameLocalsLexpr *)
  and map_le le =
    map_annotated le @@ function
    | LE_Discard -> le.desc
    | LE_Var x -> LE_Var (map_name x)
    | LE_Slice (le1, slices) -> LE_Slice (map_le le1, map_slices slices)
    | LE_SetArray (le1, i) -> LE_SetArray (map_le le1, map_e i)
    | LE_SetEnumArray (le, i) -> LE_SetEnumArray (map_le le, map_e i)
    | LE_SetField (le1, f) -> LE_SetField (map_le le1, f)
    | LE_SetFields (le1, fl, annot) -> LE_SetFields (map_le le1, fl, annot)
    | LE_SetCollectionFields _ as le -> le (* No collection is local *)
    | LE_Destructuring les -> LE_Destructuring (List.map map_le les)
  (* End *)
  (* Begin RenameLocalsLDI *)
  and map_ldi = function
    | LDI_Var x -> LDI_Var (map_name x)
    | LDI_Tuple names -> LDI_Tuple (List.map map_name names)
  (* End *)
  and map_body = function
    | SB_Primitive _ as b -> b
    | SB_ASL s -> SB_ASL (map_s s)
  (* Begin RenameLocalsFunc *)
  and map_func f =
    (* RenameLocalsArgs( *)
    let map_args li = List.map (fun (name, t) -> (map_name name, map_t t)) li in
    (* RenameLocalsArgs) *)
    (* RenameLocalsNamedArgs( *)
    let map_nargs li =
      List.map (fun (name, t) -> (map_name name, Option.map map_t t)) li
      (* RenameLocalsNamedArgs) *)
    in
    {
      f with
      parameters = map_nargs f.parameters;
      args = map_args f.args;
      body = map_body f.body;
      return_type = Option.map map_t f.return_type;
    }
  (* End *)
  (* Begin RenameLocalsPattern *)
  and map_pattern p =
    map_annotated p @@ function
    | Pattern_All -> Pattern_All
    | Pattern_Any pl -> Pattern_Any (List.map map_pattern pl)
    | Pattern_Geq p_e -> Pattern_Geq (map_e p_e)
    | Pattern_Leq p_e -> Pattern_Leq (map_e p_e)
    | Pattern_Mask _ -> p.desc
    | Pattern_Not sub_p -> Pattern_Not (map_pattern sub_p)
    | Pattern_Range (e1, e2) -> Pattern_Range (map_e e1, map_e e2)
    | Pattern_Single p_e -> Pattern_Single (map_e p_e)
    | Pattern_Tuple pl -> Pattern_Tuple (List.map map_pattern pl)
  (* End *)
  (* Begin RenameCatcher *)
  and map_catcher (opt_exn_name, exn_ty, when_stmt) =
    (Option.map map_name opt_exn_name, map_t exn_ty, map_s when_stmt)
  (* End *)
  (* Begin RenameLocalsArrayIndex *)
  and map_array_index = function
    | ArrayLength_Enum _ as i -> i
    | ArrayLength_Expr e_length -> ArrayLength_Expr (map_e e_length)
  (* End *)
  (* Begin RenameLocals *)
  and map_decl d =
    map_annotated d @@ function D_Func f -> D_Func (map_func f) | d -> d
    (* End *)
  in
  List.map map_decl ast

(* Taken from lib/innerRel.ml *)
let rec transitive_closure m0 =
  let m1 =
    IMap.fold
      (fun x ys m ->
        let zs =
          ISet.fold
            (fun y k -> try IMap.find y m :: k with Not_found -> k)
            ys [ ys ]
        in
        IMap.add x (ISet.unions zs) m)
      m0 m0
  in
  if IMap.equal ISet.equal m0 m1 then m0 else transitive_closure m1

let get_cycle m =
  (* Depth first search for a cycle.

     [m] is the graph represented as a Map from a node to its successors;
     [seen] is the set of already seen nodes;
     [path] is the set of nodes along the branch currently taken, with the
       parent of the current node just on top, the starting point of the
       algorithm being the lowest on the stack;
     [above] is the set of nodes in path;

     When a cycle is found, the exception [Cycle] is used for early return.
  *)
  let exception Cycle of identifier list in
  let rec dfs path above e seen =
    if ISet.mem e above then
      let cycle =
        e :: list_take_while (fun e' -> not (String.equal e e')) path
      in
      raise (Cycle cycle)
    else if ISet.mem e seen then seen
    else
      let path' = e :: path
      and above' = ISet.add e above
      and seen' = ISet.add e seen
      and succs = try IMap.find e m with Not_found -> ISet.empty in
      ISet.fold (dfs path' above') succs seen'
  in
  try
    let above0 = ISet.empty and seen0 = ISet.empty and path0 = [] in
    let _ = IMap.fold (fun x _ -> dfs path0 above0 x) m seen0 in
    None
  with Cycle e -> Some (List.rev e)
