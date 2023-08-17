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
    fprintf f "{@[@,%a@]}"
      (pp_print_list ~pp_sep:pp_comma pp_one) (bindings t)
end

let dummy_pos = Lexing.dummy_pos
let annotated desc pos_start pos_end = { desc; pos_start; pos_end }
let add_dummy_pos desc = annotated desc dummy_pos dummy_pos
let dummy_annotated = add_dummy_pos ()
let to_pos pos = { pos with desc = () }

let add_pos_from_st pos desc =
  if pos.desc == desc then pos else { pos with desc }

let add_pos_from pos desc = { pos with desc }
let with_pos_from pos { desc; _ } = add_pos_from pos desc
let map_desc f thing = f thing |> add_pos_from thing

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
let s_then = map2_desc (fun s1 s2 -> S_Then (s1, s2))
let boolean = T_Bool |> add_dummy_pos
let integer = T_Int None |> add_dummy_pos
let string = T_String |> add_dummy_pos
let real = T_Real |> add_dummy_pos
let underconstrained_integer = T_Int (Some []) |> add_dummy_pos

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
  | E_Typed (e, ty) -> use_e (use_ty acc ty) e
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
  | E_Record (_ty, li) -> use_fields acc li
  | E_Concat es -> use_es acc es
  | E_Tuple es -> use_es acc es
  | E_Unknown _ -> acc
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
  | T_Int None | T_Enum _ | T_Bool | T_Real | T_String -> acc
  | T_Int (Some cs) -> use_constraints acc cs
  | T_Tuple li -> List.fold_left use_ty acc li
  | T_Record fields | T_Exception fields -> fold_named_list use_ty acc fields
  | T_Array (e, t') -> use_ty (use_e acc e) t'
  | T_Bits (bit_constraint, bit_fields) ->
      let acc =
        match bit_constraint with
        | BitWidth_SingleExpr e -> use_e acc e
        | BitWidth_ConstrainedFormType t' -> use_ty acc t'
        | BitWidth_Constraints cs -> use_constraints acc cs
      in
      fold_named_list use_slices acc bit_fields

and use_constraints acc cs = List.fold_left use_constraint acc cs

and use_constraint acc = function
  | Constraint_Exact e -> use_e acc e
  | Constraint_Range (e1, e2) -> use_e (use_e acc e1) e2

let rec use_s acc s =
  match s.desc with
  | S_Pass | S_Return None -> acc
  | S_Then (s1, s2) -> use_s (use_s acc s1) s2
  | S_Assert e | S_Return (Some e) -> use_e acc e
  | S_Assign (le, e) -> use_le (use_e acc e) le
  | S_Call (x, args, named_args) ->
      let acc = ISet.add x acc in
      let acc = use_fields acc named_args in
      use_es acc args
  | S_Cond (e, s1, s2) -> use_s (use_s (use_e acc e) s2) s1
  | S_Case (e, cases) -> List.fold_left use_case (use_e acc e) cases
  | S_For (_, e1, _, e2, s) -> use_s (use_e (use_e acc e1) e2) s
  | S_While (e, s) | S_Repeat (s, e) -> use_s (use_e acc e) s
  | S_Decl (_, _, Some e) -> use_e acc e
  | S_Decl (_, _, None) -> acc
  | S_Throw (Some (e, _)) -> use_e acc e
  | S_Throw None -> acc
  | S_Try (s, catchers, None) -> use_catchers (use_s acc s) catchers
  | S_Try (s, catchers, Some s') ->
      use_catchers (use_s (use_s acc s') s) catchers

and use_case acc { desc = _p, stmt; _ } = use_s acc stmt
and use_le acc _le = acc
and use_catcher acc (_name, _ty, s) = use_s acc s
and use_catchers acc = List.fold_left use_catcher acc

and use_decl acc d =
  match d.desc with
  | D_Func { body = SB_ASL s; _ } -> use_s acc s
  | D_GlobalStorage { initial_value = Some e; _ } -> use_e acc e
  | _ -> acc

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
  | E_Typed (e1, t1), E_Typed (e2, t2) ->
      expr_equal eq e1 e2 && type_equal eq t1 t2
  | E_Typed _, _ | _, E_Typed _ -> false
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

and type_equal eq t1 t2 =
  t1.desc == t2.desc
  ||
  match (t1.desc, t2.desc) with
  | T_Bool, T_Bool
  | T_Real, T_Real
  | T_String, T_String
  | T_Int None, T_Int None ->
      true
  | T_Int (Some c1), T_Int (Some c2) -> constraints_equal eq c1 c2
  | T_Bits (w1, bf1), T_Bits (w2, bf2) ->
      bitwidth_equal eq w1 w2 && bitfields_equal eq bf1 bf2
  | T_Array (l1, t1), T_Array (l2, t2) ->
      expr_equal eq l1 l2 && type_equal eq t1 t2
  | T_Named s1, T_Named s2 -> String.equal s1 s2
  | T_Enum li1, T_Enum li2 ->
      (* TODO: order of fields? *) list_equal String.equal li1 li2
  | T_Exception f1, T_Exception f2 | T_Record f1, T_Record f2 ->
      list_equal
        (pair_equal String.equal (type_equal eq))
        (canonical_fields f1) (canonical_fields f2)
  | T_Tuple ts1, T_Tuple ts2 -> list_equal (type_equal eq) ts1 ts2
  | _ -> false

and bitwidth_equal eq w1 w2 =
  w1 == w2
  ||
  match (w1, w2) with
  | BitWidth_Constraints c1, BitWidth_Constraints c2 ->
      constraints_equal eq c1 c2
  | BitWidth_ConstrainedFormType t1, BitWidth_ConstrainedFormType t2 ->
      type_equal eq t1 t2
  | BitWidth_SingleExpr e1, BitWidth_SingleExpr e2 -> expr_equal eq e1 e2
  | _ -> false

and bitfields_equal eq bf1 bf2 =
  bf1 == bf2 || (list_equal (pair_equal String.equal (slices_equal eq))) bf1 bf2

let var_ x = E_Var x |> add_dummy_pos
let binop op = map2_desc (fun e1 e2 -> E_Binop (op, e1, e2))
let literal v = E_Literal v |> add_dummy_pos
let expr_of_int i = literal (L_Int (Z.of_int i))

module Infix = struct
  let ( ~$ ) i = L_Int (Z.of_int i)
  let ( !$ ) i = expr_of_int i
end

let expr_of_lexpr : lexpr -> expr =
  let rec aux le =
    match le.desc with
    | LE_Var x -> E_Var x
    | LE_Slice (le, args) -> E_Slice (map_desc aux le, args)
    | LE_SetArray (le, e) -> E_GetArray (map_desc aux le, e)
    | LE_SetField (le, x) -> E_GetField (map_desc aux le, x)
    | LE_SetFields (le, x) -> E_GetFields (map_desc aux le, x)
    | LE_Ignore -> E_Var "-"
    | LE_TupleUnpack les -> E_Tuple (List.map (map_desc aux) les)
  in
  map_desc aux

let fresh_var =
  let i = ref 0 in
  fun s ->
    let () = incr i in
    s ^ "-" ^ string_of_int !i

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
        let le = LDI_Var (x, Some integer) in
        annotated (S_Decl (LDK_Let, le, Some e)) pos e.pos_end
      in
      S_Then (assign, cases_to_cond x cases)
  | _ -> raise (Invalid_argument "case_to_conds")

let slice_as_single = function
  | Slice_Single e -> e
  | _ -> raise @@ Invalid_argument "slice_as_single"

let default_t_bits = T_Bits (BitWidth_Constraints [], [])

let patch ~src ~patches =
  (* Size considerations:
     - [src] is BIG.
     - [patches] is not that little. *)
  let identifier_of_decl d =
    match d.desc with
    | D_Func { name; _ } | D_GlobalStorage { name; _ } | D_TypeDecl (name, _, _)
      ->
        name
  in
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
    match (c1, c2) with
    | Constraint_Exact e1, Constraint_Exact e2 ->
        Constraint_Exact (binop op e1 e2)
    | _ -> raise_notrace FailedConstraintOp
  in
  fun cs1 cs2 -> try list_cross do_op cs1 cs2 with FailedConstraintOp -> []

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
  | E_Typed (e, t) -> E_Typed (tr e, t)
  | E_Unknown _ -> e.desc
  | E_Unop (op, e) -> E_Unop (op, tr e)

let dag_fold (def : 'p AST.decl -> identifier) (use : 'p AST.decl -> ISet.t)
    (folder : 'p AST.decl -> 'a -> 'a) (ast : 'p AST.t) : 'a -> 'a =
  let def_use_map =
    List.fold_left
      (fun def_use_map d ->
        let x = def d and use_set = use d in
        IMap.update x
          (function
            | None -> Some ([ d ], use_set)
            | Some (li, use_set') -> Some (d :: li, ISet.union use_set use_set'))
          def_use_map)
      IMap.empty ast
  in
  let rec loop s (seen, acc) =
    if (not (ISet.mem s seen)) && IMap.mem s def_use_map then
      let li, use_set = IMap.find s def_use_map in
      let seen, acc = ISet.fold loop use_set (seen, acc) in
      let acc = List.fold_left (Fun.flip folder) acc li in
      let seen = ISet.add s seen in
      (seen, acc)
    else (seen, acc)
  in
  fun acc ->
    let _seen, acc =
      List.fold_left
        (fun (seen, acc) d -> loop (def d) (seen, acc))
        (ISet.empty, acc) ast
    in
    acc

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

let no_primitive (ast : 'p t) : 'q t =
  let one d =
    let here = add_pos_from d in
    match d.desc with
    | D_GlobalStorage g -> D_GlobalStorage g |> here
    | D_TypeDecl (a, b, c) -> D_TypeDecl (a, b, c) |> here
    | D_Func { body = SB_Primitive _; _ } -> assert false
    | D_Func
        {
          body = SB_ASL s;
          args;
          name;
          return_type;
          subprogram_type;
          parameters;
        } ->
        D_Func
          {
            body = SB_ASL s;
            args;
            name;
            return_type;
            subprogram_type;
            parameters;
          }
        |> here
  in
  List.map one ast

let rec is_simple_expr e =
  match e.desc with
  | E_Var _ | E_Literal _ | E_Unknown _ -> true
  | E_GetArray (e1, e2) | E_Binop (_, e1, e2) ->
      is_simple_expr e1 && is_simple_expr e2
  | E_Typed (e, _)
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
