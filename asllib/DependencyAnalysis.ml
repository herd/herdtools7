open AST
open ASTUtils

module Name = struct
  type t = Subprogram of string | Other of string

  let hash : t -> int = Hashtbl.hash [@@warning "-32"]

  let to_string = function
    | Subprogram s -> "Subprogram " ^ s
    | Other s -> "Var " ^ s

  let pp_print f n = Format.pp_print_string f (to_string n)

  let compare n1 n2 =
    match (n1, n2) with
    | Subprogram s1, Subprogram s2 | Other s1, Other s2 -> String.compare s1 s2
    | Subprogram _, Other _ -> -1
    | Other _, Subprogram _ -> 1

  let equal n1 n2 =
    match (n1, n2) with
    | Subprogram s1, Subprogram s2 | Other s1, Other s2 -> String.equal s1 s2
    | _ -> false
end

module NameSet = struct
  include Set.Make (Name)

  let add_list li t = of_list li |> union t

  let pp_print f t =
    let open Format in
    let pp_comma f () = fprintf f ",@ " in
    fprintf f "@[{@,%a}@]"
      (pp_print_list ~pp_sep:pp_comma Name.pp_print)
      (elements t)
end

let fold_named_list folder acc list =
  List.fold_left (fun acc (_, v) -> folder acc v) acc list

let ( $ ) f1 f2 acc = f1 acc |> f2
let use_option use_elt = function None -> Fun.id | Some elt -> use_elt elt
let use_list use_elt elts acc = List.fold_left (Fun.flip use_elt) acc elts
let add_other name = NameSet.add (Other name)

let use_named_list use_elt named_elts acc =
  fold_named_list (Fun.flip use_elt) acc named_elts

(* For V0, things that look like variables can actually be subprograms - so we
   must add both a dependency on a variable and a subprogram. *)
let add_maybe_pgm pos x =
  match pos.version with
  | V0 -> add_other x $ NameSet.add (Subprogram x)
  | V1 -> add_other x

let rec use_e e =
  match e.desc with
  | E_Literal _ -> Fun.id
  | E_ATC (e, ty) -> use_ty ty $ use_e e
  | E_Var x -> add_maybe_pgm e x
  | E_GetArray (e1, e2) | E_GetEnumArray (e1, e2) | E_Binop (_, e1, e2) ->
      use_e e1 $ use_e e2
  | E_Unop (_op, e) -> use_e e
  | E_Call { name; args; params } ->
      NameSet.add (Subprogram name) $ use_es params $ use_es args
  | E_Slice (e, slices) -> use_e e $ use_slices slices
  | E_Cond (e1, e2, e3) -> use_e e1 $ use_e e2 $ use_e e3
  | E_GetItem (e, _) -> use_e e
  | E_GetField (e, _) -> use_e e
  | E_GetFields (e, _) -> use_e e
  | E_GetCollectionFields (x, _) -> add_maybe_pgm e x
  | E_Record (ty, li) -> use_ty ty $ use_fields li
  | E_Tuple es -> use_es es
  | E_Array { length; value } -> use_e length $ use_e value
  | E_EnumArray { labels; value } ->
      use_list (fun id -> add_other id) labels $ use_e value
  | E_Arbitrary t -> use_ty t
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

(** [use_ty t s] adds the identifiers that appear in [t] to the set of
    identifiers [s] *)
and use_ty t =
  match t.desc with
  | T_Named s -> add_other s
  | T_Int (UnConstrained | Parameterized _ | PendingConstrained)
  | T_Enum _ | T_Bool | T_Real | T_String ->
      Fun.id
  | T_Int (WellConstrained (cs, _)) -> use_constraints cs
  | T_Tuple li -> use_list use_ty li
  | T_Record fields | T_Collection fields | T_Exception fields ->
      use_named_list use_ty fields
  | T_Array (ArrayLength_Expr e, t') -> use_e e $ use_ty t'
  | T_Array (ArrayLength_Enum (s, _), t') -> add_other s $ use_ty t'
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

let use_ldi = function
  | LDI_Var x -> add_other x
  | LDI_Tuple li -> List.map (fun x -> Name.Other x) li |> NameSet.add_list

let rec use_s s =
  match s.desc with
  | S_Pass | S_Return None -> Fun.id
  | S_Seq (s1, s2) -> use_s s1 $ use_s s2
  | S_Assert e | S_Return (Some e) -> use_e e
  | S_Assign (le, e) -> use_e e $ use_le le
  | S_Call { name; args; params } ->
      NameSet.add (Subprogram name) $ use_es params $ use_es args
  | S_Cond (e, s1, s2) -> use_s s1 $ use_s s2 $ use_e e
  | S_For { start_e; end_e; body; index_name; dir = _; limit } ->
      add_other index_name $ use_option use_e limit $ use_e start_e
      $ use_e end_e $ use_s body
  | S_While (e, limit, s) | S_Repeat (s, e, limit) ->
      use_option use_e limit $ use_s s $ use_e e
  | S_Decl (_, ldi, ty, e) ->
      use_ldi ldi $ use_option use_e e $ use_option use_ty ty
  | S_Throw (e, _) -> use_e e
  | S_Try (s, catchers, s') ->
      use_s s $ use_option use_s s' $ use_catchers catchers
  | S_Print { args; debug = _ } -> use_es args
  | S_Pragma (name, args) -> add_other name $ use_es args
  | S_Unreachable -> Fun.id

and use_le le =
  match le.desc with
  | LE_Var x -> add_maybe_pgm le x
  | LE_Destructuring les -> List.fold_right use_le les
  | LE_Discard -> Fun.id
  | LE_SetArray (le, e) | LE_SetEnumArray (le, e) -> use_le le $ use_e e
  | LE_SetField (le, _) | LE_SetFields (le, _, _) -> use_le le
  | LE_Slice (le, slices) -> use_slices slices $ use_le le
  | LE_SetCollectionFields (x, _, _) -> add_maybe_pgm le x

and use_catcher (_name, ty, s) = use_s s $ use_ty ty
and use_catchers catchers = use_list use_catcher catchers

and use_decl d =
  match d.desc with
  | D_TypeDecl (_name, ty, fields) -> use_ty ty $ use_option use_subtypes fields
  | D_GlobalStorage { initial_value; ty; name = _; keyword = _ } ->
      use_option use_e initial_value $ use_option use_ty ty
  | D_Func
      {
        body;
        name = _;
        args;
        return_type;
        parameters;
        subprogram_type = _;
        recurse_limit;
      } ->
      use_named_list use_ty args
      $ use_option use_ty return_type
      $ use_named_list (use_option use_ty) parameters
      $ (match body with SB_ASL s -> use_s s | SB_Primitive _ -> Fun.id)
      $ use_option use_e recurse_limit
  | D_Pragma (name, args) -> add_other name $ use_es args

and use_subtypes (x, subfields) = add_other x $ use_named_list use_ty subfields

let used_identifiers_decl d = use_decl d NameSet.empty
let used_identifiers_decls ast = use_list use_decl ast NameSet.empty
