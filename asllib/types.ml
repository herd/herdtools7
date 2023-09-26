(******************************************************************************)
(*                           the diy toolsuite                                *)
(*                                                                            *)
(* Jade Alglave, University College London, UK.                               *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
(*                                                                            *)
(* Copyright 2015-present Institut National de Recherche en Informatique et   *)
(* en Automatique and the authors. All rights reserved.                       *)
(*                                                                            *)
(* This software is governed by the CeCILL-B license under French law and     *)
(* abiding by the rules of distribution of free software. You can use,        *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B   *)
(* license as circulated by CEA, CNRS and INRIA at the following URL          *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.              *)
(******************************************************************************)
(* Authors:                                                                   *)
(* Hadrien Renaud, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
(* Jade Alglave, Arm Ltd and UCL, UK.                                         *)
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
open ASTUtils
open Infix
module SEnv = StaticEnv

type env = SEnv.env

let undefined_identifier pos x =
  Error.fatal_from pos (Error.UndefinedIdentifier x)

let thing_equal astutil_equal env =
  astutil_equal (StaticInterpreter.equal_in_env env)

let expr_equal = thing_equal expr_equal
let type_equal = thing_equal type_equal
let bitwidth_equal = thing_equal bitwidth_equal
let slices_equal = thing_equal slices_equal
let bitfield_equal = thing_equal bitfield_equal

(* --------------------------------------------------------------------------*)

let rec make_anonymous (env : env) (ty : ty) : ty =
  match ty.desc with
  | T_Named x -> (
      match IMap.find_opt x env.global.declared_types with
      | Some ty -> make_anonymous env ty
      | None -> undefined_identifier ty x)
  | _ -> ty

let get_structure (env : env) : ty -> ty =
  (* TODO: rethink to have physical equality when structural equality? *)
  (* TODO: memoize? *)
  let rec get ty =
    let () =
      if false then Format.eprintf "@[Getting structure of %a.@]@." PP.pp_ty ty
    in
    let with_pos = add_pos_from ty in
    match ty.desc with
    | T_Named x -> (
        match IMap.find_opt x env.global.declared_types with
        | None -> undefined_identifier ty x
        | Some ty -> get ty)
    | T_Int _ | T_Real | T_String | T_Bool | T_Bits _ | T_Enum _ -> ty
    | T_Tuple subtypes -> T_Tuple (List.map get subtypes) |> with_pos
    | T_Array (e, t) -> T_Array (e, get t) |> with_pos
    | T_Record fields -> T_Record (get_fields fields) |> with_pos
    | T_Exception fields -> T_Exception (get_fields fields) |> with_pos
  and get_fields fields =
    let one_field (name, t) = (name, get t) in
    let fields = List.map one_field fields in
    canonical_fields fields
  in
  get

(* --------------------------------------------------------------------------*)

(* The builtin singular types are:
   • integer
   • real
   • string
   • boolean
   • bits
   • bit
   • enumeration
*)
let is_builtin_singular ty =
  match ty.desc with
  | T_Real | T_String | T_Bool | T_Bits _ | T_Enum _ | T_Int _ -> true
  | _ -> false

(* The builtin aggregate types are:
   • tuple
   • array
   • record
   • exception
*)
let is_builtin_aggregate ty =
  match ty.desc with
  | T_Tuple _ | T_Array _ | T_Record _ | T_Exception _ -> true
  | _ -> false

let is_builtin ty = is_builtin_singular ty || is_builtin_aggregate ty
let is_named ty = match ty.desc with T_Named _ -> true | _ -> false

(* A named type is singular if it has the structure of a singular type,
   otherwise it is aggregate. *)
let is_singular env ty =
  is_builtin_singular ty
  || (is_named ty && get_structure env ty |> is_builtin_singular)

(* A named type is singular if it has the structure of a singular type,
   otherwise it is aggregate. *)
let is_aggregate env ty =
  is_builtin_aggregate ty
  || (is_named ty && get_structure env ty |> is_builtin_aggregate)

let is_anonymous ty = not (is_named ty)

let rec is_non_primitive ty =
  match ty.desc with
  | T_Real | T_String | T_Bool | T_Bits _ | T_Enum _ | T_Int _ -> false
  | T_Named _ -> true
  | T_Tuple li -> List.exists is_non_primitive li
  | T_Array (_, ty) -> is_non_primitive ty
  | T_Record fields | T_Exception fields ->
      List.exists (fun (_, ty) -> is_non_primitive ty) fields

let is_primitive ty = not (is_non_primitive ty)

(* --------------------------------------------------------------------------*)

module Domain = struct
  module IntSet = Diet.Z
  (* Pretty inefficient. Use diets instead?
     See https://web.engr.oregonstate.edu/~erwig/papers/Diet_JFP98.pdf
     or https://github.com/mirage/ocaml-diet
  *)

  (** Represents the domain of a integer expression. If there are no constraints,
    then the expression has the [Top] domain, whereas there are constraints but
    they cannot get resolved, the domain is [AlmostTop]. *)
  type int_set = Finite of IntSet.t | AlmostTop | Top

  type t =
    | D_Bool
    | D_String
    | D_Real
    | D_Symbols of ISet.t  (** The domain of an enum is a set of symbols *)
    | D_Int of int_set
    | D_Bits of int_set  (** The domain of a bitvector is given by its width. *)

  let add_interval_to_intset acc bot top =
    if bot > top then acc
    else
      let interval = IntSet.Interval.make bot top in
      IntSet.add interval acc

  let pp_int_set f =
    let open Format in
    function
    | AlmostTop -> pp_print_string f "𝕫"
    | Top -> pp_print_string f "ℤ"
    | Finite set -> fprintf f "@[{@,%a}@]" IntSet.pp set

  let pp f =
    let open Format in
    function
    | D_Bool -> pp_print_string f "𝔹"
    | D_String -> pp_print_string f "𝕊"
    | D_Real -> pp_print_string f "ℚ"
    | D_Symbols li ->
        fprintf f "@[{@,%a}@]"
          (PP.pp_print_seq ~pp_sep:pp_print_space pp_print_string)
          (ISet.to_seq li)
    | D_Int set -> pp_int_set f set
    | D_Bits set -> fprintf f "@[#bits(%a)@]" pp_int_set set

  exception StaticEvaluationTop

  let eval (env : env) (e : expr) =
    let v =
      let e =
        try StaticInterpreter.Normalize.normalize env e
        with StaticInterpreter.NotYetImplemented -> e
      in
      try StaticInterpreter.static_eval env e
      with Error.ASLException { desc = Error.UndefinedIdentifier _; _ } ->
        raise_notrace StaticEvaluationTop
    in
    match v with
    | L_Int i -> i
    | _ ->
        failwith
          "Type error? Cannot use an expression that is not an int in a \
           constraint."

  let add_constraint_to_intset env acc = function
    | Constraint_Exact e ->
        let v = eval env e in
        add_interval_to_intset acc v v
    | Constraint_Range (bot, top) ->
        let bot = eval env bot and top = eval env top in
        add_interval_to_intset acc bot top

  let int_set_of_int_constraints env constraints =
    match constraints with
    | [] -> AlmostTop
    | _ -> (
        try
          Finite
            (List.fold_left
               (add_constraint_to_intset env)
               IntSet.empty constraints)
        with StaticEvaluationTop -> AlmostTop)

  let int_set_raise_interval_op op is1 is2 =
    match (is1, is2) with
    | Top, _ | _, Top -> Top
    | AlmostTop, _ | _, AlmostTop -> AlmostTop
    | Finite is1, Finite is2 ->
        Finite
          (IntSet.fold
             (fun i1 -> IntSet.fold (fun i2 -> IntSet.add (op i1 i2)) is2)
             is1 IntSet.empty)

  let monotone_interval_op op i1 i2 =
    let open IntSet.Interval in
    make (op (x i1) (x i2)) (op (y i1) (y i2))

  let anti_monotone_interval_op op i1 i2 =
    let open IntSet.Interval in
    make (op (x i1) (y i2)) (op (y i1) (x i2))

  let rec int_set_of_bits_width env = function
    | BitWidth_SingleExpr e -> int_set_of_expr env e
    | BitWidth_ConstrainedFormType ty -> (
        match of_type env ty with
        | D_Bits is | D_Int is -> is
        | _ ->
            failwith
              "An bit width cannot be constrained from a type that is neither \
               a bitvector nor an integer.")
    | BitWidth_Constraints constraints ->
        int_set_of_int_constraints env constraints

  and int_set_of_value = function
    | L_Int i -> Finite (IntSet.singleton i)
    | _ -> assert false

  and int_set_of_expr env e =
    match e.desc with
    | E_Literal v -> int_set_of_value v
    | E_Var x -> (
        try StaticEnv.lookup_constants env x |> int_set_of_value
        with Not_found -> (
          try
            match (StaticEnv.type_of env x).desc with
            | T_Int None -> Top
            | T_Int (Some constraints) ->
                int_set_of_int_constraints env constraints
            | _ -> assert false
          with Not_found ->
            Error.fatal_unknown_pos (Error.UndefinedIdentifier x)))
    | E_Unop (NEG, e') ->
        int_set_of_expr env (E_Binop (MINUS, !$0, e') |> add_pos_from e)
    | E_Unop _ -> assert false
    | E_Binop (op, e1, e2) ->
        let is1 = int_set_of_expr env e1
        and is2 = int_set_of_expr env e2
        and op =
          match op with
          | PLUS -> monotone_interval_op Z.add
          | MINUS -> anti_monotone_interval_op Z.sub
          | MUL -> monotone_interval_op Z.mul
          | _ -> assert false
        in
        int_set_raise_interval_op op is1 is2
    | _ ->
        let () =
          Format.eprintf "@[<2>Cannot interpret as int set:@ @[%a@]@]@."
            PP.pp_expr e
        in
        assert false

  and of_type env ty =
    match ty.desc with
    | T_Bool -> D_Bool
    | T_String -> D_String
    | T_Real -> D_Real
    | T_Enum li -> D_Symbols (ISet.of_list li)
    | T_Int None -> D_Int Top
    | T_Int (Some constraints) ->
        D_Int (int_set_of_int_constraints env constraints)
    | T_Bits (width, _) -> D_Bits (int_set_of_bits_width env width)
    | T_Array _ | T_Exception _ | T_Record _ | T_Tuple _ ->
        failwith "Unimplemented: domain of a non singular type."
    | T_Named _ ->
        failwith "Cannot construct a domain of a non-structural type."

  let mem v d =
    match (v, d) with
    | L_Bool _, D_Bool | L_Real _, D_Real -> true
    | L_Bool _, _ | L_Real _, _ | _, D_Bool | _, D_Real -> false
    | L_BitVector _, D_Bits Top -> true
    | L_BitVector bv, D_Bits (Finite intset) ->
        IntSet.mem (Bitvector.length bv |> Z.of_int) intset
    | L_BitVector _, _ | _, D_Bits _ -> false
    | L_Int _, D_Int Top -> true
    | L_Int i, D_Int (Finite intset) -> IntSet.mem i intset
    | L_Int _, _ | _, D_Int _ -> false
    | L_String _, D_String -> true
    | L_String _, _ (* | _, D_String *) -> false

  let equal d1 d2 =
    match (d1, d2) with
    | D_Bool, D_Bool | D_String, D_String | D_Real, D_Real -> true
    | D_Symbols s1, D_Symbols s2 -> ISet.equal s1 s2
    | D_Bits Top, D_Bits Top | D_Int Top, D_Int Top -> true
    | D_Int (Finite is1), D_Int (Finite is2)
    | D_Bits (Finite is1), D_Bits (Finite is2) ->
        IntSet.equal is1 is2
    | _ -> false

  let compare _d1 _d2 = assert false

  let int_set_is_subset is1 is2 =
    match (is1, is2) with
    | _, Top -> true
    | Top, _ -> false
    | _, AlmostTop -> true
    | AlmostTop, _ -> false
    | Finite is1, Finite is2 -> IntSet.(is_empty (diff is1 is2))

  let is_subset d1 d2 =
    let () =
      if false then Format.eprintf "Is %a a subset of %a?@." pp d1 pp d2
    in
    match (d1, d2) with
    | D_Bool, D_Bool | D_String, D_String | D_Real, D_Real -> true
    | D_Symbols s1, D_Symbols s2 -> ISet.subset s1 s2
    | D_Bits is1, D_Bits is2 | D_Int is1, D_Int is2 -> int_set_is_subset is1 is2
    | _ -> false

  let get_width_singleton_opt = function
    | D_Bits (Finite int_set) ->
        if Z.equal (IntSet.cardinal int_set) Z.one then
          Some (IntSet.min_elt int_set |> IntSet.Interval.x)
        else None
    | D_Bits _ -> None
    | _ -> failwith "Cannot get width from non-bits domain."
end

(* --------------------------------------------------------------------------*)

let is_bits_width_fixed env ty =
  match ty.desc with
  | T_Bits _ -> (
      let open Domain in
      match of_type env ty with
      | D_Int (Finite int_set) -> IntSet.cardinal int_set = Z.one
      | D_Int Top -> false
      | _ -> failwith "Wrong domain for a bitwidth.")
  | _ -> failwith "Wrong type for some bits."

let _is_bits_width_constrained env ty = not (is_bits_width_fixed env ty)

(* --------------------------------------------------------------------------*)

let rec subtypes_names env s1 s2 =
  if String.equal s1 s2 then true
  else
    match IMap.find_opt s1 env.SEnv.global.subtypes with
    | None -> false
    | Some s1' -> subtypes_names env s1' s2

let subtypes env t1 t2 =
  match (t1.desc, t2.desc) with
  | T_Named s1, T_Named s2 -> subtypes_names env s1 s2
  | _ -> false

let rec bitfields_included env bfs1 bfs2 =
  let rec mem_bfs bfs2 bf1 =
    match find_bitfield_opt (bitfield_get_name bf1) bfs2 with
    | None -> false
    | Some (BitField_Simple _ as bf2) -> bitfield_equal env bf1 bf2
    | Some (BitField_Nested (name2, slices2, bfs2') as bf2) -> (
        match bf1 with
        | BitField_Simple _ -> bitfield_equal env bf1 bf2
        | BitField_Nested (name1, slices1, bfs1) ->
            String.equal name1 name2
            && slices_equal env slices1 slices2
            && incl_bfs bfs1 bfs2'
        | BitField_Type _ -> false)
    | Some (BitField_Type (name2, slices2, ty2) as bf2) -> (
        match bf1 with
        | BitField_Simple _ -> bitfield_equal env bf1 bf2
        | BitField_Nested _ -> false
        | BitField_Type (name1, slices1, ty1) ->
            String.equal name1 name2
            && slices_equal env slices1 slices2
            && structural_subtype_satisfies env ty1 ty2)
  and incl_bfs bfs1 bfs2 = List.for_all (mem_bfs bfs2) bfs1 in
  incl_bfs bfs1 bfs2

and structural_subtype_satisfies env t s =
  (* A type T subtype-satisfies type S if and only if all of the following
     conditions hold: *)
  match ((make_anonymous env s).desc, (make_anonymous env t).desc) with
  (* If S has the structure of an integer type then T must have the structure
     of an integer type. *)
  | T_Int _, T_Int _ -> true
  | T_Int _, _ -> false
  (* If S has the structure of a real type then T must have the structure of a
     real type. *)
  | T_Real, T_Real -> true
  | T_Real, _ -> false
  (* If S has the structure of a string type then T must have the structure of
     a string type. *)
  | T_String, T_String -> true
  | T_String, _ -> false
  (* If S has the structure of a boolean type then T must have the structure of
     a boolean type. *)
  | T_Bool, T_Bool -> true
  | T_Bool, _ -> false
  (* If S has the structure of an enumeration type then T must have the
     structure of an enumeration type with exactly the same enumeration
     literals. *)
  | T_Enum li_s, T_Enum li_t -> list_equal String.equal li_s li_t
  | T_Enum _, _ -> false
  (*
    • If S has the structure of a bitvector type with determined width then
      either T must have the structure of a bitvector type of the same
      determined width or T must have the structure of a bitvector type with
      undetermined width.
    • If S has the structure of a bitvector type with undetermined width then T
      must have the structure of a bitvector type.
    • If S has the structure of a bitvector type which has bitfields then T
      must have the structure of a bitvector type of the same width and for
      every bitfield in S there must be a bitfield in T of the same name, width
      and offset, whose type type-satisfies the bitfield in S.
  *)
  | T_Bits (w_s, bf_s), T_Bits (w_t, bf_t) -> (
      (* I interprete the first two condition as just a condition on domains. *)
      match (bf_s, bf_t) with
      | [], _ -> true
      | _, [] -> false
      | bfs_s, bfs_t ->
          bitwidth_equal env w_s w_t && bitfields_included env bfs_s bfs_t)
  | T_Bits _, _ -> false
  (* If S has the structure of an array type with elements of type E then T
     must have the structure of an array type with elements of type E, and T
     must have the same element indices as S. *)
  | T_Array (length_s, ty_s), T_Array (length_t, ty_t) ->
      expr_equal env length_s length_t && type_equal env ty_s ty_t
  | T_Array _, _ -> false
  (* If S has the structure of a tuple type then T must have the structure of
     a tuple type with same number of elements as S, and each element in T
     must type-satisfy the corresponding element in S.*)
  | T_Tuple li_s, T_Tuple li_t ->
      List.compare_lengths li_s li_t = 0
      && List.for_all2 (type_satisfies env) li_t li_s
  | T_Tuple _, _ -> false
  (* If S has the structure of an exception type then T must have the structure
     of an exception type with at least the same fields (each with the same
     type) as S.
     If S has the structure of a record type then T must have the structure of
     a record type with at least the same fields (each with the same type) as
     S.
     TODO: order of fields? *)
  | T_Exception fields_s, T_Exception fields_t
  | T_Record fields_s, T_Record fields_t ->
      List.for_all
        (fun (name_s, ty_s) ->
          List.exists
            (fun (name_t, ty_t) ->
              String.equal name_s name_t && type_equal env ty_s ty_t)
            fields_t)
        fields_s
  | T_Exception _, _ | T_Record _, _ -> false (* A structure cannot be a name *)
  | T_Named _, _ -> assert false

and domain_subtype_satisfies env t s =
  let s_struct = get_structure env s in
  match s_struct.desc with
  | T_Named _ ->
      (* Cannot happen *)
      assert false
      (* If S does not have the structure of an aggregate type or bitvector type
         then the domain of T must be a subset of the domain of S. *)
  | T_Tuple _ | T_Array _ | T_Record _ | T_Exception _ -> true
  | T_Real | T_String | T_Bool | T_Enum _ | T_Int _ ->
      let d_s = Domain.of_type env s_struct
      and d_t = get_structure env t |> Domain.of_type env in
      Domain.is_subset d_t d_s
  | T_Bits _ -> (
      (*
        • If either S or T have the structure of a bitvector type with
          undetermined width then the domain of T must be a subset of the domain
          of S.
         *)
      (* Implicitely, T must have the structure of a bitvector. *)
      let t_struct = get_structure env t in
      let t_domain = Domain.of_type env t_struct
      and s_domain = Domain.of_type env s_struct in
      let () =
        if false then
          Format.eprintf "Is %a included in %a?@." Domain.pp t_domain Domain.pp
            s_domain
      in
      match
        ( Domain.get_width_singleton_opt s_domain,
          Domain.get_width_singleton_opt t_domain )
      with
      | Some w_s, Some w_t -> Z.equal w_s w_t
      | _ -> Domain.is_subset t_domain s_domain)

and subtype_satisfies env t s =
  let () =
    if false then
      let b1 = structural_subtype_satisfies env t s in
      let b2 = domain_subtype_satisfies env t s in
      Format.eprintf "%a subtypes %a ? struct: %B -- domain: %B@." PP.pp_ty t
        PP.pp_ty s b1 b2
  in
  structural_subtype_satisfies env t s && domain_subtype_satisfies env t s

and type_satisfies env t s =
  (* Type T type-satisfies type S if and only if at least one of the following
     conditions holds: *)
  (* T is a subtype of S *)
  subtypes env t s
  (* T subtype-satisfies S and at least one of S or T is an anonymous type *)
  || ((is_anonymous t || is_anonymous s) && subtype_satisfies env t s)
  ||
  (* T is an anonymous bitvector with no bitfields and S has the structure of a
     bitvector (with or without bitfields) of the same width as T. *)
  (* Here I interprete "same width" as statically the same width, otherwise
     it's strange. *)
  match (t.desc, (get_structure env s).desc) with
  | T_Bits (width_t, []), T_Bits (width_s, _) ->
      bitwidth_equal env width_t width_s
  | _ -> false

(* --------------------------------------------------------------------------*)

let rec type_clashes env t s =
  (*
   Definition VPZZ:
   A type T type-clashes with S if any of the following hold:
      • they both have the structure of integers
      • they both have the structure of reals
      • they both have the structure of strings
      • they both have the structure of enumeration types with the same
        enumeration literals
      • they both have the structure of bit vectors
      • they both have the structure of arrays whose element types type-clash
      • they both have the structure of tuples of the same length whose
        corresponding element types type-clash
      • S is either a subtype or a supertype of T *)
  (* We will add a rule for boolean and boolean. *)
  (subtypes env s t || subtypes env t s)
  ||
  let s_struct = get_structure env s and t_struct = get_structure env t in
  match (s_struct.desc, t_struct.desc) with
  | T_Int _, T_Int _
  | T_Real, T_Real
  | T_String, T_String
  | T_Bits _, T_Bits _
  | T_Bool, T_Bool ->
      true
  | T_Enum li_s, T_Enum li_t -> list_equal String.equal li_s li_t
  | T_Array (_, ty_s), T_Array (_, ty_t) -> type_clashes env ty_s ty_t
  | T_Tuple li_s, T_Tuple li_t ->
      List.compare_lengths li_s li_t = 0
      && List.for_all2 (type_clashes env) li_s li_t
  | _ -> false

let subprogram_clashes env (f1 : 'a func) (f2 : 'b func) =
  (* Two subprograms clash if all of the following hold:
      • they have the same name
      • they are the same kind of subprogram
      • they have the same number of formal arguments
      • every formal argument in one type-clashes with the corresponding formal
        argument in the other

     TODO: they are the same kind of subprogram
  *)
  String.equal f1.name f2.name
  && List.compare_lengths f1.args f2.args = 0
  && List.for_all2
       (fun (_, t1) (_, t2) -> type_clashes env t1 t2)
       f1.args f2.args

(* --------------------------------------------------------------------------*)

let supertypes_set (env : env) =
  let rec aux acc x =
    let acc = ISet.add x acc in
    match IMap.find_opt x env.global.subtypes with
    | Some x' -> aux acc x'
    | None -> acc
  in
  aux ISet.empty

let find_named_lowest_common_supertype env x1 x2 =
  (* TODO: Have a better algorithm? This is in O(h * log h) because set
     insertions are in O (log h), where h is the max height of the subtype
     tree. Wikipedia says it is in O(h) generally, and it can be precomputed,
     in which case it becomes O(1). *)
  let set1 = supertypes_set env x1 in
  let rec aux x =
    if ISet.mem x set1 then Some x
    else
      match IMap.find_opt x env.global.subtypes with
      | None -> None
      | Some x' -> aux x'
  in
  aux x2

let rec lowest_common_ancestor env s t =
  (* The lowest common ancestor of types S and T is: *)
  (* • If S and T are the same type: S (or T). *)
  if type_equal env s t then Some s
  else
    match (s.desc, t.desc) with
    | T_Named name_s, T_Named name_t -> (
        (* If S and T are both named types: the (unique) common supertype of S
           and T that is a subtype of all other common supertypes of S and T. *)
        match find_named_lowest_common_supertype env name_s name_t with
        | None -> None
        | Some name -> Some (T_Named name |> add_dummy_pos))
    | _ -> (
        let struct_s = get_structure env s and struct_t = get_structure env t in
        match (struct_s.desc, struct_t.desc) with
        | T_Array (l_s, t_s), T_Array (l_t, t_t)
          when type_equal env t_s t_t && expr_equal env l_s l_t -> (
            (* If S and T both have the structure of array types with the same
               index type and the same element types:
                – If S is a named type and T is an anonymous type: S
                – If S is an anonymous type and T is a named type: T *)
            match (s.desc, t.desc) with
            | T_Named _, T_Named _ -> assert false
            | T_Named _, _ -> Some s
            | _, T_Named _ -> Some t
            | _ -> assert false)
        | T_Tuple li_s, T_Tuple li_t
          when List.compare_lengths li_s li_t = 0
               && List.for_all2 (type_satisfies env) li_s li_t
               && List.for_all2 (type_satisfies env) li_t li_s -> (
            (* If S and T both have the structure of tuple types with the same
               number of elements and the types of elements of S type-satisfy the
               types of the elements of T and vice-versa:
                – If S is a named type and T is an anonymous type: S
                – If S is an anonymous type and T is a named type: T
                – If S and T are both anonymous types: the tuple type with the
                  type of each element the lowest common ancestor of the types of
                  the corresponding elements of S and T. *)
            match (s.desc, t.desc) with
            | T_Named _, T_Named _ -> assert false
            | T_Named _, _ -> Some s
            | _, T_Named _ -> Some t
            | _ ->
                let maybe_ancestors =
                  List.map2 (lowest_common_ancestor env) li_s li_t
                in
                let ancestors = List.filter_map Fun.id maybe_ancestors in
                if List.compare_lengths ancestors li_s = 0 then
                  Some (add_dummy_pos (T_Tuple ancestors))
                else None)
        | T_Int (Some []), _ ->
            (* TODO: revisit? *)
            (* If either S or T have the structure of an under-constrained
               integer type: the under-constrained integer type. *)
            Some s
        | _, T_Int (Some []) ->
            (* TODO: revisit? *)
            (* If either S or T have the structure of an under-constrained
               integer type: the under-constrained integer type. *)
            Some t
        | T_Int (Some cs_s), T_Int (Some cs_t) -> (
            (* Implicit: cs_s and cs_t are non-empty, see patterns above. *)
            (* If S and T both have the structure of well-constrained integer
               types:
               – If S is a named type and T is an anonymous type: S
               – If T is an anonymous type and S is a named type: T
               – If S and T are both anonymous types: the well-constrained
                 integer type with domain the union of the domains of S and T.
            *)
            match (s.desc, t.desc) with
            | T_Named _, T_Named _ -> assert false
            | T_Named _, _ -> Some s
            | _, T_Named _ -> Some t
            | _ ->
                (* TODO: simplify domains ? If domains use a form of diets,
                   this could be more efficient. *)
                Some (add_dummy_pos (T_Int (Some (cs_s @ cs_t)))))
        | T_Int None, _ -> (
            (* Here S has the structure of an unconstrained integer type. *)
            (* TODO: revisit? *)
            (* TODO: typo corrected here, on point 2 S and T have
               been swapped. *)
            (* If either S or T have the structure of an unconstrained integer
               type:
               – If S is a named type with the structure of an unconstrained
                 integer type and T is an anonymous type: S
               – If T is an anonymous type and S is a named type with the
                 structure of an unconstrained integer type: T
               – If S and T are both anonymous types: the unconstrained integer
                 type. *)
            match (s.desc, t.desc) with
            | T_Named _, T_Named _ -> assert false
            | T_Named _, _ -> Some s
            | _, T_Named _ -> assert false
            | _, _ -> Some (add_dummy_pos (T_Int None)))
        | _, T_Int None -> (
            (* Here T has the structure of an unconstrained integer type. *)
            (* TODO: revisit? *)
            (* TODO: typo corrected here, on point 2 S and T have
               been swapped. *)
            (* If either S or T have the structure of an unconstrained integer
               type:
               – If S is a named type with the structure of an unconstrained
                 integer type and T is an anonymous type: S
               – If T is an anonymous type and S is a named type with the
                 structure of an unconstrained integer type: T
               – If S and T are both anonymous types: the unconstrained integer
                 type. *)
            match (s.desc, t.desc) with
            | T_Named _, T_Named _ -> assert false
            | T_Named _, _ -> assert false
            | _, T_Named _ -> Some t
            | _, _ -> Some (add_dummy_pos (T_Int None)))
        | _ -> None)

(* --------------------------------------------------------------------------*)

let rec base_value loc env t =
  let lit v = E_Literal v |> add_pos_from t in
  let normalize env e =
    let open StaticInterpreter in
    try Normalize.normalize env e
    with NotYetImplemented -> (
      try static_eval env e |> lit
      with Error.ASLException _ | NotYetImplemented -> e)
  in
  let t_struct = get_structure env t in
  match t_struct.desc with
  | T_Array _ ->
      Error.fatal_from loc
        (Error.NotYetImplemented "Base value of array types.")
  | T_Bool -> L_Bool true |> lit
  | T_Bits (BitWidth_Constraints (Constraint_Exact e :: _), _)
  | T_Bits (BitWidth_Constraints (Constraint_Range (e, _) :: _), _)
  | T_Bits (BitWidth_SingleExpr e, _) ->
      let e = normalize env e in
      E_Call ("Zeros", [ e ], []) |> add_pos_from t
  | T_Bits (BitWidth_ConstrainedFormType _, _) ->
      Error.fatal_from loc
        (Error.NotYetImplemented "Base value of type-constrained bitvectors.")
  | T_Bits (BitWidth_Constraints [], _) ->
      Error.fatal_from loc
        (Error.NotYetImplemented "Base value of under-constrained bitvectors.")
  | T_Enum li -> IMap.find (List.hd li) env.global.constants_values |> lit
  | T_Int None | T_Int (Some []) -> !$0
  | T_Int (Some (Constraint_Exact e :: _))
  | T_Int (Some (Constraint_Range (e, _) :: _)) ->
      normalize env e
  | T_Named _ -> assert false
  | T_Real -> L_Real Q.zero |> lit
  | T_Exception fields | T_Record fields ->
      let one_field (name, t) = (name, base_value loc env t) in
      E_Record (t, List.map one_field fields) |> add_pos_from t
  | T_String ->
      Error.fatal_from loc
        (Error.NotYetImplemented "Base value of string types.")
  | T_Tuple li ->
      let one t = base_value loc env t in
      E_Tuple (List.map one li) |> add_pos_from t
