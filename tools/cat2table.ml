(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

let prog =
  if Array.length Sys.argv > 0 then
    Filename.basename Sys.argv.(0)
  else "cat2table"

module Make
  (O: sig
    val verbose: bool
    val includes: string list
    val libdir: string
    val assumptions_file: string
  end) =
struct

  let libfind =
    let module ML =
      MyLib.Make
        (struct
          let includes = O.includes
          let env = Some "HERDLIB"
          let libdir = O.libdir
          let debug = O.verbose
        end) in
    ML.find

  module ParserConfig =
    struct
      let debug = false
      let libfind = libfind
    end

  module P = ParseModel.Make(ParserConfig)

  open AST

  module Cache = TxtLoc.Extract()

  let rec fold_left_short f stop acc = function
  | [] -> acc
  | x :: xs ->
    let acc' = f acc x in
    if stop acc' then acc'
    else fold_left_short f stop acc' xs

  let universe_effect = Konst (TxtLoc.none, (Universe SET))
  let empty_effect = Konst (TxtLoc.none, (Empty SET))

  let is_empty = function
  | Konst (_, (Empty _)) | Var (_, "emptyset") | ExplicitSet (_, []) -> true
  | _ -> false

  let is_universe = function
  | Konst (_, (Universe _)) -> true
  | _ -> false

  (* A character that we know can't be part of a variable name *)
  let sep = '$'

  let to_alias s n =
    s ^ String.make 1 sep ^ string_of_int n

  let from_alias s =
    match String.index_opt s sep with
    | Some idx -> String.sub s 0 idx
    | None -> s

  let contains_alias s =
    String.contains s sep

  let make_var id = Var (TxtLoc.none, id)

  let pp_expr_with_loc e = ASTUtils.exp2loc e |> Cache.extract
  let pp_expr =
    let rec pp_with_sep paran rec_paran sep es =
      let str = String.concat sep (List.map (do_pp_expr rec_paran) es) in
      if paran then Printf.sprintf "(%s)" str else str
    and do_pp_expr paran = function
    | Konst (_, (Empty _)) -> "empty"
    | Konst (_, (Universe _)) -> "universe"
    | Var (_, id) -> id
    | Op1 (_, Comp, e) -> Printf.sprintf "~%s" (do_pp_expr true e)
    | Op (_, Inter, es) -> pp_with_sep paran true " & " es
    | Op (_, Union, es) -> pp_with_sep paran true " | " es
    | Op (_, Diff, es) -> pp_with_sep paran true " \\ " es
    | Op (_, Seq, es) -> pp_with_sep paran false "; " es
    | Try (_, e1, e2) ->
      Printf.sprintf "try %s with %s" (do_pp_expr false e1) (do_pp_expr false e2)
    | If (_, _, e1, e2) ->
      Printf.sprintf "if _some_cond then %s else %s" (do_pp_expr false e1) (do_pp_expr false e2)
    | e -> pp_expr_with_loc e in
    do_pp_expr false

  let can_be_effect id defs primitives =
    let rec loop looked_up_ids = function
    | Fun _
    | Tag _
    | Op (_, (Seq | Cartesian | Tuple), _)
    | Op1 (_, (Plus | Star | Opt | Inv | ToId), _) -> false
    | Try (_, e1, e2) | If (_, _, e1, e2) ->
      loop looked_up_ids e1 && loop looked_up_ids e2
    | Op (_, (Union | Inter | Diff | Add), es) ->
      List.for_all (loop looked_up_ids) es
    | Var (_, id) ->
      if StringSet.mem id looked_up_ids then true
      else begin
        match StringMap.find_opt id defs with
        | None -> StringSet.mem (from_alias id) primitives
        | Some expr -> loop (StringSet.add id looked_up_ids) expr
      end
    | _ -> true
    in
    loop StringSet.empty (make_var id)

  (** Comparison function on expressions - assumes that sub-expression in op
    case are sorted based on itself *)
  let compare_expr e1 e2 =
    let is_commutative = function
    | Union | Inter | Add -> true
    | _ -> false in

    let do_compare_pairs (a1, b1) (a2, b2) cmp1 cmp2 =
      let c = cmp1 a1 a2 in
      if c != 0 then c
      else cmp2 b1 b2 in

    let rec do_compare_variant_cond vc1 vc2 = match vc1, vc2 with
    | Variant s1, Variant s2 -> String.compare s1 s2
    | OpNot vc1, OpNot vc2 -> do_compare_variant_cond vc1 vc2
    | OpAnd (vc11, vc12), OpAnd (vc21, vc22) ->
      do_compare_pairs (vc11, vc12) (vc21, vc22)
        do_compare_variant_cond do_compare_variant_cond
    | OpOr (vc11, vc12), OpOr (vc21, vc22) ->
      do_compare_pairs (vc11, vc12) (vc21, vc22)
        do_compare_variant_cond do_compare_variant_cond
    | _, _ -> compare vc1 vc2 in

    let rec sort_variant_cond = function
    | OpNot vc -> OpNot (sort_variant_cond vc)
    | OpAnd (vc1, vc2) as e ->
      let vc1 = sort_variant_cond vc1 in
      let vc2 = sort_variant_cond vc2 in
      if do_compare_variant_cond vc1 vc2 > 0 then OpAnd (vc2, vc1) else e
    | OpOr (vc1, vc2) as e ->
      let vc1 = sort_variant_cond vc1 in
      let vc2 = sort_variant_cond vc2 in
      if do_compare_variant_cond vc1 vc2 > 0 then OpOr (vc2, vc1) else e
    | Variant _ as e -> e in

    let rec do_compare_expr e1 e2 = match e1, e2 with
    | e1, e2 when is_empty e1 && is_empty e2 -> 0
    | Konst (_, (Universe _)), Konst (_, (Universe _)) -> 0
    | e, _ when is_empty e -> -1
    | _, e when is_empty e -> 1
    | Var (_, id1), Var (_, id2) -> String.compare id1 id2
    | Op1 (_, op1, e1), Op1 (_, op2, e2) ->
      do_compare_pairs (op1, e1) (op2, e2) compare do_compare_expr
    | Op (_, op1, es1), Op (_, op2, es2) ->
      do_compare_pairs (op1, es1) (op2, es2) compare (Misc.list_compare do_compare_expr)
    | App (_, e11, e12), App (_, e21, e22) ->
      do_compare_expr_pairs (e11, e12) (e21, e22)
    | Try (_, e11, e12), Try (_, e21, e22) ->
      do_compare_expr_pairs (e11, e12) (e21, e22)
    | If (_, c1, e11, e12), If (_, c2, e21, e22) ->
      do_compare_pairs (c1, (e11, e12)) (c2, (e21, e22))
        do_compare_cond
        do_compare_expr_pairs
    | _, _ ->
      (* We don't care about the rest, as they are not effects or relations *)
      compare e1 e2

    and do_compare_expr_pairs p1 p2 =
      do_compare_pairs p1 p2 do_compare_expr do_compare_expr

    and do_compare_cond c1 c2 = match c1, c2 with
    | Eq (e11, e12), Eq (e21, e22) ->
      do_compare_expr_pairs (e11, e12) (e21, e22)
    | Subset (e11, e12), Subset (e21, e22) -> do_compare_expr_pairs (e11, e12) (e21, e22)
    | In (e11, e12), In (e21, e22) -> do_compare_expr_pairs (e11, e12) (e21, e22)
    | VariantCond vc1, VariantCond vc2 -> do_compare_variant_cond vc1 vc2
    | _, _ -> compare c1 c2 in

    let rec sort_subexprs = function
    | (Konst (_) | Var (_)) as e -> e
    | Op1 (loc, op, e) -> Op1 (loc, op, sort_subexprs e)
    | Op (loc, op, es) ->
      let es = List.map sort_subexprs es in
      let es = if is_commutative op then
        List.sort do_compare_expr es
      else es in
      Op (loc, op, es)
    | App (loc, e1, e2) ->
      App (loc, sort_subexprs e1, sort_subexprs e2)
    | Try (loc, e1, e2) -> Try (loc, sort_subexprs e1, sort_subexprs e2)
    | If (loc, cond, e1, e2) ->
      If (loc, sort_cond cond, sort_subexprs e1, sort_subexprs e2)
    | e -> e

    and sort_cond = function
    | Eq (e1, e2) as e ->
      let e1 = sort_subexprs e1 in
      let e2 = sort_subexprs e2 in
      if do_compare_expr e1 e2 > 0 then Eq (e2, e1) else e
    | Subset (e1, e2) -> Subset (sort_subexprs e1, sort_subexprs e2)
    | In (e1, e2) -> In (sort_subexprs e1, sort_subexprs e2)
    | VariantCond vc -> VariantCond (sort_variant_cond vc) in

    do_compare_expr (sort_subexprs e1) (sort_subexprs e2)

  let expr_equal e1 e2 = compare_expr e1 e2 = 0

  module MakeGraph
    (VSet: MySet.S)
    (VMap: MyMap.S with type key = VSet.elt)
  = struct
    module VSet = VSet
    module VMap = VMap
    type vertex = VMap.key
    type t = vertex list VMap.t   (* adjacency: v -> neighbors *)

    let empty : t = VMap.empty

    let neighbours (v : vertex) (g : t) : vertex list =
      match VMap.find_opt v g with
      | Some l -> l
      | None -> []

    let add_edge (src : vertex) (dst : vertex) (g : t) : t =
      let ns = neighbours src g in
      VMap.add src (dst :: ns) g

    (* Generic DFS “engine”: you control what happens on enter/exit, and what
      is threaded through (acc). Also returns the set of visited vertices. *)
    let dfs_fold
        ~(per_vertex : vertex -> int -> 'acc -> 'acc)
        ~(per_neighbour  : vertex -> vertex -> 'acc -> 'acc)
        (g : t)
        (start : vertex)
        (seen : VSet.t)
        (acc0 : 'acc)
      : VSet.t * 'acc =
      let rec go v lvl seen acc =
        if VSet.mem v seen then (seen, acc)
        else
          let seen = VSet.add v seen in
          let acc = per_vertex v lvl acc in
          List.fold_left (fun (seen, acc) n -> 
            let seen, acc = go n (lvl + 1) seen acc in
            let acc = per_neighbour v n acc in
            seen, acc
          ) (seen, acc) (neighbours v g)
      in
      go start 1 seen acc0

    let dfs_fold_multiple_starts
        ~(per_vertex : vertex -> int -> 'acc -> 'acc)
        ~(per_neighbour  : vertex -> vertex -> 'acc -> 'acc)
        (g : t)
        (acc0 : 'acc)
      : 'acc =
      let _, res = VMap.fold (fun v _ (seen, acc) ->
        dfs_fold ~per_vertex ~per_neighbour g v seen acc
      ) g (VSet.empty, acc0) in
      res
  end

  module OrderedExpr = struct
    type t = AST.exp
    let compare = compare_expr
  end

  module ExprSet = MySet.Make(OrderedExpr)
  module ExprMap = MyMap.Make(OrderedExpr)
  module ExprGraph = MakeGraph(ExprSet)(ExprMap)

  module VarGraph = MakeGraph(StringSet)(StringMap)

  (* Takes the assumptions in the form of a cat AST and returns 3 data structures:
     - supersets: represents pairs of effects where one is a superset of
     the other
     - intersects: represents pairs of effects that are intersectable
     - rel_effects: represents the effects that can sit on either side of
     a primitive relation
     The data structures as returned by this function are not ready to be used
     by the intersection algorithm, and need to be post-processed using the
     propagate_supersets and propagate_intersects functions.
  *)
  let get_assumptions ast =
    let pp_opt_string name = Option.fold ~none:"(unknown)" ~some:Fun.id name in
    let add_to_map key value els =
      let existing_els = StringMap.safe_find [] key els in
      StringMap.add key (value :: existing_els) els in
    
    let invalid_assert_err e name =
        Warn.fatal "Assertion %s inside assumptions file %s contains invalid \
          expression: %s\n" (pp_opt_string name) O.assumptions_file (pp_expr e) in

    List.fold_left (fun (supersets, intersects, rel_effects) ins ->
      match ins with
      | Test ((_, _, Yes TestEmpty, Op (_, Diff, [subs_expr; Var (_, super)]), name), Assert) ->
        (* subs is either a primitive or a disjunction of primitives *)
        let supersets = match subs_expr with
        | Var (_, id) -> VarGraph.add_edge id super supersets
        | Op (_, Union, subs) ->
          List.fold_left (fun supersets sub ->
            match sub with
            | Var (_, id) -> VarGraph.add_edge id super supersets
            | _ -> invalid_assert_err subs_expr name
          ) supersets subs
        | e ->  invalid_assert_err e name in
        supersets, intersects, rel_effects
      | Test ((_, _, Yes TestEmpty, Op (_, Diff, [Var (_, rel); e]), name), Assert) ->
        let rel_effects = match e with
        | Op (_, Cartesian, [e1; e2]) -> add_to_map rel (e1, e2) rel_effects
        | Op (_, Union, es) ->
          List.fold_left (fun rel_effects e ->
            match e with
            | Op (_, Cartesian, [e1; e2]) -> add_to_map rel (e1, e2) rel_effects
            | _ -> invalid_assert_err e name
          ) rel_effects es
        | e -> invalid_assert_err e name in
        supersets, intersects, rel_effects
      | Test ((_, _, No TestEmpty, (Op (_, Inter, [e1; e2]) as e), name), Flagged) ->
        let intersects = match e1, e2 with
        | Var (_, id1), Var (_, id2) -> VarGraph.add_edge id1 id2 intersects
        | _, _ ->
          Warn.fatal "Flag %s inside assumptions file %s contains invalid \
            expression %s\n" (pp_opt_string name) O.assumptions_file (pp_expr e) in
        supersets, intersects, rel_effects
      | _ -> supersets, intersects, rel_effects
    ) (StringMap.empty, StringMap.empty, StringMap.empty) ast

  let propagate_supersets supersets =
    VarGraph.dfs_fold_multiple_starts
      ~per_vertex:(fun _ _ acc -> acc)
      ~per_neighbour:(fun id n acc ->
        let id_supers = VarGraph.neighbours id acc |> StringSet.of_list in
        let n_supers = VarGraph.neighbours n acc |> StringSet.of_list in
        let new_supers = StringSet.diff n_supers id_supers in
        StringSet.fold (VarGraph.add_edge id) new_supers acc
      ) supersets supersets |>
    StringMap.map StringSet.of_list

  let propagate_intersects intersects supersets =
    let get_children key map = StringMap.safe_find StringSet.empty key map in
    StringMap.fold (fun id _ intersects ->
      let id_supersets = get_children id supersets |> StringSet.add id in
      let vals = get_children id intersects in
      StringSet.fold (fun id2 intersects ->
        (* All supersets of id will also intersect with all supersets of id2 *)
        let id2_supersets = get_children id2 supersets |> StringSet.add id2 in
        let id1_diff_supersets = StringSet.diff id_supersets id2_supersets in
        let id2_diff_supersets = StringSet.diff id2_supersets id_supersets in

        let update_intersects id1_diff_supersets id2_diff_supersets intersects =
          StringSet.fold (fun e intersects ->
            let new_intersects = get_children e intersects
              |> StringSet.union id1_diff_supersets in
            StringMap.add e new_intersects intersects
          ) id2_diff_supersets intersects in

        intersects |>
          update_intersects id1_diff_supersets id2_diff_supersets |>
          update_intersects id2_diff_supersets id1_diff_supersets
      ) vals intersects
    ) intersects intersects

  let supersets, intersects, rel_effects =
    let (_, _, ast) = P.parse O.assumptions_file in
    let supersets, intersects, rel_effects = get_assumptions ast in
    let supersets = propagate_supersets supersets in
    (* Make sure to propagate the supersets before the intersects *)
    let intersects = VarGraph.VMap.map StringSet.of_list intersects in
    let intersects = propagate_intersects intersects supersets in
    supersets, intersects, rel_effects

  let valid_primitives =
    let lambda k d s =
      let s = StringSet.add k s in
      StringSet.union s d
    in
    StringSet.empty
      |> StringMap.fold lambda supersets
      |> StringMap.fold lambda intersects
      |> StringMap.fold (fun _ pairs s ->
        List.fold_left (fun s (e1, e2) ->
          let ids1 = ASTUtils.free_body [] e1 in
          let ids2 = ASTUtils.free_body [] e2 in
          s |> StringSet.union ids1 |> StringSet.union ids2
      ) s pairs
    ) rel_effects

  let check_map_of_sets map e1 e2 =
    let sups = StringMap.safe_find StringSet.empty e1 map in
    StringSet.mem e2 sups

  let id_is_subset_of = check_map_of_sets supersets
  let id_intersects_with = check_map_of_sets intersects

  let rec is_subset_of defs e1 e2 = match e1, e2 with
  | e, _ when is_empty e -> true
  | _, e when is_universe e -> true
  | e, _ when is_universe e -> false
  | _, e when is_empty e -> false
  | Var (_, id1) as e1, (Var (_, id2) as e2) ->
    if id1 = id2 then true
    else begin
      let l1 = StringMap.find_opt id1 defs in
      let l2 = StringMap.find_opt id2 defs in
      match l1, l2 with
      | None, None ->
        let id1 = from_alias id1 in
        let id2 = from_alias id2 in
        id1 = id2 || id_is_subset_of id1 id2
      | Some l1, None -> is_subset_of defs l1 e2
      | None, Some l2 -> is_subset_of defs e1 l2
      | Some l1, Some l2 -> is_subset_of defs l1 l2
    end
  | App (_, f, e1), e2 ->
    let e1 = apply_func defs f e1 in
    is_subset_of defs e1 e2
  | e1, App (_, f, e2) ->
    let e2 = apply_func defs f e2 in
    is_subset_of defs e1 e2
  | Op1 (_, Comp, e1), Op1 (_, Comp, e2) ->
    is_subset_of defs e2 e1
  | Op1 (_, Comp, e1), e2 ->
    is_universe (unite defs e1 e2)
  | e1, Op1 (_, Comp, e2) -> 
    is_empty (intersect defs e1 e2)
  | e1, Op (_, Inter, es) -> List.for_all (fun e ->
      is_subset_of defs e1 e
    ) es
  | Op (_, Union, es), e2 -> List.for_all (fun e ->
      is_subset_of defs e e2
    ) es
  | e, Op (_, Union, es) ->
    (* Should work if the union is irreducible - eg. we don't have
      things like R | W = M *)
    List.exists (fun e2 -> is_subset_of defs e e2) es
  | Op (_, Inter, es), e ->
    (* An approximation but good enough for our purposes. *)
    List.exists (fun e1 -> is_subset_of defs e1 e) es
  | e, Op (_, Diff, e1 :: es) ->
    is_subset_of defs e e1 && List.for_all (fun el ->
      is_empty (intersect defs e el)
    ) es
  | Op (_, Diff, e1 :: _), e ->
    (* Approximation - should work when the diff is irreducible *)
    is_subset_of defs e1 e
  | (If (_, _, e1, e2) | Try (_, e1, e2)), e ->
    let union = unite defs e1 e2 in
    is_subset_of defs union e
  | e, (If (_, _, e1, e2) | Try (_, e1, e2)) ->
    let union = unite defs e1 e2 in
    is_subset_of defs e union
  | _ -> Printf.eprintf "is_subset_of not supppoted for %s and %s\n"
          (pp_expr e1) (pp_expr e2);
    false

  and intersect defs e1 e2 =
    let recover_ids (id1, e1) (id2, e2) = function
    | e when is_empty e -> empty_effect
    | e ->
      let res1 = make_var id1 in
      let res2 = make_var id2 in
      if expr_equal e1 e then res1
      else if expr_equal e2 e then res2
      else Op (TxtLoc.none, Inter, [res1; res2]) in

    let is_optimised_inter e1 e2 inter =
      let unoptimised = ASTUtils.flatten (Op (TxtLoc.none, Inter, [e1; e2])) in
      not (expr_equal inter unoptimised) in

    match e1, e2 with
    | e, _ when is_empty e -> empty_effect
    | _, e when is_empty e -> empty_effect
    | App (_, f, e1), e2 | e2, App (_, f, e1) ->
      let e1 = apply_func defs f e1 in
      intersect defs e1 e2
    | Konst (_, (Universe _)), e | e, Konst (_, (Universe _)) -> e
    | Var (_, id1) as e1, (Var (_, id2) as e2) ->
      if id1 = id2 then e1
      else if is_subset_of defs e1 e2 then e1
      else if is_subset_of defs e2 e1 then e2
      else begin
        let l1 = StringMap.find_opt id1 defs in
        let l2 = StringMap.find_opt id2 defs in
        match l1, l2 with
        | None, None ->
          let id1 = from_alias id1 in
          let id2 = from_alias id2 in
          if id1 = id2 || id_intersects_with id1 id2 then
            Op (TxtLoc.none, Inter, [e1; e2])
          else empty_effect
        | Some l1, None ->
          recover_ids (id1, l1) (id2, e2) (intersect defs l1 e2)
        | None, Some l2 ->
          recover_ids (id1, e1) (id2, l2) (intersect defs e1 l2)
        | Some l1, Some l2 ->
          recover_ids (id1, l1) (id2, l2) (intersect defs l1 l2)
      end
    | e1, Op1 (_, Comp, e2) | Op1 (_, Comp, e2), e1 -> diff defs e1 e2
    | e, Op (_, Diff, e1 :: es) | Op (_, Diff, e1 :: es), e ->
      let union_of_diffs = List.fold_left (unite defs) empty_effect es in
      let inter = intersect defs e e1 in
      diff defs inter union_of_diffs
    | Op (_, Inter, es), e | e, Op (_, Inter, es) ->
      (* If e intersects with every element of es, then it should
      intersect with the conjuction of es. Again, not  true in the
      general case, as we can have A & C and B & C non-empty, but
      the overall A & B & C can be empty. But for our purposes it
      is good enough. Need to make sure that A and B are irreducible. *)
      let rec traverse_inter els e = function
      | [] -> List.rev (e :: els)
      | x :: xs ->
        let inter = intersect defs x e in
        if is_empty inter then [empty_effect]
        else if is_subset_of defs x e then es
        else if not (is_optimised_inter x e inter) then
          traverse_inter (x :: els) e xs
        else traverse_inter els inter xs in
      begin match traverse_inter [] e es with      
      | [e] -> e
      | es -> ASTUtils.flatten (Op (TxtLoc.none, Inter, es))
      end
    | Op (_, Union, es), e | e, Op (_, Union, es) ->
      let rec traverse_union acc = function
      | [] -> List.rev acc
      | x :: xs ->
        if is_subset_of defs e x then [e]
        else
          let inter = intersect defs x e in
          if is_empty inter then traverse_union acc xs
          else traverse_union (inter :: acc) xs in
      List.fold_left (unite defs) empty_effect (traverse_union [] es)
    | (If (_, _, e1, e2) | Try (_, e1, e2)), e
    | e, (If (_, _, e1, e2) | Try (_, e1, e2)) ->
      let union = unite defs e1 e2 in
      intersect defs union e
    | e1, e2 ->
      Printf.eprintf "Intersection for %s and %s is not supported\n"
        (pp_expr e1) (pp_expr e2);
      Konst (TxtLoc.none, (Empty SET))

  (* TODO: Write a version of this that also takes care of optimisations
  eg. R | W = M *)
  and unite defs e1 e2 = match e1, e2 with
  | App (_, f, e1), e2 | e2, App (_, f, e1) ->
    let e1 = apply_func defs f e1 in
    unite defs e1 e2
  | e1, e2 when is_subset_of defs e1 e2 -> e2
  | e1, e2 when is_subset_of defs e2 e1 -> e1
  | Op1 (_, Comp, e1), e2 when expr_equal e1 e2 -> universe_effect
  | e1, Op1 (_, Comp, e2) when expr_equal e1 e2 -> universe_effect
  | Op (_, Union, es1), Op (_, Union, es2) -> Op (TxtLoc.none, Union, es1 @ es2)
  | Op (_, Union, es), e | e, Op (_, Union, es) -> Op (TxtLoc.none, Union, e :: es)
  | _, _ -> Op (TxtLoc.none, Union, [e1; e2])

  and diff defs e1 e2 = match e1, e2 with
  | App (_, f, e1), e2 ->
    let e1 = apply_func defs f e1 in
    diff defs e1 e2
  | e1, App (_, f, e2) ->
    let e2 = apply_func defs f e2 in
    diff defs e1 e2
  | Konst (_, (Universe _)), e -> Op1 (TxtLoc.none, Comp, e)
  | e1, e2 when is_subset_of defs e1 e2 -> empty_effect
  | e1, e2 when is_empty (intersect defs e1 e2) -> e1
  | Op (_, Union, es), e ->
    let es = List.filter (fun el ->
      not (is_subset_of defs el e)
    ) es in
    let union = List.fold_left (unite defs) empty_effect es in
    if List.exists (fun el ->
      not (is_empty (intersect defs el e))
    ) es then Op (TxtLoc.none, Diff, [union; e])
    else union
  | Op (_, Diff, es), e -> Op (TxtLoc.none, Diff, es @ [e])
  | _, _ -> Op (TxtLoc.none, Diff, [e1; e2])

  and op2_to_func = function
  | Union -> unite
  | Inter -> intersect
  | Diff -> diff
  | _ -> Warn.fatal "op2_to_func called on invalid operator"

  and apply_func defs f e =
    let f = match f with
    | Var (_, id) -> from_alias id
    | e -> Warn.fatal "%s is applied as a function but is not a var"
            (pp_expr e) in
    let f = match f with
    | "same-oa" -> (fun _ expr -> expr)
    | "fencerel" -> (fun _ _ -> 
        Op (TxtLoc.none, Cartesian, [universe_effect; universe_effect]))
    | "domain" -> domain StringSet.empty;
    | "range" -> range StringSet.empty
    | "at-least-one-writable" -> (fun _ expr -> match expr with
        | Op (_, Tuple, [e; _]) -> e
        | e -> Warn.fatal "at-least-one-writable called on invalid \
          expression %s" (pp_expr e)
      )
    | "oa-changes" -> (fun _ expr -> match expr with
        | Op (_, Tuple, [e; _]) -> e
        | e -> Warn.fatal "oa-changes called on invalid expression %s" (pp_expr e)
      )
    | _ -> Warn.fatal "Function %s not supported" f in
    f defs e

  and tr_effect defs = function
  | App (_, f, e) -> apply_func defs f e
  | Tag (_, _) | Bind (_, _, _) | BindRec (_, _, _) | Fun (_, _, _, _, _)
  | ExplicitSet (_, _) | Match (_, _, _, _) | MatchSet (_, _, _, _) as e ->
    Warn.fatal "%s is not an effect" (pp_expr e);
  | e -> e

  and domain looked_up_ids defs expr = match expr with
  | Var (_, id) ->
    if StringMap.mem id defs then
      if StringSet.mem id looked_up_ids then empty_effect
      else
        domain (StringSet.add id looked_up_ids) defs (StringMap.find id defs)
    else
      let id = from_alias id in
      if id = "same-instance" then universe_effect
      else begin
        match StringMap.find_opt id rel_effects with
        | Some pairs -> List.fold_left (fun acc (eff, _) ->
            unite defs acc eff
          ) empty_effect pairs
        | None ->
          Printf.eprintf "Could not find relation %s in map of primitives\n" id;
          empty_effect
      end
  | Op1 (_, ToId, e) -> tr_effect defs e
  | Op1 (_, (Plus | Star), e) -> domain looked_up_ids defs e
  | Op1 (_, Inv, e) -> range looked_up_ids defs e
  | Op1 (_, Comp, Op1 (_, ToId, e)) -> Op1 (TxtLoc.none, Comp, e)
  | Op1 (_, Comp, _) -> universe_effect (* over-approximation *)
  | Op (_, Cartesian, [e1; _]) -> e1
  | Op (_, (Seq | Diff), rels) ->
    let first_non_opt = List.find (function
      | Op1 (_, Opt, _) -> false
      | _ -> true
    ) rels in
    domain looked_up_ids defs first_non_opt
  | Op (_, op, rels) ->
    let results = List.map (domain looked_up_ids defs) rels in
    let results = List.filter (fun e -> not (is_empty e)) results in
    let func = op2_to_func op in
    if results = [] then empty_effect
    else List.fold_left (func defs) (List.hd results) (List.tl results)
  | Try (_, e1, e2) | If (_, _, e1, e2) ->
    let d1 = domain looked_up_ids defs e1 in
    let d2 = domain looked_up_ids defs e2 in
    unite defs d1 d2
  | _ -> Warn.fatal "Domain called on an invalid expression"

  and range looked_up_ids defs expr = match expr with
  | Var (_, id) ->
    if StringMap.mem id defs then
      if StringSet.mem id looked_up_ids then empty_effect
      else
        range (StringSet.add id looked_up_ids) defs (StringMap.find id defs)
    else
      let id = from_alias id in
      if id = "same-instance" then universe_effect
      else begin
        match StringMap.find_opt id rel_effects with
        | Some pairs -> List.fold_left (fun acc (_, eff) ->
            unite defs acc eff
          ) empty_effect pairs
        | None ->
          Printf.eprintf "Could not find relation %s in map of primitives\n" id;
          empty_effect
      end
  | Op1 (_, ToId, e) -> tr_effect defs e
  | Op1 (_, (Plus | Star), e) -> range looked_up_ids defs e
  | Op1 (_, Inv, e) -> domain looked_up_ids defs e
  | Op1 (_, Comp, Op1 (_, ToId, e)) -> Op1 (TxtLoc.none, Comp, e)
  | Op1 (_, Comp, _) -> universe_effect (* over-approximation *)
  | Op (_, Cartesian, [_; e2]) -> e2
  | Op (_, (Seq | Diff), rels) ->
    let first_non_opt = List.find (function
      | Op1 (_, Opt, _) -> false
      | _ -> true
    ) (List.rev rels) in
    range looked_up_ids defs first_non_opt
  | Op (_, op, rels) ->
    let results = List.map (range looked_up_ids defs) rels in
    let results = List.filter (fun e -> not (is_empty e)) results in
    let func = op2_to_func op in
    if results = [] then empty_effect
    else List.fold_left (func defs) (List.hd results) (List.tl results)
  | Try (_, e1, e2) | If (_, _, e1, e2) ->
    let d1 = range looked_up_ids defs e1 in
    let d2 = range looked_up_ids defs e2 in
    unite defs d1 d2
  | _ -> Warn.fatal "Range called on an invalid expression"

  let tr_rel e1 e2 id defs =
    let any_match e2 = List.exists (fun e ->
      not (is_empty (intersect defs e e2))) in

    let add_to_sol id chain sols =
      let rel = make_var id in
      ExprGraph.add_edge rel chain sols in

    let rec do_tr_effect e1s = function
    | App (_, f, e) ->
      let e = apply_func defs f e in
      do_tr_effect e1s e
    | Tag (_, _) | Bind (_, _, _) | BindRec (_, _, _) | Fun (_, _, _, _, _)
    | ExplicitSet (_, _) | Match (_, _, _, _) | MatchSet (_, _, _, _) as e ->
      Printf.eprintf "Matching %s not supported\n" (pp_expr e);
      []
    | e ->
      List.filter (fun e -> not (is_empty e))
        (List.map (intersect defs e) e1s) in

    (* Traverses a cat clause as part of a relation. Takes as arguments:
       - looked_up_ids: List of Vars that are defined in the cat file
       and have already been replaced with their expansions. Useful
       for shadowing.
       - ret_sol: A boolean denoting whether the return value of the current
       call of this function needs to be part of the final solution. This
       happens, for example, when dealing with a complete `ob` expansion,
       but not when recursing into relations that are in the middle of
       a chain.
       - sol: List of current top level solutions
       - e1s: List of effects that we need to check if we can traverse this
        relation with. These can come, for example, from the rhs of a previous
       relation in the chain.
       - is_inv: A boolean denoting whether the chain needs to be traversed in
       reverse. This can be because its parent relation r has been included
       as r^-1 in a chain further up the stack.
        *)
    let rec do_tr_chain
      (looked_up_ids : StringSet.t)
      (ret_sol : bool)
      (sol: ExprGraph.t)
      (e1s : AST.exp list)
      (is_inv : bool) = function
    | Op (_, Seq, es) ->
      let es = if is_inv then List.rev es else es in
      let e2s = fold_left_short (fun e1s e ->
        let _, e1s = do_tr_chain looked_up_ids false ExprGraph.empty e1s is_inv e in
        e1s
      ) (fun l -> l = []) e1s es in
      sol, e2s
    | Var (_, id) -> do_tr_rel looked_up_ids ret_sol sol e1s is_inv id
    | Op1 (_, ToId, e) -> sol, do_tr_effect e1s e
    | Op1 (_, Inv, e) ->
      do_tr_chain looked_up_ids ret_sol sol e1s (not is_inv) e
    | Op1 (_, Comp, Op1 (_, ToId, e)) ->
      sol, do_tr_effect e1s (Op1 (TxtLoc.none, Comp, e))
    | Op1 (_, Comp, _) -> sol, [universe_effect] (* over-approximation *)
    | Op1 (_, Plus, e) -> do_tr_chain looked_up_ids ret_sol sol e1s is_inv e
    | Op1 (_, (Star | Opt), e) ->
      let sol, e2s = do_tr_chain looked_up_ids ret_sol sol e1s is_inv e in
      sol, e1s @ e2s
    | Op (_, Cartesian, [e1; e2]) ->
      if not (do_tr_effect e1s e1 = []) then sol, [e2]
      else sol, []
    | Op (_, Cartesian, _) as e ->
      Warn.fatal "Cartesian product of more than 2 effects is not a relation: %s\n"
        (pp_expr e)
    | App (_, f, e) ->
      let e = apply_func defs f e in
      do_tr_chain looked_up_ids ret_sol sol e1s is_inv e
    | Op (_, Inter, es) ->
      let results = List.map (fun e -> 
        let _, res = do_tr_chain looked_up_ids false ExprGraph.empty e1s is_inv e in
        res
      ) es in
      sol, Misc.fold_cross results (fun effs acc ->
        let inter = List.fold_left (intersect defs) universe_effect effs in
        match inter with
        | Konst (_, (Empty _)) -> acc
        | _ -> inter :: acc
      ) []
    | Op (_, Union, es) ->
      (* TODO: This can be one big disjunction. Would help eliminate
        duplicates. *)
      let results = List.fold_right (fun e acc ->
        let _, res = do_tr_chain looked_up_ids false ExprGraph.empty e1s is_inv e in
        res @ acc
      ) es [] in
      sol, results
    | e ->
      Printf.eprintf "Traversing %s not supported\n" (pp_expr e);
      sol, []
    
    and do_tr_rel
        (looked_up_ids : StringSet.t)
        (ret_sol : bool)
        (sol: ExprGraph.t)
        (e1s : AST.exp list)
        (is_inv : bool)
        (id : string) =
      if StringMap.mem id defs then begin
        if StringSet.mem id looked_up_ids then sol, []
        else
          let looked_up_ids = StringSet.add id looked_up_ids in
          let chain = StringMap.find id defs in
          if ret_sol then
            let chains = match chain with
            | Op (_, Union, es) -> es
            | e -> [e] in
            List.fold_right (fun chain (sol, e2s_acc) ->
              let sol, e2s = do_tr_chain looked_up_ids ret_sol sol e1s is_inv chain in
              if any_match e2 e2s then (add_to_sol id chain sol), e2s @ e2s_acc
              else sol, e2s_acc
            ) chains (sol, [])
          else
            do_tr_chain looked_up_ids ret_sol sol e1s is_inv chain
      end
      else
        let id = from_alias id in
        if id = "same-instance" then
          sol, e1s
        else
          let default = [(universe_effect, universe_effect)] in
          let pairs = StringMap.safe_find default id rel_effects in
          let pairs = if is_inv then
            List.map (fun (a, b) -> b, a) pairs
          else pairs in
          let res = List.fold_left (fun res (e_left, e_right) ->
            if any_match e_left e1s then e_right :: res
            else res
          ) [] pairs in
          sol, res in

    let sol, _ = do_tr_rel StringSet.empty true ExprGraph.empty [e1] false id in
    sol

  let parse_effect_from_string name value =
    Cache.cache ~fname:name ~cts:value;
    let module ML = ModelLexer.Make(struct
      let debug = false
    end) in
    let open Lexing in
    let lexbuf = Lexing.from_string value in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
    GenParserUtils.call_parser "effect" lexbuf ML.token ModelParser.base_start

  let replace_ifs_and_tries expr =
    let rec run = function
    | Konst _ | Tag _ | Var _ as e -> e
    | Op1 (loc, op, e) -> Op1 (loc, op, run e)
    | Op (loc, op, es) -> Op (loc, op, List.map run es)
    | App (loc, e1, e2) -> App (loc, run e1, run e2)
    | Bind (loc, bds, e) -> Bind (loc, run_bds bds, run e)
    | BindRec (loc, bds, e) -> BindRec (loc, run_bds bds, run e)
    | Fun (loc, pat, e, f, xs) -> Fun (loc, pat, run e, f, xs)
    | ExplicitSet (loc, es) -> ExplicitSet (loc, List.map run es)
    | Match (loc, e, cls, eo) ->
        let cls = List.map (fun (x, e) -> x, run e) cls in
        let eo = Option.map (fun e -> run e) eo in
        Match (loc, run e, cls, eo)
    | MatchSet (loc, e1, e2, cl) ->
      let cl = match cl with
      | EltRem (p1, p2, e) -> EltRem (p1, p2, run e)
      | PreEltPost (p1, p2, p3, e) -> PreEltPost (p1, p2, p3, run e) in
      MatchSet (loc, run e1, run e2, cl)
    | Try (_, e, e2) | If (_, _, e, e2) when is_empty e2 -> e
    | Try (_, e1, e) | If (_, _, e1, e) when is_empty e1 -> e
    | Try (loc, e1, e2) -> Try (loc, run e1, run e2)
    | If (loc, cond, e1, e2) -> If (loc, run_cond cond, run e1, run e2)
    and run_bds bds = List.map (fun (loc, pat, e) -> (loc, pat, run e)) bds
    and run_cond = function
    | Eq (e1, e2) -> Eq (run e1, run e2)
    | Subset (e1, e2) -> Subset (run e1, run e2)
    | In (e1, e2) -> In (run e1, run e2)
    | VariantCond _ as c -> c
    in
    run expr

  (* Adds special character to distinguish between non-terminals and primitives
    and replaces shadowed variables with their definitions *)
  let convert_defs num_defs crt_nums def_id is_rec expr =
    let rec run = function
    | Var (_, "emptyset") as e -> e
    | Var (_, id) as e ->
      let crt_num = StringMap.safe_find 0 id crt_nums in
      let total_num = StringMap.safe_find 0 id num_defs in
      if is_rec && id = def_id then
        if total_num = crt_num + 1 then e
        else make_var (to_alias id (crt_num + 1))
      else if total_num = crt_num then e
      else make_var (to_alias id crt_num)
    | Konst _ | Tag _ as e -> e
    | Op1 (loc, op, e) -> Op1 (loc, op, run e)
    | Op (loc, op, es) -> Op (loc, op, List.map run es)
    | App (loc, e1, e2) -> App (loc, run e1, run e2)
    | Bind (loc, bds, e) -> Bind (loc, run_bds bds, run e)
    | BindRec (loc, bds, e) -> BindRec (loc, run_bds bds, run e)
    | Fun (loc, pat, e, f, xs) -> Fun (loc, pat, run e, f, xs)
    | ExplicitSet (loc, es) -> ExplicitSet (loc, List.map run es)
    | Match (loc, e, cls, eo) ->
        let cls = List.map (fun (x, e) -> x, run e) cls in
        let eo = Option.map (fun e -> run e) eo in
        Match (loc, run e, cls, eo)
    | MatchSet (loc, e1, e2, cl) ->
      let cl = match cl with
      | EltRem (p1, p2, e) -> EltRem (p1, p2, run e)
      | PreEltPost (p1, p2, p3, e) -> PreEltPost (p1, p2, p3, run e) in
      MatchSet (loc, run e1, run e2, cl)
    | Try (loc, e1, e2) -> Try (loc, run e1, run e2)
    | If (loc, cond, e1, e2) -> If (loc, run_cond cond, run e1, run e2)
    and run_bds bds = List.map (fun (loc, pat, e) -> (loc, pat, run e)) bds
    and run_cond = function
    | Eq (e1, e2) -> Eq (run e1, run e2)
    | Subset (e1, e2) -> Subset (run e1, run e2)
    | In (e1, e2) -> In (run e1, run e2)
    | VariantCond _ as c -> c
    in
    run expr

  let tr_bds defs kont bds =
    List.fold_left (fun defs (_, p, expr) ->
      match p with
      | Pvar (Some id) -> kont defs id expr
      | _ -> defs
    ) defs bds

  let rec tr_ast defs kont parsed_files fname =
    let parsed_files = StringSet.add fname parsed_files in
    let (_, _, ast) = P.parse fname in
    List.fold_left (fun (defs, parsed_files) ins ->
      tr_ins defs kont parsed_files ins
    ) (defs, parsed_files) ast

  and tr_ins defs kont parsed_files = function
  | Let (_, bds) ->
    let defs = tr_bds defs (kont false) bds in
    (defs, parsed_files)
  | Rec (_, bds, _) ->
    let defs = tr_bds defs (kont true) bds in
    (defs, parsed_files)
  | Include (_,fname) when not (StringSet.mem fname parsed_files) ->
      tr_ast defs kont parsed_files fname
  | _ -> (defs, parsed_files)

  let count_defs fname existing_num_defs =
    let num_defs, _ = tr_ast existing_num_defs (fun _ num_defs id _ ->
      let num = StringMap.safe_find 0 id num_defs in
      StringMap.add id (num + 1) num_defs
    ) StringSet.empty fname in
    num_defs

  let get_defs fname ~existing_defs ~num_defs ~crt_nums =
    let (defs, crt_nums), _= tr_ast (existing_defs, crt_nums)
      (fun is_rec (defs, crt_nums) id expr ->
        let converted_def = convert_defs num_defs crt_nums id is_rec expr in
        let num = StringMap.safe_find 0 id crt_nums + 1 in
        let total_num = StringMap.find id num_defs in
        let crt_nums = StringMap.add id num crt_nums in
        let id = if num = total_num then id else to_alias id num in
        let defs = StringMap.add id converted_def defs in
        defs, crt_nums
      ) StringSet.empty fname in
    defs, crt_nums

  let execute fname e1 e2 start_rel =
    let stdlib_fname = libfind "stdlib.cat" in
    let stdlib_num_defs = count_defs stdlib_fname StringMap.empty in
    let num_defs = count_defs fname stdlib_num_defs in
    let stdlib_defs, crt_nums = get_defs stdlib_fname
      ~existing_defs:StringMap.empty 
      ~num_defs
      ~crt_nums:StringMap.empty in
    let defs, _ = get_defs fname ~existing_defs:stdlib_defs
      ~num_defs ~crt_nums in
    let defs = StringMap.map (fun def ->
      replace_ifs_and_tries (ASTUtils.flatten def)
    ) defs in
    let is_valid_user_defined_eff (id, _) =
      if not (contains_alias id) && can_be_effect id defs valid_primitives then
        Some id
      else None
    in
    let user_defined_effs = StringMap.bindings defs
      |> List.filter_map is_valid_user_defined_eff
      |> StringSet.of_list in
    let valid_effs = StringSet.union valid_primitives user_defined_effs in
    let check_valid_eff e =
      if not (StringSet.mem e valid_effs) then begin
        Warn.warn_always "Effects %s not supported or non existent" e;
        Printf.printf "No solutions\n";
        exit 1
      end
    in
    match e1, e2 with
    | Some e1, Some e2 ->
      let e1 = parse_effect_from_string "e1" e1 |> ASTUtils.flatten in
      let e2 = parse_effect_from_string "e2" e2 |> ASTUtils.flatten in
      ASTUtils.free_body [] e1 |> StringSet.iter check_valid_eff;
      ASTUtils.free_body [] e2 |> StringSet.iter check_valid_eff;
      let sol = tr_rel e1 e2 start_rel defs in
      if sol = ExprGraph.empty then
        Printf.printf "No solutions\n"
      else
        ignore (ExprGraph.dfs_fold
          ~per_vertex:(fun rel lvl _ ->
            Printf.printf "%s-%s\n" (String.make (lvl - 1) ' ') (pp_expr rel)
          )
          ~per_neighbour:(fun _ _ _ -> ())
          sol
          (make_var start_rel)
          ExprGraph.VSet.empty
          ()
        )
    | _ -> Warn.fatal "Omission of e1 or e2 not implemented yet"

end

let verbose = ref false
let herd_libdir = ref (Filename.concat Version.libdir "herd")
let libdir = ref (Filename.concat Version.libdir "tools")
let includes = ref []
let model = ref (Filename.concat !herd_libdir "aarch64.cat")
let assumptions_file = ref (Filename.concat !libdir "aarch64assumptions.cat")
let e1 = ref None
let e2 = ref None
let start_rel = ref "ob"

let options = [
  ("-version", Arg.Unit
    (fun () -> Printf.printf "%s, Rev: %s\n" Version.version Version.rev;
      exit 0), " show version number and exit");
  ("-libdir", Arg.Unit (fun () -> print_endline !libdir; exit 0),
    " show installation directory and exit");
  ("-set-libdir", Arg.String (fun s -> libdir := s),
    "<path> set installation directory to <path>");
  ("-herd-libdir", Arg.Unit (fun () -> print_endline !herd_libdir; exit 0),
    " show herd installation directory and exit");
  ("-set-herd-libdir", Arg.String (fun s -> herd_libdir := s),
    "<path> set herd installation directory to <path>");
  ("-I", Arg.String (fun s -> includes := !includes @ [s]),
    "<dir> add <dir> to search path. Files inside <dir> must not conflict \
    with herd-libdir");
  (ArgUtils.parse_string "-model" model "path to cat model");
  (ArgUtils.parse_string "-assumptions-file" assumptions_file "path to assumptions file");
  (ArgUtils.parse_string_opt "-e1" e1 "first effect");
  (ArgUtils.parse_string_opt "-e2" e2 "second effect");
  (ArgUtils.parse_string "-start-rel" start_rel "top level relation");
  (ArgUtils.parse_bool "-v" verbose "show various diagnostics");
]

let arg_handler s =
  raise (Arg.Bad (Printf.sprintf "Unexpected argument: %s" s))

let () =
  try
    Arg.parse options
      arg_handler
      (Printf.sprintf "Usage %s [options], output all chains of relations \
        between e1 and e2." prog)
  with
  | Misc.Fatal msg -> Printf.eprintf "%s: %s\n" prog msg; exit 2

let () = includes := !herd_libdir :: !includes

let () =
  let module Run =
    Make
      (struct
        let verbose = !verbose
        let includes = !includes
        let libdir = !libdir
        let assumptions_file = !assumptions_file
      end) in
  ignore (Run.execute !model !e1 !e2 !start_rel)
