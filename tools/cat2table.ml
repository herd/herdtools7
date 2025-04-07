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

  let pp_expr_with_loc e = ASTUtils.exp2loc e |> Cache.extract
  let pp_expr =
    let rec pp_with_sep paran rec_paran sep es =
      let str = String.concat sep (List.map (do_pp_expr rec_paran) es) in
      if paran then Printf.sprintf "(%s)" str else str
    and do_pp_expr paran = function
    | Konst (_, (Empty _)) -> "empty"
    | Konst (_, (Universe _)) -> "universe"
    | Var (_, id) -> from_alias id
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

  (** Comparison function on expressions - assumes that sub-expression in op
    case are sorted based on itself *)
  let rec compare_expr e1 e2 = match e1, e2 with
  | e1, e2 when is_empty e1 && is_empty e2 -> 0
  | Konst (_, (Universe _)), Konst (_, (Universe _)) -> 0
  | e, _ when is_empty e -> -1
  | _, e when is_empty e -> 1
  | Konst (_, (Universe _)), _ -> -1
  | _, Konst (_, (Universe _)) -> 1
  | Op1 (_, op1, e1), Op1 (_, op2, e2) ->
    let c = compare op1 op2 in
    if c != 0 then c
    else compare_expr e1 e2
  | Op1 _, _ -> -1
  | _, Op1 _ -> 1
  | Op (_, op1, es1), Op (_, op2, es2) ->
    let c = compare op1 op2 in
    if c != 0 then c
    else compare_expr_list es1 es2
  | Op _, _ -> -1
  | _, Op _ -> 1
  | _, _ -> 0

  and compare_expr_list es1 es2 = match es1, es2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | e1 :: e1s, e2 :: e2s ->
    let c = compare_expr e1 e2 in
    if c != 0 then c
    else compare_expr_list e1s e2s

  let rec sort_subexprs = function
  | (Konst (_) | Var (_)) as e -> e
  | Op1 (loc, op, e) -> Op1 (loc, op, sort_subexprs e)
  | Op (loc, op, es) ->
    let es = List.map sort_subexprs es in
    let es = List.sort compare_expr es in
    Op (loc, op, es)
  | e -> e

  let expr_equal e1 e2 =
    let do_op op1 op2 e1 e2 =
      if op1 != op2 then false
      else
        let e1 = sort_subexprs e1 in
        let e2 = sort_subexprs e2 in
        compare_expr e1 e2 = 0 in
    match e1, e2 with
    | e1, e2 when is_empty e1 && is_empty e2 -> true
    | Konst (_, (Universe _)), Konst (_, (Universe _)) -> true
    | Var (_, id1), Var (_, id2) -> id1 = id2
    | Op (_, op1, _), Op (_, op2, _) -> do_op op1 op2 e1 e2
    | Op1 (_, op1, e1), Op1 (_, op2, e2) -> do_op op1 op2 e1 e2
    | _, _ -> false

  let barriers = ["DMB.ISH"; "DMB.ISHLD"; "DMB.ISHST"; "DMB.LD"; "DMB.OSH";
    "DMB.OSHLD"; "DMB.OSHST"; "DMB.ST"; "DMB.SY"; "DSB.ISH"; "DSB.ISHLD";
    "DSB.ISHST"; "DSB.LD"; "DSB.OSH"; "DSB.OSHST"; "DSB.ST"; "DSB.SY"; "ISB"]

  let pte_accesses = ["PTEDevice"; "PTEDevice-GRE"; "PTEDevice-nGRE";
    "PTEDevice-nGnRE"; "PTEDevice-nGnRnE"; "PTEInner-non-cacheable";
    "PTEInner-shareable"; "PTEInner-write-back"; "PTEInner-write-through";
    "PTENon-shareable"; "PTENormal"; "PTEOuter-non-cacheable";
    "PTEOuter-shareable"; "PTEOuter-write-back"; "PTEOuter-write-through";
    "PTETaggedNormal"; "PTEXS"]

  let pte_vals = ["PTEINV"; "PTEV"]
  let all_ptes = "PTEMemAttr" :: (pte_vals @ pte_accesses)

  let supersets = [
    ("A", ["M"; "R"; "Exp"]);
    ("AF", ["NExp"]);
    ("AccessFlag", ["MMU"; "EXC-ENTRY"; "FAULT"]);
    ("BCC", ["B"]);
    ("DB", ["NExp"]);
    ("EXC-ENTRY", ["FAULT"]);
    ("EXC-RET", ["B"]);
    ("Instr", ["M"]);
    ("IW", ["W"; "M"]);
    ("L", ["M"; "W"; "Exp"]);
    ("MMU", ["EXC-ENTRY"; "FAULT"]);
    ("NoRet", ["R"; "M"]);
    ("PA", ["M"]);
    ("Permission", ["MMU"; "EXC-ENTRY"; "FAULT"]);
    ("PTE", ["M"]);
    ("PTEMemAttr", ["PTE"; "M"]);
    ("Q", ["M"; "R"; "Exp"]);
    ("R", ["M"]);
    ("Restricted-CMODX", ["Instr"; "M"]);
    ("SupervisorCall", ["EXC-ENTRY"; "FAULT"]);
    ("T", ["M"]);
    ("TagCheck", ["FAULT"]);
    ("Translation", ["MMU"; "EXC-ENTRY"; "FAULT"]);
    ("TLBIIS", ["TLBI"]);
    ("TLBInXS", ["TLBI"]);
    ("UndefinedInstruction", ["EXC-ENTRY"; "FAULT"]);
    ("W", ["M"]);
  ] @ List.map (fun b -> b, ["F"]) barriers
    @ List.map (fun e -> e, ["PTE"; "M"; "PTEMemAttr"]) pte_accesses

  let intersects = [
    ("DATA", ["Exp"; "NExp"; "R"; "M"; "Instr"; "Restricted-CMODX"; "PTE"; "PA"; "T"; "Rreg"] @ all_ptes);  (* TODO: Get confirmation on this *)
    ("EXC-ENTRY", ["TagCheck"]);
    ("Exp", ["DATA"; "M"; "R"; "W"; "Instr"; "Restricted-CMODX"; "PTE"; "PA"; "T"; "Rreg"; "Wreg"] @ all_ptes);
    ("Instr", ["DATA"; "Exp"; "NExp"; "R"; "W"]);
    ("IW", ["PTE"; "PA"; "T"] @ all_ptes);
    ("M", ["DATA"; "Exp"; "NExp"] @ pte_vals);
    ("NExp", ["DATA"; "M"; "R"; "W"; "Instr"; "Restricted-CMODX"; "PTE"; "PA"; "T"; "Rreg"; "Wreg"] @ all_ptes);
    ("PA", ["DATA"; "Exp"; "NExp"; "R"; "W"; "IW"]);
    ("PTE", ["DATA"; "Exp"; "NExp"; "R"; "W"; "IW"] @ pte_vals);
    ("PTEINV", ["DATA"; "Exp"; "NExp"; "M"; "R"; "W"; "IW"; "PTE"; "PTEMemAttr"; "Wreg"; "Rreg"] @ pte_accesses);
    ("PTEMemAttr", ["DATA"; "Exp"; "NExp"; "R"; "W"; "IW"] @ pte_vals);
    ("PTEV", ["DATA"; "Exp"; "NExp"; "M"; "R"; "W"; "IW"; "PTE"; "PTEMemAttr"; "Wreg"; "Rreg"] @ pte_accesses);
    ("R", ["DATA"; "Exp"; "NExp"; "Instr"; "Restricted-CMODX"; "PTE"; "PA"; "T"] @ all_ptes);
    ("Restricted-CMODX", ["DATA"; "Exp"; "NExp"; "R"; "W"]);
    ("Rreg", ["DATA"; "Exp"; "NExp"] @ pte_vals);
    ("T", ["DATA"; "Exp"; "NExp"; "R"; "W"; "IW"]);
    ("TagCheck", ["EXC-ENTRY"]);
    ("W", ["Exp"; "NExp"; "Instr"; "Restricted-CMODX"; "PTE"; "PA"; "T"] @ all_ptes);
    ("Wreg", ["Exp"; "NExp"] @ pte_vals);
  ] @ List.map (fun e -> e, ["DATA"; "Exp"; "NExp"; "R"; "W"; "IW"] @ pte_vals) pte_accesses

  let read_effect = Var (TxtLoc.none, "R")
  let write_effect = Var (TxtLoc.none, "W")
  let memory_effect = Var (TxtLoc.none, "M")
  let fault_effect = Var (TxtLoc.none, "FAULT")
  let reg_read_effect = Var (TxtLoc.none, "Rreg")
  let pte_effect = Var (TxtLoc.none, "PTE")
  let tlbi_effect = Var (TxtLoc.none, "TLBI")
  let mem_or_fault = Op (TxtLoc.none, Union, [memory_effect; fault_effect])

  let build_map f = List.fold_left (fun map (key, value) ->
    StringMap.add key (f value) map
  ) StringMap.empty

  let build_map_of_sets = build_map (List.fold_left (fun set e ->
      StringSet.add e set
    ) StringSet.empty
  )
  
  let supersets = build_map_of_sets supersets
  let intersects = build_map_of_sets intersects
  let rel_effects = build_map Fun.id [
    ("po", [(universe_effect, universe_effect)]);
    ("ext", [(universe_effect, universe_effect)]);

    (* When extremities are not specified (only f-ib), we assume universe *)
    ("iico_data", [(universe_effect, universe_effect)]);
    ("iico_ctrl", [(universe_effect, universe_effect)]);
    ("iico_order", [(universe_effect, universe_effect)]);
    ("TLBI-after", [(universe_effect, universe_effect)]);
    ("DC-after", [(universe_effect, universe_effect)]);
    ("IC-after", [(universe_effect, universe_effect)]);

    (* Imprecise, the effects also need to be to the same type of location *)
    ("loc", [(universe_effect, universe_effect)]);
    ("amo", [(read_effect, write_effect)]);
    ("rmw", [(read_effect, write_effect)]);
    ("lxsx", [(read_effect, write_effect)]);
    ("co", [(write_effect, write_effect)]);
    ("co0", [(write_effect, write_effect)]);
    ("rf", [(write_effect, read_effect)]);
    ("rf-reg", [(write_effect, reg_read_effect)]);
    ("fr", [(read_effect, write_effect)]);
    ("sm", [(mem_or_fault, mem_or_fault)]);

    ("same-low-order-bits", [(mem_or_fault, mem_or_fault)]);
    ("inv-domain", [(pte_effect, tlbi_effect); (tlbi_effect, pte_effect)])
  ]

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
      let res1 = Var (TxtLoc.none, id1) in
      let res2 = Var (TxtLoc.none, id2) in
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
    let functions = get_functions () in
    let f = match f with
    | Var (_, id) -> from_alias id
    | e -> Warn.fatal "%s is applied as a function but is not a var"
            (pp_expr e) in
    let f = try StringMap.find f functions
    with Not_found -> Warn.fatal "Function %s not found in map" f in
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
        try
          let pairs = StringMap.find id rel_effects in
          List.fold_left (fun acc (eff, _) ->
            unite defs acc eff
          ) empty_effect pairs
        with Not_found ->
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
        try
          let pairs = StringMap.find id rel_effects in
          List.fold_left (fun acc (_, eff) ->
            unite defs acc eff
          ) empty_effect pairs
        with Not_found ->
          Printf.eprintf "Could not find relation %s in map of primitives" id;
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
  
  and get_functions =
    let lazy_map = lazy (build_map Fun.id [
        ("same-oa", fun _ expr -> expr);
        ("fencerel", fun _ _ -> 
            Op (TxtLoc.none, Cartesian, [universe_effect; universe_effect]));
        ("domain", domain StringSet.empty);
        ("range", range StringSet.empty);
        ("at-least-one-writable", fun _ expr -> match expr with
        | Op (_, Tuple, [e; _]) -> e
        | e -> Warn.fatal "at-least-one-writable called on invalid expression %s" (pp_expr e)
        );
        ("oa-changes", fun _ expr -> match expr with
        | Op (_, Tuple, [e; _]) -> e
        | e -> Warn.fatal "oa-changes called on invalid expression %s" (pp_expr e)
        )
      ]
    ) in
    fun () -> Lazy.force lazy_map

  let tr_rel e1 e2 id defs =
    let any_match e2 = List.exists (fun e ->
      not (is_empty (intersect defs e e2))) in

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

    (* TODO: Rewrite this so it filters out duplicates in the result *)
    let rec do_tr_chain looked_up_ids ret_sol sol e1s is_inv = function
    | Op (_, Seq, es) ->
      let es = if is_inv then List.rev es else es in
      let e2s = fold_left_short (fun e1s e ->
        let _, e1s = do_tr_chain looked_up_ids false [] e1s is_inv e in
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
    | Op (_, Inter, es) ->
      let results = List.map (fun e -> 
        let _, res = do_tr_chain looked_up_ids false [] e1s is_inv e in
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
        let _, res = do_tr_chain looked_up_ids false [] e1s is_inv e in
        res @ acc
      ) es [] in
      sol, results
    | e ->
      Printf.eprintf "Traversing %s not supported\n" (pp_expr e);
      sol, []
    
    and do_tr_rel looked_up_ids ret_sol sol e1s is_inv id =
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
              if any_match e2 e2s then chain :: sol, e2s @ e2s_acc
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
          let pairs = try StringMap.find id rel_effects
          with Not_found ->
            Printf.eprintf "Relation %s not supported\n" id;
            [] in
          let pairs = if is_inv then
            List.map (fun (a, b) -> b, a) pairs
          else pairs in
          let res = List.fold_left (fun res (e_left, e_right) ->
            if any_match e_left e1s then e_right :: res
            else res
          ) [] pairs in
          sol, res in

    let sol, e2s = do_tr_rel StringSet.empty true [] [e1] false id in
    if any_match e2 e2s then Var (TxtLoc.none, id) :: sol else sol

  let parse_effect_from_string name value =
    Cache.cache name value;
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
      if StringMap.mem id crt_nums then
        let total_num = try StringMap.find id num_defs
        with Not_found ->
          Warn.fatal "Found definition for %s that was not counted" id in
        let crt_num = StringMap.find id crt_nums in
        if is_rec && id = def_id then
          if total_num = crt_num + 1 then e
          else Var (TxtLoc.none, to_alias id (crt_num + 1))
        else if total_num = crt_num then e
        else Var (TxtLoc.none, to_alias id crt_num)
      else
        if is_rec && id = def_id then
          let total_num = try StringMap.find id num_defs
          with Not_found ->
            Warn.fatal "Found definition for %s that was not counted" id in
          if total_num = 1 then e
          else Var (TxtLoc.none, to_alias id 1)
        else
          Var (TxtLoc.none, to_alias id 0)
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

  and tr_bds defs kont bds =
    List.fold_left (fun defs (_, p, expr) ->
      match p with
      | Pvar (Some id) -> kont defs id expr
      | _ -> defs
    ) defs bds

  let tr_ast fname e1 e2 start_rel =
    let num_defs, _ = tr_ast StringMap.empty (fun _ num_defs id _ ->
      let num = StringMap.safe_find 0 id num_defs in
      StringMap.add id (num + 1) num_defs
    ) StringSet.empty fname in
    let (defs, _), _= tr_ast (StringMap.empty, StringMap.empty)
      (fun is_rec (defs, crt_nums) id expr ->
        let converted_def = convert_defs num_defs crt_nums id is_rec expr in
        let num = StringMap.safe_find 0 id crt_nums + 1 in
        let total_num = StringMap.find id num_defs in
        let crt_nums = StringMap.add id num crt_nums in
        let id = if num = total_num then id else to_alias id num in
        let defs = StringMap.add id converted_def defs in
        defs, crt_nums
      ) StringSet.empty fname in
      let defs = StringMap.map (fun def ->
        replace_ifs_and_tries (ASTUtils.flatten def)
      ) defs in
    match e1, e2 with
    | Some e1, Some e2 ->
      let e1 = parse_effect_from_string "e1" e1 |> ASTUtils.flatten in
      let e2 = parse_effect_from_string "e2" e2 |> ASTUtils.flatten in
      let sol = tr_rel e1 e2 start_rel defs in
      if sol = [] then
        Printf.printf "No solutions\n"
      else
        Printf.printf "%s\n" (String.concat "\n" (List.map pp_expr sol))
    | _ -> Warn.fatal "Omission of e1 or e2 not implemented yet"

end

let verbose = ref false
let libdir = ref (Filename.concat Version.libdir "herd")
let includes = ref []
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
  ("-e1", Arg.String (fun s -> e1 := Some s), "<str> first effect");
  ("-e2", Arg.String (fun s -> e2 := Some s), "<str> second effect");
  ("-start", Arg.String (fun s -> start_rel := s), "<str> top level relation");
  (ArgUtils.parse_bool "-v" verbose "show various diagnostics");
]

let args = ref []
let get_cmd_arg s = args := s :: !args

let () =
  try
    Arg.parse options
      get_cmd_arg
      (Printf.sprintf "Usage %s [options] [cat_file], output all chains of \
        relations between e1 and e2." prog)
  with
  | Misc.Fatal msg -> Printf.eprintf "%s: %s\n" prog msg; exit 2

let cat_file = List.hd !args

let () =
  let module Run =
    Make
      (struct
        let verbose = !verbose
        let includes = !includes
        let libdir = !libdir
      end) in
  ignore (Run.tr_ast cat_file !e1 !e2 !start_rel)
