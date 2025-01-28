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

(*
    Translate ASL's grammar in ASLRef to a BNFC-style structure
 *)

(* Translate ids to be bnfc compliant *)
let cvt_id id =
  String.split_on_char '_' id
  |> List.filter (function "" -> false | _ -> true)
  |> List.map (fun n -> String.lowercase_ascii n |> String.capitalize_ascii)
  |> String.concat ""

(** Convert a menhir cmly file to bnfc AST *)
module Convert (MenhirGrammar : MenhirSdk.Cmly_api.GRAMMAR) : sig
  val entrypoints : string list
  val decls : BNFC.decl list
end = struct
  (* Load the grammar module *)
  open MenhirGrammar

  (* A hardcoded value of a common End-of-file token *)
  let eof_token = "EOF"

  (* Create useful mapping structures *)
  module ProductionMap = Map.Make (Production)
  module NonterminalMap = Map.Make (Nonterminal)

  (* Create a terminal set structure *)
  module TerminalSet = Set.Make (Terminal)

  (* Utility functions for easy name extraction *)
  let n_name nterm = Nonterminal.mangled_name nterm |> cvt_id
  let t_name = Terminal.name

  (* A utility function to check if a set of attributes contains any ones which signal the object being internal *)
  let is_external attrs =
    List.for_all (fun a -> not @@ Attribute.has_label "internal" a) attrs

  (*
     A utility function to check if a nonterminal is external
     A nonterminal is external if its attributes don't mark it as internal.
    *)
  let is_external_nterm nterm = is_external @@ Nonterminal.attributes nterm

  (*
     A utility function to check if a production is external
     A production is external when:
         * It doesn't contain an attribute marking it as internal
         * Its parent nonterminal is external
         * All its rhs symbols are external
    *)
  let is_external_prod prod =
    let is_regular =
      match Production.kind prod with `REGULAR -> true | _ -> false
    in
    let is_external_prod = is_external @@ Production.attributes prod in
    let is_external_nterm = is_external_nterm @@ Production.lhs prod in
    let has_external_sym =
      Array.for_all (fun (_, _, attrs) -> is_external attrs)
      @@ Production.rhs prod
    in
    is_regular && is_external_prod && is_external_nterm && has_external_sym

  (* 1. Collect entry points *)
  let entrypoints : string list =
    let is_allowed (nterm, _, _) =
      if is_external_nterm nterm then Some (n_name nterm) else None
    in
    List.filter_map is_allowed Grammar.entry_points

  (* 2. Infer operator precedence from the lr(1) state machine. *)
  (* 2.1. Crate a mapping linking each nonterminal to a mapping which contains
          all of that nonterminal's productions linked to the set of tokens which cause
          the productions to reduce *)
  let production_to_terminals : TerminalSet.t ProductionMap.t NonterminalMap.t =
    let build_map lr1 acc =
      let reductions = Lr1.get_reductions lr1 in
      List.fold_left
        (fun a (term, prod) ->
          let nterm = Production.lhs prod in
          NonterminalMap.update nterm
            (fun opt_prod_map ->
              let prod_map =
                Option.value ~default:ProductionMap.empty opt_prod_map
              in
              Some
                (ProductionMap.update prod
                   (fun opt_set ->
                     let set =
                       Option.value ~default:TerminalSet.empty opt_set
                     in
                     Some (TerminalSet.add term set))
                   prod_map))
            a)
        acc reductions
    in
    Lr1.fold build_map NonterminalMap.empty
    (* Only retain external nonterminals and productions *)
    |> NonterminalMap.filter (fun n _ -> is_external_nterm n)
    |> NonterminalMap.map (ProductionMap.filter (fun p _ -> is_external_prod p))

  (* 2.2. Determine the nonterminals with ambiguous precedence *)
  (* We do this by finding all the nonterminals whose productions are reduced by differing terminal sets *)
  let ambiguous_production_sets : TerminalSet.t ProductionMap.t NonterminalMap.t
      =
    NonterminalMap.filter
      (fun _ prod_map ->
        let opt_set = ProductionMap.choose_opt prod_map in
        match opt_set with
        | None -> false
        | Some (_, set) ->
            not
            @@ ProductionMap.for_all
                 (fun _ set2 -> TerminalSet.equal set set2)
                 prod_map)
      production_to_terminals

  (* 2.3. (TODO) It may be necessary to verify that the nonterminals are actually related (are part of the same component on the parse graph *)
  (* The next steps should likely be run for each connected component and produce a `production list list` for each *)

  (* 2.4. (TODO) We need to determine the associativity of each of the terminals in the ambiguous precedence cases *)
  (* For now left associativity is assumed. This should be possible to derive using the LR(1) table, but it's outside of the scope of this project for now *)

  (* A utility function to get the last element of an array or None if the array is empty *)
  let get_last arr =
    let len = Array.length arr in
    if len = 0 then None else Some arr.(len - 1)

  (* 2.5. Determine if any of the detected nonterminals can be removed and do so if possible *)
  (* This is sometimes possible. There seems to be an inefficiency in the LR(1) table which causes the following case to happen:

        A := ... B [t1, t2]
        A := ... A [t1, t2, t3, ...]

        B := ... A [t1, t2, t3, ...]

        C := ... A [t1, t2, t3, ...]
        ...

     In this case we can infer that all highlighted cases share the same terminal reduction set as `A := ... B` because:
         * `A := ... B` is the last reduction of `A` (as all others recurse as their last step), so its terminals are what reduces the wider `A`
         * `B := ... A` will always reduce with the last reduction terminal set of `A` (or the subset overlapping with `A`)
         * `C := ... A` will always reduce with the last reduction terminal set of `A` (or the subset overlapping with `A`)
  *)
  let reduced_production_sets : TerminalSet.t ProductionMap.t NonterminalMap.t =
    let get_last_rhs prod = get_last @@ Production.rhs prod in
    let prod_ends_with prod nterm =
      match get_last_rhs prod with
      | Some (N n, _, _) -> Nonterminal.equal n nterm
      | _ -> false
    in
    let get_last_set prod_map =
      let non_rec_only =
        ProductionMap.filter
          (fun prod _ -> not @@ prod_ends_with prod @@ Production.lhs prod)
          prod_map
      in
      if ProductionMap.cardinal non_rec_only = 1 then
        let _, set = ProductionMap.choose non_rec_only in
        Some set
      else None
    in
    let has_last_map, rest_map =
      NonterminalMap.partition
        (fun _ prod_map -> get_last_set prod_map |> Option.is_some)
        ambiguous_production_sets
    in
    let nterm_to_set : TerminalSet.t NonterminalMap.t =
      NonterminalMap.map
        (fun prod_map -> get_last_set prod_map |> Option.get)
        has_last_map
    in
    NonterminalMap.map
      (fun prod_map ->
        ProductionMap.mapi
          (fun prod set ->
            match get_last_rhs prod with
            | Some (N n, _, _) -> (
                match NonterminalMap.find_opt n nterm_to_set with
                | None -> set
                | Some s -> TerminalSet.inter set s)
            | _ -> set)
          prod_map)
      rest_map

  (* 2.6. Generate a precedence list by sorting the productions by the
          length of terminals following them - shortest meaning lowest precedence *)
  (* An example of what this nested list could look like (production type being expanded for clarity):

       [
           [
               A := A "&&" B
               B := B "&&" B
           ];
           [
               A := A "+" B
               B := B "+" B
           ]
       ]

     Notes:
       * Since we could have more than one (related) nonterminal we expect
         the precedence levels of the nonterminals to overlap.
         Each nonterminal, however, could appear in a different part of the parse tree
         as such we need to only consider terminals coming from binary ops at higher precedence levels
  *)
  let precedence_list : Production.t list list =
    (* First create a nested production list for each nonterminal.
       The lowest precedence is the one produced by the fewest terminals. *)
    let nterm_prec_levels : Production.t list list NonterminalMap.t =
      NonterminalMap.map
        (fun prod_map ->
          let simplified = ProductionMap.map TerminalSet.cardinal prod_map in
          let sorted =
            ProductionMap.bindings simplified
            |> List.sort (fun (_, c1) (_, c2) -> Int.compare c1 c2)
          in
          let rec loop prod_list =
            match prod_list with
            | [] -> []
            | (_, c1) :: _ ->
                let current_level, rest =
                  List.partition (fun (_, c2) -> Int.equal c1 c2) prod_list
                in
                let level = List.split current_level |> fst in
                level :: loop rest
          in
          loop sorted)
        reduced_production_sets
    in
    (* For each precedence level in each nonterminal we collect the set of terminals which cause a reduction in each production.
       In other words we collect all terminals which follow one of the target nonterminals
       We do this to determine which levels introduce which operands.

       Note: Ideally these sets would exactly match the binary ops which have a set precedence and associativity
             This is not the case as this approach is fairly naive (but is almost exact) a better one might be
             to try to build an LR1 parser and see near which terminals we get shift/reduce conflicts. *)
    let term_sets : TerminalSet.t list NonterminalMap.t =
      NonterminalMap.map
        (fun prec_list ->
          List.fold_right
            (fun level acc ->
              List.fold_right
                (fun prod set ->
                  let rhs = Production.rhs prod |> Array.to_list in
                  let rec collect_terms terms set =
                    match terms with
                    | (N n, _, _) :: (T t, _, _) :: tl
                      when NonterminalMap.mem n nterm_prec_levels ->
                        collect_terms tl (TerminalSet.add t set)
                    | (N n, _, _) :: (N n2, _, _) :: tl
                      when NonterminalMap.mem n nterm_prec_levels ->
                        let follow : TerminalSet.t =
                          Nonterminal.first n2 |> TerminalSet.of_list
                        in
                        collect_terms tl (TerminalSet.union set follow)
                    | _ :: tl -> collect_terms tl set
                    | [] -> set
                  in
                  collect_terms rhs set)
                level TerminalSet.empty
              :: acc)
            prec_list [])
        nterm_prec_levels
    in
    (* Lastly we merge all the nonterminal precedence levels by matching them up against each other's
       terminal sets. If two sets overlap the they belong to the same precedence level. If a set is found to not
       overlap then it either belongs at a higher or lower precedence than any other one in the other nonterminal. *)
    NonterminalMap.fold
      (fun nterm prec_levels acc ->
        let other_ops_per_level = NonterminalMap.find nterm term_sets in
        let new_levels = List.combine prec_levels other_ops_per_level in
        let is_inter s1 s2 =
          not @@ TerminalSet.is_empty @@ TerminalSet.inter s1 s2
        in
        let rec merge_levels old_levels new_levels =
          match (old_levels, new_levels) with
          | _, [] -> old_levels
          | [], _ -> new_levels
          | (l1, s1) :: tl1, (l2, s2) :: tl2 when is_inter s1 s2 ->
              (l1 @ l2, TerminalSet.union s1 s2) :: merge_levels tl1 tl2
          | (l1, s1) :: tl1, (l2, s2) :: tl2 -> (
              match List.find_opt (fun (_, s2) -> is_inter s1 s2) tl2 with
              | None -> (l1, s1) :: merge_levels tl1 new_levels
              | Some _ -> (l2, s2) :: merge_levels old_levels tl2)
        in
        merge_levels acc new_levels)
      nterm_prec_levels []
    |> List.split |> fst

  (* 2.7. Split off any non left recursive rules from each precedence level to their own lower one (e.g. unary ops) *)
  (* Unary ops don't produce other unary ops, which means unary ops always end up in a precedence level above thier own.
     They must be separate to avoid shift/reduce conflicts *)
  let with_unary_precedence_levels : Production.t list list =
    let split_l_rec prec_grp acc =
      let is_not_l_rec prod =
        let rhs = Production.rhs prod in
        let is_rec (sym, _, _) =
          match sym with
          | N n -> NonterminalMap.mem n reduced_production_sets
          | _ -> false
        in
        Array.length rhs > 0 && is_rec rhs.(0)
      in
      let l_rec, rest = List.partition is_not_l_rec prec_grp in
      match (l_rec, rest) with
      | [], _ -> rest :: acc
      | _, [] -> l_rec :: acc
      | _ -> l_rec :: rest :: acc
    in
    List.fold_right split_l_rec precedence_list []

  (* 2.8. Correct any binary expr productions which are in the wrong level *)
  (* Binary ops of the form:

       A := A op TERMINAL

     appear to not mirror the precedence level of other occurances of op where

       A := A op A

     these must be at the same level to avoid shift/reduce conflicts *)
  let final_precedence_levels : Production.t list list =
    let get_binary_op p =
      let nterm = Production.lhs p in
      let is_nterm sym =
        match sym with N n -> Nonterminal.equal nterm n | _ -> false
      in
      match Production.rhs p |> Array.map (fun (s, _, _) -> s) with
      | [| s1; T op; s2 |] when is_nterm s1 || is_nterm s2 -> Some op
      | _ -> None
    in
    let rec reorder_binops ?(seen = TerminalSet.empty) prec_levels =
      match prec_levels with
      | [] -> []
      | level :: rest ->
          let ops, level =
            List.fold_right
              (fun p (ops, level) ->
                match get_binary_op p with
                | None -> (ops, p :: level)
                | Some t when TerminalSet.mem t seen -> (ops, level)
                | Some t -> (TerminalSet.add t ops, p :: level))
              level (TerminalSet.empty, [])
          in
          let same_binary_ops =
            List.filter
              (fun prod ->
                match get_binary_op prod with
                | Some op -> TerminalSet.mem op ops
                | None -> false)
              (List.concat rest)
          in
          let seen = TerminalSet.union ops seen in
          (level @ same_binary_ops) :: reorder_binops ~seen rest
    in
    reorder_binops with_unary_precedence_levels
    (* Sort the productions in definition order for consistency *)
    |> List.map (List.sort Production.compare)

  (* 3. Convert all productions to bnfc *)
  (* We first convert the precedence related tokens by adding a new bnf level for each entry in the precedence list
     while keeping track of which level we are at for each nonterminal *)
  (* Last we trivailly convert all remaining productions. They are all a 1:1 mapping from menhir productions to bnfc lines *)

  (* Define some common case suffixes *)
  let single_rule_suffix = "_Rule"
  let none_rule_suffix = "_None"
  let some_rule_suffix = "_Some"

  (* A utility function to create a list of suffixes from a list of productions *)
  let mk_short_suffixes (prod_list : Production.t list) : string list =
    let mk_verbose_suffix prod =
      let rhs = Production.rhs prod in
      if Array.length rhs = 0 then [ none_rule_suffix ]
      else
        Array.map
          (function N n, _, _ -> n_name n | T t, _, _ -> t_name t |> cvt_id)
          rhs
        |> Array.to_list
    in
    let suffix_list = List.map mk_verbose_suffix prod_list in
    let rec trim_prefix suff_ls =
      let same_prefix =
        if List.for_all (fun l -> List.length l > 1) suff_ls then
          let prefix = List.hd @@ List.hd suff_ls in
          List.for_all (fun l -> String.equal (List.hd l) prefix)
          @@ List.tl suff_ls
        else false
      in
      if same_prefix then trim_prefix (List.map List.tl suff_ls) else suff_ls
    in
    let trim_suffix suff_ls =
      let shorten name_list =
        let rec cmp_lists test ls =
          match (test, ls) with
          | hd1 :: tl1, hd2 :: tl2 when String.equal hd1 hd2 ->
              cmp_lists tl1 tl2
          | [], _ -> 1
          | _ -> 0
        in
        let count_prefixed_by test acc ls =
          if List.length test > List.length ls then acc
          else cmp_lists test ls + acc
        in
        let rec find_shortest test rem =
          match rem with
          | hd :: tl
            when not (List.fold_left (count_prefixed_by test) 0 suff_ls = 1) ->
              find_shortest (test @ [ hd ]) tl
          | _ -> test
        in
        match name_list with [] -> [] | hd :: tl -> find_shortest [ hd ] tl
      in
      List.map shorten suff_ls
    in
    let mk_suffix ls = String.concat "_" ls in
    trim_prefix suffix_list |> trim_suffix |> List.map mk_suffix

  (* A utility function to build a decl from a production and an ast name suffix *)
  let mk_decl prod suffix =
    let name = Production.lhs prod |> n_name in
    let mk_terms prod =
      let rhs = Production.rhs prod in
      let mk_term (s, _, _) acc =
        match s with
        | N n -> BNFC.Reference (n_name n) :: acc
        | T t when String.equal (t_name t) eof_token -> acc
        | T t -> BNFC.LitReference (t_name t) :: acc
      in
      Array.fold_right mk_term rhs []
    in
    let ast_name = name ^ "_" ^ suffix in
    let terms = mk_terms prod in
    BNFC.Decl { ast_name; name; terms }

  (* A utility function to create named bnfc nodes for a specific nonterminal's productions *)
  let mk_productions prod_list =
    let is_empty prod = Array.length @@ Production.rhs prod = 0 in
    match prod_list with
    | [] -> []
    | [ p ] -> [ mk_decl p single_rule_suffix ]
    | [ p1; p2 ] when is_empty p1 ->
        [ mk_decl p1 none_rule_suffix; mk_decl p2 some_rule_suffix ]
    | [ p1; p2 ] when is_empty p2 ->
        [ mk_decl p1 some_rule_suffix; mk_decl p2 none_rule_suffix ]
    | _ ->
        let short_suffixes = mk_short_suffixes prod_list in
        List.map2 mk_decl prod_list short_suffixes

  (* A utility function to check if a production is a unary op *)
  let is_unary expected_nterms prod =
    let rev_terms = Array.to_list @@ Production.rhs prod |> List.rev in
    match rev_terms with
    | (N n, _, _) :: tl ->
        List.exists (Nonterminal.equal n) expected_nterms
        && List.for_all (function T _, _, _ -> true | _ -> false) tl
    | _ -> false

  (* A utility function to create precedence related bnfc decl data *)
  let mk_precedence_productions prec_levels ast_name_map =
    let nterm_level_count : int NonterminalMap.t =
      let loop acc level =
        let nterms =
          List.map Production.lhs level |> List.sort_uniq Nonterminal.compare
        in
        List.fold_left
          (fun acc nterm ->
            NonterminalMap.update nterm
              (function Some idx -> Some (idx + 1) | None -> Some 0)
              acc)
          acc nterms
      in
      List.fold_left loop NonterminalMap.empty prec_levels
    in
    let indexes = NonterminalMap.map (fun _ -> 0) nterm_level_count in
    let is_last_idx nterm idx =
      Int.equal (NonterminalMap.find nterm nterm_level_count) idx
    in
    let mk_decls (acc, indexes) level =
      let mk_name ?(succ = false) nterm =
        let nterm_idx =
          let i = NonterminalMap.find nterm indexes in
          if succ then i + 1 else i
        in
        let base_name = n_name nterm in
        if nterm_idx = 0 then base_name else base_name ^ Int.to_string nterm_idx
      in
      let used_nterms =
        List.map Production.lhs level |> List.sort_uniq Nonterminal.compare
      in
      let fallback_states =
        List.filter_map
          (fun n ->
            let current_idx = NonterminalMap.find n indexes in
            if is_last_idx n current_idx then None
            else
              Some
                (BNFC.Decl
                   {
                     ast_name = "_";
                     name = mk_name n;
                     terms = [ BNFC.Reference (mk_name ~succ:true n) ];
                   }))
          used_nterms
      in
      let mk_prec_decls level =
        let mk_decl prod =
          let nterm = Production.lhs prod in
          let name = mk_name nterm in
          let ast_name = ProductionMap.find prod ast_name_map in
          let fst_is_rec =
            let rhs = Production.rhs prod in
            let is_rec (sym, _, _) =
              match sym with N n -> Nonterminal.equal n nterm | _ -> false
            in
            Array.length rhs > 0 && is_rec rhs.(0)
          in

          let binop_comps =
            match Array.map (fun (s, _, _) -> s) @@ Production.rhs prod with
            | [| N n1; T op; N n2 |]
              when List.exists (Nonterminal.equal n1) used_nterms
                   && List.exists (Nonterminal.equal n2) used_nterms ->
                Some (n1, op, n2)
            | _ -> None
          in

          let mk_simple_lit (sym, _, _) =
            match sym with
            | N n -> BNFC.Reference (n_name n)
            | T t -> BNFC.LitReference (t_name t)
          in

          if Option.is_some binop_comps then
            (* TODO this should use collected associativity data *)
            let get_associativity _ = `Left in

            let lhs, op, rhs = Option.get binop_comps in
            match get_associativity op with
            | `Left ->
                let l_term = BNFC.Reference (mk_name lhs) in
                let op_term = BNFC.LitReference (t_name op) in
                let r_term = BNFC.Reference (mk_name ~succ:true rhs) in
                let terms = [ l_term; op_term; r_term ] in
                BNFC.Decl { ast_name; name; terms }
          else if is_unary used_nterms prod then
            let mk_lit (sym, _, _) =
              match sym with
              | N n -> BNFC.Reference (mk_name n)
              | T t -> BNFC.LitReference (t_name t)
            in
            let terms =
              Array.map mk_lit (Production.rhs prod) |> Array.to_list
            in
            BNFC.Decl { ast_name; name; terms }
          else if fst_is_rec then
            let rhs = Production.rhs prod |> Array.to_list in
            let fst_term =
              match List.hd rhs with
              | N n, _, _ -> BNFC.Reference (mk_name n)
              | _ -> assert false
            in
            let rem_terms = List.map mk_simple_lit (List.tl rhs) in
            let terms = fst_term :: rem_terms in
            BNFC.Decl { ast_name; name; terms }
          else
            let terms =
              Array.map mk_simple_lit (Production.rhs prod) |> Array.to_list
            in
            BNFC.Decl { ast_name; name; terms }
        in
        List.map mk_decl level
      in
      let level_decls =
        let rec group_by_nterm level =
          match level with
          | [] -> []
          | prod :: _ ->
              let nterm = Production.lhs prod in
              let is_same p = Nonterminal.equal nterm (Production.lhs p) in
              let same_nterm, rest = List.partition is_same level in
              same_nterm :: group_by_nterm rest
        in
        let grouped_by_nterm = group_by_nterm level in
        List.map mk_prec_decls grouped_by_nterm |> List.concat
      in
      (* Add the new decls, increment the used nonterminal indexes and move to the next level *)
      let acc = acc @ level_decls @ fallback_states in
      let indexes =
        List.fold_left
          (fun acc nterm ->
            NonterminalMap.update nterm
              (function Some i -> Some (i + 1) | None -> assert false)
              acc)
          indexes used_nterms
      in
      (acc, indexes)
    in
    List.fold_left mk_decls ([], indexes) prec_levels |> fst

  let decls : BNFC.decl list =
    (* 3.1. Build precedence related productions *)
    (* First I build the ast names for each precedence production.
       This is done to minimize the change in ast names if a new precedence level is added *)
    let prec_prod_ast_names : string ProductionMap.t =
      NonterminalMap.fold
        (fun nterm prod_map acc ->
          let name = n_name nterm in
          let prods = ProductionMap.bindings prod_map |> List.split |> fst in
          let suffixes = mk_short_suffixes prods in
          let names = List.map (Printf.sprintf "%s_%s" name) suffixes in
          let ast_name_map = List.combine prods names in
          List.fold_left
            (fun acc (prod, name) -> ProductionMap.add prod name acc)
            acc ast_name_map)
        reduced_production_sets ProductionMap.empty
    in
    let prec_decls =
      mk_precedence_productions final_precedence_levels prec_prod_ast_names
    in

    (* 3.2. Build all other productions *)
    let decls : BNFC.decl list =
      NonterminalMap.filter
        (fun nterm _ -> not @@ NonterminalMap.mem nterm reduced_production_sets)
        production_to_terminals
      |> NonterminalMap.map (fun m ->
             ProductionMap.bindings m |> List.split |> fst |> mk_productions)
      |> NonterminalMap.bindings |> List.split |> snd |> List.concat
    in
    decls @ prec_decls
end
