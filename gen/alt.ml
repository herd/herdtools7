(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf
open Code


module type AltConfig = sig
  include DumpAll.Config
  val upto : bool
  val max_ins : int
  val mix : bool
  val max_relax : int
  val min_relax : int
  val choice : check
  val prefix : string list
  val variant : Variant_gen.t -> bool
  val varatom : string list
  type fence
  val cumul : fence list Config.cumul
  val wildcard : bool
end

module Filter
    (C : Builder.S)
    (O : sig
      val cumul : C.A.fence list Config.cumul
      val choice : check
    end) =
struct
  let dbg = false

  open C.E

    let is_cumul =
      let open Config in
      let equal_fence f1 f2 = C.A.compare_fence f1 f2 = 0 in
      match O.cumul with
      | Empty -> (fun _ -> false)
      | All -> (fun _ -> true)
      | Set fs ->
          (fun f -> List.exists (equal_fence f) fs)

    let choice_sc po_safe e1 e2 =
      let seq_sd e1 e2 =
        match Code.seq_sd e1 e2 with
        | None -> Warn.user_error "Unexpected UnspecLoc"
        | Some b -> b in
      let r = match e1.edge,e2.edge with
(*
  Now accept internal with internal composition
  when the do not match safe, explicit po candidates.
  A bit rude, maybe...

  Also notice that we are more tolerant for Rfi.
 *)
(* Assuming Dp is safe *)
    | (Rf Int|Po(Same,Dir W,Dir R)),Dp _
    | Dp _,(Rf Int|Po(Same,Dir W,Dir R)) -> true
    | Dp (_,sd,_),(Ws Int|Po(Same,Dir W,Dir W)|Fr Int|Po(Same,Dir R,Dir W)) ->
        not (po_safe sd (dir_src e1) (dir_tgt e2))
    | Po (sd1,_,_), Dp (_,sd2,_) ->
        not (po_safe sd1 (dir_src e1) (dir_tgt e1)) &&
        not (po_safe (seq_sd sd1 sd2) (dir_src e1) (dir_tgt e2))
    | Dp (_,sd1,_),Po (sd2,_,_) ->
        not (po_safe sd2 (dir_src e2) (dir_tgt e2)) &&
        not (po_safe (seq_sd sd1 sd2) (dir_src e1) (dir_tgt e2))
(* Check Po is safe *)
    | Po (sd1,_,_),Po (sd2,_,_) ->
        not (po_safe (seq_sd sd1 sd2) (dir_src e1) (dir_tgt e2))
    | Rf Int,Po (sd,_,_) ->
        po_safe sd (dir_src e2) (dir_tgt e2) &&
        not (po_safe sd (dir_src e1) (dir_tgt e2))
    | Po (sd,_,_),Rf Int ->
        po_safe sd (dir_src e1) (dir_tgt e1) &&
        not (po_safe sd (dir_src e1) (dir_tgt e2))
(* Allow Rmw *)
    | (Rmw _,_)|(_,Rmw _) -> true
(* Added *)
    | _,_ ->
        match get_ie e1, get_ie e2 with
        | Int,Int -> false
        | Ext,_|_,Ext -> true
        | UnspecCom,_ | _,UnspecCom -> assert false in
      if dbg then
        eprintf "Choice: %s %s -> %b\n%!" (C.E.pp_edge e1) (C.E.pp_edge e2) r ;
      r

    let choice_default e1 e2 =
      let r = match e1.edge,e2.edge with
(*
  Now accept some internal with internal composition
 *)
      | (Ws Int|Po(Same,Dir W,Dir W)
        |Rf Int|Po(Same,Dir W,Dir R)
        |Fr Int|Po(Same,Dir R,Dir W)|Insert _),(Dp (_,_,_)|Po (Diff,_,_))
      | (Dp (_,_,_)|Po (Diff,_,_)),
        (Ws Int|Po(Same,Dir W,Dir W)
        |Rf Int|Po(Same,Dir W,Dir R)
        |Fr Int|Po(Same,Dir R,Dir W)|Insert _)
      | Dp (_,Diff,_),Po (Diff,_,_)
      | Po (Diff,_,_),Dp (_,Diff,_)
      | (Rf Int|Po(Same,Dir W,Dir R)),Po (Same,_,_)
      | Po (Same,_,_),(Rf Int|Po(Same,Dir W,Dir R))
      | (Rmw _,_)|(_,Rmw _) -> true
      | _,_ ->
          (* Reject other internal followed by internal sequences *)
          match get_ie e1, get_ie e2 with
          | Int,Int -> false
          | Ext,_|_,Ext -> true
          | UnspecCom,_ | _,UnspecCom -> assert false in
      if dbg then
        eprintf "Choice: %s %s -> %b\n%!" (C.E.pp_edge e1) (C.E.pp_edge e2) r ;
      r

(* Check altenance of com/po *)
    let choice_critical e1 e2 =
      let r =
        match e1.edge,e2.edge with
(* Two cases of allowed com composition *)
        | (Ws _|Leave CWs|Back CWs|Fr _|Leave CFr|Back CFr),
          (Rf _|Leave CRf|Back CRf) -> true
(* Rmw allowed to compose arbitrarily *)
        | (Rmw _,_)|(_,Rmw _) -> true
(* Otherwise require alternance *)
        | _,_ ->  C.E.get_ie e1 <> C.E.get_ie e2 in
(*      eprintf "Choice: %s %s -> %b\n" (C.E.pp_edge e1) (C.E.pp_edge e2) r ; *)
      r
    let choice_mixed e1 e2 =
      let r =
        match e1.edge,e2.edge with
(* Two cases of allowed com composition *)
        | (Ws _|Leave CWs|Back CWs|Fr _|Leave CFr|Back CFr),
          (Rf _|Leave CRf|Back CRf) -> true
(* Rmw allowed to compose arbitrarily *)
        | (Rmw _,_)|(_,Rmw _) -> true
(* Otherwise accept composition *)
        | _,_ ->
            let ie1 = C.E.get_ie e1 and ie2 =  C.E.get_ie e2 in
            match ie1,ie2 with
            | Int,Int ->
                begin match loc_sd e1,loc_sd e2 with
                | (Same,Same) | (Diff,Same) | (Same,Diff)
                  -> true
                | Diff,Diff -> false
                | _ -> assert false
                end
            | Ext,Ext -> false
            | (Ext,Int) | (Int,Ext) -> true
            | UnspecCom,_ | _,UnspecCom -> assert false in
(*      eprintf "Choice: %s %s -> %b\n" (C.E.pp_edge e1) (C.E.pp_edge e2) r ; *)
      r
    let choice_uni e1 e2 =  match e1.edge,e2.edge with
    | (Ws _,Ws _)
    | (Fr _,Ws _)
    | (Rf _,Fr _)
    | (Rf _,Hat)
    | (Hat,Fr _)
      -> C.E.get_ie e1 <> C.E.get_ie e2 (* Allow alternance *)
    | Po _,Po _ -> false
    | _,_ -> true

    let choice_id _ _ = true

    let choice_free e1 e2 = match e1.edge,e2.edge with
    | (Ws _,Ws _)
    | (Fr _,Ws _)
    | (Rf _,Fr _)
      -> false
    | _,_ -> true

    let choice_free_alt e1 e2 = match e1.edge,e2.edge with
    | (Ws _,Ws _)
    | (Fr _,Ws _)
    | (Rf _,Fr _)
      -> C.E.get_ie e1 <> C.E.get_ie e2 (* Allow alternance *)
    | _,_ -> true

    let choice_ppo e1 e2 =
      choice_free e1 e2 &&
      C.E.compare e1 e2 <> 0 &&
      (match e1.edge with
      | Dp (dp,_,Dir R) when C.A.is_ctrlr dp -> is_ext e2
      | _ -> true)

    let choice_transitive safes xs ys e1 e2 =
      choice_free_alt e1 e2 &&
      begin match  C.E.get_ie e1, C.E.get_ie e2 with
      | Int,Int ->
          let cs = C.E.compact_sequence xs ys e1 e2 in
          let r =
            not
              (List.exists
                 (fun es -> C.R.Set.mem es safes)
                 cs) in
          r
      | _,_ -> true
      end

    let choose c =
    let iarg f = fun _ _ _ _ -> f in
    match c with
    | Sc -> fun _safes po_safe _xs _ys -> choice_sc po_safe
    | Default -> iarg choice_default
    | MixedCheck -> iarg choice_mixed
    | Critical -> iarg choice_critical
    | Uni -> iarg choice_uni
    | Thin |Total -> iarg choice_id
    | Free -> iarg choice_free_alt
    | Ppo -> iarg choice_ppo
    | Transitive ->
        (fun safes _po_safe -> choice_transitive safes)


    let compat_id ao d = match ao,d with
    | (None,_)|(_,(Irr|NoDir)) -> true
    | Some a,(Dir d) -> C.A.applies_atom a d

    let rec hd_non_insert = function
      | [] -> assert false
      | [x] -> x
      | x::xs ->
          if C.E.is_insert_store x.C.E.edge then hd_non_insert xs
          else x
    let last_non_insert xs = hd_non_insert (List.rev xs)

    (* Check whether relaxation list `xs` can precede relaxation list `ys`.
       This uses the effective boundary edges of the two sequences,
       ignoring insert/store pseudo-edges when necessary, and checks:
       - whether the boundary edges are compatible via `Edge.can_precede`
       - whether the mode-specific rule holds *)
    let can_precede safes po_safe xs ys =
      let e1 = last_non_insert xs in
      let e2 = hd_non_insert ys in
      C.E.can_precede e1 e2
      && match e1.edge,e2.edge with
(*
  First reject some of hb' ; hb'
 *)
    | Hat,Hat   (* Hat *)
(* Ext Ext Only? *)
    | Ws _,Ws _ (* -> Ws *)
    | Fr _,Ws _ (* -> Fr*)
    | Rf _,Fr _ (* -> Ws *)
(*    Rf _,Fr _ (* -> Ws *) May be interesting, because
      values are observed by outcome itself,
      also useful to add Fre after B-cumulativity *)
      ->  C.E.get_ie e1 <> C.E.get_ie e2 (* Allow alternance *)
    | Id,Id -> false
    | Id,_ -> compat_id e1.a2 (dir_src e2)
    | _,Id -> compat_id e2.a1 (dir_tgt e1)
(* Fence cumulativity *)
    | Rf _,Fenced (f,_,_,_)
    | Fenced (f,_,_,_),Rf _ ->
        is_cumul f && choose O.choice safes po_safe xs ys e1 e2
    | _,_ -> choose O.choice safes po_safe xs ys e1 e2
end

module Make(C:Builder.S)
    (O:AltConfig with type fence = C.A.fence) :
    sig
      type predicate_relax

      val plain : predicate_relax -> C.R.relax
      val lift : C.R.relax -> predicate_relax
      val gen : ?relax:predicate_relax list -> ?safe:predicate_relax list -> ?reject:predicate_relax list -> int -> unit
      val parse_argument : string -> predicate_relax list
      val parse_input :
        relax:string list -> safe:string list ->
        reject:string list -> predicate_relax list * predicate_relax list * predicate_relax list
      val remove_invalid_relaxes : predicate_relax list -> predicate_relax list
      val pp_predicate_relax_list : predicate_relax list -> string
      val filter_check: safe:predicate_relax list -> predicate_relax -> predicate_relax -> bool
    end

    =
  struct
    module D = DumpAll.Make(O) (C)
    module FilterImpl = Filter(C)(O)
    module RelaxSet = C.R.Set
    open C.E
    open C.R

    type edge_predicate =
      | Before
      | After

    type predicate_edge = {
      plain : C.E.edge;
      pred : edge_predicate option;
    }

    type predicate_relax = predicate_edge list

    let parse_predicate = function
      | "before" -> Before
      | "after" -> After
      | s -> Warn.user_error "predicate %s is not supported." s

    let predicate_edge plain = { plain; pred = None }

    let plain relax = List.map (fun e -> e.plain) relax
    let with_plain relax = plain relax, relax

    let dbg = false

    let is_int e = match get_ie e with
    | Int -> true
    | Ext -> false
    | UnspecCom -> assert false

    let can_precede safes po_safe (_,xs) k = match k with
    | [] -> true
    | (_,ys)::_ ->
        FilterImpl.can_precede safes po_safe (plain xs) (plain ys)

    let can_precede_plain safes po_safe (_,xs) k = match k with
    | [] -> true
    | (_,ys)::_ -> FilterImpl.can_precede safes po_safe xs ys

    (* List.is_empty only supports for ocaml 5.1 afterwards *)
    let is_empty_list l = (l = [])

    let parse_argument_ast input =
      String.trim input |> C.R.parse_ast Parser.diy7

    let varatom_ess =
      if C.A.bellatom then Misc.identity
      else match O.varatom with
      | [] -> Misc.identity
      | ["all"] ->
          let module Fold = struct
            type atom = C.E.atom
            let fold = C.E.fold_atomo
          end in
          let module V = VarAtomic.Make(C.E)(Fold) in
          V.varatom_es
      | atoms ->
          let atoms = C.E.parse_atoms atoms in
          let module Fold = struct
            type atom = C.E.atom
            let fold f k = C.E.fold_atomo_list atoms f k
          end in
          let module V = VarAtomic.Make(C.E)(Fold) in
          V.varatom_es

    let reattach_predicates template edges =
      try
        List.map2 (fun predicate_edge edge -> { predicate_edge with plain = edge }) template edges
      with Invalid_argument _ ->
        Warn.fatal "predicate expansion changed relaxation length"

    let varatom_predicate_ess predicate_relaxes =
      List.concat_map
        (fun predicate_relax ->
          let edges = plain predicate_relax in
          varatom_ess [edges]
          |> List.map (reattach_predicates predicate_relax))
        predicate_relaxes

    let compare_predicate lhs rhs = match lhs,rhs with
    | Before,Before
    | After,After -> 0
    | Before,After -> -1
    | After,Before -> 1

    let compare_predicate_opt = Option.compare compare_predicate

    let compare_predicate_edge lhs rhs =
      match C.E.compare lhs.plain rhs.plain with
      | 0 -> compare_predicate_opt lhs.pred rhs.pred
      | r -> r

    let compare_predicate_relax = List.compare compare_predicate_edge

    module PredicateRelaxSet =
      MySet.Make
        (struct
          type t = predicate_relax
          let compare = compare_predicate_relax
        end)

    let remove_invalid_relaxes relaxes =
      let valid_relaxes =
        List.map plain relaxes
        |> C.R.remove_invalid_relaxes
        |> C.R.Set.of_list in
      (* Predicate-only edges are only meaningful at relaxation boundaries:
         `before(...)` predicates must form a leading prefix, and `after(...)`
         predicates must form a trailing suffix. Once a plain edge appears,
         no later `before(...)` is valid; once an `after(...)` appears, only
         more `after(...)` predicates may follow. *)
      let leading_before_trailing_after_predicate list =
        let valid,_,_ =
          List.fold_left
            (fun (valid, leading_before, trailing_after) edge ->
              match valid, leading_before, trailing_after, edge.pred with
              | false, _, _, _ -> false, leading_before, trailing_after
              | true, true, _, pred ->
                  true, pred = Some Before, pred = Some After
              | true, false, trailing_after, Some Before ->
                  false, false, trailing_after
              | true, false, false, pred ->
                  true, false, pred = Some After
              | true, false, true, pred ->
                  pred = Some After, false, true)
            (true,true,false) list in
        valid in
      let has_plain_edge =
        List.exists (fun edge -> edge.pred = None) in
      List.filter
        (fun relax ->
          has_plain_edge relax
          && C.R.Set.mem (plain relax) valid_relaxes
          && leading_before_trailing_after_predicate relax)
        relaxes

    let parse_argument_ast_expanded ast =
      let add_predicate pred edge =
        { edge with pred = Some (parse_predicate pred) } in
      ast
      |> C.R.parse_expand_relaxs_ast ~ppo:C.ppo
      |> fun ast -> Ast.bind ast (fun edge -> Ast.One (predicate_edge edge))
      |> Ast.expand add_predicate
      |> varatom_predicate_ess

    let parse_argument input_argument =
      parse_argument_ast input_argument
      |> parse_argument_ast_expanded

    let parse_arguments input_argument_list =
      List.map parse_argument input_argument_list
      |> List.flatten
      |> remove_invalid_relaxes
      |> List.sort_uniq compare_predicate_relax

    let pp_ess ess =
      let list_sep = " " in
      let list_list_sep = " " in
      ess |> List.map
        ( fun (_,es) ->
          es |> plain |> List.map (fun e -> pp_edge e)
             |> String.concat list_list_sep )
        |> String.concat list_sep

    let lift r = List.map predicate_edge r
    let lift_list rs = List.map lift rs

    (* Check whether `list` starts with `expected`, using `pred` for element
       comparison. *)
    let rec starts_with pred list expected =
      match list, expected with
      | _, [] -> true
      | [], _::_  -> false
      | hd :: tail, hd_expected :: tail_expected ->
          pred hd hd_expected
          && starts_with pred tail tail_expected

    (* Check whether `list` ends with `expected`, using `pred` for element
       comparison. *)
    let ends_with pred list expected =
      starts_with pred (List.rev list) (List.rev expected)

    (* Given `next = [....; after(..); after(..)]` and
       `exist = [before(..); before(..); ....]`, check whether the optional
       boundary predicates can be merged with the neighbouring concrete edge:
         - `before` merges with concrete if edge matches.
         - `after` merges with concrete if edge matches.
         - `before` pairing with `after` fails. *)
    let merge_predicate next exist =
      (* Separate the trailing `after` predicate from `next` *)
      let after =
        List.fold_right ( fun e (after,seen_non_after) ->
          match seen_non_after, e.pred = Some After with
          | true, _ | _, false -> after, true
          | false, true -> e :: after, false ) next ([],false)
        |> fst in
      (* Separate the beginning `before` predicate from `exist` *)
      let before =
        List.fold_left ( fun (before,seen_non_before) e ->
          match seen_non_before, e.pred = Some Before with
          | true, _ | _, false -> before, true
          | false, true -> e :: before, false ) ([],false) exist
        |> fst |> List.rev in
      (* Match `after` or `before` predicates when present. *)
      match after, before with
      | [], [] -> true
      | after, [] ->
          starts_with
            (fun (lhs:predicate_edge) (rhs:predicate_edge) ->
              C.E.equal_edge_atoms lhs.plain rhs.plain)
            exist after
      | [], before ->
          ends_with
            (fun (lhs:predicate_edge) (rhs:predicate_edge) ->
              C.E.equal_edge_atoms lhs.plain rhs.plain)
            next before
      (* Reject an `after` predicate directly meeting a `before` predicate. *)
      | _, _ -> false

    let needs_merge next_edges exist_edges =
      match List.rev next_edges, exist_edges with
      | next_last::_, exist_first::_ ->
          next_last.pred = Some After ||
          exist_first.pred = Some Before
      | _ -> false

    (* Check whether `next_edges` may be placed immediately before
       `exist_edges`: if `next_edges` contains trailing `after` predicates
       or `exist_edges` contains leading `before` predicates,
       match those predicates against the neighbouring concrete edges;
       otherwise use the ordinary `can_precede` relation. *)
    let check_precede can_precede next_edges exist_edges =
      if O.verbose > 2 then
        eprintf "next: %s, exists: %s\n"
          (C.E.pp_edges (plain next_edges))
          (C.E.pp_edges (plain exist_edges));
      if needs_merge next_edges exist_edges then
        merge_predicate next_edges exist_edges
      else
        can_precede (plain next_edges) (plain exist_edges)

    (* Check whether a candidate relaxation `next` can be prepended to the
       current suffix `exist`. The suffix is stored newest-first, so only
       its head must be checked against `next`; an empty suffix accepts any
       first relaxation. *)
    let precede_relax_edge_list safes po_safe next exist =
      match exist with
      | [] -> true
      | (_,exist_edges) :: _ ->
          check_precede
            (FilterImpl.can_precede safes po_safe)
            (snd next) exist_edges

(* Functional for recursive call of generators *)

    let sz (_,es) =
      if List.for_all (fun e -> is_id e.plain.edge) es then 0 else 1

    let c_minprocs_es c =
      List.fold_left ( fun c e ->
        match e.pred,e.plain.C.E.edge,get_ie e.plain with
        | Some _,_,_
        | None,(Back _|Leave _),_
        | None,_,Int -> c
        | None,_,Ext -> c + 1
        | None,_,UnspecCom -> assert false
      ) c

    let c_minprocs_suff c =
      List.fold_left ( fun c (_,es) -> c_minprocs_es c es) c

    let minprocs suff =
      let r = c_minprocs_suff 0 suff in
      if O.verbose > 3 then eprintf "MIN [%s] => %i\n" (pp_ess suff) r ;
      r

    let rec c_minint_es c = function
      | [] -> false,c
      | {pred=Some _; _}::es
      | {plain={edge=Id; _}; _}::es ->  c_minint_es c es
      | e::es ->
          match get_ie e.plain with
          | Ext -> true,c
          | Int -> c_minint_es (c+1) es
          | UnspecCom -> assert false

    let rec c_minint c = function
      | [] -> c
      | (_,es)::suff ->
          let stop,c = c_minint_es c es in
          if stop then c
          else c_minint c suff

    let minint suff = c_minint 0 suff

(* Prefix *)
    let parse_prefixes prefix =
      (* Parse each `-prefix` argument separately, then combine them as one
         top-level choice. Thus `-prefix A -prefix B` is interpreted as
         `-prefix [A|B]`. *)
      let prefixes =
        List.map parse_argument_ast prefix
        |> fun prefixes -> Ast.Choice prefixes
        |> parse_argument_ast_expanded
        |> List.map (fun predicate_relax -> [plain predicate_relax, predicate_relax]) in
      match prefixes with
      | [] -> [[]] (* No prefix <=> one empty prefix *)
      | prefixes -> prefixes

    let () =
      if O.verbose > 0 && O.prefix <> [] then begin
        eprintf "Prefixes:\n" ;
        List.iter
          (fun rs ->
            eprintf "  %s\n" (C.R.pp_relax_list (List.map fst rs)))
          (parse_prefixes O.prefix)
      end

    let prefixes = parse_prefixes O.prefix

    let rec mk_can_prefix = function
      | [] -> (fun _ _ -> true)
      | [x] -> (fun p -> p x)
      | _::xs -> mk_can_prefix xs

    let can_prefix prefix = mk_can_prefix prefix

    let rec is_prefix l rl =
      match rl,l with
      | hrl::trl, hl::tl -> if hl = hrl then  is_prefix tl trl else false
      | [], _ -> true (* end of rl before or at the end of l *)
      | _, [] -> false (* end of l before end of rl*)


    let check_cycle rsuff rl =
      let rsuff = List.split rsuff |> snd |> List.concat |> plain in
      not (List.exists (fun rl -> is_prefix rsuff rl) rl)


    (* This function is used `zyva` *)
    let call_rec_base prefix f0 safes po_safe over n r suff f_rec k ?(reject=[])=
      if
        precede_relax_edge_list safes po_safe r suff &&
        minprocs suff <= O.nprocs &&
        minint (r::suff) <= O.max_ins-1 &&
        check_cycle (r::suff) reject
      then
        let suff = r::suff
        and n = n-sz r in
        if O.verbose > 2 then eprintf "CALL: %i %s\n%!" n (pp_ess suff) ;
        let k =
          if
            precede_relax_edge_list safes po_safe (Misc.last suff) suff &&
            over &&
            (n = 0 || (n > 0 && O.upto)) &&
            can_prefix prefix (can_precede safes po_safe) suff
          then begin
            (* Find an actual candidate cycle and add `prefix`. Predicate
               edges have been resolved at this point, so remove them before
               calling `test_generator`. *)
            let tr =
              List.map ( fun (relax,edges) ->
                let edges =
                  List.filter (fun edge -> edge.pred = None) edges
                  |> plain in
                (relax,edges)
              ) (prefix@suff) in
            if O.verbose > 2 then
            eprintf "TRY: '%s'\n"
              (C.E.pp_edges (List.flatten (List.map snd tr))) ;
            try f0 po_safe tr k
            with  Misc.Exit -> k
            | Misc.Fatal msg |Misc.UserError msg ->
                eprintf "Marche pas: '%s'\n" msg ;
                k
            | e ->
              eprintf "Exc in F0: '%s'\n" (Printexc.to_string e) ;
              raise e
          end else k in
        if n <= 0 then k
        else f_rec n suff k
      else k
    (* END of call_rec_base *)

    module SdDir2Set =
      MySet.Make
        (struct
          type t = sd * extr * extr
          let compare = Misc.polymorphic_compare
        end)

    let extract_po rs =
      match O.choice with
      | Sc ->
          let d2 =
            List.fold_right
              (fun (r,_) k -> match r with
              | [{edge=Po (sd,e1,e2); _}] -> SdDir2Set.add (sd,e1,e2) k
              | _ -> k)
              rs SdDir2Set.empty in
          if dbg then
            eprintf
              "PoSafe: {%s}\n"
              (SdDir2Set.pp_str ","
                 (fun (sd,e1,e2) -> pp_sd sd ^ "-" ^ pp_extr e1 ^ "-" ^ pp_extr e2)
                 d2) ;
          fun sd e1 e2 -> SdDir2Set.mem (sd,e1,e2) d2
      | m ->
          fun _ _ _ ->
            eprintf "Function po_safe called in mode %s\n%!"
              (pp_check m) ;
            assert false

    let zyva prefix aset relax safe reject n f =
(*      let safes = C.R.Set.of_list safe in *)
      let po_safe = extract_po safe in

      (* ********************************** *)
      (* iterates over all relax edges `rs` *)
      (* ********************************** *)
      let choose_relax rs k =
      List.fold_left (fun k relex_edge ->
        (* Build simple cycles for relaxation `relex_edge` *)
        (* Partially apply function `call_rec_base` *)
        let call_rec_add_safe = call_rec_base prefix (f [fst relex_edge]) aset po_safe ~reject:reject in
        (* Add safe edge to suffix *)
        let rec add_safe over ss n suf k =
          List.fold_left ( fun k s -> call_rec_add_safe over n s suf (add_relaxs over) k ) k ss
        (* Add some relax edges `relex_edge` to suffix, or nothing *)
        and add_relaxs over n suf k =
          let k = call_rec_add_safe true n relex_edge suf (add_relaxs true) k in
          add_safe over safe n suf k in

        (* Decide what is the accumulator `k` for the next iteration
           based on if `prefix` is empty *)
        if is_empty_list prefix then
          (* Optimise: start with a relax edge `relex_edge` *)
            call_rec_add_safe true n relex_edge [] (add_relaxs true) k
        else
            add_relaxs false n [] k
      ) k rs in

      (* ******************************************* *)
      (* Alternative: mix relaxation from relax list *)
      (* ******************************************* *)
      let all_relax k =
        let relax_set = RelaxSet.of_list (List.map fst relax) in
        let extract_relaxs suff =
          let suff_set = RelaxSet.of_list (List.map fst suff)  in
          RelaxSet.elements (RelaxSet.inter suff_set relax_set) in

        (* Partially apply function `call_rec_base` *)
        let call_rec_all_relax =
          call_rec_base prefix
            (fun po_safe suff k ->
              let rs = extract_relaxs suff in
              let nrs = List.length rs in
              if nrs > O.max_relax || nrs < O.min_relax then k
              else f rs po_safe suff k)
            aset po_safe ~reject:reject in

        (* Add a one edge to suffix *)
        let rec add_one over rs ss n suf k =
          (* Consume `rs` first *)
          let new_k = List.fold_left ( fun k r ->
            call_rec_all_relax true n r suf (add_one true relax safe) k
          ) k rs in
          (* Then consume `ss` when `rs` is empty *)
          List.fold_left ( fun k s ->
            call_rec_all_relax over n s suf (add_one over relax safe) k
          ) new_k ss in

        (* Force first edge to be a relaxed one *)
        let add_first rs k =
          List.fold_left ( fun k r ->
            call_rec_all_relax true n r [] (add_one true relax safe) k
          ) k rs in

        (* Function `all_relax` entry point depends on
           if `prefix` is empty. *)
        if is_empty_list prefix then add_first relax k
        else add_one false relax safe n [] k in

     (* New relax that does not enforce the first edge to be a relax *)

      (* ***************************************************** *)
      (* As a safety check, generate cycles with no relaxation *)
      (* ***************************************************** *)
      let rec no_relax ss n suf k =
        (* Partially apply function `call_rec_base` *)
        let call_rec_no_relax = call_rec_base prefix (f []) aset po_safe ~reject:reject in
        List.fold_left (fun k s ->
          call_rec_no_relax true n s suf (no_relax safe) k
        ) k ss in

      (* *************************************************** *)
      (* Function `zyva` starts after all the `let`-bindings *)
      (* *************************************************** *)
      fun k ->
        if is_empty_list relax then no_relax safe n [] k
        else if O.mix && O.max_relax < 1 then k (* Let us stay logical *)
        else if O.mix && O.max_relax > 1 then all_relax k
        else choose_relax relax k
      (* END of overall `zyva` *)

    let all_int l = List.for_all is_int l

    let count_e ce =
      List.fold_left ( fun ce e -> if is_int e then ce else ce + 1 ) ce


    let count_ext es = count_e 0 es

    let change_loc e = Code.is_diff_loc @@ loc_sd e

    let count_p p =
      List.fold_left ( fun acc x -> if p x then acc + 1 else acc ) 0

    let count_changes = count_p change_loc

    let build_safe r0 es =
      let rs =
        List.fold_right (fun (r,_) -> RelaxSet.add r) es RelaxSet.empty in
      let rs = RelaxSet.diff rs (RelaxSet.of_list r0) in
      RelaxSet.elements rs

    exception Result of bool

(* Is xs a prefix of s@p ? *)

    let prefix_spanp xs (p,s) =
      let rec is_prefix xs ys = match xs,ys with
        | [],_ -> raise (Result true)
        | _::_,[] -> xs (* xs -> what is still to be matched *)
        | x::xs,y::ys ->
           if C.E.compare x y = 0 then is_prefix xs ys
           else raise (Result false) in
      try
        let xs = is_prefix xs s in
        match is_prefix xs p with
        | [] -> true (* xs and s@p are equal! *)
        |  _::_ -> false (* xs larger.. *)
      with Result b -> b

    let substring_spanp rej pss =
      List.exists
        (fun xs ->
          List.exists
            (fun ps -> prefix_spanp xs ps)
            pss)
      rej

    let last_check_call rej aset f rs po_safe res k =
      if is_empty_list res then k else
          let lst = Misc.last res in
          if can_precede_plain aset po_safe lst res then
            let es = List.map snd res in
            let le = List.flatten es in
            try
              if
                (match O.choice with
                | Default| Sc | Ppo | MixedCheck -> true
                | Thin | Free | Uni | Critical | Transitive |Total -> false) &&
                (count_ext le=1 || all_int le || count_changes le < 2) then k
              else begin
                  let ok = (* Check for rejected sequenes that span over cycle "cut" *)
                  let rej = (* Keep non-trivial edge sequences only *)
                    List.filter
                      (function
                       | []|[_] -> false
                       | _::_::_ -> true)
                      rej  in
                  match rej with
                  | [] -> true
                  | _::_ ->
                     let max_sz =
                       List.fold_left (fun  k xs -> max k (List.length xs)) 0 rej in
                     let pss = Misc.cuts max_sz le in
                     not (substring_spanp rej pss) in
                if ok then
                  let mk_info _es =
                    let ss = build_safe rs res in
                    let info =
                      [
                        "Relax",pp_relax_list rs;
                        "Safe", pp_relax_list ss;
                      ] in
                    info,C.R.Set.of_list rs in
                  f le mk_info D.no_name D.no_scope k
                else k
              end
            with (Normaliser.CannotNormalise _) -> k
          else k

    let rec prefixp xs ys =
      match xs,ys with
      | [],_ -> true
      | _::_,[] -> raise Exit
      | x::xs,y::ys ->
         C.E.compare x y = 0 && prefixp xs ys

    let rec sublistp xs ys = match ys with
      | [] -> false
      | _::rem ->
         prefixp xs ys || sublistp xs rem

    let substringp xs ys =
      try sublistp xs ys
      with Exit ->
            match xs with
            | []|[_] -> false
            | _::_::_ ->
               let pss = Misc.cuts (List.length xs) ys in
               List.exists
                 (fun ps -> prefix_spanp xs ps)
                 pss

    let last_minute rej ess =
      not (List.exists (fun es -> List.length es > O.max_ins) ess)
      && begin
          match rej with
          | _::_ ->
             let es = List.flatten ess  in
             not (List.exists (fun xs -> substringp xs es) rej)
          | [] -> true
        end

    (* Note that we use `edge` here to refer a single edge or a compositional edges.
       e.g. PosRR or [PosRR Fre].
       - `zyva` is the key function that append a new edge in the cycle,
         and try to generate a new litmus test.
       - `pref` and `prefixes` the prefix of a cycle. It is often empty.
       - `aset` is all the possible edges that are used to generate cycle
          which is the union of `relax` and `safe`
       - `relax` is all the relax edges, namely, if such parameter is none empty
         the cycle must contains at least one edges from this parameter
       - `safe` is all the safe edges that are used to build a cycle.
       - `reject` is the edges that are supposed to be rejected.
       - `n` the counter for how many edges in the cycle.
       - `f` the function that checks the validity of the cycle
       - `k` is the accumulator. *)
    let zyva_prefix prefixes aset relax safe reject n f k =
      List.fold_left ( fun k pref -> zyva pref aset relax safe reject n f k ) k prefixes

    let do_gen relax safe rej n =
      let predicate_aset =
        PredicateRelaxSet.union (PredicateRelaxSet.of_list (List.map snd safe))
          (PredicateRelaxSet.of_list (List.map snd relax)) in
      let aset =
        PredicateRelaxSet.fold
          (fun pred -> C.R.Set.add (plain pred))
          predicate_aset C.R.Set.empty in
      let rej = List.map (fun (_,predicate_relax) -> plain predicate_relax) rej in
      D.all
        ~check:(last_minute rej)
        (fun f ->
          zyva_prefix prefixes aset relax safe rej n
            (last_check_call rej aset f))

    let debug_predicate_rs chan rs =
      List.iter (fun r -> fprintf chan "%s\n" (pp_relax (plain r))) rs

    let parse_input ~relax ~safe ~reject =
      let r_nempty = Misc.consp relax in
      let relax = parse_arguments relax
      and safe = parse_arguments safe
      and reject = parse_arguments reject in
      if Misc.nilp relax && r_nempty then
        Warn.fatal "relaxations provided in relaxlist could not be used to generate cycles" ;
      if O.verbose > 0 then begin
        eprintf "** Relax0 **\n" ;
        debug_predicate_rs stderr relax ;
        eprintf "** Safe0 **\n" ;
        debug_predicate_rs stderr safe
      end ;
      let relax_set = PredicateRelaxSet.of_list relax
      and safe_set = PredicateRelaxSet.of_list safe
      and reject_set = PredicateRelaxSet.of_list reject in
      let safe = PredicateRelaxSet.elements (PredicateRelaxSet.diff safe_set relax_set)
      and reject = PredicateRelaxSet.elements reject_set in
      if O.verbose > 0 then begin
        eprintf "** Relax **\n" ;
        debug_predicate_rs stderr relax ;
        eprintf "** Safe **\n" ;
        debug_predicate_rs stderr safe
      end ;
      relax, safe, reject

    let secret_gen relax safe reject n =
      do_gen (List.map with_plain relax) (List.map with_plain safe)
        (List.map with_plain reject) n

(**********************)
(* Default edge lists *)
(**********************)

    let fold_ie f k = f (Int) (f (Ext) k)
    let fold_dir f k = f Irr k (* expand later ! *)
    let fold_dir2 f = fold_dir (fun i1 k -> fold_dir (f i1) k)
    let fold_sd = Code.fold_sd O.wildcard
    let fold_sd_dir2 f =
      fold_sd
        (fun sd -> fold_dir2 (fun d1 d2 -> f sd d1 d2))
    let fold_all_fences f =
      fold_sd_dir2 (fun sd d1 d2 -> C.A.fold_all_fences (fun fe -> f fe sd d1 d2))
    let fold_cumul_fences f =
      fold_sd_dir2 (fun sd d1 d2 -> C.A.fold_cumul_fences (fun fe -> f fe sd d1 d2))
    let fold_cum f =  fold_cumul_fences f

    let er e = [plain_edge e]
    let safe =
      let k = [] in
      let k = fold_ie (fun ie k -> er (Ws ie)::er (Fr ie)::k) k in k

    let relax =
      let k = [] in
      let k = fold_dir2 (fun d1 d2 k -> er (Po (Diff, d1, d2))::k) k in
      let k = er (Po (Same, Dir R, Dir R))::k in
      let k = fold_all_fences (fun fe sd d1 d2 k -> er (Fenced (fe,sd,d1,d2))::k) k in
      let k =
        C.A.fold_dpr
          (fun dp k ->
            fold_sd (fun sd k -> er (Dp(dp,sd,Dir R))::k) k) k in
      let k =
        C.A.fold_dpw
          (fun dp k ->
            fold_sd (fun sd k -> er (Dp(dp,sd,Dir W))::k) k) k in
      let k = fold_ie (fun ie k -> er (Rf ie)::k) k in
      let k = fold_cum (fun fe sd d1 d2 k -> ac_fence fe sd d1 d2::k) k in
      let k = fold_cum (fun fe sd d1 d2 k -> bc_fence fe sd d1 d2::k) k in
      let k = er (Hat)::k in
      k

    let gen ?(relax=lift_list relax) ?(safe=lift_list safe) ?(reject=[]) n =
      try secret_gen relax safe reject n
      with e ->
        eprintf "Exc: '%s'\n" (Printexc.to_string e) ;
        raise e

    let pp_predicate_relax_list relaxes =
      List.map plain relaxes |> C.R.pp_relax_list

    let filter_check ~safe lhs rhs =
      let safe = List.map with_plain safe in
      let safe_set = C.R.Set.of_list (List.map fst safe) in
      let po_safe = extract_po safe in
      FilterImpl.can_precede safe_set po_safe (plain lhs) (plain rhs)
  end
