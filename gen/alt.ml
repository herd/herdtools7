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
    | (Communication (Rf,Int)|Po(Same,Dir W,Dir R)),Dp _
    | Dp _,(Communication (Rf,Int)|Po(Same,Dir W,Dir R)) -> true
    | Dp (_,sd,_),(Communication (Co,Int)|Po(Same,Dir W,Dir W)|Communication (Fr,Int)|Po(Same,Dir R,Dir W)) ->
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
    | Communication (Rf,Int),Po (sd,_,_) ->
        po_safe sd (dir_src e2) (dir_tgt e2) &&
        not (po_safe sd (dir_src e1) (dir_tgt e2))
    | Po (sd,_,_),Communication (Rf,Int) ->
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
      | (Communication (Co,Int)|Po(Same,Dir W,Dir W)
        |Communication (Rf,Int)|Po(Same,Dir W,Dir R)
        |Communication (Fr,Int)|Po(Same,Dir R,Dir W)|Insert _),(Dp (_,_,_)|Po (Diff,_,_))
      | (Dp (_,_,_)|Po (Diff,_,_)),
        (Communication (Co,Int)|Po(Same,Dir W,Dir W)
        |Communication (Rf,Int)|Po(Same,Dir W,Dir R)
        |Communication (Fr,Int)|Po(Same,Dir R,Dir W)|Insert _)
      | Dp (_,Diff,_),Po (Diff,_,_)
      | Po (Diff,_,_),Dp (_,Diff,_)
      | (Communication (Rf,Int)|Po(Same,Dir W,Dir R)),Po (Same,_,_)
      | Po (Same,_,_),(Communication (Rf,Int)|Po(Same,Dir W,Dir R))
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
        | (Communication (Co,_)|Leave Co|Back Co|Communication (Fr,_)|Leave Fr|Back Fr),
          (Communication (Rf,_)|Leave Rf|Back Rf) -> true
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
        | (Communication (Co,_)|Leave Co|Back Co|Communication (Fr,_)|Leave Fr|Back Fr),
          (Communication (Rf,_)|Leave Rf|Back Rf) -> true
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
    | (Communication (Co,_),Communication (Co,_))
    | (Communication (Fr,_),Communication (Co,_))
    | (Communication (Rf,_),Communication (Fr,_))
    | (Communication (Rf,_),Hat)
    | (Hat,Communication (Fr,_))
      -> C.E.get_ie e1 <> C.E.get_ie e2 (* Allow alternance *)
    | Po _,Po _ -> false
    | _,_ -> true

    let choice_id _ _ = true

    let choice_free e1 e2 = match e1.edge,e2.edge with
    | (Communication (Co,_),Communication (Co,_))
    | (Communication (Fr,_),Communication (Co,_))
    | (Communication (Rf,_),Communication (Fr,_))
      -> false
    | _,_ -> true

    let choice_free_alt e1 e2 = match e1.edge,e2.edge with
    | (Communication (Co,_),Communication (Co,_))
    | (Communication (Fr,_),Communication (Co,_))
    | (Communication (Rf,_),Communication (Fr,_))
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
    | Communication (Co,_),Communication (Co,_) (* -> Ws *)
    | Communication (Fr,_),Communication (Co,_) (* -> Fr*)
    | Communication (Rf,_),Communication (Fr,_) (* -> Ws *)
(*    Communication (Rf,_),Communication (Fr,_) (* -> Ws *) May be interesting, because
      values are observed by outcome itself,
      also useful to add Fre after B-cumulativity *)
      ->  C.E.get_ie e1 <> C.E.get_ie e2 (* Allow alternance *)
    | Id,Id -> false
    | Id,_ -> compat_id e1.a2 (dir_src e2)
    | _,Id -> compat_id e2.a1 (dir_tgt e1)
(* Fence cumulativity *)
    | Communication (Rf,_),Fenced (f,_,_,_)
    | Fenced (f,_,_,_),Communication (Rf,_) ->
        is_cumul f && choose O.choice safes po_safe xs ys e1 e2
    | _,_ -> choose O.choice safes po_safe xs ys e1 e2
end

module Make(C:Builder.S)
    (O:AltConfig with type fence = C.A.fence) :
    sig
      type predicate_relax

      val to_relax : predicate_relax -> C.R.relax
      val lift : C.R.relax -> predicate_relax
      val gen : ?relax:predicate_relax list -> ?safe:predicate_relax list ->
        ?reject:predicate_relax list -> int -> unit
      val parse_argument : string -> predicate_relax list
      val parse_input :
        relax:string list -> safe:string list ->
        reject:string list ->
        predicate_relax list * predicate_relax list * predicate_relax list
      val remove_invalid_relaxes : predicate_relax list -> predicate_relax list
      val pp_ess : predicate_relax list -> string
      val filter_check:
        safe:predicate_relax list -> predicate_relax -> predicate_relax -> bool
    end

    =
  struct
    module D = DumpAll.Make(O) (C)
    module FilterImpl = Filter(C)(O)
    module RelaxSet = C.R.Set
    open C.E
    open C.R

    type predicate_edge =
      | Plain of C.E.edge
      | Before of C.E.edge
      | After of C.E.edge
      | With of C.E.edge
      | State of string

    let compare_predicate_edge lhs rhs =
      let rank = function
        | Plain _ -> 0 | Before _ -> 1 | After _ -> 2 | With _ -> 3 | State _ -> 4 in
      match Misc.int_compare (rank lhs) (rank rhs) with
      | 0 ->
          begin match lhs,rhs with
          | Plain lhs,Plain rhs
          | Before lhs,Before rhs
          | After lhs,After rhs
          | With lhs,With rhs -> C.E.compare lhs rhs
          | State lhs,State rhs -> String.compare lhs rhs
          | _,_ -> assert false
          end
      | r -> r

    let parse_predicate_node pred pred_ast parse_ast =
      let decorate make same conflict = function
        | Plain edge -> make edge
        | Before _ as edge when same edge -> edge
        | After _ as edge when same edge -> edge
        | With _ as edge when same edge -> edge
        | Before _ | After _ | With _ ->
            Warn.user_error "%s" conflict
        | State _ ->
            Warn.user_error
              "predicate state cannot be decorated by before/after/with." in
      let decorate_ast make same conflict =
        Ast.bind (parse_ast ())
          (fun edge -> Ast.One (decorate make same conflict edge)) in
      match pred,pred_ast with
      | "state",Ast.One state -> Ast.One (State state)
      | "state",_ ->
          Warn.user_error
            "predicate state expects exactly one word argument."
      | "before",_ ->
          decorate_ast
            (fun edge -> Before edge)
            (function Before _ -> true | _ -> false)
            "before and after predicates cannot apply to the same edge"
      | "after",_ ->
          decorate_ast
            (fun edge -> After edge)
            (function After _ -> true | _ -> false)
            "before and after predicates cannot apply to the same edge"
      | "with",_ ->
          decorate_ast
            (fun edge -> With edge)
            (function With _ -> true | _ -> false)
            "with predicate cannot decorate another predicate"
      | pred,_ -> Warn.user_error "predicate %s is not supported." pred

    let pp_predicate_edge = function
      | Plain edge -> pp_edge edge
      | Before edge -> sprintf "@before(%s)" (pp_edge edge)
      | After edge -> sprintf "@after(%s)" (pp_edge edge)
      | With edge -> sprintf "@with(%s)" (pp_edge edge)
      | State state -> sprintf "@state(%s)" state

    type predicate_relax = predicate_edge list

    let pp_predicate_relax = function
      | [edge] -> pp_predicate_edge edge
      | edges -> sprintf "[%s]" (String.concat "," (List.map pp_predicate_edge edges))

    let compare_predicate_relax = List.compare compare_predicate_edge

    module PredicateRelaxSet =
      MySet.Make
        (struct
          type t = predicate_relax
          let compare = compare_predicate_relax
        end)

    let to_relax relax =
      List.filter_map
        (function
          | Plain edge | Before edge | After edge | With edge -> Some edge
          | State _ -> None)
        relax

    let dbg = false

    let is_int e = match get_ie e with
    | Int -> true
    | Ext -> false
    | UnspecCom -> assert false

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

    let leading_with =
      let rec do_rec = function
        | With edge::rest -> edge::do_rec rest
        | State _::rest -> do_rec rest
        | _ -> [] in
      do_rec

    let trailing_with edges = leading_with (List.rev edges) |> List.rev

    let rec has_leading_with_predicate = function
      | State _::rest -> has_leading_with_predicate rest
      | With _::_ -> true
      | _ -> false

    let has_trailing_with_predicate edges =
      has_leading_with_predicate (List.rev edges)

    let remove_leading_with =
      let rec do_rec = function
        | With _::rest -> do_rec rest
        | State state::rest -> State state::do_rec rest
        | rest -> rest in
      do_rec

    let remove_trailing_with edges =
      remove_leading_with (List.rev edges) |> List.rev

    let merge_with next exist =
      match trailing_with next,leading_with exist with
      | _::_,_::_ as pair ->
          let trailing,leading = pair in
          if C.R.compare trailing leading = 0 then
            Some (remove_trailing_with next,remove_leading_with exist)
          else None
      | _ -> None

    (* Given `next = [....; after(..); after(..)]` and
       `exist = [before(..); before(..); ....]`, check whether the optional
       boundary predicates can be merged with the neighbouring concrete edge:
         - `before` merges with concrete if edge matches.
         - `after` merges with concrete if edge matches.
         - `before` pairing with `after` fails. *)
    let merge_predicate next exist =
      let next_edges = List.filter (function State _ -> false | _ -> true) next
      and exist_edges = List.filter (function State _ -> false | _ -> true) exist in
      (* Separate the trailing `after` predicate from `next` *)
      let after =
        List.fold_right ( fun e (after,seen_non_after) ->
          match seen_non_after,e with
          | false,After _ -> e::after,false
          | _,_ -> after,true) next_edges ([],false)
        |> fst in
      (* Separate the beginning `before` predicate from `exist` *)
      let before =
        List.fold_left ( fun (before,seen_non_before) e ->
          match seen_non_before,e with
          | false,Before _ -> e::before,false
          | _,_ -> before,true) ([],false) exist_edges
        |> fst |> List.rev in
      (* Match `after` or `before` predicates when present. *)
      match after,before with
      | (_::_ as after),[] ->
          starts_with
            (fun lhs rhs -> match lhs,rhs with
              | (Plain lhs|Before lhs|After lhs|With lhs),After rhs ->
                  C.E.equal_edge_atoms lhs rhs
              | _,_ -> false)
            exist_edges after
      | [],(_::_ as before) ->
          ends_with
            (fun lhs rhs -> match lhs,rhs with
              | (Plain lhs|Before lhs|After lhs|With lhs),Before rhs ->
                  C.E.equal_edge_atoms lhs rhs
              | _,_ -> false)
            next_edges before
      (* Reject an `after` predicate directly meeting a `before` predicate. *)
      | [],[] | _::_,_::_ -> false

    let boundary_states next exist =
      let rec trailing_state = function
        | State state::_ -> Some state
        | (After _|With _)::rest -> trailing_state rest
        | Plain _::_ | Before _::_ | [] -> None in
      let rec leading_state = function
        | State state::_ -> Some state
        | (Before _|With _)::rest -> leading_state rest
        | Plain _::_ | After _::_ | [] -> None in
      let next_state = trailing_state (List.rev next)
      and exist_state = leading_state exist in
      next_state,exist_state

    let needs_state_check next_state exist_state =
      Option.is_some next_state || Option.is_some exist_state

    let state_compatible next_state exist_state =
      match next_state,exist_state with
      | Some next_state,Some exist_state -> String.equal next_state exist_state
      | None,None -> true
      | None,Some _ | Some _,None -> false

    let needs_merge next exist =
      let trailing_edge =
        List.fold_left
          (fun edge -> function State _ -> edge | edge -> Some edge)
          None next in
      let rec leading_edge = function
        | State _::rest -> leading_edge rest
        | edge::_ -> Some edge
        | [] -> None in
      match trailing_edge,leading_edge exist with
      | Some (After _|With _),_ | _,Some (Before _|With _) -> true
      | _,_ -> false

    let check_before_after_plain can_precede next_edges exist_edges =
      if needs_merge next_edges exist_edges then
        merge_predicate next_edges exist_edges
      else
        can_precede (to_relax next_edges) (to_relax exist_edges)

    (* Check whether `next_edges` may be placed immediately before
       `exist_edges`: if `next_edges` contains trailing `after` predicates
       or `exist_edges` contains leading `before` predicates,
       match those predicates against the neighbouring concrete edges;
       otherwise use the ordinary `can_precede` relation. *)
    let check_precede can_precede next_edges exist_edges =
      if O.verbose > 2 then
        eprintf "next: %s, exists: %s\n"
          (pp_predicate_relax next_edges)
          (pp_predicate_relax exist_edges);
      let next_state,exist_state = boundary_states next_edges exist_edges in
      if needs_state_check next_state exist_state &&
         not (state_compatible next_state exist_state) then false
      else if has_trailing_with_predicate next_edges ||
              has_leading_with_predicate exist_edges then
        match merge_with next_edges exist_edges with
        | Some (next_edges,exist_edges) ->
          if needs_merge next_edges exist_edges then
            merge_predicate next_edges exist_edges
          else true
        | None -> false
      else
        check_before_after_plain can_precede next_edges exist_edges

    let parse_argument_ast input =
      String.trim input |> C.R.parse_ast Parser.diy7

    let varatom_ess predicate_relaxes =
      let varatom_es =
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
            V.varatom_es in
      let reattach_predicates template_predicate_relax edges =
        let rec do_rec template edges = match template,edges with
          | [],[] -> []
          | State state::template,edges -> State state::do_rec template edges
          | Plain _::template,edge::edges -> Plain edge::do_rec template edges
          | Before _::template,edge::edges -> Before edge::do_rec template edges
          | After _::template,edge::edges -> After edge::do_rec template edges
          | With _::template,edge::edges -> With edge::do_rec template edges
          | _,_ -> Warn.fatal "predicate expansion changed relaxation length" in
        do_rec template_predicate_relax edges in
      List.concat_map
        (fun predicate_relax ->
          varatom_es [to_relax predicate_relax]
          |> List.map (reattach_predicates predicate_relax))
        predicate_relaxes

    let remove_invalid_relaxes relaxes =
      let valid_relaxes =
        List.map to_relax relaxes
        |> C.R.remove_invalid_relaxes
        |> C.R.Set.of_list in
      (* Predicate-only edges are only meaningful at relaxation boundaries:
         `before(...)` predicates must form a leading prefix, and `after(...)`
         predicates must form a trailing suffix. Once a plain edge appears,
         no later `before(...)` is valid; once an `after(...)` appears, only
         more `after(...)` predicates may follow. *)
      let rec leading_before_trailing_after_predicate = function
        | Before _::rest ->
            leading_before_trailing_after_predicate rest
        | rest -> plain_then_after rest
      and plain_then_after = function
        | [] -> true
        | Plain _::rest -> plain_then_after rest
        | After _::rest -> List.for_all (function After _ -> true | _ -> false) rest
        | Before _::_ | With _::_ | State _::_ -> false in
      let valid_state_boundaries relax =
        let rec leading state = function
          | Before _::rest -> leading state rest
          | State new_state::rest when Option.is_none state ->
              leading (Some new_state) rest
          | State _::_ -> false
          | [] ->
              begin match state with
              | Some state ->
                  Warn.user_error
                    "predicate state(%s) cannot be used without a relaxation."
                    state
              | None -> true
              end
          | rest -> middle rest
        and middle = function
          | [] -> true
          | Plain _::rest -> middle rest
          | (State _|After _)::_ as rest -> trailing None rest
          | Before _::_ | With _::_ -> false
        and trailing state = function
          | [] -> true
          | After _::rest -> trailing state rest
          | State new_state::rest when Option.is_none state ->
              trailing (Some new_state) rest
          | State _::_ | Plain _::_ | Before _::_ | With _::_ -> false in
        leading None relax in
      let has_plain_edge =
        List.exists (function Plain _ -> true | _ -> false) in
      let without_boundary_with relax =
        remove_leading_with relax |> remove_trailing_with in
      List.filter
        (fun relax ->
          let core = without_boundary_with relax in
          let edges = List.filter (function State _ -> false | _ -> true) core in
          valid_state_boundaries core
          && has_plain_edge edges
          && C.R.Set.mem (to_relax relax) valid_relaxes
          && leading_before_trailing_after_predicate edges)
        relaxes

    let parse_argument_ast_expanded ast =
      let parse_one str =
        C.R.parse_expand_relaxs_ast ~ppo:C.ppo (Ast.One str)
        |> Ast.map
             ~one:(fun edge -> Ast.One (Plain edge))
             ~predicate:
               (fun pred _ _ ->
                 Warn.fatal "unexpected predicate %s in relaxation expansion" pred) in
      Ast.map ~one:parse_one ~predicate:parse_predicate_node ast
      |> Ast.expand (fun _ _ -> assert false)
      |> varatom_ess

    let parse_argument input_argument =
      parse_argument_ast input_argument
      |> parse_argument_ast_expanded

    let parse_arguments input_argument_list =
      List.map parse_argument input_argument_list
      |> List.flatten
      |> remove_invalid_relaxes
      |> List.sort_uniq compare_predicate_relax

    let pp_ess ess = String.concat " " (List.map pp_predicate_relax ess)

    let lift r = List.map (fun edge -> Plain edge) r
    let lift_list rs = List.map lift rs

    let to_cycle_edges edges =
      let concrete =
        List.filter_map (function Plain edge -> Some edge | _ -> None) edges in
      concrete @ trailing_with edges

    module Chunk : sig
      type chunk
      type t

      val predicate_relax : chunk -> predicate_relax
      val concrete_edges : chunk -> C.E.edge list
      val process_count : chunk list -> int
      val max_instruction_count : chunk list -> int
      val max_instruction_count_cycle : chunk list -> int
      val pp_list : chunk list -> string
      val make :
        C.R.Set.t -> (sd -> extr -> extr -> bool) ->
        predicate_relax list -> predicate_relax list -> predicate_relax list ->
        chunk list * chunk list * chunk list * t
      val can_precede : t -> chunk -> chunk list -> bool
    end = struct
      type chunk = {
        id : int;
        predicate_relax : predicate_relax;
        plain_relax : C.R.relax;
        concrete_edges : C.E.edge list;
        concrete_edges_with_atom : C.E.edge list;
        non_pseudo_edges : C.E.edge list;
        left_state : string option;
        right_state : string option;
        has_leading_before : bool;
        has_trailing_after : bool;
        process_count : int;
        left_instruction_count : int;
        max_instruction_count_opt : int option;
        right_instruction_count : int;
      }

      type t = bool array array

      let predicate_relax c = c.predicate_relax
      let concrete_edges c = c.concrete_edges
      let pp_list chunks =
        String.concat " "
          (List.map (fun chunk -> pp_predicate_relax chunk.predicate_relax) chunks)

      let process_count chunks =
        let count =
          List.fold_left
            (fun count chunk -> count + chunk.process_count) 0 chunks in
        if O.verbose > 3 then
          eprintf "PROCS [%s] => %i\n" (pp_list chunks) count ;
        count

      let count_processes edges =
        List.fold_left
          (fun count edge ->
            match edge.edge with
            | Id|Back _|Leave _ -> count
            | _ -> if is_int edge then count else count + 1)
          0 edges

      let count_instructions edges =
        List.fold_left
          (fun (left,max,right) edge ->
            match edge.edge with
            | Id|Back _|Leave _ -> left,max,right
            | _ when is_int edge ->
                if Option.is_none max then left+1,max,right+1
                else left,max,right+1
            | _ ->
                let max = match max with
                  | None -> left
                  | Some max -> Stdlib.max max right in
                left,Some max,0)
          (0,None,0) edges

      let combine_instruction_counts
          (left_l,max_l,right_l) (left_r,max_r,right_r) =
        let max = match max_l,max_r with
          | None,None -> None
          | left_max,right_max ->
              let max = right_l+left_r in
              let max = match left_max with
                | None -> max
                | Some left_max -> Stdlib.max left_max max in
              let max = match right_max with
                | None -> max
                | Some right_max -> Stdlib.max right_max max in
              Some max in
        (if Option.is_none max_l then left_l+left_r else left_l),
        max,
        (if Option.is_none max_r then right_l+right_r else right_r)

      let instruction_count chunks =
        List.fold_left
          (fun count chunk ->
            combine_instruction_counts count
              (chunk.left_instruction_count,
               chunk.max_instruction_count_opt,
               chunk.right_instruction_count))
          (0,None,0) chunks

      let max_instruction_count chunks =
        let left,max,right = instruction_count chunks in
        match max with
        | None -> left
        | Some max -> Stdlib.max max right

      let max_instruction_count_cycle chunks =
        let count = instruction_count chunks in
        let _,max,_ = combine_instruction_counts count count in
        Option.value ~default:0 max

      let edge_lists_can_precede next exist =
        match next,exist with
        | _::_,exist::_ -> C.E.can_precede (Misc.last next) exist
        | _ -> true

      let can_precede can_precede next exist =
        if O.verbose > 2 then
          eprintf "next: %s, exists: %s\n"
            (pp_predicate_relax next.predicate_relax)
            (pp_predicate_relax exist.predicate_relax) ;
        if not (state_compatible next.right_state exist.left_state) then false
        else if has_trailing_with_predicate next.predicate_relax ||
                has_leading_with_predicate exist.predicate_relax then
          check_precede can_precede next.predicate_relax exist.predicate_relax
          && edge_lists_can_precede
               next.concrete_edges_with_atom exist.concrete_edges_with_atom
          && edge_lists_can_precede next.non_pseudo_edges exist.non_pseudo_edges
        else if next.has_trailing_after || exist.has_leading_before then
          merge_predicate next.predicate_relax exist.predicate_relax
        else
          edge_lists_can_precede next.non_pseudo_edges exist.non_pseudo_edges
          && edge_lists_can_precede
               next.concrete_edges_with_atom exist.concrete_edges_with_atom
          && can_precede next.plain_relax exist.plain_relax

      let make safes po_safe prefix relax safe =
        let next_id = ref 0 in
        let mk_chunk predicate_relax =
          let id = !next_id in
          incr next_id ;
          let plain_relax = to_relax predicate_relax in
          let concrete_edges = to_cycle_edges predicate_relax in
          let left_instruction_count,max_instruction_count_opt,
              right_instruction_count = count_instructions concrete_edges in
          let concrete_edges_with_atom =
            List.filter
              (fun edge -> not (C.E.is_insert_store edge.C.E.edge))
              concrete_edges in
          let non_pseudo_edges =
            List.filter (fun edge -> C.E.is_non_pseudo edge.C.E.edge) concrete_edges in
          let _,left_state = boundary_states [] predicate_relax
          and right_state,_ = boundary_states predicate_relax [] in
          {
            id;
            predicate_relax;
            plain_relax;
            concrete_edges;
            concrete_edges_with_atom;
            non_pseudo_edges;
            left_state;
            right_state;
            has_leading_before=needs_merge [] predicate_relax;
            has_trailing_after=needs_merge predicate_relax [];
            process_count=count_processes concrete_edges;
            left_instruction_count;
            max_instruction_count_opt;
            right_instruction_count;
          } in
        let prefix = List.map mk_chunk prefix in
        let relax = List.map mk_chunk relax in
        let safe = List.map mk_chunk safe in
        let chunks = prefix@relax@safe in
        let table = Array.make_matrix !next_id !next_id false in
        List.iter
          (fun next ->
            List.iter
              (fun exist ->
                table.(next.id).(exist.id) <-
                  can_precede (FilterImpl.can_precede safes po_safe) next exist)
              chunks)
          chunks ;
        prefix,relax,safe,table

      let can_precede table next exist = match exist with
        | [] -> true
        | head::_ -> table.(next.id).(head.id)
    end

(* Prefix *)
    let parse_prefixes prefix =
      (* Parse each `-prefix` argument separately, then combine them as one
         top-level choice. Thus `-prefix A -prefix B` is interpreted as
         `-prefix [A|B]`. *)
      let prefixes =
        parse_arguments prefix
        |> List.map (fun chunk -> [chunk]) in
      match prefixes with
      | [] -> [[]] (* No prefix <=> one empty prefix *)
      | prefixes -> prefixes

    let prefixes = parse_prefixes O.prefix

    let () =
      if O.verbose > 0 && O.prefix <> [] then begin
        eprintf "Prefixes:\n" ;
        List.iter
          (fun rs -> eprintf "  %s\n" (pp_ess rs))
          prefixes
      end

    let can_prefix prefix can_precede_relax r_suff = match prefix with
      | [] -> can_precede_relax (Misc.last r_suff) r_suff
      | _::_ ->
          can_precede_relax (Misc.last prefix) r_suff &&
          can_precede_relax (Misc.last r_suff) prefix

    let rec is_prefix l rl =
      match rl,l with
      | hrl::trl, hl::tl ->
          if compare_predicate_edge hl hrl = 0 then is_prefix tl trl
          else false
      | [], _ -> true (* end of rl before or at the end of l *)
      | _, [] -> false (* end of l before end of rl*)


    let check_cycle rsuff = function
      | [] -> true
      | rejects ->
          let rsuff = List.map Chunk.predicate_relax rsuff |> List.concat in
          not (List.exists (fun reject -> is_prefix rsuff reject) rejects)


    (* This function is used `zyva` *)
    let call_rec_base prefix f0 po_safe can_precede_relax
        over n r suff f_rec k ?(reject=[])=
      let r_suff = r::suff in
      if
        can_precede_relax r suff &&
        Chunk.process_count r_suff <= O.nprocs &&
        Chunk.max_instruction_count r_suff <= O.max_ins-1 &&
        check_cycle r_suff reject
      then
        let n = n-1 in
        if O.verbose > 2 then
          eprintf "CALL: %i %s\n%!" n (Chunk.pp_list r_suff) ;
        let k =
          if
            over &&
            (n = 0 || (n > 0 && O.upto)) &&
            can_prefix prefix can_precede_relax r_suff
          then begin
            (* Find an actual candidate cycle and add `prefix`. Predicate
               edges have been resolved at this point, so remove them before
               calling `test_generator`. *)
            let tr = prefix@r_suff in
            if O.verbose > 2 then
            eprintf "TRY: '%s'\n"
              (C.E.pp_edges
                 (List.flatten (List.map Chunk.concrete_edges tr))) ;
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
        else f_rec n r_suff k
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
              (fun chunk k -> match to_relax chunk with
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
      let prefix,relax,safe,adjacency =
        Chunk.make aset po_safe prefix relax safe in
      let can_precede_relax next exist =
        Chunk.can_precede adjacency next exist in

      (* ********************************** *)
      (* iterates over all relax edges `rs` *)
      (* ********************************** *)
      let choose_relax rs k =
      List.fold_left (fun k relex_edge ->
        (* Build simple cycles for relaxation `relex_edge` *)
        (* Partially apply function `call_rec_base` *)
        let call_rec_add_safe =
          call_rec_base prefix (f [Chunk.predicate_relax relex_edge])
            po_safe can_precede_relax
            ~reject:reject in
        (* Add safe edge to suffix *)
        let rec add_safe over ss n suf k =
          List.fold_left ( fun k s -> call_rec_add_safe over n s suf (add_relaxs over) k ) k ss
        (* Add some relax edges `relex_edge` to suffix, or nothing *)
        and add_relaxs over n suf k =
          let k = call_rec_add_safe true n relex_edge suf (add_relaxs true) k in
          add_safe over safe n suf k in

        (* Decide what is the accumulator `k` for the next iteration
           based on if `prefix` is empty *)
        if Misc.nilp prefix then
          (* Optimise: start with a relax edge `relex_edge` *)
            call_rec_add_safe true n relex_edge [] (add_relaxs true) k
        else
            add_relaxs false n [] k
      ) k rs in

      (* ******************************************* *)
      (* Alternative: mix relaxation from relax list *)
      (* ******************************************* *)
      let all_relax k =
        let relax_set =
          PredicateRelaxSet.of_list (List.map Chunk.predicate_relax relax) in
        let extract_relaxs suff =
          let suff_set =
            PredicateRelaxSet.of_list (List.map Chunk.predicate_relax suff) in
          PredicateRelaxSet.inter suff_set relax_set
          |> PredicateRelaxSet.elements in

        (* Partially apply function `call_rec_base` *)
        let call_rec_all_relax =
          call_rec_base prefix
            (fun po_safe suff k ->
              let rs = extract_relaxs suff in
              let nrs = List.length rs in
              if nrs > O.max_relax || nrs < O.min_relax then k
              else f rs po_safe suff k)
            po_safe can_precede_relax ~reject:reject in

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
        if Misc.nilp prefix then add_first relax k
        else add_one false relax safe n [] k in

     (* New relax that does not enforce the first edge to be a relax *)

      (* ***************************************************** *)
      (* As a safety check, generate cycles with no relaxation *)
      (* ***************************************************** *)
      let rec no_relax ss n suf k =
        (* Partially apply function `call_rec_base` *)
        let call_rec_no_relax =
          call_rec_base prefix (f []) po_safe can_precede_relax ~reject:reject in
        List.fold_left (fun k s ->
          call_rec_no_relax true n s suf (no_relax safe) k
        ) k ss in

      (* *************************************************** *)
      (* Function `zyva` starts after all the `let`-bindings *)
      (* *************************************************** *)
      fun k ->
        if Misc.nilp relax then no_relax safe n [] k
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

    let build_safe relaxes candidate =
      PredicateRelaxSet.diff
        (PredicateRelaxSet.of_list (List.map Chunk.predicate_relax candidate))
        (PredicateRelaxSet.of_list relaxes)
      |> PredicateRelaxSet.elements

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
      let prefix_spanp xs (p,s) =
        let rec is_prefix xs ys = match xs,ys with
          | [],_ -> raise (Result true)
          | _::_,[] -> xs
          | x::xs,y::ys ->
              if compare_predicate_edge x y = 0 then is_prefix xs ys
              else raise (Result false) in
        try
          let xs = is_prefix xs s in
          match is_prefix xs p with
          | [] -> true
          | _::_ -> false
        with Result b -> b in
      List.exists
        (fun xs ->
          List.exists
            (fun ps -> prefix_spanp xs ps)
            pss)
      rej

    let last_check_call rej f rs _po_safe res k =
      if Misc.nilp res then k else
          let le = List.map Chunk.concrete_edges res |> List.flatten in
          if Chunk.process_count res <= O.nprocs &&
             Chunk.max_instruction_count_cycle res <= O.max_ins-1 &&
             not
               ((match O.choice with
                | Default| Sc | Ppo | MixedCheck -> true
                | Thin | Free | Uni | Critical | Transitive |Total -> false) &&
                (count_ext le=1 || all_int le || count_changes le < 2)) then begin
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
                   let predicate_cycle =
                     List.concat (List.map Chunk.predicate_relax res) in
                   let pss = Misc.cuts max_sz predicate_cycle in
                   not (substring_spanp rej pss) in
              if ok then
                let ss = build_safe rs res in
                let mk_info =
                  let info =
                    [
                      "Relax",pp_ess rs;
                      "Safe",pp_ess ss;
                    ] in
                  info,pp_ess rs in
                try f le mk_info D.no_name D.no_scope k
                with Normaliser.CannotNormalise _ -> k
              else k
            end
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
        PredicateRelaxSet.union (PredicateRelaxSet.of_list safe) (PredicateRelaxSet.of_list relax) in
      let aset =
        PredicateRelaxSet.fold
          (fun pred -> C.R.Set.add (to_relax pred))
          predicate_aset C.R.Set.empty in
      let plain_rej =
        List.filter_map
          (fun reject ->
            if List.for_all (function Plain _ -> true | _ -> false) reject
            then Some (to_relax reject)
            else None)
          rej in
      D.all
        ~check:(last_minute plain_rej)
        (fun f ->
          zyva_prefix prefixes aset relax safe rej n
            (last_check_call rej f))

    let debug_rs chan rs =
      fprintf chan "%s\n" (pp_ess rs)

    let parse_input ~relax ~safe ~reject =
      let r_nempty = Misc.consp relax in
      let s_nempty = Misc.consp safe in
      let relax_set = parse_arguments relax |> PredicateRelaxSet.of_list
      and safe_set = parse_arguments safe |> PredicateRelaxSet.of_list
      and reject_set = parse_arguments reject |> PredicateRelaxSet.of_list in
      let relax_set = PredicateRelaxSet.diff relax_set reject_set in
      let safe_set = PredicateRelaxSet.diff safe_set (PredicateRelaxSet.union relax_set reject_set) in
      if PredicateRelaxSet.is_empty relax_set && r_nempty then
        Warn.fatal "relaxations provided in relaxlist could not be used to generate cycles" ;
      if PredicateRelaxSet.is_empty safe_set && s_nempty then
        Warn.fatal "relaxations provided in safelist could not be used to generate cycles" ;
      let relax = PredicateRelaxSet.elements relax_set
      and safe = PredicateRelaxSet.elements safe_set
      and reject = PredicateRelaxSet.elements reject_set in
      if O.verbose > 0 then begin
        eprintf "** Relax **\n" ;
        debug_rs stderr relax ;
        eprintf "** Safe **\n" ;
        debug_rs stderr safe
      end ;
      relax, safe, reject

    let secret_gen relax safe reject n =
      do_gen relax safe reject n

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
      let k =
        fold_ie
          (fun ie k ->
            er (Communication (Co,ie))::er (Communication (Fr,ie))::k)
          k in k

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
      let k = fold_ie (fun ie k -> er (Communication (Rf,ie))::k) k in
      let k = fold_cum (fun fe sd d1 d2 k -> ac_fence fe sd d1 d2::k) k in
      let k = fold_cum (fun fe sd d1 d2 k -> bc_fence fe sd d1 d2::k) k in
      let k = er (Hat)::k in
      k

    let gen ?(relax=lift_list relax) ?(safe=lift_list safe) ?(reject=[]) n =
      try secret_gen relax safe reject n
      with e ->
        eprintf "Exc: '%s'\n" (Printexc.to_string e) ;
        raise e

    let filter_check ~safe lhs rhs =
      let predicate_aset =
        PredicateRelaxSet.of_list (lhs::rhs::safe) in
      let aset =
        PredicateRelaxSet.fold
          (fun pred -> C.R.Set.add (to_relax pred))
          predicate_aset C.R.Set.empty in
      let po_safe = extract_po safe in
      let _,lhs,rhs,adjacency =
        Chunk.make aset po_safe [] [lhs] [rhs] in
      match lhs,rhs with
      | [lhs],[rhs] -> Chunk.can_precede adjacency lhs [rhs]
      | _,_ -> assert false
  end
