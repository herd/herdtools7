(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025 Arm Limited and/or its affiliates                         *)
(* <open-source-office@arm.com>                                             *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Log = (val Logs.src_log (Logs.Src.create "translation") : Logs.LOG)
module UList = Util.List
module A = AArch64Arch_gen.Make (AArch64Arch_gen.Config)

module EdgeConfig = struct
  include Edge.Config

  let wildcard = true
end

module E = Edge.Make (EdgeConfig) (A : Fence.S) (A : Atom.S)
module R = Relax.Make (A) (E)

type prune =
  | Amo_all
  | Amo_m
  | Communication
  | Communication_state
  | Dpctrl
  | Pos_w

(* [Communication_state] is an alternative representation of [Communication],
   so [-prune all] retains the existing communication representation. *)
let prune_all = [ Amo_all; Communication; Dpctrl; Pos_w ]

let merge_dir_opt d1 d2 =
  let open Code in
  match (d1, d2) with
  | Irr, Dir d | Dir d, Irr -> Some (Dir d)
  | Dir d1, Dir d2 -> if d1 = d2 then Some (Dir d1) else None
  | Irr, Irr -> Some Irr
  | NoDir, _ | _, NoDir -> None

let merge_atomo_opt a1 a2 =
  match (a1, a2) with
  | None, Some _ -> Some a2
  | Some _, None -> Some a1
  | None, None -> Some None
  | Some a1, Some a2 -> (
      match A.merge_atoms a1 a2 with None -> None | Some _ as a -> Some a)

let get_ie edge =
  let open E in
  let open Code in
  match edge with
  | Id | Po _ | Dp _ | Fenced _ | Rmw _ -> Int
  | Communication (_, ie) -> ie
  | Leave _ | Back _ | Hat -> Ext
  | Insert _ | Store | Node _ -> Int

let get_sd (edge : E.tedge) =
  match edge with
  | Po (sd, _, _) | Dp (_, sd, _) | Fenced (_, sd, _, _) -> sd
  | Leave _ | Back _ | Hat | Id | Communication _ | Rmw _ -> Same
  | Insert _ | Store | Node _ -> raise (Invalid_argument "Unexpected edge kind")

let set_src (extr : Code.extr) (e : E.edge) : E.edge =
  match extr with Code.Dir dir -> E.set_src dir e | _ -> e

let set_tgt (extr : Code.extr) (e : E.edge) : E.edge =
  match extr with Code.Dir dir -> E.set_tgt dir e | _ -> e

type tedge_head = Concrete of E.tedge | Macro of string
type tedge = { head : tedge_head; insert : E.fence option }

let mk_tedge edge = { head = Concrete edge; insert = None }
let mk_macro name = { head = Macro name; insert = None }
let mk_macro_insert name insert = { head = Macro name; insert = Some insert }

let is_fence_macro name =
  A.fold_all_fences
    (fun f found -> found || String.equal (A.pp_fence f) name)
    false

let filter_by_ie ie = function
  | { head = Macro name; _ } ->
      Warn.fatal "Macro %s must be unfolded before filter_by_ie" name
  | { head = Concrete _; _ } as tedge when Option.is_none ie -> [ tedge ]
  | { head = Concrete edge; _ } as tedge ->
      let e_ie = get_ie edge in
      if Some e_ie = ie then [ tedge ] else []

let filter_by_sd (sd : Code.sd option) = function
  | { head = Macro name; _ } ->
      Warn.fatal "Macro %s must be unfolded before filter_by_sd" name
  | { head = Concrete _; _ } as tedge when Option.is_none sd -> [ tedge ]
  | { head = Concrete edge; _ } as tedge -> (
      let e_sd = get_sd edge in
      match sd with
      | None -> assert false
      | Some sd -> if sd = e_sd then [ tedge ] else [])

(* Keep macros in macro form unless Relax can parse them to a singleton edge.
   Communication macros receive their internal/external suffix directly, for
   example `Fr & ext` becomes `Fre`.  Dp macros receive their explicit
   location/destination suffix when one of those constraints is present, for
   example `DpData` becomes `DpData*W`.  Po macros receive the explicit
   location/source/destination suffix when needed, for example `Po` becomes
   `Po**W` or `PosWR`.  Fence macros always receive that suffix, so an
   unconstrained fence is printed as, for example, `DMB.SY***`. *)
let filter_macro sd ie src tgt tedge name =
  let pp_sd_option = function None -> "*" | Some sd -> Code.pp_sd sd in
  let name =
    match name with
    | "Fr" | "Co" | "Rf" -> (
        match ie with
        | None -> name
        | Some ie -> Format.sprintf "%s%s" name (Code.pp_ie ie))
    | name
      when Misc.is_prefix "Dp" name && (Option.is_some sd || tgt <> Code.Irr) ->
        Format.sprintf "%s%s%s" name (pp_sd_option sd) (Code.pp_extr tgt)
    | "Po" when Option.is_some sd || src <> Code.Irr || tgt <> Code.Irr ->
        Format.sprintf "Po%s%s%s" (pp_sd_option sd) (Code.pp_extr src)
          (Code.pp_extr tgt)
    | name when is_fence_macro name ->
        Format.sprintf "%s%s%s%s" name (pp_sd_option sd) (Code.pp_extr src)
          (Code.pp_extr tgt)
    | _ -> name
  in
  let head =
    match R.parse_expand_relaxs (Ast.One name) with
    | [] -> Warn.fatal "Macro %s expands to no relaxation" name
    | [ [ { E.edge; a1 = None; a2 = None } ] ] -> Concrete edge
    | _ -> Macro name
  in
  [ { tedge with head } ]

let filter_tedge sd ie src tgt tedge =
  let filter tedges =
    tedges
    |> List.concat_map (filter_by_sd sd)
    |> List.concat_map (filter_by_ie ie)
  in
  match tedge with
  | { head = Concrete _; _ } -> filter [ tedge ]
  | { head = Macro name; _ } -> filter_macro sd ie src tgt tedge name

(* Partial structures

   Translation from cat relations of the form `r_1 & r_2 & ... & r_n` into diy
   relaxations is done by assigning a "partial" diy edge to each `r_i`, then
   combining these partial edges into a proper diy edge (if possible).

   Note that cat relations may not always correspond to a diy edge. Some, like
   `loc` or `ext`, represent _properties_ of edges. Hence the need for
   "partial" edges in this building process.

   Identity relations `[e_1 & e_2 & ... & e_n]` are mapped to diy edge
   extremities in a similar way, by iteratively building partial effects.
*)
type partial_effect = {
  extr : Code.extr;
  atom : E.atom option;
  explicit_mem : bool;
}

type partial_edge = {
  tedges : tedge list option;
  ie : Code.ie option;
  sd : Code.sd option;
}

type prim_set = Ir.prim_set
type prim_rel = Ir.prim_rel
type seq_item = Ir.seq_item

let initial_effect : partial_effect =
  { extr = Code.Irr; atom = None; explicit_mem = false }

let initial_edge : partial_edge = { tedges = None; ie = None; sd = None }

let apply_prim_set : partial_effect -> prim_set -> partial_effect option =
  let build_dir_eff eff d =
    match merge_dir_opt eff.extr (Code.Dir d) with
    | Some extr -> Some { eff with extr; explicit_mem = true }
    | None -> None
  in
  let build_atom_eff eff a =
    Option.map
      (fun atom -> { eff with atom; explicit_mem = true })
      (merge_atomo_opt eff.atom (Some a))
  in
  let open A.StructuredAtom in
  fun eff x ->
    match x with
    | Event
        {
          kind = None | Some Exp;
          direction = R;
          field = None | Some Plain;
          is_no_return = _;
        } ->
        build_dir_eff eff Code.R
    | Event
        {
          kind = None | Some Exp;
          direction = W;
          field = None | Some Plain;
          is_no_return = _;
        } ->
        build_dir_eff eff Code.W
    | Event
        {
          kind = None | Some Exp;
          direction = M;
          field = None | Some Plain;
          is_no_return = _;
        } ->
        Some { eff with explicit_mem = true }
    | Event
        {
          kind = None | Some Exp;
          direction = A;
          field = None | Some Plain;
          is_no_return = _;
        } ->
        Option.bind (build_dir_eff eff Code.R) (fun eff ->
            build_atom_eff eff (OrdinaryAccess `Acquire))
    | Event
        {
          kind = None | Some Exp;
          direction = Q;
          field = None | Some Plain;
          is_no_return = _;
        } ->
        Option.bind (build_dir_eff eff Code.R) (fun eff ->
            build_atom_eff eff (OrdinaryAccess `AcquirePC))
    | Event
        {
          kind = None | Some Exp;
          direction = L;
          field = None | Some Plain;
          is_no_return = _;
        } ->
        Option.bind (build_dir_eff eff Code.W) (fun eff ->
            build_atom_eff eff (OrdinaryAccess `Release))
    | _ -> None

let build_effect : partial_effect -> prim_set list -> partial_effect option =
  UList.fold_left_opt apply_prim_set

let build_tedges : prim_rel -> tedge list = function
  | Prim "po" -> [ mk_macro "Po" ]
  | Prim "fr" -> [ mk_macro "Fr" ]
  | Prim "co" -> [ mk_macro "Co" ]
  | Prim "rf" -> [ mk_macro "Rf" ]
  | Fence f -> [ mk_macro (A.pp_fence (A.Barrier f)) ]
  | Prim "amo" -> [ mk_macro "Amo.Safe" ]
  | Prim "lxsx" -> [ mk_tedge (E.Rmw A.RMW.LrSc) ]
  | Prim "rmw" -> [ mk_tedge (E.Rmw A.RMW.LrSc); mk_macro "Amo.Safe" ]
  | Prim "rmw-noret" ->
      [ mk_tedge (E.Rmw A.RMW.LrSc); mk_macro "Amo.SafeNoReturn" ]
  | Prim "addr" -> [ mk_macro "DpAddr" ]
  | Prim "ctrl" -> [ mk_macro "DpCtrl" ]
  | Prim "data" -> [ mk_macro "DpData" ]
  | Prim "pick-addr-dep" -> [ mk_macro "DpAddrCsel" ]
  | Prim "pick-ctrl-dep" -> [ mk_macro "DpCtrlCsel" ]
  | Prim "pick-data-dep" -> [ mk_macro "DpDataCsel" ]
  | _ -> []

let apply_prim_rel (ed : partial_edge) (r : prim_rel) : partial_edge option =
  let tedges = build_tedges r in
  if tedges <> [] && ed.tedges = None then Some { ed with tedges = Some tedges }
  else
    match r with
    | Prim ("loc" | "same-loc") when ed.sd <> Some Code.Diff ->
        Some { ed with sd = Some Code.Same }
    | Prim "ext" when ed.ie <> Some Code.Int ->
        Some { ed with ie = Some Code.Ext }
    | _ -> None

let build_edge : partial_edge -> prim_rel list -> partial_edge option =
  UList.fold_left_opt apply_prim_rel

let implied_constraints (l : prim_rel list) :
    prim_set list * prim_rel list * prim_set list =
  let loc : prim_rel = Prim "loc" in
  l
  |> List.map (fun (x : prim_rel) ->
      match x with
      | Prim "fr" ->
          ( [ Ir.Event { Ir.default_event with direction = R } ],
            [ loc ],
            [ Ir.Event { Ir.default_event with direction = W } ] )
      | Prim "rf" ->
          ( [ Event { Ir.default_event with direction = W } ],
            [ loc ],
            [ Event { Ir.default_event with direction = R } ] )
      | Prim "co" ->
          ( [ Event { Ir.default_event with direction = W } ],
            [ loc ],
            [ Event { Ir.default_event with direction = W } ] )
      | Prim "amo" ->
          ( [ Event { Ir.default_event with direction = R } ],
            [ loc ],
            [ Event { Ir.default_event with direction = W } ] )
      | Prim "data" ->
          ( [ Event { Ir.default_event with direction = R } ],
            [],
            [ Event { Ir.default_event with direction = W } ] )
      | Prim ("ctrl" | "addr") ->
          ([ Event { Ir.default_event with direction = R } ], [], [])
      | _ -> ([], [], []))
  |> List.fold_left
       (fun (x, y, z) (x', y', z') -> (x @ x', y @ y', z @ z'))
       ([], [], [])

type relax_item = Concrete of E.edge | Macro of string

(* [relax] is the final factored form consumed by the pretty-printer. *)
type relax = (string, relax_item) Ast.t

(* Predicate ASTs remain opaque while plain edges pass through the concrete,
   prune, and refold stages. *)
type 'edge predicate_edge = Plain of 'edge | Predicate of relax

(* [concrete_relax] contains only concrete plain edges and is the input to
   pruning and macro refolding. *)
type concrete_relax = E.edge predicate_edge list

(* [macro_relax] is macro-capable: its plain edges have been wrapped as
   [Concrete] and may subsequently be replaced with [Macro] leaves. *)
type macro_relax = relax_item predicate_edge list

let compare_relax_item lhs rhs =
  match (lhs, rhs) with
  | Concrete lhs, Concrete rhs -> E.compare lhs rhs
  | Concrete _, Macro _ -> -1
  | Macro _, Concrete _ -> 1
  | Macro lhs, Macro rhs -> String.compare lhs rhs

let rec compare_relax lhs rhs =
  let rank = function
    | Ast.One _ -> 0
    | Ast.Opt _ -> 1
    | Ast.Seq _ -> 2
    | Ast.Choice _ -> 3
    | Ast.Predicate _ -> 4
  in
  match Misc.int_compare (rank lhs) (rank rhs) with
  | 0 -> begin
      match (lhs, rhs) with
      | Ast.One lhs, Ast.One rhs -> compare_relax_item lhs rhs
      | Ast.Opt lhs, Ast.Opt rhs -> compare_relax lhs rhs
      | Ast.Seq lhs, Ast.Seq rhs | Ast.Choice lhs, Ast.Choice rhs ->
          List.compare compare_relax lhs rhs
      | Ast.Predicate (lpred, lrelax), Ast.Predicate (rpred, rrelax) ->
          Misc.pair_compare String.compare compare_relax (lpred, lrelax)
            (rpred, rrelax)
      | _, _ -> assert false
    end
  | order -> order

let compare_folded_edge lhs rhs =
  match (lhs, rhs) with
  | Plain lhs, Plain rhs -> compare_relax_item lhs rhs
  | Plain _, Predicate _ -> -1
  | Predicate _, Plain _ -> 1
  | Predicate lhs, Predicate rhs -> compare_relax lhs rhs

let compare_folded_relax = List.compare compare_folded_edge

let concat_relax (relaxs : relax list) : relax =
  let items =
    List.concat_map
      (function Ast.Seq items -> items | item -> [ item ])
      relaxs
  in
  let items =
    List.fold_right
      (fun item items ->
        match (item, items) with
        | Ast.One (Concrete e1), Ast.One (Concrete e2) :: _
          when E.is_id e1.edge && E.compare e1 e2 = 0 ->
            items
        | _ -> item :: items)
      items []
  in
  match items with [ item ] -> item | items -> Ast.Seq items

let insert_choice prefix choice suffix =
  prefix @ List.map (fun edge -> Plain (Concrete edge)) choice @ suffix

let find_sized_window_with_prefix_suffix size predicate list =
  let rec take count window = function
    | suffix when count = 0 -> Some (List.rev window, suffix)
    | [] -> None
    | item :: suffix -> take (count - 1) (item :: window) suffix
  in
  let rec do_rec rev_prefix = function
    | [] -> None
    | item :: suffix as list -> (
        match take size [] list with
        | None -> None
        | Some (window, suffix_after_window) ->
            let prefix = List.rev rev_prefix in
            if predicate prefix window suffix_after_window then
              Some (prefix, suffix_after_window)
            else do_rec (item :: rev_prefix) suffix)
  in
  if size <= 0 then invalid_arg "find_sized_window_with_prefix_suffix"
  else do_rec [] list

(* All choices have the same size. For each relaxation, consider every
   position where a choice could occur and retain its surrounding prefix and
   suffix. The window itself need not be inspected: rebuilding the relaxation
   with every choice and finding all of them in [relaxs] proves that the full
   macro family occurs in this context. Return the first such context. *)
let find_common_prefix_suffix choices relaxs =
  let size = List.length (R.Set.choose choices) in
  List.find_map
    (fun relax ->
      find_sized_window_with_prefix_suffix size
        (fun prefix _window suffix ->
          R.Set.for_all
            (fun choice -> List.mem (insert_choice prefix choice suffix) relaxs)
            choices)
        relax)
    relaxs

let replace_macro_once name choices relaxs =
  let candidate = find_common_prefix_suffix choices relaxs in
  match candidate with
  | None -> None
  | Some (prefix, suffix) ->
      let matched =
        R.Set.fold
          (fun choice matched -> insert_choice prefix choice suffix :: matched)
          choices []
      in
      let replacement = prefix @ (Plain (Macro name) :: suffix) in
      let relaxs =
        List.filter (fun relax -> not (List.mem relax matched)) relaxs
      in
      Some (List.sort compare_folded_relax (replacement :: relaxs))

let fold_macro (name, choices) relaxs =
  let rec do_rec relaxs =
    match replace_macro_once name choices relaxs with
    | None -> relaxs
    | Some relaxs -> do_rec relaxs
  in
  do_rec relaxs

let characteristic_macros =
  [
    ( "Amo.SafeNoReturn",
      R.parse_expand_relaxs
        (Ast.Choice
           [ Ast.One "Amo.Swp"; Ast.One "Amo.Cas"; Ast.One "Amo.LdAdd" ]) );
    ("Amo.Safe", R.parse_expand_relaxs (Ast.One "Amo.Cas"));
  ]

let fold_macros =
  (* Fold broader macros first so smaller overlapping expansions do not consume
     their alternatives. Prefer more choices; lexical order makes equal
     candidates deterministic. *)
  let priority (name, choices) = (-R.Set.cardinal choices, name) in
  let macro_entries =
    R.MacroTable.fold_wildcard
      (fun name relaxs entries ->
        match relaxs with
        | [] -> assert false
        | choice :: choices ->
            let size = List.length choice in
            assert (size > 0);
            assert (
              List.for_all (fun choice -> List.length choice = size) choices);
            (name, R.Set.of_list relaxs) :: entries)
      []
    |> List.sort (fun lhs rhs ->
        Misc.pair_compare Misc.int_compare String.compare (priority lhs)
          (priority rhs))
  in
  let macro_entries =
    List.map
      (fun (name, relaxs) -> (name, R.Set.of_list relaxs))
      characteristic_macros
    @ macro_entries
  in
  fun relaxs ->
    List.fold_left
      (fun relaxs entry -> fold_macro entry relaxs)
      relaxs macro_entries

let unfold_macro =
  let macros =
    R.MacroTable.fold_wildcard
      (fun name relaxs macros ->
        StringMap.add name (R.Set.of_list relaxs) macros)
      StringMap.empty
  in
  fun name ->
    match List.assoc_opt name characteristic_macros with
    | Some relaxs -> relaxs
    | None -> (
        match StringMap.find_opt name macros with
        | Some relaxs -> R.Set.elements relaxs
        | None -> Warn.fatal "Unknown macro %s" name)

(* Reconstruct choices and optional items from flat alternatives so that
   [pp_relax] prints, for example, `[A|Q]` instead of separate `A` and `Q`
   relaxations. *)
let factor_relaxes relaxs =
  let sequence = function Ast.Seq items -> items | item -> [ item ] in
  let merge_optional shorter longer =
    let rec do_rec prefix shorter longer =
      match (shorter, longer) with
      | [], [ extra ] -> Some (List.rev_append prefix [ Ast.Opt extra ])
      | short :: shorter, long :: longer when short = long ->
          do_rec (short :: prefix) shorter longer
      | _, extra :: longer when shorter = longer ->
          Some (List.rev_append prefix (Ast.Opt extra :: shorter))
      | _, _ -> None
    in
    do_rec [] shorter longer
  in
  let merge_optional_relaxes lhs rhs =
    let lhs = sequence lhs and rhs = sequence rhs in
    if List.length lhs + 1 = List.length rhs then
      Option.map concat_relax (merge_optional lhs rhs)
    else if List.length rhs + 1 = List.length lhs then
      Option.map concat_relax (merge_optional rhs lhs)
    else None
  in
  let merge_choices lhs rhs =
    let lhs = sequence lhs and rhs = sequence rhs in
    if List.length lhs <> List.length rhs then None
    else
      let differences =
        List.fold_left2
          (fun differences lhs rhs ->
            if lhs = rhs then differences else (lhs, rhs) :: differences)
          [] lhs rhs
      in
      match differences with
      | [ differing ] ->
          let differing_lhs, differing_rhs = differing in
          let merged =
            List.map2
              (fun lhs rhs ->
                if lhs = rhs then lhs
                else Ast.Choice [ differing_lhs; differing_rhs ])
              lhs rhs
          in
          Some (concat_relax merged)
      | _ -> None
  in
  let rec merge_one merge prefix = function
    | [] -> None
    | relax :: rest ->
        let rec with_relax between = function
          | [] -> merge_one merge (relax :: prefix) rest
          | candidate :: candidates -> begin
              match merge relax candidate with
              | Some merged ->
                  Some
                    (List.rev_append prefix
                       (merged :: List.rev_append between candidates))
              | None -> with_relax (candidate :: between) candidates
            end
        in
        with_relax [] rest
  in
  let rec do_rec relaxs =
    match merge_one merge_choices [] relaxs with
    | Some relaxs -> do_rec relaxs
    | None -> (
        match merge_one merge_optional_relaxes [] relaxs with
        | Some relaxs -> do_rec relaxs
        | None -> relaxs)
  in
  do_rec relaxs

let macro_relax_to_ast relax =
  List.map
    (function Plain item -> Ast.One item | Predicate relax -> relax)
    relax
  |> concat_relax

let fold_relaxes relaxs =
  let macro_relaxs : macro_relax list =
    List.map
      (List.map (function
        | Plain edge -> Plain (Concrete edge)
        | Predicate relax -> Predicate relax))
      relaxs
  in
  fold_macros macro_relaxs |> List.map macro_relax_to_ast |> factor_relaxes

let covered_by_pos_star_w = function
  | Plain edge -> begin
      match edge.E.edge with
      | E.Po _ | E.Dp _ | E.Fenced _ ->
          E.loc_sd edge = Code.Same && E.dir_tgt edge = Code.Dir Code.W
      | E.Id | E.Communication _ | E.Rmw _ | E.Leave _ | E.Back _ | E.Hat
      | E.Insert _ | E.Store | E.Node _ ->
          false
    end
  | Predicate _ -> false

let is_pos_star_w_relaxs relaxs =
  let relaxs =
    List.map
      (List.filter (function Plain _ -> true | Predicate _ -> false))
      relaxs
  in
  let choices =
    unfold_macro "Pos*W" |> List.map (List.map (fun edge -> Plain edge))
  in
  List.length choices = List.length relaxs
  && List.for_all (fun choice -> List.mem choice relaxs) choices

let prune_relaxes relaxs =
  let has_two_same_location_po relax =
    let locations =
      List.filter_map
        (function
          | Plain { E.edge = E.Po (sd, _, _); _ } -> Some sd
          | Plain _ | Predicate _ -> None)
        relax
    in
    match locations with [ Code.Same; Code.Same ] -> true | _ -> false
  in
  (* Preserve the reference relaxation itself, including versions decorated
     with boundary predicates. Its concrete [PosRW] and [PosWW] alternatives
     satisfy [covered_by_pos_star_w], but only strictly stronger relaxations
     should be removed. Do not prune a multi-edge relaxation merely because
     an intermediate edge is covered by [Pos*W]. *)
  if not (is_pos_star_w_relaxs relaxs) then
    List.filter
      (fun relax ->
        (not (has_two_same_location_po relax))
        &&
        match
          List.filter (function Plain _ -> true | Predicate _ -> false) relax
        with
        | [ edge ] -> not (covered_by_pos_star_w edge)
        | _ -> true)
      relaxs
  else relaxs

let prune_dpctrl_relaxes =
  List.filter (fun relax ->
      let has_dpctrl_w =
        List.exists
          (function
            | Plain { E.edge = E.Dp (dp, _, Code.Dir Code.W); _ } -> begin
                match A.pp_dp dp with "Ctrl" | "CtrlCsel" -> true | _ -> false
              end
            | Plain _ | Predicate _ -> false)
          relax
      in
      let has_insert =
        List.exists
          (function
            | Plain { E.edge = E.Insert _; _ } -> true
            | Plain _ | Predicate _ -> false)
          relax
      in
      not (has_dpctrl_w && has_insert))

(* For example, wrapping an edge with `L` and `A` produces the relaxation
   `[L,edge,A]`. *)
let split_annotations item left right =
  let annotation atom = E.{ edge = Id; a1 = Some atom; a2 = Some atom } in
  let annotations = function None -> [] | Some atom -> [ annotation atom ] in
  annotations left @ [ item ] @ annotations right

let make_concrete_relax left_atom right_atom insert edges =
  let edges =
    match edges with
    | [] -> []
    | first :: rest -> split_annotations first left_atom None @ rest
  in
  let edges =
    match List.rev edges with
    | [] -> []
    | last :: rest -> List.rev rest @ split_annotations last None right_atom
  in
  let edges =
    match insert with
    | None -> edges
    | Some insert -> edges @ [ E.plain_edge (Insert insert) ]
  in
  List.map (fun edge -> Plain edge) edges

let try_match_edge (left : prim_set list) (core : seq_item list)
    (right : prim_set list) : concrete_relax list option =
  let open Util.Option.Infix in
  let* implied_left, pedge, implied_right =
    match core with
    | [ Rel (Inter rs) ] ->
        let implied_left, implied_core, implied_right =
          implied_constraints rs
        in
        let* pedge = build_edge initial_edge (rs @ implied_core) in
        Some (implied_left, pedge, implied_right)
    | [
     Rel (Inter [ Prim "po" ]);
     Set (Inter [ Fence f ]);
     Rel (Inter [ Prim "po" ]);
    ] ->
        let f =
          match f with None -> AArch64Base.(DSB (SY, FULL)) | Some f -> f
        in
        let tedges = [ mk_macro (A.pp_fence (A.Barrier f)) ] in
        let pedge = { tedges = Some tedges; ie = Some Code.Int; sd = None } in
        Some ([], pedge, [])
    | [
     Rel (Inter [ Prim "ctrl" ]);
     Set (Inter [ Fence (Some AArch64Base.ISB) ]);
     Rel (Inter [ Prim "po" ]);
    ] ->
        let tedges = [ mk_macro_insert "DpCtrl" (A.Barrier AArch64Base.ISB) ] in
        let pedge = { tedges = Some tedges; ie = Some Code.Int; sd = None } in
        Some ([], pedge, [])
    | [
     Rel (Inter [ Prim "pick-ctrl-dep" ]);
     Set (Inter [ Fence (Some AArch64Base.ISB) ]);
     Rel (Inter [ Prim "po" ]);
    ] ->
        let tedges =
          [ mk_macro_insert "DpCtrlCsel" (A.Barrier AArch64Base.ISB) ]
        in
        let pedge = { tedges = Some tedges; ie = Some Code.Int; sd = None } in
        Some ([], pedge, [])
    | [
     Rel (Inter [ Prim "po" ]);
     Set (Inter [ Fence (Some f) ]);
     Rel (Inter [ Prim "po" ]);
     Set (Inter [ Fence (Some ins) ]);
     Rel (Inter [ Prim "po" ]);
    ] ->
        let insert = A.Barrier ins in
        let tedges = [ mk_macro_insert (A.pp_fence (A.Barrier f)) insert ] in
        let pedge = { tedges = Some tedges; ie = Some Code.Int; sd = None } in
        Some ([], pedge, [])
    | _ -> None
  in
  let* tedges = pedge.tedges in
  let* left = build_effect initial_effect (left @ implied_left) in
  let* _ = Util.Option.guard left.explicit_mem in
  let* right = build_effect initial_effect (right @ implied_right) in
  let* _ = Util.Option.guard right.explicit_mem in
  let relaxs =
    tedges
    |> List.concat_map (filter_tedge pedge.sd pedge.ie left.extr right.extr)
    |> List.concat_map (fun (tedge : tedge) ->
        let head_relaxs =
          match tedge.head with
          | Macro name -> unfold_macro name
          | Concrete edge ->
              let edge = E.{ edge; a1 = None; a2 = None } in
              let edge = set_src left.extr edge in
              let edge = set_tgt right.extr edge in
              [ [ edge ] ]
        in
        List.map
          (make_concrete_relax left.atom right.atom tedge.insert)
          head_relaxs)
  in
  Some relaxs

type state = {
  relaxs : concrete_relax list;
  left : prim_set Ir.inter;
  core : seq_item list;
  right : prim_set Ir.inter;
}

let rec fold_with_rest (f : 'acc -> 'a -> 'a list -> 'acc) (acc : 'acc) :
    'a list -> 'acc = function
  | [] -> acc
  | x :: xs ->
      let acc = f acc x xs in
      fold_with_rest f acc xs

let is_po = function
  | Ir.Rel (Inter relations) ->
      List.exists
        (fun (relation : prim_rel) ->
          match relation with Ir.Prim "po" -> true | _ -> false)
        relations
  | _ -> false

let is_explicit_memory edge relation =
  match edge with
  | Ir.Set
      (Inter
         [
           Event
             {
               kind = None | Some Exp;
               direction = M;
               field = None | Some Plain;
               is_no_return = _;
             };
         ]) ->
      is_po relation || Ir.is_rmw relation
  | _ -> false

(* The definitions of `addr`, `data`, and `ctrl` are kept as primitive
   relations during normalisation. Of these, only a standalone `addr` leaves
   its target memory event open: `data` ends at a write, while uses of `ctrl`
   either constrain the target or retain a trailing `po`. *)
let dependency_has_open_target = function
  | Ir.Rel (Inter [ Prim "addr" ]) -> true
  | Ir.Rel (Inter [ Prim ("data" | "ctrl") ]) -> false
  | _ -> false

let resolve_predicates nfs =
  let final_set items =
    match List.rev items with Ir.Set set :: _ -> Some set | _ -> None
  in
  let leading_range_target = function
    | Ir.Set (Inter predicates) :: _ ->
        List.find_map
          (function
            | Ir.Range (Ir.Seq witness) -> final_set witness | _ -> None)
          predicates
    | _ -> None
  in
  let compose lhs rhs =
    match (List.rev lhs, rhs) with
    | Ir.Set lhs_set :: lhs, Ir.Set rhs_set :: rhs ->
        Ir.Seq (List.rev lhs @ [ Ir.Set (Ir.inter lhs_set rhs_set) ] @ rhs)
    | _ -> assert false
  in
  let rec resolve before = function
    | [] -> []
    | (Ir.Union seqs as nf) :: after ->
        let other_candidates =
          List.concat_map (fun (Ir.Union seqs) -> seqs) (before @ after)
        in
        let resolved_new_seqs =
          List.concat_map
            (fun (Ir.Seq range) ->
              match leading_range_target range with
              | None -> []
              | Some target ->
                  List.filter_map
                    (fun (Ir.Seq candidate) ->
                      match final_set candidate with
                      | Some candidate_target ->
                          if candidate_target = target then
                            Some (compose candidate range)
                          else None
                      | None -> None)
                    other_candidates)
            seqs
        in
        Ir.Union (seqs @ resolved_new_seqs) :: resolve (nf :: before) after
  in
  resolve [] nfs

let filter_relations f (Ir.Union seqs) =
  Ir.Union
    (List.map
       (fun (Ir.Seq items) ->
         Ir.Seq
           (List.filter_map
              (function
                | Ir.Rel (Inter rs) -> begin
                    match List.filter f rs with
                    | [] -> None
                    | rs -> Some (Ir.Rel (Inter rs))
                  end
                | item -> Some item)
              items))
       seqs)

let filter_unsupported_relations =
  filter_relations (function
    | Prim ("sca-class" | "intervening") -> false
    | _ -> true)

let exp_obs =
  List.map (List.map (fun edge -> Plain edge)) (unfold_macro "ExpObs")

let after_observation =
  Ast.Predicate
    ( "after",
      Ast.Choice
        [ Ast.One (Macro "ExpObs"); Ast.One (Concrete E.(plain_edge Hat)) ] )

let rmw_marker = function
  | Ir.Set (Ir.Inter [ Ir.Rmw endpoint ]) -> Some endpoint
  | _ -> None

let prefix_external_communication_edge = function
  (* For example, `[M]; po; [dmb.full]; ...`. *)
  | left :: rest -> begin
      match
        List.find_opt (function Ir.Rel _ -> true | Ir.Set _ -> false) rest
      with
      | Some po -> is_explicit_memory left po
      | None -> false
    end
  | _ -> false

let suffix_external_communication_edge l =
  match List.rev l with
  (* For example, `...; po; [M]`. *)
  | right :: po :: _ -> is_explicit_memory right po
  (* For example, a standalone `addr` leaves its target open. *)
  | edge :: _ -> dependency_has_open_target edge
  | _ -> false

let rec first_relation_is_rmw = function
  | Ir.Set _ :: items -> first_relation_is_rmw items
  | item :: _ -> Ir.is_rmw item
  | [] -> false

let external_communication_boundaries l =
  let prefix = prefix_external_communication_edge l in
  let suffix = suffix_external_communication_edge l in
  let rmw_prefix = first_relation_is_rmw l in
  (prefix, suffix, rmw_prefix)

let optional_hat = Predicate (Ast.Opt (Ast.One (Concrete E.(plain_edge Hat))))
let hat = Plain E.(plain_edge Hat)

let add_external_communication_edges l relaxs =
  let prefix, suffix, rmw_prefix = external_communication_boundaries l in
  let ordinary =
    if prefix then
      List.concat_map
        (fun relax -> List.map (fun prefix -> prefix @ relax) exp_obs)
        relaxs
    else relaxs
  in
  let rmw_routes =
    if rmw_prefix then
      let hat = if prefix then hat else optional_hat in
      [ (suffix, List.map (fun relax -> hat :: relax) relaxs) ]
    else []
  in
  let routes =
    if rmw_prefix && not prefix then rmw_routes
    else (suffix, ordinary) :: rmw_routes
  in
  List.concat_map
    (fun (suffix, relaxs) ->
      if suffix then
        List.map (fun relax -> relax @ [ Predicate after_observation ]) relaxs
      else relaxs)
    routes

let state name = Predicate (Ast.Predicate ("state", Ast.One (Macro name)))

let optional_state name =
  Predicate (Ast.Opt (Ast.Predicate ("state", Ast.One (Macro name))))

let is_communication_edge (edge : E.edge) =
  match edge.edge with E.Communication _ -> true | _ -> false

let add_external_communication_states l relaxs =
  let prefix, suffix, rmw_prefix = external_communication_boundaries l in
  let ordinary =
    List.map
      (fun relax ->
        (* A leading communication finishes at the relaxation's far end, while
         a trailing communication starts at its near end. *)
        let starts_with_communication =
          match relax with
          | Plain edge :: _ -> is_communication_edge edge
          | Predicate _ :: _ | [] -> false
        in
        let ends_with_communication =
          match List.rev relax with
          | Plain edge :: _ -> is_communication_edge edge
          | Predicate _ :: _ | [] -> false
        in
        let needs_start_state = prefix || ends_with_communication in
        let needs_end_state = suffix || starts_with_communication in
        let relax =
          match (needs_start_state, starts_with_communication) with
          | true, true -> optional_state "EndExpM" :: relax
          | true, false -> state "StartExpM" :: relax
          | false, _ -> relax
        in
        match (needs_end_state, ends_with_communication) with
        | true, true -> relax @ [ optional_state "StartExpM" ]
        | true, false -> relax @ [ state "EndExpM" ]
        | false, _ -> relax)
      relaxs
  in
  if rmw_prefix then
    let hat = if prefix then hat else optional_hat in
    let rmw_relaxs =
      List.map
        (fun relax ->
          let relax = optional_state "EndExpM" :: hat :: relax in
          if suffix then relax @ [ state "EndExpM" ] else relax)
        relaxs
    in
    if prefix then ordinary @ rmw_relaxs else rmw_relaxs
  else ordinary

let translate_seq (Seq l : seq_item Ir.seq) : concrete_relax list =
  let explicit_memory = Ir.Inter [ Ir.Event Ir.default_event ] in
  let st =
    fold_with_rest
      (fun st item rest ->
        match (st.core, item) with
        | [], Ir.Set s ->
            let left = Ir.inter st.left s in
            { st with left }
        | _, Set s ->
            let right = Ir.inter st.right s in
            let st = { st with right } in
            let should_try_match =
              match rest with [] -> true | Rel _ :: _ -> true | _ -> false
            in
            if should_try_match then
              match
                try_match_edge (Ir.get_inter st.left) st.core
                  (Ir.get_inter st.right)
              with
              | Some edge_alts ->
                  let relaxs =
                    let open Util.List.Infix in
                    let* edge = edge_alts in
                    let* prev_edges = st.relaxs in
                    [ prev_edges @ edge ]
                  in
                  let left = st.right in
                  { relaxs; left; core = []; right = Inter [] }
              | None -> st
            else st
        | core, Rel r ->
            let core =
              if st.right = Inter [] then core @ [ Rel r ]
              else core @ [ Set st.right; Rel r ]
            in
            { st with core; right = Inter [] })
      { relaxs = [ [] ]; left = explicit_memory; core = []; right = Inter [] }
      (l @ [ Ir.Set explicit_memory ])
  in
  if st.core = [] then st.relaxs else []

let resolve_rmw ~partial ~all (Ir.Seq items) =
  let rmw = function
    | Ir.Event { is_no_return = true; _ } ->
        Ir.Rel (Ir.Inter [ Ir.Prim "rmw-noret" ])
    | _ -> Ir.Rel (Ir.Inter [ Ir.Prim "rmw" ])
  in
  let endpoints marked endpoint =
    match marked with
    | Ir.Event { direction = Ir.M; _ } -> (endpoint, endpoint)
    | Ir.Event ({ direction = Ir.R; _ } as event) ->
        ( endpoint,
          Ir.Set (Ir.Inter [ Ir.Event { event with direction = Ir.W } ]) )
    | Ir.Event ({ direction = Ir.W; _ } as event) ->
        ( Ir.Set (Ir.Inter [ Ir.Event { event with direction = Ir.R } ]),
          endpoint )
    | Ir.Event ({ direction = Ir.A | Ir.Q; _ } as event) ->
        ( endpoint,
          Ir.Set (Ir.Inter [ Ir.Event { event with direction = Ir.W } ]) )
    | Ir.Event ({ direction = Ir.L; _ } as event) ->
        ( Ir.Set (Ir.Inter [ Ir.Event { event with direction = Ir.R } ]),
          endpoint )
    | _ -> assert false
  in
  let is_pruned = function
    | Ir.Event { direction = Ir.M | Ir.R | Ir.W; _ } -> partial || all
    | Ir.Event { direction = Ir.A | Ir.Q | Ir.L; _ } -> all
    | _ -> true
  in
  let rec resolve = function
    | [] -> Some []
    | first :: second :: items -> begin
        match (rmw_marker first, rmw_marker second, first, second) with
        | Some marked, None, _, (Ir.Set _ as endpoint) ->
            if is_pruned marked then None
            else
              let left, right = endpoints marked endpoint in
              Option.map
                (fun items -> left :: rmw marked :: right :: items)
                (resolve items)
        | None, Some marked, (Ir.Set _ as endpoint), _ ->
            if is_pruned marked then None
            else
              let left, right = endpoints marked endpoint in
              Option.map
                (fun items -> left :: rmw marked :: right :: items)
                (resolve items)
        | _ ->
            Option.map (fun items -> first :: items) (resolve (second :: items))
      end
    | item :: items -> Option.map (fun items -> item :: items) (resolve items)
  in
  Option.map (fun items -> Ir.Seq items) (resolve items)

let translate ~prune ~binding (nf : Ir.rel_nf) : relax list =
  Log.info (fun m -> m "Translating component of `%s`" binding);
  Log.debug (fun m -> m "`%s` expression:@.%a" binding Ir.pp_rel_nf nf);
  let nf = Ir.compress nf in
  let nf = Ir.expand_trailing_po nf in
  let nf = Ir.expand_domain_range nf in
  Log.debug (fun m ->
      m "`%s` after expanding domain/range:@.%a" binding Ir.pp_rel_nf nf);
  let nf = Ir.compress nf in
  let nf = Ir.expand_rmw nf in
  Log.debug (fun m ->
      m "`%s` after expanding RMW endpoints:@.%a" binding Ir.pp_rel_nf nf);
  let nf = filter_unsupported_relations nf in
  let relaxs =
    List.fold_left
      (fun acc (Ir.Seq _ as seq) ->
        match
          resolve_rmw ~partial:(List.mem Amo_m prune)
            ~all:(List.mem Amo_all prune) seq
        with
        | None -> acc
        | Some (Ir.Seq l as seq) ->
            let (Ir.Union seqs) = Ir.compress (Ir.Union [ seq ]) in
            let relaxs = List.concat_map translate_seq seqs in
            let relaxs =
              if List.mem Communication prune then
                add_external_communication_edges l relaxs
              else relaxs
            in
            let relaxs =
              if List.mem Communication_state prune then
                add_external_communication_states l relaxs
              else relaxs
            in
            acc @ relaxs)
      [] (Ir.get_union nf)
  in
  let relaxs = Util.List.uniq ~eq:( = ) relaxs in
  let relaxs = if List.mem Pos_w prune then prune_relaxes relaxs else relaxs in
  let relaxs =
    if List.mem Dpctrl prune then prune_dpctrl_relaxes relaxs else relaxs
  in
  relaxs |> fold_relaxes

let pp_relax_item = function
  | Concrete edge -> E.pp_edge edge
  | Macro name -> name

let pp_relax relax = Ast.pp Misc.identity pp_relax_item relax
