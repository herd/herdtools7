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
module E = Edge.Make (Edge.Config) (A : Fence.S) (A : Atom.S)

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
  | Rf ie | Fr ie | Ws ie | Coms ie -> ie
  | Leave _ | Back _ | Hat -> Ext
  | Insert _ | Store | Node _ -> Int

let set_ie ie (edge : E.tedge) =
  match edge with
  | Rf _ -> E.Rf ie
  | Fr _ -> Fr ie
  | Ws _ -> Ws ie
  | _ -> raise (Invalid_argument "Cannot set ie on this edge kind")

let get_sd (edge : E.tedge) =
  match edge with
  | Po (sd, _, _) | Dp (_, sd, _) | Fenced (_, sd, _, _) -> sd
  | Leave _ | Back _ | Hat | Id | Rf _ | Fr _ | Ws _ | Coms _ | Rmw _ -> Same
  | Insert _ | Store | Node _ -> raise (Invalid_argument "Unexpected edge kind")

let set_sd sd (edge : E.tedge) =
  match edge with
  | Po (_, src, tgt) -> E.Po (sd, src, tgt)
  | Dp (dp, _, tgt) -> Dp (dp, sd, tgt)
  | Fenced (f, _, src, tgt) -> Fenced (f, sd, src, tgt)
  | _ -> raise (Invalid_argument "Cannot set sd on this edge kind")

let set_src (extr : Code.extr) (e : E.edge) : E.edge =
  match extr with Code.Dir dir -> E.set_src dir e | _ -> e

let set_tgt (extr : Code.extr) (e : E.edge) : E.edge =
  match extr with Code.Dir dir -> E.set_tgt dir e | _ -> e

type tedge = Tedge of { edge : E.tedge; insert : E.fence option }

let filter_by_ie (ie : Code.ie) (Tedge ed : tedge) : tedge list =
  let e_ie = get_ie ed.edge in
  if ie = UnspecCom || ie = e_ie then [ Tedge ed ]
  else if e_ie = UnspecCom then [ Tedge { ed with edge = set_ie ie ed.edge } ]
  else []

let filter_by_sd (sd : Code.sd) (Tedge ed : tedge) : tedge list =
  let e_sd = get_sd ed.edge in
  if sd = UnspecLoc || sd = e_sd then [ Tedge ed ]
  else if e_sd = UnspecLoc then [ Tedge { ed with edge = set_sd sd ed.edge } ]
  else []

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

type partial_edge = { tedges : tedge list option; ie : Code.ie; sd : Code.sd }
type prim_set = Ir.prim_set
type prim_rel = Ir.prim_rel
type seq_item = Ir.seq_item

let initial_effect : partial_effect =
  { extr = Code.Irr; atom = None; explicit_mem = false }

let initial_edge : partial_edge =
  { tedges = None; ie = UnspecCom; sd = UnspecLoc }

let apply_prim_set : partial_effect -> prim_set -> partial_effect option =
  let build_dir_eff eff d =
    match merge_dir_opt eff.extr (Code.Dir d) with
    | Some extr -> Some { eff with extr; explicit_mem = true }
    | None -> None
  in
  let build_atom_eff eff a =
    Option.map
      (fun atom -> { eff with atom; explicit_mem = true })
      (merge_atomo_opt eff.atom (Some (a, None)))
  in
  fun eff x ->
    match x with
    | Prim "R" -> build_dir_eff eff Code.R
    | Prim "W" -> build_dir_eff eff Code.W
    | Prim "M" -> Some { eff with explicit_mem = true }
    | Prim "A" -> build_atom_eff eff (A.Acq None)
    | Prim "Q" -> build_atom_eff eff (A.AcqPc None)
    | Prim "L" -> build_atom_eff eff (A.Rel None)
    | _ -> None

let build_effect : partial_effect -> prim_set list -> partial_effect option =
  UList.fold_left_opt apply_prim_set

let build_tedges : prim_rel -> E.tedge list =
  let dp_tedges dp csel = [ E.Dp ((dp, csel), UnspecLoc, Code.Irr) ] in
  function
  | Prim "po" -> [ E.(Po (UnspecLoc, Code.Irr, Code.Irr)) ]
  | Prim "fr" -> [ E.Fr UnspecCom ]
  | Prim "co" -> [ E.Ws UnspecCom ]
  | Prim "rf" -> [ E.Rf UnspecCom ]
  | Fence f -> [ E.Fenced (A.Barrier f, UnspecLoc, Code.Irr, Code.Irr) ]
  | Prim "amo" -> [ E.Rmw A.RMW.AllAmo ]
  | Prim "lxsx" -> [ E.Rmw A.RMW.LrSc ]
  | Prim "rmw" -> [ E.Rmw A.RMW.LrSc; E.Rmw A.RMW.AllAmo ]
  | Prim "addr" -> dp_tedges Dep.ADDR A.NoCsel
  | Prim "ctrl" -> dp_tedges Dep.CTRL A.NoCsel
  | Prim "data" -> dp_tedges Dep.DATA A.NoCsel
  | Prim "pick-addr-dep" -> dp_tedges Dep.ADDR A.OkCsel
  | Prim "pick-ctrl-dep" -> dp_tedges Dep.CTRL A.OkCsel
  | Prim "pick-data-dep" -> dp_tedges Dep.DATA A.OkCsel
  | _ -> []

let apply_prim_rel (ed : partial_edge) (r : prim_rel) : partial_edge option =
  let tedges = build_tedges r in
  if tedges <> [] && ed.tedges = None then
    let relaxs = List.map (fun edge -> Tedge { edge; insert = None }) tedges in
    Some { ed with tedges = Some relaxs }
  else
    match r with
    | Prim "loc" when ed.sd <> Code.Diff -> Some { ed with sd = Code.Same }
    | Prim "ext" when ed.ie <> Code.Int -> Some { ed with ie = Code.Ext }
    | _ -> None

let build_edge : partial_edge -> prim_rel list -> partial_edge option =
  UList.fold_left_opt apply_prim_rel

let implied_constraints (l : prim_rel list) :
    prim_set list * prim_rel list * prim_set list =
  let loc : prim_rel = Prim "loc" in
  l
  |> List.map (fun (x : prim_rel) ->
      match x with
      | Prim "fr" -> ([ Ir.Prim "R" ], [ loc ], [ Ir.Prim "W" ])
      | Prim "rf" -> ([ Prim "W" ], [ loc ], [ Prim "R" ])
      | Prim "co" -> ([ Prim "W" ], [ loc ], [ Prim "W" ])
      | Prim "amo" -> ([ Prim "R" ], [ loc ], [ Prim "W" ])
      | Prim "data" -> ([ Prim "R" ], [], [ Prim "W" ])
      | Prim ("ctrl" | "addr") -> ([ Prim "R" ], [], [])
      | _ -> ([], [], []))
  |> List.fold_left
       (fun (x, y, z) (x', y', z') -> (x @ x', y @ y', z @ z'))
       ([], [], [])

type relax = Relax of E.edge list

let join_relax (Relax r1 : relax) (Relax r2 : relax) : relax =
  Relax (List.append r1 r2)

let try_match_edge (left : prim_set list) (core : seq_item list)
    (right : prim_set list) : relax list option =
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
        let edge = E.Fenced (A.Barrier f, UnspecLoc, Code.Irr, Code.Irr) in
        let tedges = [ Tedge { edge; insert = None } ] in
        let pedge = { tedges = Some tedges; ie = Code.Int; sd = UnspecLoc } in
        Some ([], pedge, [])
    | [
     Rel (Inter [ Prim "ctrl" ]);
     Set (Inter [ Fence (Some AArch64Base.ISB) ]);
     Rel (Inter [ Prim "po" ]);
    ] ->
        let edge = E.Dp ((A.D.CTRL, A.NoCsel), UnspecLoc, Code.Irr) in
        let tedges =
          [ Tedge { edge; insert = Some (A.Barrier AArch64Base.ISB) } ]
        in
        let pedge = { tedges = Some tedges; ie = Code.Int; sd = UnspecLoc } in
        Some ([], pedge, [])
    | [
     Rel (Inter [ Prim "pick-ctrl-dep" ]);
     Set (Inter [ Fence (Some AArch64Base.ISB) ]);
     Rel (Inter [ Prim "po" ]);
    ] ->
        let edge = E.Dp ((A.D.CTRL, A.OkCsel), UnspecLoc, Code.Irr) in
        let tedges =
          [ Tedge { edge; insert = Some (A.Barrier AArch64Base.ISB) } ]
        in
        let pedge = { tedges = Some tedges; ie = Code.Int; sd = UnspecLoc } in
        Some ([], pedge, [])
    | [
     Rel (Inter [ Prim "po" ]);
     Set (Inter [ Fence (Some f) ]);
     Rel (Inter [ Prim "po" ]);
     Set (Inter [ Fence (Some ins) ]);
     Rel (Inter [ Prim "po" ]);
    ] ->
        let insert = Some (A.Barrier ins) in
        let edge = E.Fenced (A.Barrier f, UnspecLoc, Code.Irr, Code.Irr) in
        let tedges = [ Tedge { edge; insert } ] in
        let pedge = { tedges = Some tedges; ie = Code.Int; sd = UnspecLoc } in
        Some ([], pedge, [])
    | _ -> None
  in
  let* tedges = pedge.tedges in
  let* left = build_effect initial_effect (left @ implied_left) in
  let* _ = Util.Option.guard left.explicit_mem in
  let* right = build_effect initial_effect (right @ implied_right) in
  let* _ = Util.Option.guard right.explicit_mem in
  let tedges = tedges |> UList.concat_map (filter_by_sd pedge.sd) in
  let tedges = tedges |> UList.concat_map (filter_by_ie pedge.ie) in
  let relaxs =
    tedges
    |> List.map (fun (Tedge { edge; insert }) ->
        let edge = E.{ edge; a1 = left.atom; a2 = right.atom } in
        let edge = set_src left.extr edge in
        let edge = set_tgt right.extr edge in
        let edges =
          match insert with
          | None -> [ edge ]
          | Some ins -> E.[ edge; plain_edge (Insert ins) ]
        in
        Relax edges)
  in
  Some relaxs

type state = {
  relaxs : relax list;
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

let translate_seq (Seq l : seq_item Ir.seq) : relax list =
  let l = [ Ir.Set (Inter [ Prim "M" ]) ] @ l @ [ Set (Inter [ Prim "M" ]) ] in
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
                    [ join_relax prev_edges edge ]
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
      { relaxs = [ Relax [] ]; left = Inter []; core = []; right = Inter [] }
      l
  in
  if st.core = [] then st.relaxs else []

let translate ~binding (nf : Ir.rel_nf) : relax list =
  Log.info (fun m -> m "Translating component of `%s`" binding);
  Log.debug (fun m -> m "`%s` expression:@.%a" binding Ir.pp_rel_nf nf);
  let nf = Ir.expand_acq_rel nf in
  Log.debug (fun m -> m "`%s` after expanding A/L:@.%a" binding Ir.pp_rel_nf nf);
  let nf = Ir.expand_domain_range nf in
  Log.debug (fun m ->
      m "`%s` after expanding domain/range:@.%a" binding Ir.pp_rel_nf nf);
  let relaxs =
    List.fold_left (fun acc seq -> acc @ translate_seq seq) [] (Ir.get_union nf)
  in
  let relaxs = Util.List.uniq ~eq:( = ) relaxs in
  relaxs

let pp_relax : relax -> string = function
  | Relax [ rlx ] -> E.pp_edge rlx
  | Relax rlxs ->
      Format.sprintf "[%s]" (String.concat ", " (List.map E.pp_edge rlxs))
