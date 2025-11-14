[@@@warning "-42"]

module UList = Util.List

module List_traversal = Util.List.Traversal (struct
  type 'a t = 'a option

  include Util.Option
end)

open Ir

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
  | Rf ie | Fr ie | Ws ie -> ie
  | Leave _ | Back _ | Hat -> Ext
  | Insert _ | Store | Node _ -> Int

let get_sd edge =
  let open E in
  let open Code in
  match edge with
  | Po (sd, _, _) | Dp (_, sd, _) | Fenced (_, sd, _, _) -> Some sd
  | Leave _ | Back _ | Hat | Id | Rf _ | Fr _ | Ws _ | Rmw _ -> Some Same
  | Insert _ | Store | Node _ -> None

let set_src (extr : Code.extr) (e : E.edge) : E.edge =
  match extr with Code.Dir dir -> E.set_src dir e | _ -> e

let set_tgt (extr : Code.extr) (e : E.edge) : E.edge =
  match extr with Code.Dir dir -> E.set_tgt dir e | _ -> e

let amo_tedges : E.tedge list =
  (* let atomic_ops = A.[ A_ADD; A_EOR; A_SET; A_CLR ] in *)
  (* let ld_ops = List.map (fun x -> A.LdOp x) atomic_ops in *)
  (* let st_ops = List.map (fun x -> A.StOp x) atomic_ops in *)
  (* let amo_ops = ld_ops @ st_ops @ A.[ Swp; Cas ] in *)
  (* List.map (fun x -> E.Rmw x) amo_ops *)

  (* Keeping it simple with just Swp and Cas for now *)
  let amo_ops = A.[ Swp; Cas ] in
  List.map (fun x -> E.Rmw x) amo_ops

type tedge = Tedge of { edge : E.tedge; insert : E.fence option }

let has_ie (ie : Code.ie) (Tedge { edge; _ } : tedge) : bool = get_ie edge = ie

let has_sd (sd : Code.sd) (Tedge { edge; _ } : tedge) : bool =
  get_sd edge = Some sd

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

let initial_effect : partial_effect =
  { extr = Code.Irr; atom = None; explicit_mem = false }

let initial_edge : partial_edge = { tedges = None; ie = None; sd = None }

let build_effect (eff : partial_effect) (l : prim_set list) :
    partial_effect option =
  List_traversal.fold_left
    (fun eff x ->
      match x with
      | Dir d -> (
          match merge_dir_opt eff.extr (Code.Dir d) with
          | Some extr -> Some { eff with extr; explicit_mem = true }
          | None -> None)
      | M -> Some { eff with explicit_mem = true }
      | Atom a ->
          Option.map
            (fun atom -> { eff with atom; explicit_mem = true })
            (merge_atomo_opt eff.atom (Some (a, None)))
      | _ -> None)
    eff l

let build_tedges (ed : prim_edge) : E.tedge list =
  let sds = Code.[ Diff; Same ] in
  let ies = Code.[ Ext; Int ] in
  let open Util.List.Infix in
  match ed with
  | Po -> UList.concat_map (fun sd -> [ E.(Po (sd, Code.Irr, Code.Irr)) ]) sds
  | Fr -> UList.concat_map (fun ie -> [ E.Fr ie ]) ies
  | Co -> UList.concat_map (fun ie -> [ E.Ws ie ]) ies
  | Rf -> UList.concat_map (fun ie -> [ E.Rf ie ]) ies
  | Fence f ->
      UList.concat_map
        (fun sd -> [ E.Fenced (A.Barrier f, sd, Code.Irr, Code.Irr) ])
        sds
  | Amo -> amo_tedges
  | LxSx -> [ E.Rmw A.LrSc ]
  | Rmw -> [ E.Rmw A.LrSc ] @ amo_tedges
  | Dp dp ->
      UList.concat_map (fun sd -> [ E.Dp ((dp, A.NoCsel), sd, Code.Irr) ]) sds
  | PickDp dp ->
      UList.concat_map (fun sd -> [ E.Dp ((dp, A.OkCsel), sd, Code.Irr) ]) sds

let build_edge (edge : partial_edge) (l : prim_rel list) : partial_edge option =
  List_traversal.fold_left
    (fun ed x ->
      match x with
      | Edge e when ed.tedges = None ->
          let tedges = build_tedges e in
          let relaxs =
            List.map (fun edge -> Tedge { edge; insert = None }) tedges
          in
          Some { ed with tedges = Some relaxs }
      | Loc when ed.sd <> Some Code.Diff -> Some { ed with sd = Some Code.Same }
      | Ext when ed.ie <> Some Code.Int -> Some { ed with ie = Some Code.Ext }
      | _ -> None)
    edge l

let implied_constraints (l : prim_rel list) :
    prim_set list * prim_rel list * prim_set list =
  l
  |> List.map (function
    | Edge Fr -> ([ Dir Code.R ], [ Loc ], [ Dir Code.W ])
    | Edge Rf -> ([ Dir Code.W ], [ Loc ], [ Dir Code.R ])
    | Edge Co -> ([ Dir Code.W ], [ Loc ], [ Dir Code.W ])
    | Edge Amo -> ([ Dir Code.R ], [ Loc ], [ Dir Code.W ])
    | Edge (Dp A.D.DATA) -> ([ Dir Code.R ], [], [ Dir Code.W ])
    | Edge (Dp _) -> ([ Dir Code.R ], [], [])
    | _ -> ([], [], []))
  |> List.fold_left
       (fun (x, y, z) (x', y', z') -> (x @ x', y @ y', z @ z'))
       ([], [], [])

type relax = Relax of E.edge list

let join_relax (Relax r1 : relax) (Relax r2 : relax) : relax =
  Relax (List.append r1 r2)

let try_match_edge (left : prim_set list)
    (core : (prim_set, prim_rel) seq_item list) (right : prim_set list) :
    relax list option =
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
     Rel (Inter [ Edge Po ]); Set (Inter [ Fence f ]); Rel (Inter [ Edge Po ]);
    ] ->
        let f =
          match f with None -> AArch64Base.(DSB (SY, FULL)) | Some f -> f
        in
        let tedges =
          [
            E.Fenced (A.Barrier f, Code.Diff, Code.Irr, Code.Irr);
            E.Fenced (A.Barrier f, Code.Same, Code.Irr, Code.Irr);
          ]
          |> List.map (fun edge -> Tedge { edge; insert = None })
        in
        let pedge = { tedges = Some tedges; ie = Some Code.Int; sd = None } in
        Some ([], pedge, [])
    | [
     Rel (Inter [ Edge (Dp Dep.CTRL) ]);
     Set (Inter [ Fence (Some AArch64Base.ISB) ]);
     Rel (Inter [ Edge Po ]);
    ] ->
        let tedges =
          [
            E.Dp ((A.D.CTRL, A.NoCsel), Code.Diff, Code.Irr);
            E.Dp ((A.D.CTRL, A.NoCsel), Code.Same, Code.Irr);
          ]
          |> List.map (fun edge ->
              Tedge { edge; insert = Some (A.Barrier AArch64Base.ISB) })
        in
        let pedge = { tedges = Some tedges; ie = Some Code.Int; sd = None } in
        Some ([], pedge, [])
    | [
     Rel (Inter [ Edge (PickDp Dep.CTRL) ]);
     Set (Inter [ Fence (Some AArch64Base.ISB) ]);
     Rel (Inter [ Edge Po ]);
    ] ->
        let tedges =
          [
            E.Dp ((A.D.CTRL, A.OkCsel), Code.Diff, Code.Irr);
            E.Dp ((A.D.CTRL, A.OkCsel), Code.Same, Code.Irr);
          ]
          |> List.map (fun edge ->
              Tedge { edge; insert = Some (A.Barrier AArch64Base.ISB) })
        in
        let pedge = { tedges = Some tedges; ie = Some Code.Int; sd = None } in
        Some ([], pedge, [])
    | [
     Rel (Inter [ Edge Po ]);
     Set (Inter [ Fence (Some f) ]);
     Rel (Inter [ Edge Po ]);
     Set (Inter [ Fence (Some ins) ]);
     Rel (Inter [ Edge Po ]);
    ] ->
        let insert = Some (A.Barrier ins) in
        let tedges =
          [
            E.Fenced (A.Barrier f, Code.Diff, Code.Irr, Code.Irr);
            E.Fenced (A.Barrier f, Code.Same, Code.Irr, Code.Irr);
          ]
          |> List.map (fun edge -> Tedge { edge; insert })
        in
        let pedge = { tedges = Some tedges; ie = Some Code.Int; sd = None } in
        Some ([], pedge, [])
    | _ -> None
  in
  let* tedges = pedge.tedges in
  let* left = build_effect initial_effect (left @ implied_left) in
  let* _ = Util.Option.guard left.explicit_mem in
  let* right = build_effect initial_effect (right @ implied_right) in
  let* _ = Util.Option.guard right.explicit_mem in
  let tedges =
    match pedge.sd with
    | Some sd -> List.filter (has_sd sd) tedges
    | None -> tedges
  in
  let tedges =
    match pedge.ie with
    | Some ie -> List.filter (has_ie ie) tedges
    | None -> tedges
  in
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
  left : prim_set inter;
  core : (prim_set, prim_rel) seq_item list;
  right : prim_set inter;
}

let try_translate_seq (Seq l : (prim_set, prim_rel) seq) : relax list =
  let l = [ Set (Inter [ M ]) ] @ l @ [ Set (Inter [ M ]) ] in
  let st =
    List.fold_left
      (fun st item ->
        match (st.core, item) with
        | [], Set s ->
            let left = inter st.left s in
            { st with left }
        | _, Set s -> (
            let right = inter st.right s in
            let st = { st with right } in
            match
              try_match_edge (get_inter st.left) st.core (get_inter st.right)
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
            | None -> st)
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

let pp_relax : relax -> string = function
  | Relax [ rlx ] -> E.pp_edge rlx
  | Relax rlxs ->
      Format.sprintf "[%s]" (String.concat ", " (List.map E.pp_edge rlxs))

let pp_relax_list : relax list -> string =
 fun rl -> String.concat " " (List.map pp_relax rl)
