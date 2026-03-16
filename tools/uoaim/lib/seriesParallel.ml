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

module Log = (val Logs.src_log (Logs.Src.create "spgraph") : Logs.LOG)

module Make (SP : sig
  module G : Graph.S

  val series : G.label list -> G.label
  val parallel : G.label list -> G.label
  val invert : G.label -> G.label
  val from_set : G.vertex -> G.label option
end) =
struct
  module G = SP.G

  type vertex = G.vertex

  (** Given a list [nodes], compute all combinations of pairs modulo symmetry.
  *)
  let unique_pairs nodes =
    let open Misc.List.Syntax in
    let* n = nodes in
    let* m = List.filter (fun m -> m >= n) nodes in
    [ (n, m) ]

  (** Try to perform all parallel edge reductions that can be done. Returns
      [None] if no reduction could be done. *)
  let try_parallel_reduction g : G.t option =
    let changed = ref false in
    let pairs = unique_pairs (G.nodes g) in
    let g' =
      pairs
      |> List.fold_left
           (fun g (x, y) ->
             let straight_es = G.edges_between g ~src:x ~tgt:y in
             let inverted_es =
               if x <> y then
                 List.map SP.invert (G.edges_between g ~src:y ~tgt:x)
               else []
             in
             let es = straight_es @ inverted_es in
             if List.length es > 1 then (
               changed := true;
               let g = G.remove_all_between g x y in
               G.insert g (x, SP.parallel es, y))
             else g)
           g
    in
    if !changed then Some g' else None

  (** Try to perform all series edge reductions that can be done. Returns [None]
      if no reduction could be done. *)
  let try_series_reduction ~endpoints g : G.t option =
    let from_set i = Option.to_list (SP.from_set i) in
    let changed = ref false in
    let nodes =
      G.nodes g |> List.filter (fun n -> not (List.mem n endpoints))
    in
    let g' =
      nodes
      |> List.fold_left
           (fun g n ->
             match G.incident_edges g n with
             | [ (x_dir, x, x_lbl); (y_dir, y, y_lbl) ] ->
                 let x_lbl =
                   if Graph.is_outgoing x_dir then SP.invert x_lbl else x_lbl
                 in
                 let y_lbl =
                   if Graph.is_incoming y_dir then SP.invert y_lbl else y_lbl
                 in
                 let series = [ x_lbl ] @ from_set n @ [ y_lbl ] in
                 let new_edge = (x, SP.series series, y) in
                 changed := true;
                 G.(insert (remove_node g n) new_edge)
             | _ -> g)
           g
    in
    if !changed then Some g' else None

  let reduce ~(endpoints : vertex list) (g : G.t) : G.t =
    let rec loop g =
      Log.debug (fun m -> m "Intermediate graph:@.  @[<v 0>%a@]" G.pp g);
      match try_parallel_reduction g with
      | Some g' -> loop g'
      | None -> (
          match try_series_reduction ~endpoints g with
          | Some g' -> loop g'
          | None -> g)
    in
    Log.debug (fun m ->
        m "Starting graph reduction for endpoints [%a]"
          (Util.pp_list_semicolon G.pp_vertex)
          endpoints);
    let final = loop g in
    Log.debug (fun m -> m "Fully-reduced graph:@.  @[<v 0>%a@]" G.pp g);
    final
end
