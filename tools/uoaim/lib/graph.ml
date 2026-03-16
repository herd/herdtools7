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

module type Vertex = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

module type Label = sig
  type t

  val pp : Format.formatter -> t -> unit
end

type incidence = Incoming | Outgoing

let is_outgoing : incidence -> bool = function
  | Outgoing -> true
  | Incoming -> false

let is_incoming : incidence -> bool = fun i -> not (is_outgoing i)

module type S = sig
  type vertex
  type label
  type t
  type edge = vertex * label * vertex

  val nodes : t -> vertex list
  val empty : t
  val insert : t -> edge -> t
  val edges : t -> edge list
  val edges_between : t -> src:vertex -> tgt:vertex -> label list
  val remove_all_between : t -> vertex -> vertex -> t
  val remove_node : t -> vertex -> t

  type incident_edge = incidence * vertex * label

  val incident_edges : t -> vertex -> incident_edge list
  val pp_edge : Format.formatter -> edge -> unit
  val pp_vertex : Format.formatter -> vertex -> unit
  val pp_label : Format.formatter -> label -> unit
  val pp : Format.formatter -> t -> unit
end

module Make (V : Vertex) (L : Label) = struct
  module PairMap = Map.Make (struct
    type t = V.t * V.t

    let compare = Misc.pair_compare V.compare V.compare
  end)

  module VSet = Set.Make (V)

  type vertex = V.t
  type label = L.t
  type edge = V.t * L.t * V.t
  type t = L.t list PairMap.t

  let nodes (g : t) : vertex list =
    PairMap.fold (fun (x, y) _ acc -> VSet.(add x (add y acc))) g VSet.empty
    |> VSet.elements

  let empty = PairMap.empty

  let insert (g : t) (e : edge) =
    let src, lbl, tgt = e in
    PairMap.update (src, tgt)
      (function None -> Some [ lbl ] | Some lbls -> Some (lbl :: lbls))
      g

  let edges_between (g : t) ~src ~tgt : L.t list =
    match PairMap.find_opt (src, tgt) g with None -> [] | Some lbls -> lbls

  let remove_all_between (g : t) src tgt : t =
    let g = PairMap.remove (src, tgt) g in
    PairMap.remove (tgt, src) g

  let remove_node (g : t) (n : V.t) : t =
    PairMap.filter (fun (src, tgt) _ -> src <> n && tgt <> n) g

  type incident_edge = incidence * vertex * label

  let incident_edges (g : t) n : incident_edge list =
    PairMap.fold
      (fun (src, tgt) lbls acc ->
        if src = n then List.map (fun lbl -> (Outgoing, tgt, lbl)) lbls @ acc
        else if tgt = n then
          List.map (fun lbl -> (Incoming, src, lbl)) lbls @ acc
        else acc)
      g []

  let edges (g : t) : (V.t * L.t * V.t) list =
    PairMap.fold
      (fun (src, tgt) lbls acc ->
        List.map (fun lbl -> (src, lbl, tgt)) lbls @ acc)
      g []

  let pp_edge fmt ((s, lbl, t) : edge) =
    Format.fprintf fmt "(%a, %a, %a)" V.pp s L.pp lbl V.pp t

  let pp_vertex = V.pp
  let pp_label = L.pp

  let pp fmt (g : t) =
    let open Format in
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt ";@ ")
      pp_edge fmt (edges g)
end
