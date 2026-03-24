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

val is_outgoing : incidence -> bool
val is_incoming : incidence -> bool

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

module Make (V : Vertex) (L : Label) :
  S with type vertex = V.t and type label = L.t
