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

module Make (SP : sig
  module G : Graph.S

  val series : G.label list -> G.label
  val parallel : G.label list -> G.label
  val invert : G.label -> G.label
  val from_set : G.vertex -> G.label option
  (* val pp_label : Format.formatter -> G.label -> unit *)
end) : sig
  val reduce : endpoints:SP.G.vertex list -> SP.G.t -> SP.G.t
end
