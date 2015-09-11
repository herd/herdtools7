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

(** Imperative disjoint set data structure *)

module Make (O:Set.OrderedType) : sig
  type t

(* All creation steps must precede union/find operations *)
  val create : unit -> t
  val add : t -> O.t -> t

(* Union/Find *)
  val find : t -> O.t -> O.t
  val union : t -> O.t -> O.t ->  unit

(* Extract result *)
  module Sol : Map.S with type key = O.t
  val as_solution : t -> O.t Sol.t
end
