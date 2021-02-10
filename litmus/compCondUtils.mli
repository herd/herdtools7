(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Input signature for CompCond.Make and Switch.Make *)

module type X = sig
  type location
  type t = location ConstrGen.rloc
  val compare : t -> t -> int
  val dump : t -> string
  val dump_fatom : ('v -> string) -> 'v Fault.atom -> string
end

module type I = sig
  val with_ok : bool
  module C : Constr.S
  (* When present the first, location, argument allows retrieving type *)
  val dump_value : C.location option -> C.V.v -> string
  module Loc : X with type  location = C.location
end
