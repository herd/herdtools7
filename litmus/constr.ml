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

module type Arch = sig
  module V : Constant.S
  type location
  module LocSet : MySet.S with type elt = location
end

module type S = sig
  include Arch

  type prop = (location,V.v) ConstrGen.prop
  type cond = prop ConstrGen.constr

(* List of read locations *)
  val locations : cond -> LocSet.t
  val locations_prop : prop -> LocSet.t

(* List locations that appears as  values *)
  val location_values : cond -> string list
  val location_values_prop : prop -> string list
end

open ConstrGen

module Make(A : Arch) : S with
module V = A.V and
type location = A.location and module LocSet = A.LocSet =
  struct
    open Constant

    module V = A.V
    type location = A.location
    module LocSet = A.LocSet

    type prop = (location,V.v) ConstrGen.prop
    type cond = prop ConstrGen.constr

    let locations_atom a r =
      let open ConstrGen in
      match a with
      | LV (loc,_) -> LocSet.add loc r
      | LL (loc1,loc2) -> LocSet.add loc1 (LocSet.add loc2 r)

    let locations (c:cond) =
      let locs = fold_constr locations_atom c LocSet.empty in
      locs

    let locations_prop p = fold_prop locations_atom p LocSet.empty

    module Strings = StringSet

    let atom_values a k =
      let open ConstrGen in
      match a with
      | LV (_,v) ->
          begin
            match v with
            | Symbolic ((s,None),_) -> Strings.add s k
            | Concrete _ -> k
            | Label _|Symbolic _|Tag _ -> assert false
          end
      | LL _ -> k

    let location_values c =
      let locs =  fold_constr atom_values c Strings.empty in
      Strings.elements locs

    let location_values_prop p =
      let locs =  fold_prop atom_values p Strings.empty in
      Strings.elements locs


  end
