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

  type rlocation = location ConstrGen.rloc
  module RLocSet : MySet.S with type elt = rlocation

  module FaultType : FaultType.S
end

module type S = sig
  include Arch

  type prop = (location,V.v,FaultType.t) ConstrGen.prop
  type cond = prop ConstrGen.constr

(* List of read locations *)
  val locations : cond -> LocSet.t
  val locations_prop : prop -> LocSet.t
  val rlocations : cond -> RLocSet.t
  val rlocations_prop : prop -> RLocSet.t

(* List locations that appears as  values *)
  val location_values : cond -> string list
  val location_values_prop : prop -> string list

(* All faults *)
  val get_faults : cond -> (V.v,FaultType.t) Fault.atom list

(* Collect instructions *)
  val get_instrs: cond -> V.Instr.Set.t
  val get_labels: cond -> Label.Full.Set.t
end

open ConstrGen

module Make(A : Arch) : S with
module V = A.V and
type location = A.location and module LocSet = A.LocSet and
module RLocSet = A.RLocSet and module FaultType = A.FaultType =
  struct
    open Constant

    module V = A.V
    type location = A.location
    module LocSet = A.LocSet

    type rlocation = location ConstrGen.rloc
    module RLocSet = A.RLocSet

    module FaultType = A.FaultType
    type prop = (location,V.v,FaultType.t) ConstrGen.prop
    type cond = prop ConstrGen.constr

    let locations_atom a r =
      match a with
      | LV (loc,_) -> LocSet.add (ConstrGen.loc_of_rloc loc) r
      | LL (loc1,loc2) -> LocSet.add loc1 (LocSet.add loc2 r)
      | FF _ -> r

    let locations (c:cond) =
      let locs = fold_constr locations_atom c LocSet.empty in
      locs

    let locations_prop p = fold_prop locations_atom p LocSet.empty

    let add_loc_as_rloc  loc = RLocSet.add(ConstrGen.Loc loc)

    let rlocations_atom a r =
      match a with
      | LV (loc,_) -> RLocSet.add loc r
      | LL (loc1,loc2) ->
          add_loc_as_rloc loc1 (add_loc_as_rloc loc2 r)
      | FF _ -> r

    let rlocations (c:cond) =
      let locs = fold_constr rlocations_atom c RLocSet.empty in
      locs

    let rlocations_prop p = fold_prop rlocations_atom p RLocSet.empty

    module Strings = StringSet

    let atom_values a k =
      let open ConstrGen in
      match a with
      | LV (_,v) ->
            let rec f v k = match v with
            | Symbolic (Virtual {name=s;_}) when Symbol.is_label s -> k
            | Symbolic (Virtual {name=s;offset=0;tag=None;_}) -> Strings.add (Symbol.pp s) k
            | Concrete _|PteVal _|AddrReg _|Instruction _ -> k
            | ConcreteVector vs ->
                List.fold_right f vs k
            | ConcreteRecord vs ->
                StringMap.fold_values f vs k
            | Symbolic _|Tag _|Frozen _
              -> assert false in
            f v k
      | LL _|FF _ -> k

    let location_values c =
      let locs =  fold_constr atom_values c Strings.empty in
      Strings.elements locs

    let location_values_prop p =
      let locs =  fold_prop atom_values p Strings.empty in
      Strings.elements locs

    module F = struct
      type t = (A.V.v,FaultType.t) Fault.atom
      let compare = Fault.atom_compare A.V.compare FaultType.compare
    end

    module FSet = MySet.Make(F)

    let add_fault a k = match a with
    | LV _|LL _ -> k
    | FF a -> FSet.add a k

    let get_faults c =
      let fs = fold_constr add_fault c FSet.empty in
      FSet.elements fs


    let get_instrs c =
      let fold_atom a k = match a with
        | LV (_,Constant.Instruction i) -> V.Instr.Set.add i k
        | LV _ | LL _ | FF _ -> k in
      ConstrGen.fold_constr fold_atom c V.Instr.Set.empty

    let get_labels c =
      let fold_atom a k = match a with
        | LV (_,Symbolic (Virtual {name=Symbol.Label(p,l);_}))
            -> Label.Full.Set.add (p, l) k
        | LV _ | LL _ | FF _ -> k in
      ConstrGen.fold_constr fold_atom c Label.Full.Set.empty


  end
