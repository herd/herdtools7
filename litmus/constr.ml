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


module type S = sig
  module A : Arch_litmus.Base

  type prop = (A.location,Constant.v) ConstrGen.prop
  type constr = prop ConstrGen.constr

(* List of read locations *)
  val locations : constr -> A.LocSet.t
  val locations_prop : prop -> A.LocSet.t

(* List locations that appears as  values *)
  val location_values : constr -> string list
end

open ConstrGen

module Make(A : Arch_litmus.Base) : S with module A = A  =
  struct
    open Constant
    module A = A

    type prop = (A.location,Constant.v) ConstrGen.prop
    type constr = prop ConstrGen.constr

    let locations_atom a r =
      let open ConstrGen in
      match a with
      | LV (loc,_) -> A.LocSet.add loc r
      | LL (loc1,loc2) -> A.LocSet.add loc1 (A.LocSet.add loc2 r)

    let locations (c:constr) =
      let locs = fold_constr locations_atom c A.LocSet.empty in
      locs

    let locations_prop p = fold_prop locations_atom p A.LocSet.empty

    module Strings = Set.Make(String)

    let location_values c =
      let locs =
        fold_constr
          (fun a k ->
            let open ConstrGen in
            match a with
            | LV (_,v) ->
                begin
                  match v with
                  | Symbolic s -> Strings.add s k
                  | Concrete _ -> k
                end
            | LL _ -> k)
          c Strings.empty in
      Strings.elements locs
  end
