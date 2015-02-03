(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


module type S = sig
  module A : Arch.Base

  type prop = (A.location,Constant.v) ConstrGen.prop
  type constr = prop ConstrGen.constr

(* List of read locations *)
  val locations : constr -> A.LocSet.t
(* List locations that appears as  values *)
  val location_values : constr -> string list
end

open ConstrGen

module Make(A : Arch.Base) : S with module A = A  =
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
