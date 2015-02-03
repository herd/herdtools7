(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open ConstrGen

module M =
  MyMap.Make
    (struct
      type t = MiscParser.location
      let compare = MiscParser.location_compare
    end)

module V = struct
  type t = SymbConstant.v
  let compare = SymbConstant.compare
end

module VS = MySet.Make(V)

module X =
  LogConstr.Make
    (struct
      type v = V.t
      type state = v M.t
      let state_mem env loc v =
        try
          let w = M.find loc env in
          V.compare v w = 0
        with Not_found -> assert false
      let state_eqloc _ _ _ = assert false
    end)

exception NotInside

let rec collect p m = match p with
|  Atom (LV (loc,v)) ->
    let old = M.safe_find VS.empty loc m in
    M.add loc (VS.add v old) m
| And ps|Or ps ->
    List.fold_right collect ps m
| Implies _
|  Atom (LL _)|Not _ -> raise NotInside

let fold_outcomes c kont k =
  try
    let p = prop_of c in
    let m = collect p M.empty in
    let xss =
      M.fold
        (fun loc vs k -> (loc,VS.elements vs)::k)
        m [] in
    let xss = List.rev xss in (* follow map order for locations *)
    let locs,vss = List.split xss in
    Misc.fold_cross vss
      (fun vs k ->
        let bds = List.combine locs vs in
        if X.check_prop p (M.from_bindings bds) then begin
          kont bds k
        end else k)
      k
  with NotInside -> k

