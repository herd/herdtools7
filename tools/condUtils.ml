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

open ConstrGen

module M =
  MyMap.Make
    (struct
      type t = MiscParser.location
      let compare = MiscParser.location_compare
    end)

module V = struct
  type t = Int64Constant.v
  let compare = Int64Constant.compare
end

module VS = MySet.Make(V)

module X =
  LogConstr.Make
    (struct
      module V = Int64Constant
      type state = V.v M.t
      let state_mem env loc v =
        try
          let w = M.find loc env in
          V.compare v w = 0
        with Not_found -> assert false
      let state_eqloc _ _ _ = assert false
    end)

let rec collect p m = match p with
|  Atom (LV (Loc loc,v)) ->
    let old = M.safe_find VS.empty loc m in
    M.add loc (VS.add v old) m
| And ps|Or ps ->
    List.fold_right collect ps m
| Implies _
|  Atom (LV (Deref _,_)|LL _|FF _)|Not _ -> raise Exit

let rec as_outcome p = match p with
| Atom (LV (Loc loc,v)) -> [loc,v]
| Or [p] -> as_outcome p
| And ps ->
    List.fold_left (fun k p -> as_outcome p@k) [] ps
| Atom (LV (Deref _,_)|LL _|FF _)|Or (_::_::_|[])|Not _|Implies (_, _)
    -> raise Exit

let rec as_outcomes p = match p with
| Or ps ->
    List.fold_left (fun k p -> as_outcomes p @ k) [] ps
| _ -> [as_outcome p]


let fold_outcomes c kont k =
  let p = prop_of c in
  try
    let bdss = as_outcomes p in
    List.fold_left (fun k bds -> kont bds k) k bdss
  with Exit -> try
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
(*
        Printf.eprintf "Test: %s\n%!"
          (String.concat " "
             (List.map
                (fun (loc,v) ->
                  MiscParser.dump_location loc ^ "=" ^
                  SymbConstant.pp_v v)
                bds)) ;
*)
        if X.check_prop p (M.from_bindings bds) then begin
          kont bds k
        end else k)
      k
  with Exit -> k
