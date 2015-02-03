(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)


(* Test interpretation as all cycles of relaxations *)

module Make(A:AutoArch.S) = struct
  module R = A.R
  module L = A.L

  type t = R.SetSet.t
  type outcome = L.outcome
  type relax = R.relax
  type relax_set = R.Set.t
  type count = int R.Map.t

  let pp = R.pp_set_set 

  let interpret all o = R.relaxs_of all (A.E.parse_edges o.L.cycle)

  let intest i = R.Set.unions (R.SetSet.elements i)

  let expand_cumul i =
    let xs =
      R.SetSet.fold
        (fun rs k -> R.expand_cumul rs::k)
        i [] in
    R.SetSet.of_list xs


  let get_relaxed_assuming safe i =
    R.SetSet.fold
      (fun rs k ->
        match R.Set.as_singleton (R.Set.diff rs safe) with
        | None -> k
        | Some r -> r::k)
      i

  let shows_relax safe r i =
    R.SetSet.exists
      (fun rs ->
        match R.Set.as_singleton (R.Set.diff rs safe) with
        | None -> false
        | Some s -> R.compare r s = 0)
      i

  let simplify_for_safes relaxed testing i =
    try
      let i =
        R.SetSet.of_list
          (R.SetSet.fold
             (fun rs k ->
               (* Irrelevant interpretation,
                  corresponding cycle is non-global *)
               if R.Set.is_empty (R.Set.inter relaxed rs) then rs::k
               else k)
             i []) in    
    let i =
      R.SetSet.of_list
        (R.SetSet.fold
           (fun rs k ->
             let rs = R.Set.inter testing rs in
             (* No explained *)
             if R.Set.is_empty rs then raise Exit
             else rs::k)
           i []) in
    if R.SetSet.is_empty i then None
    else Some i
    with Exit -> None


(* Safe heuristics *)
  let safe_by_inter i =
    let xss = R.SetSet.elements i in
    match xss with
    | []|[_] -> R.Set.empty
    | xs::(_::_ as xss) -> List.fold_left R.Set.inter xs xss

  let get_mins le ps =
    let rec select_rec r = function
        [] -> r
      | p::ps ->
          if List.exists (fun p0 -> le p0 p) ps
          then select_rec r ps
          else select_rec (p::r) ps in
    select_rec [] (select_rec [] ps)
 
  let safe_by_cardinal i k =
    let i =
      R.SetSet.of_list
        (get_mins
           (fun s1 s2 -> R.Set.cardinal s1 < R.Set.cardinal s2)
           (R.SetSet.elements i)) in
    let c = R.SetSet.cardinal i in
    if c > 0 then
      R.SetSet.fold
        (fun rs k -> (rs,c)::k)
        i k
    else k
    

(* Relaxation connt for false safe heuristic *)
  let unexplained safe i =
    let x =
      R.SetSet.filter
        (fun rs -> R.Set.subset rs safe)
        i in
    if R.SetSet.is_empty x then None
    else Some x

 let count _name safe i m =
    R.SetSet.fold
      (fun rs m ->
        if R.Set.subset rs safe then
          R.Set.fold
            (fun r m ->
              let v = try  R.Map.find r m with Not_found -> 0 in
              R.Map.add r (v+1) m)
            rs m
        else m)
      i m
end
