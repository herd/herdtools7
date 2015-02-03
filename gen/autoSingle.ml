(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)


(* Test interpretation as a single cycle of relaxations *)


module Make(A:AutoArch.S) = struct
  module R = A.R
  module L = A.L

  type t = R.Set.t
  type outcome = L.outcome
  type relax = R.relax
  type relax_set = R.Set.t
  type count = int R.Map.t

  let pp = R.pp_set 

  let interpret _ o = R.Set.of_list (o.L.relaxs @ o.L.safes)

  let intest rs = rs

  let expand_cumul i = R.expand_cumul i

  let get_relaxed_assuming safe rs k =
    match R.Set.as_singleton ( R.Set.diff rs safe) with
    | None -> k
    | Some r -> r::k

  let shows_relax safe r rs =
    match R.Set.as_singleton (R.Set.diff rs safe) with
    | None -> false
    | Some s -> R.compare r s = 0
    
  let simplify_for_safes relaxed testing i =
    if R.Set.is_empty (R.Set.inter relaxed i) then
      let i = R.Set.inter i testing in
      if R.Set.is_empty i then
        None
      else
        Some i
    else
      None

(* Safe heuristics *)
  let safe_by_inter _i = R.Set.empty

  let safe_by_cardinal i k = (i,1)::k

(* Relaxation count for false safe heuristic *)
  let unexplained safe cy = 
    if R.Set.subset cy safe then Some cy
    else None


  let count _name safe cy m =
    if R.Set.subset cy safe then begin
      R.Set.fold
        (fun r m ->
          let v = try  R.Map.find r m with Not_found -> 0 in
          R.Map.add r (v+1) m)
        cy m
    end else m
end
