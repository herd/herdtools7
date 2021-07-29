(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type ('loc,'v) fault_atom = ('v,('loc,'v) ConstrGen.prop) Fault.atom

type ('loc,'v) t =
  | Loc of 'loc ConstrGen.rloc * TestType.t
  | Fault of ('loc,'v) fault_atom

let fold_loc f i r = match i with
| Loc (loc,_) -> ConstrGen.fold_rloc f loc r
| Fault _-> r

let fold_locs f is r = List.fold_right (fold_loc f) is r

let iter_loc f loc = fold_loc  (fun loc () -> f loc) loc ()
let iter_locs f = List.iter (iter_loc f)

let map_loc f i = match i with
| Loc (loc,t) -> Loc (ConstrGen.map_rloc f loc,t)
| Fault (_,_,None) as f -> f
| Fault (p,l,Some prop) ->
   let open ConstrGen in
   let f_atom = function
     | LV (l,v) -> LV (map_rloc f l,v)
     | LL (l1,l2) -> LL (f l1, f l2)
     | FF _ -> assert false
   in
   let prop = map_prop f_atom prop in
   Fault (p,l,Some prop)
let map_locs f = List.map (map_loc f)

let locs_and_faults locs =
  List.fold_right
    (fun i (ls,fs) -> match i with
    | Loc (loc,_) -> loc::ls,fs
    | Fault f -> ls,f::fs)
    locs ([],[])
