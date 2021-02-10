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

type ('loc,'v) t =
  | Loc of 'loc ConstrGen.rloc * TestType.t
  | Fault of 'v Fault.atom

let fold_loc f i r = match i with
| Loc (loc,_) -> ConstrGen.fold_rloc f loc r
| Fault _-> r

let fold_locs f is r = List.fold_right (fold_loc f) is r

let iter_loc f loc = fold_loc  (fun loc () -> f loc) loc ()
let iter_locs f = List.iter (iter_loc f)

let map_loc f i = match i with
| Loc (loc,t) -> Loc (ConstrGen.map_rloc f loc,t)
| Fault _ as j -> j
let map_locs f = List.map (map_loc f)

let locs_and_faults locs =
  List.fold_right
    (fun i (ls,fs) -> match i with
    | Loc (loc,_) -> loc::ls,fs
    | Fault f -> ls,f::fs)
    locs ([],[])
