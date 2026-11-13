(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


type 'a t = (Proc.t * 'a list) list

let empty = []
and is_empty = Misc.nilp

let sorted2byproc lbls =
  Misc.group_sorted (fun (id1,_) (id2,_) -> Proc.equal id1 id2) lbls
  |> List.filter_map
    (function
      | [] -> None
      | (p,_)::_ as lbls -> Some (p,List.map snd lbls))

let iter f =
  List.iter (fun (p,lbls) -> List.iter (f p) lbls)

let iteri f =
  let i = ref 0 in
  iter
    (fun p lbl ->
       let j = !i in
       incr i ; f j p lbl)

let map f = List.map (fun (p,lbls)  -> p,List.map (f p) lbls)

let fold f =
  List.fold_left
    (fun k (p,lbls) ->
       List.fold_left (fun k lbl -> f k p lbl) k lbls)

let of_list lbls = List.sort Label.Full.compare lbls |> sorted2byproc
and to_set lbls =
  fold (fun k p lbl -> (p,lbl)::k) [] lbls
  |> Label.Full.Set.of_list
and of_set s = Label.Full.Set.elements s |> sorted2byproc

let to_procs lbls = List.map fst lbls
let of_proc p lbls =
  try List.find (fun (q,_) -> Proc.equal p q) lbls |> snd
  with Not_found -> []
