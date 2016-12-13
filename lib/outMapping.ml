(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

let key = "Mapping"

let info_to_tr info =
  try
    let map =  List.assoc key info in
    let map = try LexOutMapping.parse map with _ -> assert false in
    fun s -> StringMap.safe_find s s map
  with Not_found -> Misc.identity

let inverse m =
  StringMap.fold
    (fun k v r ->
      assert (not (StringMap.mem v r)) ;
      StringMap.add v k r)
    m StringMap.empty

let toloc s =
  let i =
    try String.index s ':' with Not_found -> assert false in
  let proc =
    try int_of_string (String.sub s 0 i) with Failure _ -> assert false in
  let reg = String.sub s (i+1) (String.length s-i-1) in
  MiscParser.Location_reg (proc,reg)


let locmap_inverse m =
  StringMap.fold
    (fun k v r ->
      let k = toloc k
      and v = toloc v in
      assert (not (MiscParser.LocMap.mem v r)) ;
      MiscParser.LocMap.add v k r)
    m MiscParser.LocMap.empty
