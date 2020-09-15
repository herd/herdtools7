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


type t = string StringMap.t StringMap.t

let empty = StringMap.empty

(*  label -> attribute -> value -> oldt -> newt *)
let add lbl att v m =
  let old =
    try StringMap.find lbl m with Not_found -> StringMap.empty in
  StringMap.add lbl (StringMap.add att v old) m


(* label -> attribute -> t -> value *)
let find lbl att m =  StringMap.find att (StringMap.find lbl m)

let find_all lbl m =
  try
    let n = StringMap.find lbl m in
    StringMap.fold (fun a v k -> (a,v)::k) n []
  with Not_found -> []
