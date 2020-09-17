(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


(* Map for renaming, also provides an order *)
type 'a t =  ('a * int) StringMap.t

let empty = StringMap.empty

let add_binding t key o v = StringMap.add key (v,o) t
let find_value t key = let v,_ = StringMap.find key t in v
let find_value_opt t key =
  try
    let v,_ = StringMap.find key t in Some v
  with Not_found -> None
let find_order t key = let _,o = StringMap.find key t in o

let fold f m a = StringMap.fold f m a
