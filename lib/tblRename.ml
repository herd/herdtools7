(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


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
