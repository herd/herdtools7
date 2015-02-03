(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


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

