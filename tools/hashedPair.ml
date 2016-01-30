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

module S = struct
  type t = HashedString.t * HashedString.t
  let equal (a1,b1) (a2,b2) =  a1 == a2 && b1 == b2
  let combine seed v =
    (v + 0x9e3779b9 + (seed lsl 6) + (seed lsr 2)) lxor seed
  let hash (a,b) =
    let seed =  HashedString.as_hash a in
    abs (combine seed (HashedString.as_hash b))
end

include(Hashcons.Make(S))

let table = create 101

let as_hashed a b =
  hashcons table (HashedString.as_hashed a, HashedString.as_hashed b)

let as_tt h = h.Hashcons.node

let get_loc h =
  let loc,_ =  h.Hashcons.node in
  HashedString.as_t loc

let get_v h =
  let _,v = h.Hashcons.node in
  HashedString.as_t v


let as_t h =
  let  (a,b) = as_tt h in
  HashedString.as_t a, HashedString.as_t b

let as_hash h = h.Hashcons.hkey

let compare_loc h1 h2 =
  let a1,_ = as_tt h1 and a2,_ = as_tt h2 in
  HashedString.compare a1 a2

let compare_v h1 h2 =
  let _,b1 = as_tt h1 and _,b2 = as_tt h2 in
  HashedString.compare b1 b2


let compare h1 h2 =
  match compare_loc h1 h2 with
  | 0 ->  compare_v h1 h2
  | r -> r

