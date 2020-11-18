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

module S = struct
  type t = int * HashedStringOpt.t * HashedString.t

  let equal (p1,a1,b1) (p2,a2,b2) =  p1 == p2 && a1 == a2 && b1 == b2

  let hash (p,a,b) =
    let ah =  HashedStringOpt.as_hash a
    and bh = HashedString.as_hash b in
    abs (Misc.mix (0x4F1BBCDC+ah) (0x4F1BBCDC+bh) (0x4F1BBCDC+p))
end

include (Hashcons.Make(S))

let table = create 101

let as_tt h = h.Hashcons.node

let as_hashed ((p,lab),x) =
  hashcons table (p,HashedStringOpt.as_hashed lab,HashedString.as_hashed x)

let as_t h =
  let p,hlab,hx = h.Hashcons.node in
  ((p,HashedStringOpt.as_t hlab),HashedString.as_t hx)

let as_hash h = h.Hashcons.hkey
    
let compare h1 h2 =
  let p1,lab1,x1 = as_tt h1
  and p2,lab2,x2 = as_tt h2 in
  match Misc.int_compare p1 p2 with
  | 0 -> begin  match HashedStringOpt.compare lab1 lab2 with
    | 0 -> HashedString.compare x1 x2
    | r -> r
  end
  | r -> r
