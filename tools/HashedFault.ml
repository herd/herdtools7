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
  type t = int * HashedStringOpt.t * HashedStringOpt.t * HashedStringOpt.t

  let equal (p1,a1,b1,c1) (p2,a2,b2,c2) =  p1 == p2 && a1 == a2 && b1 == b2 && c1 == c2

  let hash (p,a,b,c) =
    let ah =  HashedStringOpt.as_hash a
    and bh = HashedStringOpt.as_hash b
    and ch = HashedStringOpt.as_hash c in
    abs (Misc.mix (Misc.mix (0x4F1BBCDC+ah) (0x4F1BBCDC+bh) (0x4F1BBCDC+p)) (0x4F1BBCDC+ch) 0)
end

include (Hashcons.Make(S))

let table = create 101

let as_tt h = h.Hashcons.node

let as_hashed ((p,lab),x,ft) =
  hashcons table
    (p,HashedStringOpt.as_hashed lab,
     HashedStringOpt.as_hashed x,
     HashedStringOpt.as_hashed ft)

let as_t h =
  let p,hlab,hx,hft = h.Hashcons.node in
  ((p,HashedStringOpt.as_t hlab),HashedStringOpt.as_t hx,HashedStringOpt.as_t hft)

let as_hash h = h.Hashcons.hkey

let warn_once = ref true

let compare h1 h2 =
  let p1,lab1,x1,ft1 = as_tt h1
  and p2,lab2,x2,ft2 = as_tt h2 in
  match Misc.int_compare p1 p2 with
  | 0 -> begin  match HashedStringOpt.compare lab1 lab2 with
         | 0 -> begin match HashedStringOpt.compare x1 x2 with
                | 0 -> begin match HashedStringOpt.as_t ft1, HashedStringOpt.as_t ft2 with
                       | Some ft1, Some ft2 -> String.compare ft1 ft2
                       | None, _ | _, None ->
                          if !warn_once then begin
                            Warn.warn_always "Comparing faults with and without fault type, \
                                              assuming same type";
                            warn_once := false;
                            end;
                          0
                       end
                | r -> r
                end
         | r -> r
  end
  | r -> r

let has_fault_type h =
  let _,_,_,ft = as_tt h in
  let ft = HashedStringOpt.as_t ft in
  match ft with
  | Some _ -> true
  | None -> false
