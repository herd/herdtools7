(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module S = struct
  type t = HashedString.t * HashedString.t
  let equal (a1,b1) (a2,b2) =  a1 == a2 && b1 == b2
  let hash (a,b) = abs (19 * HashedString.as_hash a +  HashedString.as_hash b)
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

