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
  type t = string
  let equal s1 s2 = String.compare s1 s2 = 0
  let hash = Hashtbl.hash
end

module H = Hashcons.Make(S)

type t = string Hashcons.hash_consed

let table = H.create 101

let as_hashed s = H.hashcons table s

let as_t h = h.Hashcons.node

let as_hash h = h.Hashcons.hkey

let compare s1 s2 = String.compare (as_t s1) (as_t s2)
