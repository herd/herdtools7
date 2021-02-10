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
  type t = string
  let equal s1 s2 = Misc.string_eq s1 s2
  let hash = Hashtbl.hash
end

module H = Hashcons.Make(S)

type t = string Hashcons.hash_consed

let table = H.create 101

let as_hashed s = H.hashcons table s

let as_t h = h.Hashcons.node

let as_hash h = h.Hashcons.hkey

let compare s1 s2 = String.compare (as_t s1) (as_t s2)
