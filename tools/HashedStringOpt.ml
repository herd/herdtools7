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
  type t = string option
  let equal s1 s2 = match s1,s2 with
  | Some s1,Some s2 -> Misc.string_eq s1 s2
  | None,None -> true
  | (Some _,None)|(None,Some _)
      -> false

  let hash = function
    | None -> 0
    | Some s -> Hashtbl.hash s
end

module H = Hashcons.Make(S)

type t = string option Hashcons.hash_consed

let table = H.create 101

let as_hashed s = H.hashcons table s

let as_t h = h.Hashcons.node

let as_hash h = h.Hashcons.hkey

let compare s1 s2 = Misc.opt_compare String.compare (as_t s1) (as_t s2)
