(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2024-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

type t =
  { target : int;
    params : string StringMap.t }

let empty = { target = 0; params = StringMap.empty }

let add_param k v p =
  if StringMap.mem k p.params then
    Warn.user_error "multiple definition of property %s" k;
  let params = StringMap.add k v p.params in
  { p with params = params }

let add_target v p = { p with target = v }

let compare p1 p2 =
  match Misc.int_compare p1.target p2.target with
  | 0 -> StringMap.compare String.compare p1.params p2.params
  | v -> v

let eq p1 p2 = p1.target == p2.target && StringMap.equal String.equal p1.params p2.params

let pp p =
  sprintf "(%s%s)"
    (sprintf "target:%d, " p.target)
    (StringMap.pp_str_delim ", "
      (fun k v -> sprintf "%s:%s" k v)
      p.params)
