(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

type t = { co : bool ; init : bool ; sc : bool } 

let default = {co=false; init=true; sc=false}
let compat = {co=true; init=false; sc=false}

let pp_opt tag default b =
  if b = default then ""
  else
    sprintf "%s%s" (if b then "with" else "without") tag

let pp { co; init; sc; } =
  let pp =
    [pp_opt "co" default.co co;
     pp_opt "init" default.init init;
     pp_opt "sc" default.sc sc;] in
  let pp =
    List.filter
      (fun pp -> pp <> "")
      pp in
  match pp with
  | [] -> ""
  | _::_ ->
      sprintf "[%s]" (String.concat "," pp)


let set_enumco b t =
  if not b then { t with co=false; init=true; }
  else { t with co=b; }

let set_init b t =
  if not t.co then t else { t with init=b; }

let set_enumsc b t = { t with sc=b; }
