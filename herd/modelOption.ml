(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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
