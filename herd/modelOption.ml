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

type t = { co : bool ; init : bool ; sc : bool } 

let default = {co=true; init=false; sc=false}

let pp { co; init; sc; } = match co,init,sc with
| true,true,true -> "[withco withinit withsc]"
| true,true,false -> "[withco withinit]"
| true,false,true -> "[withsc]"
| true,false,false -> ""
| false,_,true -> "[withsc]"
| false,_,false -> "[withoutco]"


let set_enumco b t =
  if not b then { t with co=false; init=true; }
  else { t with co=b; }

let set_init b t =
  if not t.co then t else { t with init=b; }

let set_enumsc b t = { t with sc=b; }
