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


(* Crossrun option *)

type addr ={ host : string ; port : int option ; }

type t =
  | Host of addr
  | Adb
  | No

let tags = ["none";"adb";"host[:port]";]

let parse tag = match tag with
| "none" -> Some No
| "adb"  -> Some Adb
| _ ->
    let h =
      try
        let j = try String.index tag ':' with Not_found -> raise Exit in
        let h = String.sub tag 0 j
        and p = String.sub tag (j+1) (String.length tag - (j+1)) in
        let p = try int_of_string p with _ -> raise Exit in
        { host=h; port=Some p;}
      with
      | Exit -> { host=tag ; port=None; } in
    Some (Host h)

open Printf

let pp = function
  | No -> "none"
  | Adb -> "adb"
  | Host h ->
      match h.port with
      | None -> h.host
      | Some p ->  sprintf "%s:%i" h.host p

