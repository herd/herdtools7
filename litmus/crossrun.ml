(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


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

