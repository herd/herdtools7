(*********************************************************************)
(*                        DIY                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf
open MiscParser


let dump_locations dump_location env = match env with
| [] -> ""
| _::_ -> 
    let dump_loc_type (loc,t) = match t with
    | TyDef -> dump_location loc ^";"
    | TyDefPointer -> dump_location loc ^"*;"
    | Ty t -> sprintf "%s %s;" (dump_location loc) t
    | Pointer t -> sprintf "%s %s*;" (dump_location loc) t
    | TyArray _ -> assert false (* No arrays here *) in
    let pp = List.map dump_loc_type env in
    let pp = String.concat " " pp in
    sprintf "locations [%s]" pp
