(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf
open MiscParser


let dump_locations dump_location dump_v env = match env with
| [] -> ""
| _::_ ->
    let open TestType in
    let dump_loc_type loc t = match t with
    | TyDef -> dump_location loc ^";"
    | TyDefPointer -> dump_location loc ^"*;"
    | Ty t -> sprintf "%s %s;" (dump_location loc) t
    | Pointer t -> sprintf "%s %s*;" (dump_location loc) t
    | TyArray _|Atomic _ -> assert false (* No arrays nor atomics here *)
    and dump_fault f = sprintf "%s;" (Fault.pp_fatom dump_v f) in
    let dump_item i =
      let open LocationsItem in
      match i with
      | Loc (loc,t) -> dump_loc_type loc t
      | Fault f -> dump_fault f in
    let pp = List.map dump_item env in
    let pp = String.concat " " pp in
    sprintf "locations [%s]" pp
