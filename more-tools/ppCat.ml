(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Print PreCat as Cat *)

open Format
open PreCat

let pp_name f n = fprintf f "%s" @@ PreCat.pp_name n

type nat = NatRel|NatSet

let pp_tag = function
| And -> '&'
| Or -> '|'
| Seq -> ';'

let rec pp_cat nat lvl f = function
| Arg (Set (name,_),_) ->
    begin match nat with
      | NatSet -> pp_name f name
      | NatRel -> fprintf f "[%a]" pp_name name
    end
| Arg (Rel (name,_),_) ->
    pp_name f name
| Connect (op,ASet _,ts,_) ->
    begin
      match nat with
      | NatRel ->
          fprintf f "@[<h 0>[%a]@]" (pp_args NatSet 0 op) ts
      | NatSet ->
          pp_args NatSet lvl op f ts
    end
| Connect (op,(ANone|ARel _),ts,_) ->
    begin
      match nat with
      | NatRel ->
          fprintf f "@[<hv 0>%a@]" (pp_args NatRel lvl op) ts
      | NatSet -> assert false
    end

and pp_args nat lvl op f ts =
  let has_par =
    match op with
    | Or -> lvl > 0
    | Seq -> lvl > 1
    | And -> false in
  let nlvl =
    match op with
    | Or -> 0
    | Seq -> 1
    | And -> 2 in
  let pp_sep =
    let c = pp_tag op in
      match nat with
      | NatSet -> fun f () -> fprintf f "%c" c
      | NatRel ->
          match op with
          | Seq ->
              fun f () -> fprintf f "%c@ " c
          | And|Or ->
              fun f () -> fprintf f "@ %c " c in
  let pp =  pp_print_list ~pp_sep:pp_sep (pp_cat nat nlvl) in
  if has_par then
    fprintf f "(%a)" pp ts
  else
    pp f ts

let is_rec name d =
  StringSet.mem (PreCat.pp_name name) @@ PreCat.all_names d

let pp_def f (Def (op,r,ts,_) as d) =
  let nat = match r with Rel _ -> NatRel | Set _ -> NatSet in
  let name = match r with Rel (n,_)|Set (n,_) -> n in
  fprintf f
    "@[<hv 2>let%s %a =@ %a@]@."
    (if is_rec name d then " rec" else "")
    pp_name name
    (pp_args nat 0 op) ts
