(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type t = PTE of string | PHY of string

let pp_old = function
| PTE s -> Misc.add_pte s
| PHY s -> Misc.add_physical s

let pp = function
| PTE s -> Misc.pp_pte s
| PHY s -> Misc.pp_physical s

let parse s = match Misc.tr_physical s with
| Some s -> PHY s
| None ->
   begin
     match Misc.tr_pte s with
     | Some s -> PTE s
     | None ->
        Warn.user_error
          "identifier %s cannot be used as output address" s
   end

let compare oa1 oa2 = match oa1,oa2 with
| (PHY s1,PHY s2)
| (PTE s1,PTE s2)
  -> String.compare s1 s2
| PHY _,PTE _ -> -1
| PTE _,PHY _ -> 1

let eq oa1 oa2 = match oa1,oa2 with
| (PHY s1,PHY s2)
| (PTE s1,PTE s2)
  -> Misc.string_eq s1 s2
| (PHY _,PTE _)
| (PTE _,PHY _)
  -> false

let as_physical = function
| PHY s -> Some s
| PTE _ -> None

let as_pte = function
| PTE s -> Some s
| PHY _ -> None

let refers_virtual = function
| PTE s|PHY s -> Some s
