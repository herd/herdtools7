(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2011-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Definition order gives sorting order *)

open Printf

type bigatom = PP | AA

let pp_bigatom = function
  | PP -> "P"
  | AA -> "A"

type e =
  | Po | PoS
  | Addr
  | Data
  | Ctrl
  | CtrlISync
  | CtrlISB
  | Rfi
  | Fri
  | Wsi
  | ISync
  | ISB
  | Eieio
  | LwSync
  | Sync
  | DMBST
  | DMB
  | DSBST
  | DSB
  | Atom of bigatom list
  | Custom of string

let pp_e = function
| Po -> "po"
| PoS -> "pos"
| Addr -> "addr"
| Data -> "data"
| Ctrl -> "ctrl"
| CtrlISync -> "ctrlisync"
| CtrlISB -> "ctrlisb"
| Rfi -> "rfi"
| Fri -> "fri"
| Wsi -> "wsi"
| ISync -> "isync"
| ISB -> "isb"
| Eieio -> "eieio"
| LwSync -> "lwsync"
| Sync -> "sync"
| DMBST -> "dmb.st"
| DMB -> "dmb"
| DSBST -> "dsb.st"
| DSB -> "dsb"
| Atom xs -> String.concat "" (List.map pp_bigatom xs)
| Custom s -> s

type a = P | R | A

let pp_a = function
  | P -> "p"
  | R -> "r"
  | A -> "a"

let pp_a2 a1 a2 = match a1,a2 with
| P,P -> ""
| _,_ -> pp_a a1 ^ pp_a a2

type p = N|S

let pp_p = function
  | S -> "s"
  | N -> ""

type t = e * a * a * p

let pp (e,a1,a2,p) = sprintf "%s%s%s" (pp_e e) (pp_a2 a1 a2) (pp_p p)
let dbg (e,a1,a2,p) = sprintf "%s|%s|%s" (pp_e e) (pp_a2 a1 a2) (pp_p p)
