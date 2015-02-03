(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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

