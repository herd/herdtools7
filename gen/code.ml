(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Event components *)
type loc = string
let ok = "ok"

type v = int
type proc = int

(* Direction of event *)
type dir = W | R

(* Edges compoments that do not depend on architecture *)

(* Change or proc accross edge *)
type ie = Int|Ext

(* Change of location across edge *)
type sd = Same|Diff

(* Direction of related events *)
type extr = Dir of dir | Irr | NoDir

(* Associated pretty print & generators *)
let pp_dir = function
  | W -> "W"
  | R -> "R"


let pp_ie = function
  | Int -> "i"
  | Ext -> "e"

let pp_extr = function
  | Dir d -> pp_dir d
  | Irr -> "*"
  | NoDir -> ""

let pp_sd = function
  | Same -> "s"
  | Diff -> "d"

let fold_ie f r = f Ext (f Int r)
let fold_sd f r = f Diff (f Same r)
let fold_extr f r = f (Dir W) (f (Dir R) (f Irr r))
let fold_sd_extr f = fold_sd (fun sd -> fold_extr (fun e -> f sd e))
let fold_sd_extr_extr f =
  fold_sd_extr (fun sd e1 -> fold_extr (fun e2 -> f sd e1 e2))

type check =  Sc | Uni | Thin | Critical
| Free | Ppo | Transitive | Total | MixedCheck



(* Com relation *)
type com =  CRf | CFr | CWs

let pp_com = function
  | CRf -> "Rf"
  | CFr -> "Fr"
  | CWs -> "Ws"

let fold_com f r = f CRf (f CFr (f CWs r))

(* Info in tests *)
type info = (string * string) list

let plain = "Na"
