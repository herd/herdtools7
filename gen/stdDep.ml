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

(** Standard dependencies *)

type dp = ADDR | DATA | CTRL

let fold_dpr f r =  f ADDR (f CTRL r)
let fold_dpw f r =  f ADDR (f DATA (f CTRL r))

let ddr_default = Some ADDR
let ddw_default = Some DATA
let ctrlr_default = Some CTRL
let ctrlw_default = Some CTRL

let is_ctrlr = function
  | CTRL -> true
  | _ -> false

let fst_dp = function
  | CTRL -> [CTRL]
  | ADDR|DATA -> []

let sequence_dp d1 d2 = match d1 with
| ADDR -> [d2]
| DATA|CTRL -> []


let pp_dp = function
  | ADDR -> "Addr"
  | DATA -> "Data"
  | CTRL -> "Ctrl"
