(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
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

type t = REG | VIR | PHY | TLB | TAG

let pp = function
  | REG -> "REG"
  | VIR -> "VIR"
  | PHY -> "PHY"
  | TLB -> "TLB"
  | TAG -> "TAG"

let is_physical = function
  | PHY -> true
  | REG|VIR|TLB|TAG -> false

let compatible k1 k2 = k1=k2
