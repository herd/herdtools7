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

type t = REG | VIR | PHY | PTE | TLB | TAG | PHY_PTE

let pp = function
  | REG -> "REG"
  | VIR -> "VIR"
  | PHY -> "PHY"
  | PTE -> "PTE"
  | TLB -> "TLB"
  | TAG -> "TAG"
  | PHY_PTE -> "PHY_PTE"

let is_physical = function
  | PHY|PHY_PTE -> true
  | REG|VIR|PTE|TLB|TAG -> false

let compatible k1 k2 = match k1,k2 with
  | ((PTE|PHY_PTE),(PTE|PHY_PTE)) -> true
  | _,_ -> k1=k2
