(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type v = (Int64Scalar.t,ParsedPteVal.t,InstrLit.t) Constant.t

let pp hexa = Constant.pp (Int64Scalar.pp hexa) ParsedPteVal.pp InstrLit.pp

let pp_norm hexa =
  Constant.pp
    (Int64Scalar.pp hexa)
    (ParsedPteVal.pp_norm AArch64PteVal.norm)
    (InstrLit.pp)

let pp_v = pp false

let compare =
  Constant.compare Int64.compare ParsedPteVal.compare InstrLit.compare
let eq =
  Constant.eq Int64.equal ParsedPteVal.eq InstrLit.eq
