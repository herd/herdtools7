(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2024-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make (C : sig
  val is_morello : bool
end) : Value.AArch64 = struct
  module AArch64Instr = AArch64Instr.Make (C)
  module AArch64Cst =
    SymbConstant.Make (SVEScalar) (AArch64PteVal) (AArch64AddrReg) (AArch64Instr)
  module NoCst =
    SymbConstant.Make (SVEScalar) (PteVal.No) (AddrReg.No) (AArch64Instr)
  module NoArchOp = ArchOp.No(NoCst)
  module AArch64Op = AArch64Op.Make (SVEScalar)(NoArchOp)
  include SymbValue.Make (AArch64Cst) (SymData.No) (AArch64Op)
end
