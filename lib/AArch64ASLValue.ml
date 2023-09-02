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

module Make (C : sig
  val is_morello : bool
end) : Value.AArch64 = struct
  type extra_op1 = ASLValue.ASLArchOp.op1
  module AArch64I = AArch64Instr.Make (C)
  module AArch64Cst = SymbConstant.Make (ASLScalar) (AArch64PteVal) (AArch64I)
  module AArch64Op = AArch64Op.Make (ASLScalar)(ASLValue.ASLArchOp)
  include SymbValue.Make (AArch64Cst) (AArch64Op)
end
