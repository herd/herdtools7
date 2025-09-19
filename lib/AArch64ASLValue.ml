(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
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
end) : Value.AArch64ASL = struct
  if C.is_morello then
    Warn.fatal "-variant asl and -variant morello are not conmpatible" ;
  module AArch64I = AArch64Instr.Make (C)
  module ASLScalar = struct
    include ASLScalar

    let printable = function
      | S_BitVector bv -> S_Int (Asllib.Bitvector.printable  bv)
      | S_Bool b -> S_Int (if b then Z.one else Z.zero)
      | S_Int i -> S_Int (printable_z i)
      | S_Label _ as s -> s
  end
  module AArch64Cst = SymbConstant.Make (ASLScalar) (AArch64PteVal) (AArch64AddrReg) (AArch64I)
  module AArch64Op = AArch64Op.Make(ASLScalar)(ASLOp)
  include SymbValue.Make (AArch64Cst) (SymData.No) (AArch64Op)
end
