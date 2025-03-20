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

module Make : functor
  (V: Value.S with type arch_pred = AArch64Op.predicate)
  ->
  Value.S
    with module Cst = V.Cst
     and module Cst.Scalar = V.Cst.Scalar
     and type arch_extra_op1 = V.arch_extra_op1
     and type 'a arch_constr_op1 = 'a V.arch_constr_op1
     and type arch_extra_op = V.arch_extra_op
     and type 'a arch_constr_op = 'a V.arch_constr_op
     and type solver_state = PAC.solver_state
     and type arch_pred = V.arch_pred
