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

(** Operations on symbolic values *)

val reset_gensym : unit -> unit

module Make : functor
  (Cst : Constant.S)
  (ArchOp :
     ArchOp.S
   with type scalar = Cst.Scalar.t
    and type pteval = Cst.PteVal.t
    and type instr = Cst.Instr.t)
  ->
  Value.S
    with module Cst = Cst
     and module Cst.Scalar = Cst.Scalar
     and type arch_extra_op1 = ArchOp.extra_op1
     and type 'a arch_constr_op1 = 'a ArchOp.constr_op1
     and type arch_op = ArchOp.op
