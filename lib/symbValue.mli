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

module Make :
  functor (Cst:Constant.S) ->
  functor (ArchOp:ArchOp.S
           with type scalar = Cst.Scalar.t
           and type pteval = Cst.PteVal.t
           and type instr = Cst.Instr.t) ->
  sig
    include Value.S
    with module Cst = Cst and type arch_op1 = ArchOp.op1
  end
