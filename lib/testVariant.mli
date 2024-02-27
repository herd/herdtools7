(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Parse in-test variant info *)

module Make : functor
  (Var:sig
      module Opt:sig
        include ParseTag.Opt
        val compare : t -> t -> int
      end
      val info : MiscParser.info
      val variant : Opt.t -> bool
      val mte_precision : Precision.t
      val set_mte_precision : Precision.t ref -> Opt.t -> bool
      val fault_handling : Fault.Handling.t
      val set_fault_handling : Fault.Handling.t ref -> Opt.t -> bool
    end) ->
      sig
        type t = Var.Opt.t
        val mte_precision : Precision.t
        val fault_handling : Fault.Handling.t
        val variant : t -> bool
      end
