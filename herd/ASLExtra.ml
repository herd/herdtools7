(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
(* Authors:                                                                 *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(* Hadrien Renaud, University College London, UK.                           *)
(****************************************************************************)

module type ArchBaseHerd = sig
  include ArchBase.S

  val endian : Endian.t
end

module Make (B : ArchBaseHerd) (C : Arch_herd.Config) (V : Value.S) = struct
  module V = V
  include B
  include NoSemEnv
  include NoLevelNorTLBI

  let is_amo _ = false
  let pp_barrier_short = pp_barrier
  let reject_mixed = false
  let mem_access_size _ = None

  include
    ArchExtra_herd.Make
      (C)
      (struct
        module V = V
        module FaultType = FaultType.AArch64

        type arch_reg = reg

        let endian = endian
        let pp_reg = pp_reg
        let reg_compare = reg_compare
        let fromto_of_instr _ = None
        let get_val _ v = v
      end)

  module MemType = MemoryType.No

  module Barrier = AllBarrier.No (struct
    type a = barrier
  end)

  module CMO = Cmo.No
end
