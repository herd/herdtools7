(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Define Bell architecture *)

module Make
         (C:Arch_herd.Config)
         (V:Value.S with type Cst.Instr.t  = BellBase.instruction) = struct
  include BellBase
  let is_amo = function
    | Prmw _ -> true
    | Pnop|Pld _|Pst _|Pfence _|Pcall _|Pbranch _|Pmov _ -> false

  let pp_barrier_short = pp_barrier
  let reject_mixed = false
  let mem_access_size _ = None

  include NoSemEnv

  module V = V

  include NoLevelNorTLBI

    include
      IFetchTrait.NotImplemented
        (struct
          type arch_instruction = instruction
          type arch_reg = reg
        end)

  include ArchExtra_herd.Make(C)
      (struct
        module V = V
        let endian = endian

        type arch_reg = reg
        let pp_reg = pp_reg
        let reg_compare = reg_compare

        let fromto_of_instr ins = match ins with
          | Pfence(Fence(_,ft)) -> ft
          | _ -> None

        let get_val _ v = v

        module FaultType=FaultType.No
      end)

    module MemType=MemoryType.No

    module Barrier = AllBarrier.No(struct type a = barrier end)

end
