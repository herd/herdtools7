(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
module Make (C:Arch_herd.Config) (V:Value.S) = struct
  include CBase
  (* Not so simple, should consider expressions... *)
  let is_amo _ = assert false

  let pp_barrier_short = pp_barrier
  let reject_mixed = false
  let mem_access_size _ = None

  module V = V

  include ArchExtra_herd.Make(C)
      (struct
        module V = V
        let endian = endian

        type arch_reg = reg
        let pp_reg = pp_reg
        let reg_compare = reg_compare

        type arch_instruction = instruction
        let fromto_of_instr _ = None

      end)
end
