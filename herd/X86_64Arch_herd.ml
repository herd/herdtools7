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

(** Define X86_64 architecture *)

module Make (C:Arch_herd.Config)(V:Value.S) =
  struct
    include X86_64Base
    let is_amo = function
      | I_LOCK _ | I_EFF_EFF (I_XCHG,_,_,_) -> true
      | I_NOP | I_EFF_OP _ | I_EFF _ | I_EFF_EFF _
      | I_CMPXCHG _ | I_JMP _ | I_JCC _ | I_CMOVC _ | I_MOVNTI _
      | I_FENCE _ | I_MOVD _ | I_MOVNTDQA _
        -> false

    let pp_barrier_short = pp_barrier
    let reject_mixed = false

    type lannot = bool (* atomicity *)
    let get_machsize _ = V.Cst.Scalar.machsize
    let empty_annot = false
    let is_atomic annot = annot
    let is_barrier b1 b2 = barrier_compare b1 b2 = 0

    let barrier_sets =
      [
        "MFENCE",is_barrier MFENCE;
        "SFENCE",is_barrier SFENCE;
        "LFENCE",is_barrier LFENCE;
      ]

    let annot_sets = ["X",is_atomic]

    let is_isync _ = false
    let pp_isync = "???"

    let pp_annot annot =
      if annot then "*" else ""

    let inst_size_to_mach_size = function
      | I8b -> MachSize.Byte
      | I16b -> MachSize.Short
      | I32b | INSb -> MachSize.Word
      | I64b -> MachSize.Quad

    let reg_part_to_mach_size = function
      | R8bL | R8bH ->  MachSize.Byte
      | R16b -> MachSize.Short
      | R32b -> MachSize.Word
      | R64b -> MachSize.Quad

    let reg_to_mach_size r = match r with
      | Ireg (_,p) -> reg_part_to_mach_size p
      | RIP | Symbolic_reg _ | Internal _ | Flag _ | XMM _ -> Warn.fatal "No size for register %s" (pp_reg r)

    let mem_access_size = function
      | I_NOP | I_JMP _ | I_JCC _ | I_LOCK _ | I_FENCE _
      | I_MOVNTDQA _ (* twice a quad in fact *)
        -> None
      | I_EFF_OP (_, sz, _, _) | I_EFF (_, sz, _) | I_EFF_EFF (_, sz, _, _)
      | I_CMPXCHG (sz, _, _) | I_CMOVC (sz, _, _)
      | I_MOVNTI (sz,_,_)  | I_MOVD (sz,_,_)
        -> Some (inst_size_to_mach_size sz)


    (********************)
    (* global locations *)
    (********************)
    module V = V

    module TLBI = NoTLBI 

    include ArchExtra_herd.Make (C)
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
