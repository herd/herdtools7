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
      | I_NOP | I_RET | I_EFF_OP _ | I_EFF _ | I_EFF_EFF _
      | I_CMPXCHG _ | I_JMP _ | I_JCC _ | I_CMOVC _ | I_MOVNTI _
      | I_FENCE _ | I_MOVD _ | I_MOVNTDQA _ | I_CLFLUSH _
        -> false

    let pp_barrier_short = pp_barrier
    let reject_mixed = false

    type lannot = Plain|Atomic|NonTemporal
    let get_machsize _ = V.Cst.Scalar.machsize
    let empty_annot = Plain
    let is_atomic = function
      | Atomic -> true
      | Plain|NonTemporal -> false
    and is_nt = function
      | NonTemporal -> true
      | Plain|Atomic -> false
    let is_barrier b1 b2 = barrier_compare b1 b2 = 0

    let ifetch_value_sets = []

    let barrier_sets =
      [
        "MFENCE",is_barrier MFENCE;
        "SFENCE",is_barrier SFENCE;
        "LFENCE",is_barrier LFENCE;
      ]

    let cmo_sets = []

    let annot_sets = ["X",is_atomic; "NT",is_nt;]

    include Explicit.No
    include PteValSets.No

    let is_isync _ = false
    let pp_isync = "???"

    let pp_annot annot = match annot with
      | Atomic -> "*"
      | Plain -> ""
      | NonTemporal -> "NT"


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
      | RIP | CS | Symbolic_reg _ | Internal _ | Flag _ | XMM _ -> Warn.fatal "No size for register %s" (pp_reg r)

    let mem_access_size = function
      | I_NOP | I_RET | I_JMP _ | I_JCC _ | I_LOCK _ | I_FENCE _
      | I_CLFLUSH _
      | I_MOVNTDQA _ (* twice a quad in fact *)
        -> None
      | I_EFF_OP (_, sz, _, _) | I_EFF (_, sz, _) | I_EFF_EFF (_, sz, _, _)
      | I_CMPXCHG (sz, _, _) | I_CMOVC (sz, _, _)
      | I_MOVNTI (sz,_,_)  | I_MOVD (sz,_,_)
        -> Some (inst_size_to_mach_size sz)

    include NoSemEnv

    (********************)
    (* global locations *)
    (********************)
    module V = V

    include NoLevelNorTLBI

    include ArchExtra_herd.Make (C)
              (struct

                let arch = arch

                type instr = instruction

                module V = V

                let endian = endian

                type arch_reg = reg
                let pp_reg = pp_reg
                let reg_compare = reg_compare

                let fromto_of_instr _ = None

                let get_val _ v = v

                module FaultType=FaultType.No
              end)

    module MemType=MemoryType.X86_64

    module ArchAction = struct
      type t = ClFlush of opt * location
      type v = V.v
      type loc = location
      type value_set = V.ValueSet.t
      type solution = V.solution
      type arch_lannot = lannot
      type arch_explicit = explicit

      let pp_opt = function
        | NoOpt -> ""
        | Opt -> "Opt"

      let pp (ClFlush (opt,loc)) =
        Printf.sprintf "ClFlush%s %s" (pp_opt opt) (pp_location loc)

      let get_lannot _ = Plain
      let get_explicit _ = exp_annot
      let value_of _ = None
      let read_of _ = None
      let written_of _ = None
      let location_of (ClFlush (_,loc)) = Some loc
      let is_store _ = false
      let is_load _ = false
      let get_size _ = assert false
      let get_kind _ = assert false

      let undetermined_vars (ClFlush (_,loc)) =
        undetermined_vars_in_loc loc

      let simplify_vars sol (ClFlush (opt,loc)) =
        ClFlush (opt,simplify_vars_in_loc sol loc)

      let is_opt (ClFlush (opt,_)) =
        match opt with
        | NoOpt -> false
        | Opt -> true

      let sets =
        ["ClFlush",(fun a -> not (is_opt a));
         "ClFlushOpt",(fun a -> is_opt a);]
    end

    module Barrier = struct

      type a = barrier

      let a_to_b =
        let module N = AllBarrier in
        function
        | MFENCE -> N.MFENCE
        | SFENCE -> N.SFENCE
        | LFENCE -> N.LFENCE

      let pp_isync = "???"

    end

    module CMO = Cmo.No
  end
