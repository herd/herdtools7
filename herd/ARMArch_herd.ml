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

(** Define ARM architecture *)
module Types = struct
  type annot =
      A | L | X | XL | XA | N | NoRet
  type lannot = annot
  type explicit = Exp | NExp
end

module Make (C:Arch_herd.Config) (V:Value.S) =
  struct
    include ARMBase
    let is_amo _ = false
    let pp_barrier_short = pp_barrier
    let reject_mixed = false

    include Types
    let get_machsize _ = V.Cst.Scalar.machsize (* No mixed size instruction *)
    let empty_annot = N

    include PteValSets.No

    let is_barrier b1 b2 = barrier_compare b1 b2 = 0
    let is_atomic = function
      | A | L | X | XL | XA | NoRet -> true
      | _ -> false

    let is_acquire = function
      | A | XA -> true
      | _ -> false
    let is_release = function
      | L | XL -> true
      | _ -> false

    let is_noreturn = function
      | NoRet -> true
      | _ -> false

    let ifetch_value_sets = []

    let barrier_sets =
      [
       "DMB",is_barrier (DMB SY);
       "DMB.ISH",is_barrier (DMB ISH);
       "DSB",is_barrier (DSB SY);
       "DMB.ST",is_barrier (DMB ST);
       "DSB.ST",is_barrier (DSB ST);
       "ISB", is_barrier ISB;
     ]

    let cmo_sets = []

    let annot_sets = [
      "X", is_atomic;
      "A",  is_acquire;
      "L",  is_release;
      "AL", is_acquire;
      "NoRet", is_noreturn;
    ]
    let explicit_sets = [
    ]
    let pp_explicit = function
    | Exp -> "Exp"
    | NExp -> ""

    let is_explicit_annot = function
      | Exp -> true
      | NExp -> false

    and is_not_explicit_annot = function
      | NExp -> true
      | Exp -> false

    and is_ifetch_annot _ = false

    let nexp_annot = NExp
    let exp_annot = Exp


    let is_isync = is_barrier ISB
    let pp_isync = "isb"

    let pp_annot annot = match annot with
      | A -> "Acq"
      | L -> "Rel"
      | XA -> "Acq*"
      | XL -> "Rel*"
      | NoRet -> "NoRet"
      | N -> ""
      | X -> "*"

    module V = V

    let mem_access_size = function
      | I_NOP | I_ADD _ | I_ADD3 _ | I_SUB _ | I_SUB3 _ | I_AND _ | I_ORR _
      | I_B _ | I_BX _ |  I_BEQ _ | I_BNE _ | I_CB _ | I_CMPI _ | I_ANDC _
      | I_CMP _ | I_MOVI _ | I_MOV _ | I_MOVW _ | I_MOVT _ | I_XOR _
      | I_DMB _ | I_DSB _ | I_ISB
      | I_SADD16 _ | I_SEL _
        -> None
      | I_LDR _ | I_LDREX _ | I_LDR3 _ | I_STR _ | I_STREX _ | I_STR3 _
      | I_STL _ | I_LDA _|I_LDAEX _|I_STLEX _
      | I_STR3_S _| I_LDR3_S _
      | I_LDRO _ | I_LDM2 _ | I_LDM3 _ | I_LDRD _
        -> Some MachSize.Word

    include NoSemEnv

    include NoLevelNorTLBI

    include ArchExtra_herd.Make(C)
        (struct

          let arch = arch

          module V = V

          type instr = instruction

          let endian = endian

          type arch_reg = reg
          let pp_reg = pp_reg
          let reg_compare = reg_compare

          let fromto_of_instr _ = None

          let get_val _ v = v

          module FaultType=FaultType.No
        end)

    module MemType=MemoryType.No

    module NoConf = struct
      type v = V.v
      type loc = location
      type value_set = V.ValueSet.t
      type solution = V.solution
      type arch_lannot = lannot
      type arch_explicit = explicit
    end

    module ArchAction = ArchAction.No(NoConf)

    module Barrier = struct
      type a = barrier

      let a_to_b =
        let module N = AllBarrier in
        function
        | DSB SY -> N.DSB
        | DMB SY -> N.DMB
        | DMB ST -> N.DMBST
        | DSB ST -> N.DSBST
        | ISB -> N.ISB
        | a ->
           Warn.fatal "Barrier %s not implemented for CAV12"
             (pp_barrier a)

      let pp_isync = "isb"

    end

    module CMO = Cmo.No
  end
