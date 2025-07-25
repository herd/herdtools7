(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Define herd RISCV architecture *)

  open Printf

module Make (C:Arch_herd.Config) (V:Value.S) =
  struct
    include RISCVBase

    let is_amo = function
      | Amo _ -> true
      | INop|Ret|J _|Bcc _|Load _|Store _|LoadReserve _
      | OpI _|OpI2 _|OpIW _|Op _|OpW _|OpA _
      |StoreConditional _|FenceIns _
      |AUIPC _| Ext _
           -> false

    let pp_barrier_short = function
      | FenceI -> "fence.i"
      | FenceTSO -> "fence.tso"
      | Fence (a1,a2) ->  sprintf "F %s,%s" (pp_access a1) (pp_access a2)

    let reject_mixed = false

    type lannot = P of mo | X of mo
    let get_machsize _ = V.Cst.Scalar.machsize (* TODO, consider machsizes *)
    let empty_annot = P Rlx

    include Explicit.No
    include PteValSets.No

    let is_atomic = function
    | X _ -> true
    | P _ -> false
    let is_explicit = function | _ -> true
    let is_not_explicit = function | _ -> false

    let is_acquire = function
      | X Acq|P Acq -> true
      | X (Rlx|AcqRel|Rel|Sc)| P (Rlx|AcqRel|Rel) -> false
      | P Sc -> assert false

    let is_release = function
      | X Rel|P Rel -> true
      | X (Rlx|AcqRel|Acq|Sc)| P (Rlx|AcqRel|Acq) -> false
      | P Sc -> assert false

    let is_acquire_release = function
      | X AcqRel|P AcqRel -> true
      | X (Rlx|Rel|Acq|Sc)| P (Rlx|Rel|Acq) -> false
      | P Sc -> assert false

    let is_sc = function
      | X Sc -> true
      | P Sc -> assert false
      | X (Rlx|Rel|Acq|AcqRel)| P (Rlx|Rel|Acq|AcqRel) -> false

    let same_barrier b = fun c -> barrier_equal_semantics b c

    let ifetch_value_sets = []

    let barrier_sets =
      fold_barrier
        (fun f k ->
          let tag = Misc.capitalize (pp_barrier_dot f)
          and pred = same_barrier f in
          (tag,pred)::k)
        []

    let cmo_sets = []

    let annot_sets =
      ["X", is_atomic; "Acq", is_acquire; "Rel", is_release;
       "AcqRel",is_acquire_release;"Sc",is_sc]

    let isync =  FenceI
    let is_isync = same_barrier isync
    let pp_isync = Misc.capitalize (pp_barrier_dot isync)

    let pp_annot =
      let pp_mo = function
      | Rlx -> ""
      | Acq -> "Acq"
      | Rel -> "Rel"
      | AcqRel -> "AcqRel"
      | Sc -> "Sc" in
      function
      | P a ->  pp_mo a
      | X a -> sprintf "%s*" (pp_mo a)

    module V = V

    let mem_access_size = function
      | INop | Ret | OpI _ | OpI2 _ | OpIW _ | Op _ | OpW _
      | J _ | Bcc _ | FenceIns _ | OpA _ | AUIPC _
      | Ext _
        -> None
      | Load (w,_,_,_,_,_) | Store (w,_,_,_,_)
      | LoadReserve (w,_,_,_) | StoreConditional (w,_,_,_,_)
      | Amo (_,w,_,_,_,_)
        -> Some (tr_width w)

    include NoSemEnv

    include  NoLevelNorTLBI

    include ArchExtra_herd.Make(C)
        (struct

          let arch = arch

          module V = V
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

    module Barrier = AllBarrier.No(struct type a = barrier end)

    module CMO = Cmo.No
  end
