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

(** Define PPC architecture *)

module Make (C:Arch_herd.Config) (V:Value.S)
=
  struct
    include PPCBase

    let is_amo _ = false
    let pp_barrier_short = pp_barrier
    let reject_mixed = false

    type lannot = bool (* atomicity *)
    let get_machsize _ = V.Cst.Scalar.machsize
    let empty_annot = false
    let is_atomic annot = annot
    let is_barrier b1 b2 = barrier_compare b1 b2 = 0

    let barrier_sets =
      [
       "SYNC",is_barrier Sync;
       "ISYNC",is_barrier Isync;
       "LWSYNC",is_barrier Lwsync;
       "EIEIO",is_barrier Eieio;
     ]

    let annot_sets = ["X",is_atomic]

    let is_isync = is_barrier Isync
    let pp_isync = "isync"

    let pp_annot annot =
      if annot then "*" else ""

(* Now global locations, that include reservations *)
    module V = V

    let mem_access_size = function
      | Pnop
      | Padd _ | Psub _ | Psubf _ | Por _
      | Pand _ | Pxor _ | Pmull _ | Pdiv _
      | Paddi _ | Pori _ | Pandi _ | Pxori _ | Pmulli _
      | Pli _ | Pb _ | Pbcc _ | Pcmpwi _ | Pcmpw _
      | Pmr _ | Psync | Peieio | Pisync | Plwsync
      | Pdcbf _ | Pblr | Pnor _ | Pneg _ | Pslw _ 
      | Psrawi _| Psraw _ | Pbl _ | Pmtlr _ | Pmflr _
      | Plmw  _ | Pstmw _ | Pcomment _
        -> None
      | Plwzu _ | Pstwu _ | Plwarx _ | Pstwcx _
        -> Some MachSize.Word
      | Pload (sz,_,_,_) | Ploadx (sz,_,_,_)
      | Pstore (sz,_,_,_) | Pstorex (sz,_,_,_)
        -> Some sz
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
