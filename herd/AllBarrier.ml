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

(* Union of relevant PPC, ARM, X86, and PTX barriers *)
module type S =
  sig
    type a (* Native arch barrier *)
    type b =
      | SYNC | LWSYNC | ISYNC | EIEIO (* PPC memory model barrier *)
      | DSB | DMB | ISB               (* ARM barrier *)
      | DSBST | DMBST
      | MFENCE | SFENCE | LFENCE      (* X86 *)
      | MEMBAR_CTA | MEMBAR_GL | MEMBAR_SYS (*PTX barriers*)
    val a_to_b : a -> b
    val pp_isync : string
  end


module FromPPC(B:PPCBarrier.S) = struct
  type a = B.a

  type b =
      | SYNC | LWSYNC | ISYNC | EIEIO (* PPC memory model barrier *)
      | DSB | DMB | ISB               (* ARM barrier *)
      | DSBST | DMBST
      | MFENCE | SFENCE | LFENCE      (* X86 *)
      | MEMBAR_CTA | MEMBAR_GL | MEMBAR_SYS (*PTX barriers*)

  let a_to_b a = match B.a_to_b a with
  | B.LWSYNC -> LWSYNC
  | B.SYNC -> SYNC
  | B.ISYNC -> ISYNC
  | B.EIEIO -> EIEIO

  let pp_isync = "isync"
end

module FromARM(AB:ARMBarrier.S) = struct
  type a = AB.a

  type b =
      | SYNC | LWSYNC | ISYNC | EIEIO (* PPC memory model barrier *)
      | DSB | DMB | ISB               (* ARM barrier *)
      | DSBST | DMBST
      | MFENCE | SFENCE | LFENCE      (* X86 *)
      | MEMBAR_CTA | MEMBAR_GL | MEMBAR_SYS (*PTX barriers*)

  let a_to_b a = match AB.a_to_b a with
  | AB.DSB ARMBase.SY -> DSB
  | AB.DMB ARMBase.SY -> DMB
  | AB.DSB ARMBase.ST -> DSBST
  | AB.DMB ARMBase.ST -> DMBST
  | AB.ISB -> ISB
  | _ -> Warn.fatal "Not implemented in AllBarriers"

  let pp_isync = "isb"
end

module FromX86(XB:X86Barrier.S) = struct

  type a = XB.a

  type b =
      | SYNC | LWSYNC | ISYNC | EIEIO (* PPC memory model barrier *)
      | DSB | DMB | ISB               (* ARM barrier *)
      | DSBST | DMBST
      | MFENCE | SFENCE | LFENCE      (* X86 *)
      | MEMBAR_CTA | MEMBAR_GL | MEMBAR_SYS (*PTX barriers*)

  let a_to_b a = match XB.a_to_b a with
  | XB.MFENCE -> MFENCE
  | XB.SFENCE -> SFENCE
  | XB.LFENCE -> LFENCE

  let pp_isync = "???"
end

module FromX86_64(XB:X86_64Barrier.S) = struct

  type a = XB.a

  type b =
      | SYNC | LWSYNC | ISYNC | EIEIO (* PPC memory model barrier *)
      | DSB | DMB | ISB               (* ARM barrier *)
      | DSBST | DMBST
      | MFENCE | SFENCE | LFENCE      (* X86 *)
      | MEMBAR_CTA | MEMBAR_GL | MEMBAR_SYS (*PTX barriers*)

  let a_to_b a = match XB.a_to_b a with
  | XB.MFENCE -> MFENCE

  let pp_isync = "???"
end

module FromMIPS(MB:MIPSBarrier.S) = struct
  type a = MB.a

  type b =
      | SYNC | LWSYNC | ISYNC | EIEIO (* PPC (MIPS) memory model barrier *)
      | DSB | DMB | ISB               (* ARM barrier *)
      | DSBST | DMBST
      | MFENCE | SFENCE | LFENCE      (* X86 *)
      | MEMBAR_CTA | MEMBAR_GL | MEMBAR_SYS (*PTX barriers*)

  let a_to_b a = match MB.a_to_b a with
  | MB.SYNC -> SYNC

  let pp_isync = "???"
end

module FromBell(BB:BellBarrier.S) = struct

  type a = BB.a

  type b =
      |SYNC | LWSYNC | ISYNC | EIEIO (* PPC memory model barrier *)
      | DSB | DMB | ISB               (* ARM barrier *)
      | DSBST | DMBST
      | MFENCE | SFENCE | LFENCE      (* X86 *)
      | MEMBAR_CTA | MEMBAR_GL | MEMBAR_SYS (*PTX barriers*)


  let a_to_b _ = assert false (* no concrete barriers in Bell *)

  let pp_isync = "???"
end
