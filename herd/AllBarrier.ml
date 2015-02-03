(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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

module FromCPP11(CB:CPP11Barrier.S) = struct

  type a = CB.a

  type b = 
      | SYNC | LWSYNC | ISYNC | EIEIO (* PPC memory model barrier *)
      | DSB | DMB | ISB               (* ARM barrier *)
      | DSBST | DMBST
      | MFENCE | SFENCE | LFENCE      (* X86 *)
      | MEMBAR_CTA | MEMBAR_GL | MEMBAR_SYS (*PTX barriers*)

  let a_to_b _ = assert false (* no barriers in CPP11 *)

  let pp_isync = "???"
end

module FromOpenCL(OB:OpenCLBarrier.S) = struct

  type a = OB.a

  type b = 
      | SYNC | LWSYNC | ISYNC | EIEIO (* PPC memory model barrier *)
      | DSB | DMB | ISB               (* ARM barrier *)
      | DSBST | DMBST
      | MFENCE | SFENCE | LFENCE      (* X86 *)
      | MEMBAR_CTA | MEMBAR_GL | MEMBAR_SYS (*PTX barriers*)

  let a_to_b _ = assert false (* no (hardware) barriers in OpenCL *)

  let pp_isync = "???"
end

module FromGPU_PTX(GB:GPU_PTXBarrier.S) = struct

  type a = GB.a

  type b = 
      | SYNC | LWSYNC | ISYNC | EIEIO (* PPC memory model barrier *)
      | DSB | DMB | ISB               (* ARM barrier *)
      | DSBST | DMBST
      | MFENCE | SFENCE | LFENCE      (* X86 *)
      | MEMBAR_CTA | MEMBAR_GL | MEMBAR_SYS (*PTX barriers*)

  let a_to_b a = match GB.a_to_b a with
  | GB.Membar GPU_PTXBase.CTA_bar -> MEMBAR_CTA
  | GB.Membar GPU_PTXBase.GL_bar  -> MEMBAR_GL
  | GB.Membar GPU_PTXBase.SYS_bar -> MEMBAR_SYS

  let pp_isync = "???"
end

