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

(** Union of relevant PPC, ARM, x86, and PTX barriers *)

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


module FromPPC   : functor(B:PPCBarrier.S)   -> S with type a = B.a
module FromARM   : functor(B:ARMBarrier.S)   -> S with type a = B.a
module FromX86   : functor(B:X86Barrier.S)   -> S with type a = B.a
module FromMIPS   : functor(B:MIPSBarrier.S)   -> S with type a = B.a
module FromCPP11 : functor(B:CPP11Barrier.S) -> S with type a = B.a
module FromOpenCL : functor(B:OpenCLBarrier.S) -> S with type a = B.a
module FromGPU_PTX : functor(B:GPU_PTXBarrier.S) -> S with type a = B.a
