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
module FromBell : functor(B:BellBarrier.S) -> S with type a = B.a
