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
type b =
  | SYNC | LWSYNC | ISYNC | EIEIO (* PPC memory model barrier *)
  | DSB | DMB | ISB               (* ARM barrier *)
  | DSBST | DMBST
  | MFENCE | SFENCE | LFENCE      (* X86 *)

module type S =
  sig
    type a (* Native arch barrier *)
    val a_to_b : a -> b
    val pp_isync : string
  end


module No(B:sig type a end) = struct
  type a = B.a

  (* assert false below can be raised if CAV12 model is applied *)
  let a_to_b _ = assert false
  let pp_isync = "???"
end
