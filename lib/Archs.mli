(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Implemented archituctures *)

module System : sig

  (* Native architectures *)
    type arch = [
    | `AArch64
    | `ARM
    | `MIPS
    | `PPC
    | `X86
    | `RISCV
    | `X86_64
    ]

  (* Native architecture may be unknown, some features
     will notbe available *)
    type t = [ arch | `Unknown ]

    val tags : string list

    val parse : string -> t option

    val pp : t -> string

end

(* All implement architectures *)
type t = [
    System.arch
  | `C
  | `CPP
  | `LISA
  ]

val tags : string list

val parse : string -> t option
val pp : t -> string

val compare : t -> t -> int

val  aarch64 : t
val  arm : t
val  mips : t
val  ppc : t
val  x86 : t
val  riscv : t
val  c : t
val  cpp : t
val  lisa : t
val  x86_64 : t

val get_sysarch : [< t ] ->  System.t -> System.t
val check_carch : [< System.t ] -> System.arch
