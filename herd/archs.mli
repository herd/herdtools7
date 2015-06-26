(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

type t = [
    `X86
    | `PPC
    | `ARM
    | `AArch64
    | `MIPS
    | `LISA
    | `C ]

val tags : string list
val parse : string -> t option
val lex : string -> t
val pp : t -> string

val arm : t
val aarch64 : t
val ppc : t
val x86 : t
val mips : t
val c : t
val lisa : t
